#!/usr/bin/env python3
# pylint: disable=import-error
import sys
from os.path import dirname, join
sys.path.append(join(dirname(__file__), ".."))

from threading import Thread, RLock, Event
from collections import deque
from time import sleep
import itertools

from intcode import IntCodeVM

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

program = [int(i) for i in open(filename).read().split(",")]


def init_host(program, address, registry, blocked, global_lock, stop_flag):
    vm = IntCodeVM(program)

    def queue_read():
        while True:
            if stop_flag.is_set():
                # just a hack to get the threads to stop without modifying the
                # intcode class :)
                vm.halted = True

            with global_lock:
                queue = registry[address]
                if len(queue):
                    value = queue.popleft()
                else:
                    value = (-1,)
                    blocked[address] = True

            yield from value

    process = vm.run(itertools.chain((address,), queue_read()))

    def exec_process():
        try:
            while True:
                if stop_flag.is_set():
                    return

                addr, x, y = next(process), next(process), next(process)
                # print("Sending", (x, y), "to", addr)

                with global_lock:
                    blocked[addr] = False
                    registry[addr].append((x, y))
        except StopIteration:
            pass

    registry[address] = deque()
    blocked[address] = False

    return Thread(target=exec_process)


def init_nat(registry, blocked, global_lock, stop_flag):
    registry[255] = deque()
    y_vals = set()

    def monitor():
        while True:
            if stop_flag.is_set():
                return

            with global_lock:
                for i in registry:
                    if i != 255:
                        if len(registry[i]) or not blocked[i]:
                            break
                else:
                    if len(registry[255]):
                        last_packet = registry[255].pop()
                        y = last_packet[1]
                        if y in y_vals:
                            print(y)
                            stop_flag.set()
                            return
                        y_vals.add(y)
                        registry[255].clear()
                        # print("NAT kicking in, sending", last_packet, "to 0")
                        registry[0].append(last_packet)
                        blocked[0] = False
                    else:
                        stop_flag.set()
                        return

    return Thread(target=monitor)


def star1():
    registry = {}
    blocked = {}
    global_lock = RLock()
    stop_flag = Event()
    threads = []
    for i in range(50):
        threads.append(
            init_host(program, i, registry, blocked, global_lock, stop_flag))
    registry[255] = deque()
    blocked[255] = False

    for i in threads:
        i.start()

    while True:
        with global_lock:
            if len(registry[255]):
                print(registry[255].popleft()[1])
                stop_flag.set()
                break


def star2():
    registry = {}
    blocked = {}
    global_lock = RLock()
    stop_flag = Event()
    threads = []
    for i in range(50):
        threads.append(
            init_host(program, i, registry, blocked, global_lock, stop_flag))
    threads.append(init_nat(registry, blocked, global_lock, stop_flag))

    for i in threads:
        i.start()


star1()
star2()
