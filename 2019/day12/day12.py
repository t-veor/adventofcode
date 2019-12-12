#!/usr/bin/env python3
import sys
import math

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    filename = "input.txt"

pos = []
vel = []
for l in open(filename).read().splitlines():
    t = l[1:-1].split(", ")
    x, y, z = [i.split("=")[1] for i in t]
    pos.append((int(x), int(y), int(z)))
    vel.append((0, 0, 0))
pos = tuple(pos)
vel = tuple(vel)


def sgn(x):
    if x == 0:
        return 0
    elif x > 0:
        return 1
    else:
        return -1


def step(pos, vel):
    new_pos, new_vel = [], []
    for i in range(len(pos)):
        x, y, z = pos[i]
        vx, vy, vz = vel[i]
        for x_, y_, z_ in pos:
            vx += sgn(x_ - x)
            vy += sgn(y_ - y)
            vz += sgn(z_ - z)
        x += vx
        y += vy
        z += vz
        new_pos.append((x, y, z))
        new_vel.append((vx, vy, vz))
    return tuple(new_pos), tuple(new_vel)


def energy(pos, vel):
    total = 0
    for i in range(len(pos)):
        pot = sum(abs(j) for j in pos[i])
        kin = sum(abs(j) for j in vel[i])
        total += pot * kin
    return total


def explode_axes(pos, vel):
    posx = tuple(p[0] for p in pos)
    posy = tuple(p[1] for p in pos)
    posz = tuple(p[2] for p in pos)
    velx = tuple(v[0] for v in vel)
    vely = tuple(v[1] for v in vel)
    velz = tuple(v[2] for v in vel)
    return (posx, velx), (posy, vely), (posz, velz)


def lcm(*args):
    res = 1
    for i in args:
        g = math.gcd(res, i)
        res = res * i // g
    return res


# star 1
state = pos, vel
for i in range(1000):
    state = step(*state)
print(energy(*state))

# star 2
xs, ys, zs = {}, {}, {}
periodx, periody, periodz = None, None, None
state = pos, vel
i = 0
while True:
    x, y, z = explode_axes(*state)
    if periodx is None:
        if x in xs:
            periodx = i - xs[x]
        else:
            xs[x] = i
    if periody is None:
        if y in ys:
            periody = i - ys[y]
        else:
            ys[y] = i
    if periodz is None:
        if z in zs:
            periodz = i - zs[z]
        else:
            zs[z] = i
    if all((periodx, periody, periodz)):
        break
    state = step(*state)
    i += 1
print(lcm(periodx, periody, periodz))
