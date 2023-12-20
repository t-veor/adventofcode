# Converts the input into Graphviz input to see if I can spot some patterns. 

edges = []
nodedefs = []

lines = open("input.txt").read().splitlines()
for line in lines:
    header, outputs = line.split(" -> ")
    name = header
    if header.startswith("%"):
        name = header[1:]
        nodedefs.append(f"{name} [shape=octagon];")
    elif header.startswith("&"):
        name = header[1:]
        nodedefs.append(f"{name} [shape=doublecircle];")
    elif header == "broadcaster":
        nodedefs.append(f"{name} [shape=star];")
    
    outputs = outputs.split(", ")
    if len(outputs) > 1:
        output_spec = "{" + " ".join(outputs) + "}"
    else:
        output_spec = outputs[0]
    
    edges.append(f"{name} -> {output_spec};")

print("digraph {")
for i in edges:
    print("    " + i)
print()
for i in nodedefs:
    print("    " + i)
print("}")
