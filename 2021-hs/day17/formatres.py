pairs = []
for line in open("testres.txt", "r"):
    for pair in line.split():
        pairs.append(eval(pair))

def snd(pair):
    return (pair[1], pair[0])
pairs.sort(key=snd)
prevy = pairs[0][1]
for (x, y) in pairs:
    if y != prevy:
        print()
        prevy = y
    print((x, y), end="")
print()