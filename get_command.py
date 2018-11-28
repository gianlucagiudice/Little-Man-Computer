f = open("command", "r")
string = "["
for line in f.readlines():
    if (line[0] >= 'A' and line[0]<='Z'):
        line = line.split(" ")
        string += '"' + line[0] + '", '
string = string[:-2] + "]"
print(string)
