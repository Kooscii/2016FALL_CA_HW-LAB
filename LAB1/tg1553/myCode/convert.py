import os
import random
try:
	os.remove("myCodeBin.txt")
	os.remove("myDataBin.txt")
except:
	pass

codehex = []
f = open("myCodeHex.txt")
for line in f:
	codehex.append(line)
f.close()
codehex.append('ffffffff')

code=[]
for i in codehex:
	code.append("{0:08b}".format(int(i[0:2],16)))
	code.append("{0:08b}".format(int(i[2:4],16)))
	code.append("{0:08b}".format(int(i[4:6],16)))
	code.append("{0:08b}".format(int(i[6:8],16)))

f = open("myCodeBin.txt", 'w')
for i in code:
	f.write(i+'\n')
f.close()

data=[10, int('80000000',16)]

for i in range(0,10):
	data.append(random.randint(0, int("ffff",16)))

f = open("myDataBin.txt", 'w')
for i in data:
	t = hex(i)[2:].zfill(8)
	f.write("{0:08b}".format(int(t[0:2],16))+"\n")
	f.write("{0:08b}".format(int(t[2:4],16))+"\n")
	f.write("{0:08b}".format(int(t[4:6],16))+"\n")
	f.write("{0:08b}".format(int(t[6:8],16))+"\n")
f.close()

