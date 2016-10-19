import os
import random

try:
	os.remove("myCodeBin.txt")
	os.remove("myDataBin.txt")
except:
	pass

while (1):
	try:
		try:
			n = raw_input("Number of random numbers (default 10): ")
		except:
			n = input("Number of random numbers (default 10): ")
		n = int(n) if int(n)>0 else 10
		if (n>100):
			try:
				x = raw_input("it may take some time, are you sure (y/n):")
			except:
				x = input("it may take some time, are you sure (y/n):")
			if (x!='y'):
				int('')
		
		break
		break
	except:
		if (n==''):
			n = 10
			break
		else:
			print("please input digit only")
			pass

while (1):
	try:
		try:
			r = raw_input("Range in hex format (default ffff, max 7fffffff): ")
		except:
			r = input("Range in hex format (default ffff, max 7fffffff): ")
		r = int(r, 16) if int(r, 16)>0 else int('ffff', 16)
		if (r>int('7fffffff', 16)):
			r = int('7fffffff', 16)
		break
	except:
		if (r==''):
			r = int('ffff', 16)
			break
		else:
			print("please input hex format only")
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

data=[n, int('80000000',16)]

for i in range(0,n):
	data.append(random.randint(0, r))

f = open("myDataBin.txt", 'w')
for i in data:
	t = hex(i)[2:].zfill(8)
	f.write("{0:08b}".format(int(t[0:2],16))+"\n")
	f.write("{0:08b}".format(int(t[2:4],16))+"\n")
	f.write("{0:08b}".format(int(t[4:6],16))+"\n")
	f.write("{0:08b}".format(int(t[6:8],16))+"\n")
f.close()

