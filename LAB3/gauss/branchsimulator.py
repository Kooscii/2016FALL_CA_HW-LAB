def hex2bin(_hex, _zlen=32):
	return bin(int(_hex,16)).zfill(_zlen)[2:]

def bin2int(_bin):
	return int(_bin,2)
		
def updatePrediction(_pred, _prac):
	if _pred == 'ST':
		_pred = 'ST' if _prac == 1 else 'WT'
	elif _pred == 'WT':
		_pred = 'ST' if _prac == 1 else 'SN'
	elif _pred == 'SN':
		_pred = 'SN' if _prac == 0 else 'WN'
	elif _pred == 'WN':
		_pred = 'SN' if _prac == 0 else 'ST'
	return _pred

config = open('config.txt','r')
m = int(config.readline())
config.close()

branch_practice = []
branch_prediction = []
Saturating_counter = dict()

csv = open('trace.csv','w')
csv.write('index,predict,branch,updated,config=%d\n'%m)

f = open('trace.txt','r')
for line in f:
	# get branch state in practice
	branch_practice.append(int(line[-2:-1]))

	# get branch state in prediction
	addr_hex = line[0:8]
	m_index = bin2int(hex2bin(addr_hex)[-m:])
	# check if Saturating_counter[m_index] is initialized
	try:
		branch_prediction.append(1 if Saturating_counter[m_index][1] == 'T' else 0)
	except:
		# initialize
		Saturating_counter[m_index] = 'ST'
		branch_prediction.append(1)


	csv.write('%d,%s,%d,'%(m_index, Saturating_counter[m_index], branch_practice[-1]))
	

	# update Saturating_counte
	Saturating_counter[m_index] = updatePrediction(Saturating_counter[m_index], branch_practice[-1])


	csv.write('%s\n'%(Saturating_counter[m_index]))


f.close()
csv.close()

o = open('trace.py.out','w')
for i in branch_prediction:
	o.write('%d\n'%i)
o.close()