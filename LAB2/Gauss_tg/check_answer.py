from __future__ import print_function
import os
from math import log


try:
    os.remove('trace_resolved.txt')
    os.remove('check.csv')
except:
    pass

state = ['NA', 'RH', 'RM', 'WH', 'WM']
f = open('trace.txt', 'r')
g = open('trace.txt.out', 'r')
o = open('trace_resolved.txt', 'w')
o2 = open('check.csv', 'w')

try:
    c = open('cacheconfig.txt')
    c.readline()
    L1blocksize = int(c.readline())
    L1setsize = int(c.readline())
    L1size = int(c.readline())
    c.readline()
    L2blocksize = int(c.readline())
    L2setsize = int(c.readline())
    L2size = int(c.readline())
    c.close()
except:
    print('Invalid config file')
    quit()

if L1setsize == 0:
    L1setsize = int(L1size * 1024 / L1blocksize)

if L2setsize == 0:
    L2setsize = int(L2size * 1024 / L2blocksize)

offsetbits1 = int(log(L1blocksize, 2))
indexbits1 = int(log(L1size * 1024 / L1setsize, 2) - offsetbits1)
tagbits1 = int(32 - indexbits1 - offsetbits1)
offsetbits2 = int(log(L2blocksize, 2))
indexbits2 = int(log(L2size * 1024 / L2setsize, 2) - offsetbits2)
tagbits2 = int(32 - indexbits2 - offsetbits2)

op = []
addr_hex = []
L1_tag = []
L1_index = []
L2_tag = []
L2_index = []
L1 = []
L2 = []

for line in f:
    gline = g.readline()
    L1.append(state[int(gline[0])])
    L2.append(state[int(gline[2])])

    op.append(line[0])
    addr_hex.append(line[4:-1])
    addr_bin = bin(int(addr_hex[-1], 16))[2:].zfill(32)
    L1_tag.append(int(addr_bin[0:tagbits1].zfill(32), 2))
    L1_index.append(int(addr_bin[tagbits1:tagbits1 + indexbits1].zfill(32), 2))
    L1_offset = int(addr_bin[tagbits1 + indexbits1:].zfill(32), 2)
    L2_tag.append(int(addr_bin[0:tagbits2].zfill(32), 2))
    L2_index.append(int(addr_bin[tagbits2:tagbits2 + indexbits2].zfill(32), 2))
    L2_offset = int(addr_bin[tagbits2 + indexbits2:].zfill(32), 2)

    o.write('%s 0x%s L1: %10s %8s %6s %4s  L2:  %10s %8s %6s %4s\n' %
            (op[-1], addr_hex[-1].zfill(8), str(L1_tag[-1]), str(L1_index[-1]),
             str(L1_offset), L1[-1], str(L2_tag[-1]), str(L2_index[-1]),
             str(L2_offset), L2[-1]))

o.close()

n = len(op)
c1 = dict()
c2 = dict()
v1 = dict()
v2 = dict()
evi1 = dict()
evi2 = dict()

o2.write(',,,,L1,,after accessed,')
if L1setsize > 16:
    o2.write(',')
else:
    o2.write(',' * L1setsize)
o2.write(',,L2,,after accessed,')

if L2setsize > 16:
    o2.write(',,,\n')
else:
    o2.write(',' * L2setsize + ',,\n')

o2.write('%s,%s,%s,%s,%s,%s,%s,' % ('no', 'check', 'R/W', 'addr', 'index', 'tag', 'evicted'))
if L1setsize > 16:
    o2.write('ways,')
else:
    way1 = tuple(['way' + str(i) for i in range(0, L1setsize)])
    o2.write('%s,' * L1setsize % way1)
o2.write('%s,%s,%s,%s,%s,' % ('expect', 'answer', 'index', 'tag', 'evicted'))
if L2setsize > 16:
    o2.write('ways,')
else:
    way2 = tuple(['way' + str(i) for i in range(0, L2setsize)])
    o2.write('%s,' * L2setsize % way2)
o2.write('%s,%s,\n' % ('expect', 'answer'))

all_correct = True
for i in range(0, n):
    loading = 'Checked: %d/%d'%(i+1,n)
    print (loading, end='\r')

    flag1 = 0
    flag2 = 0
    expc1 = 'NA'
    expc2 = 'NA'
    chk = ''
    # set1 = []
    # set2 = []
    # check L1
    try:
        c1[L1_index[i]]
    except:
        c1[L1_index[i]] = [0] * L1setsize
        v1[L1_index[i]] = [0] * L1setsize
        # set1 = [0]
        evi1[L1_index[i]] = 0

    try:
        c2[L2_index[i]]
    except:
        c2[L2_index[i]] = [0] * L2setsize
        v2[L2_index[i]] = [0] * L2setsize
        # set2 = [0, 0, 0, 0]
        evi2[L2_index[i]] = 0

    way = [w for w, tag in enumerate(c1[L1_index[i]]) if tag == L1_tag[i] and v1[L1_index[i]][w] == 1]
    if way != []:
        flag1 = 1

    # L1
    if flag1 == 1:
        if op[i] == 'W':
            expc1 = 'WH'
        elif op[i] == 'R':
            expc1 = 'RH'
    else:
        if op[i] == 'W':
            expc1 = 'WM'
        elif op[i] == 'R':
            expc1 = 'RM'
            c1[L1_index[i]][evi1[L1_index[i]]] = L1_tag[i]
            v1[L1_index[i]][evi1[L1_index[i]]] = 1
            evi1[L1_index[i]] = (evi1[L1_index[i]] + 1) % L1setsize

    # L2
    if flag1 == 0:
        way = [w for w, tag in enumerate(c2[L2_index[i]]) if tag == L2_tag[i] and v2[L2_index[i]][w] == 1]
        if way != []:
            flag2 = 1

        if flag2 == 1:
            if op[i] == 'W':
                expc2 = 'WH'
            elif op[i] == 'R':
                expc2 = 'RH'
        else:
            if op[i] == 'W':
                expc2 = 'WM'
            elif op[i] == 'R':
                expc2 = 'RM'
                c2[L2_index[i]][evi2[L2_index[i]]] = L2_tag[i]
                v2[L2_index[i]][evi2[L2_index[i]]] = 1
                evi2[L2_index[i]] = (evi2[L2_index[i]] + 1) % L2setsize

    if expc1 == L1[i] and expc2 == L2[i]:
        chk = 'Correct'
    else:
        chk = 'Wrong'
        all_correct = False

    # o2.write('%d,%s,%s,%d,%d,%d,%s,%s,' % (
    #     i, chk, op[i], L1_index[i], L1_tag[i], set1[0], expc1, L1[i]))
    # o2.write('%d,%d,%d,%d,%d,%d,%s,%s,\n' % (
    #     L2_index[i], L2_tag[i], set2[0], set2[1], set2[2], set2[3], expc2, L2[i]))
    o2.write('%d,%s,%s,%s,%d,%d,%d,' % (i, chk, op[i], '0x'+addr_hex[i].zfill(8), L1_index[i], L1_tag[i], evi1[L1_index[i]]))
    if L1setsize > 16:
        o2.write('too many,')
    else:
        o2.write('%d,' * L1setsize % tuple(c1[L1_index[i]]))
    o2.write('%s,%s,%d,%d,%d,' % (expc1, L1[i], L2_index[i], L2_tag[i], evi2[L2_index[i]]))
    if L2setsize > 16:
        o2.write('too many,')
    else:
        o2.write('%d,' * L2setsize % tuple(c2[L2_index[i]]))
    o2.write('%s,%s,\n' % (expc2, L2[i]))

f.close()

if all_correct:
    print('\nAll correct.')
else:
    print('\nSomething wrong.')
