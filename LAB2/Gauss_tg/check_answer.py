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

L1blocksize = 8
L1setsize = 1
L1size = 16
L2blocksize = 16
L2setsize = 4
L2size = 32

offsetbits1 = int(log(L1blocksize,2))
indexbits1 = int(log(L1size * 1024 / L1setsize, 2) - offsetbits1)
tagbits1 = int(32 - indexbits1 - offsetbits1)
offsetbits2 = int(log(L2blocksize, 2))
indexbits2 = int(log(L2size * 1024 / L2setsize, 2) - offsetbits2)
tagbits2 = int(32 - indexbits2 - offsetbits2)

op = []
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
    addr_hex = line[4:-1]
    addr_bin = bin(int(addr_hex, 16))[2:].zfill(32)
    L1_tag.append(int(addr_bin[0:tagbits1], 2))
    L1_index.append(int(addr_bin[tagbits1:tagbits1 + indexbits1], 2))
    L1_offset = int(addr_bin[tagbits1 + indexbits1:], 2)
    L2_tag.append(int(addr_bin[0:tagbits2], 2))
    L2_index.append(int(addr_bin[tagbits2:tagbits2 + indexbits2], 2))
    L2_offset = int(addr_bin[tagbits2 + indexbits2:], 2)

    o.write('%s 0x%s L1: %10s %8s %6s %4s  L2:  %10s %8s %6s %4s\n' % (
        op[-1], addr_hex.zfill(8), str(L1_tag[-1]), str(L1_index[-1]), str(L1_offset), L1[-1], str(L2_tag[-1]),
        str(L2_index[-1]), str(L2_offset),
        L2[-1]))

o.close()

n = len(op)
c1 = dict()
c2 = dict()
v1 = dict()
v2 = dict()
evi1 = dict()
evi2 = dict()

o2.write(
    (',,,%s,,after accessed,' + ',' * L1setsize + ',,%s,,after accessed,' + ',' * L2setsize + ',,\n') % ('L1', 'L2'))
o2.write('%s,%s,%s,%s,%s,%s,' % ('no', 'check', 'R/W', 'index', 'tag', 'evicted'))

way1 = tuple(['way' + str(i) for i in range(0, L1setsize)])
o2.write('%s,' * L1setsize % way1)

o2.write('%s,%s,%s,%s,%s,' % ('expect', 'answer', 'index', 'tag', 'evicted'))

way2 = tuple(['way' + str(i) for i in range(0, L2setsize)])
o2.write('%s,' * L2setsize % way2)

o2.write('%s,%s,\n' % ('expect', 'answer'))

for i in range(0, n):
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

    for way, tag in enumerate(c1[L1_index[i]]):
        if tag == L1_tag[i] and v1[L1_index[i]][way] == 1:
            flag1 = 1
            break

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
        for way, tag in enumerate(c2[L2_index[i]]):
            if tag == L2_tag[i] and v2[L2_index[i]][way] == 1:
                flag2 = 1
                break

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

    # o2.write('%d,%s,%s,%d,%d,%d,%s,%s,' % (
    #     i, chk, op[i], L1_index[i], L1_tag[i], set1[0], expc1, L1[i]))
    # o2.write('%d,%d,%d,%d,%d,%d,%s,%s,\n' % (
    #     L2_index[i], L2_tag[i], set2[0], set2[1], set2[2], set2[3], expc2, L2[i]))
    o2.write('%d,%s,%s,%d,%d,%d,' % (i, chk, op[i], L1_index[i], L1_tag[i], evi1[L1_index[i]]))
    o2.write('%d,' * L1setsize % tuple(c1[L1_index[i]]))
    o2.write('%s,%s,%d,%d,%d,' % (expc1, L1[i], L2_index[i], L2_tag[i], evi2[L2_index[i]]))
    o2.write('%d,' * L2setsize % tuple(c2[L2_index[i]]))
    o2.write('%s,%s,\n' % (expc2, L2[i]))

f.close()
