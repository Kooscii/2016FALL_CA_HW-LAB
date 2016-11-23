import os

try:
    os.remove('trace_out.txt')
    os.remove('check.csv')
except:
    pass

state = ['NA', 'RH', 'RM', 'WH', 'WM']
f = open('trace.txt', 'r')
g = open('trace.txt.out', 'r')
o = open('trace_out.txt', 'w')
o2 = open('check.csv', 'w')
index = 210
L1set = [0]
L2set = [0, 0, 0, 0]
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
    L1_tag.append(int(addr_bin[0:18], 2))
    L1_index.append(int(addr_bin[18:18 + 11], 2))
    L1_offset = int(addr_bin[18 + 11:], 2)
    L2_tag.append(int(addr_bin[0:19], 2))
    L2_index.append(int(addr_bin[19:19 + 9], 2))
    L2_offset = int(addr_bin[19 + 9:], 2)

    o.write('%s 0x%s L1: %10s %8s %6s %4s  L2:  %10s %8s %6s %4s\n' % (
        op[-1], addr_hex.zfill(8), str(L1_tag[-1]), str(L1_index[-1]), str(L1_offset), L1[-1], str(L2_tag[-1]),
        str(L2_index[-1]), str(L2_offset),
        L2[-1]))

o.close()

n = len(op)
c1 = dict()
c2 = dict()
evi1 = dict()
evi2 = dict()

o2.write(',,,%s,,,,,%s,,,,,\n' % ('L1', 'L2'))
o2.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,\n' % (
    'no', 'check', 'R/W', 'index', 'tag', 'way0', 'expect', 'answer', 'index', 'tag', 'way0', 'way1', 'way2', 'way3',
    'expect', 'answer'))

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
        c1[L1_index[i]] = [0]
        # set1 = [0]
        evi1[L1_index[i]] = 0

    try:
        c2[L2_index[i]]
    except:
        c2[L2_index[i]] = [0, 0, 0, 0]
        # set2 = [0, 0, 0, 0]
        evi2[L2_index[i]] = 0

    for tag in c1[L1_index[i]]:
        if tag == L1_tag[i]:
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
            evi1[L1_index[i]] = (evi1[L1_index[i]] + 1) % 1

    # L2
    if flag1 == 0:
        for tag in c2[L2_index[i]]:
            if tag == L2_tag[i]:
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
                evi2[L2_index[i]] = (evi2[L2_index[i]] + 1) % 4

    if expc1 == L1[i] and expc2 == L2[i]:
        chk = 'Correct'
    else:
        chk = 'Wrong'

    # o2.write('%d,%s,%s,%d,%d,%d,%s,%s,' % (
    #     i, chk, op[i], L1_index[i], L1_tag[i], set1[0], expc1, L1[i]))
    # o2.write('%d,%d,%d,%d,%d,%d,%s,%s,\n' % (
    #     L2_index[i], L2_tag[i], set2[0], set2[1], set2[2], set2[3], expc2, L2[i]))
    o2.write('%d,%s,%s,%d,%d,%d,%s,%s,' % (
        i, chk, op[i], L1_index[i], L1_tag[i], c1[L1_index[i]][0], expc1, L1[i]))
    o2.write('%d,%d,%d,%d,%d,%d,%s,%s,\n' % (
        L2_index[i], L2_tag[i], c2[L2_index[i]][0], c2[L2_index[i]][1], c2[L2_index[i]][2], c2[L2_index[i]][3], expc2, L2[i]))

f.close()
