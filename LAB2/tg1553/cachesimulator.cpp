/* Copyright 201 tg1553 */

/*
Cache Simulator
Level one L1 and level two L2 cache parameters are read from file (block size, line per set and set per cache).
The 32 bit address is divided into tag bits (t), set index bits (s) and block offset bits (b)
s = log2(#sets)   b = log2(block size)  t=32-s-b
*/
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <iomanip>
#include <stdlib.h>
#include <cmath>
#include <cstdio>
#include <bitset>
#include <stdint.h>

using namespace std;
//access state:
#define NA 0    // no action
#define RH 1    // read hit
#define RM 2    // read miss
#define WH 3    // Write hit
#define WM 4    // write miss
#define EV 5    // block evicted

struct config {
    int L1blocksize;
    int L1setsize;
    int L1size;
    int L2blocksize;
    int L2setsize;
    int L2size;
};

/* you can define the cache class here, or design your own data structure for L1 and L2 cache
class cache {
      
      }
*/
class CacheBlock {
private:
    uint32_t u32Tag;
    bool bValid;
    bool bDirty;
    int size;
    int tag_bits;

public:
    vector<uint8_t> content;

    CacheBlock() {
        u32Tag = 0;
        bValid = false;
        bDirty = false;
    }

    void setBlocksize(int blocksize) {
        content.assign(blocksize, 0);
        size = blocksize;
    }

    void setTagbits(int tagbits) {
        tag_bits = tagbits;
    }

    void setContent(vector<uint8_t> _dat) {
        content = _dat;
    }

    vector<uint8_t> getContent() {
        return content;
    }

    void setTag(uint32_t _tag) {
        u32Tag = _tag;
    }

    void setValid() {
        bValid = true;
    }

    void resetValid() {
        bValid = false;
    }

    void setDirty() {
        bDirty = true;
    }

    void resetDirty() {
        bDirty = false;
    }

    uint32_t getTag() {
        return u32Tag;
    }

    int getSize() {
        return size;
    }

    bool isValid() {
        return bValid;
    }

    bool isDirty() {
        return bDirty;
    }
};

class CacheSet {
private:
    uint32_t u32Evi_cntr;
    int size;

public:
    CacheBlock *Block;

    CacheSet() {
        u32Evi_cntr = 0;
    }

    void setSetsize(int blocksize, int setsize) {
        Block = new CacheBlock[setsize];
        for (uint32_t i = 0; i < setsize; i++) {
            Block[i].setBlocksize(blocksize);
        }
        size = setsize;
    }

    uint32_t getEvi_indx() {            // round-robin policy counter
        uint32_t tmp_evi = u32Evi_cntr;
        u32Evi_cntr += 1;
        if (u32Evi_cntr >= size)        // loop among 0 to size-0
            u32Evi_cntr = 0;
        return tmp_evi;
    }

    int getSize() {
        return size;
    }
};

class CacheLevel {
private:
    int totalbits;
    int tagbits;
    int blockbits;
    int setbits;
    int indexbits;
    vector<uint8_t> ret_block;
    uint32_t ret_tag;

public:
    CacheSet *Set;
    int state_rd;
    int state_wrt;

    CacheLevel() {
        state_rd = NA;
        state_wrt = NA;
    }

    void setLevelsize(int _size, int _blocksize, int _setsize) {
        int max_index;
        totalbits = (int) log2(_size);
        blockbits = (int) log2(_blocksize);
        setbits = (int) log2(_setsize);
        indexbits = totalbits - setbits - blockbits;
        tagbits = 32 - indexbits - blockbits;

        max_index = (int) pow(2, indexbits);
        Set = new CacheSet[max_index];
        for (uint32_t i = 0; i < max_index; i++) {
            Set[i].setSetsize(_blocksize, _setsize);
        }

        ret_block.assign((unsigned long) Set->Block->getSize(), 0);
        ret_tag = 0;
    }

    int read(uint32_t _indx, uint32_t _tag) {
        uint32_t i;

        for (i = 0; i < Set[_indx].getSize(); i++) {
            if (Set[_indx].Block[i].getTag() == _tag && Set[_indx].Block[i].isValid()) {      // read hit
                state_rd = RH;
                ret_block = Set[_indx].Block[i].content;
                return state_rd;
            }
        }
        if (i >= Set[_indx].getSize()) {        // read miss
            state_rd = RM;
        }
        return state_rd;
    }

    int allocate(uint32_t _indx, uint32_t _tag, vector<uint8_t> _data) {
        uint32_t evi_indx;
        uint32_t way;

        for (way = 0; way < Set[_indx].getSize(); way++) {        // looking for empty way
            if (!Set[_indx].Block[way].isValid()) {
                Set[_indx].Block[way].setContent(_data);
                Set[_indx].Block[way].setTag(_tag);
                Set[_indx].Block[way].setValid();
                Set[_indx].Block[way].resetDirty();
                return NA;
            }
        }

        evi_indx = Set[_indx].getEvi_indx();        // if no empty ways
        if (Set[_indx].Block[evi_indx].isDirty()) {         // while allocating, isValid or not doesn't matter at all
            ret_block = Set[_indx].Block[evi_indx].getContent();
            ret_tag = Set[_indx].Block[evi_indx].getTag();

            Set[_indx].Block[evi_indx].setContent(_data);
            Set[_indx].Block[evi_indx].setTag(_tag);
            Set[_indx].Block[evi_indx].setValid();
            Set[_indx].Block[evi_indx].resetDirty();

            return EV;
        } else {
            Set[_indx].Block[evi_indx].setContent(_data);
            Set[_indx].Block[evi_indx].setTag(_tag);
            Set[_indx].Block[evi_indx].setValid();
            Set[_indx].Block[evi_indx].resetDirty();

            return NA;
        }
    }

    int write(uint32_t _indx, uint32_t _tag, uint32_t _offset, vector<uint8_t> _data) {
        uint32_t way, i;
        int len_data;

        state_wrt = WM;
        for (way = 0; way < Set[_indx].getSize(); way++) {
            if (Set[_indx].Block[way].getTag() == _tag && Set[_indx].Block[way].isValid()) {      // write hit
                state_wrt = WH;
                break;
            }
        }
        if (state_wrt == WH) {
            Set[_indx].Block[way].setDirty();
            // size of data may shorter than current level block size
            len_data = (int) _data.size();
            _offset /= len_data;
            _offset *= len_data;        // find the starting offset of the data block
            for (i = 0; i < len_data; i++) {
                Set[_indx].Block[way].content[_offset + i] = _data[i];
            }
        }
        // otherwise, read miss
        return state_wrt;
    }

    void resetState(char wr) {
        if (wr == 'w') {
            state_wrt = NA;
        } else if (wr == 'r') {
            state_rd = NA;
        }
//        ret_block.assign((unsigned long) Set->Block->getSize(), 0);
//        ret_tag = 0;
    }

    int getState(char wr) {
        int tmp_state;
        tmp_state = wr == 'w' ? state_wrt : state_rd;
        resetState('r');
        resetState('w');
        return tmp_state;
    }

    int getBlockbits() {
        return blockbits;
    }

    int getIndexbits() {
        return indexbits;
    }

    int getTagbits() {
        return tagbits;
    }

    vector<uint8_t> getBlock() {
        return ret_block;
    }

    bitset<32> getEVTag() {
        return ret_tag;
    }
};

class Cache {
private:
    int *size;
    int *blocksize;
    int *setsize;
    int maxLevel;
    bool configvalid;
    uint32_t *index, *tag, *offset;

public:
    CacheLevel *L;

    Cache(config _cfg, int _nLevel) {      // TODO rewrite struct config
        // check if the config is valid
        configvalid = true;
        // blocksize must be greater than 0 and power of 2
        if (configvalid && (log2(_cfg.L1blocksize) != (int) log2(_cfg.L1blocksize))) configvalid = false;
        if (configvalid && (log2(_cfg.L2blocksize) != (int) log2(_cfg.L2blocksize))) configvalid = false;
        // setsize must be and power of 2 (can be 0)
        if (configvalid && (log2(_cfg.L1setsize) != (int) log2(_cfg.L1setsize)) && (_cfg.L1setsize != 0))
            configvalid = false;
        if (configvalid && (log2(_cfg.L2setsize) != (int) log2(_cfg.L2setsize)) && (_cfg.L2setsize != 0))
            configvalid = false;
        // cache size must be non-zero and greater than setsize*blocksize
        if (configvalid && (_cfg.L1size == 0)) configvalid = false;
        if (configvalid && (_cfg.L2size == 0)) configvalid = false;
        if (configvalid && (_cfg.L1size * 1024 < _cfg.L1setsize * _cfg.L1blocksize)) configvalid = false;
        if (configvalid && (_cfg.L2size * 1024 < _cfg.L2setsize * _cfg.L2blocksize)) configvalid = false;
        // L2's blocksize must be bigger than L1
        if (configvalid && (_cfg.L2blocksize < _cfg.L1blocksize)) configvalid = false;

        if (configvalid) {
            // if fully fully-associative
            if (_cfg.L1setsize == 0) {
                _cfg.L1setsize = _cfg.L1size*1024/_cfg.L1blocksize;
            }
            if (_cfg.L2setsize == 0) {
                _cfg.L2setsize = _cfg.L2size*1024/_cfg.L2blocksize;
            }

            L = new CacheLevel[_nLevel + 1];
            maxLevel = _nLevel;
            size = new int[_nLevel + 1];
            blocksize = new int[_nLevel + 1];
            setsize = new int[_nLevel + 1];
//        for (uint32_t i=0; i<_nLevel; i++) {
//            L[i].setLevelsize(_cfg[i]);
//            size = _cfg[i].size;
//            blocksize = cfg[i].blocksize;
//            setsize = cfg[i].setsize;
//        }
            L[0].setLevelsize(1, 1, 1);
            L[1].setLevelsize(_cfg.L1size * 1024, _cfg.L1blocksize, _cfg.L1setsize);
            L[2].setLevelsize(_cfg.L2size * 1024, _cfg.L2blocksize, _cfg.L2setsize);

            index = new uint32_t[_nLevel + 1];
            tag = new uint32_t[_nLevel + 1];
            offset = new uint32_t[_nLevel + 1];

            offset[0] = 0;
        }
    }

    bool isConfigValid() {
        return configvalid;
    }

    void resolveAddr(bitset<32> _addr, int n) {
        int i;

        bitset<32> mask_offset(0); // bits mask of previous level cache block offset
        for (i = 0; i < L[n].getBlockbits(); i++)
            mask_offset.set((size_t) i);
        offset[n] = (uint32_t) (_addr & mask_offset).to_ulong();      // get previous level offset

        // get index
        bitset<32> mask_index(0); // bits of Ln cache index
        for (; i < L[n].getBlockbits() + L[n].getIndexbits(); i++)
            mask_index.set((size_t) i);
        index[n] = (uint32_t) ((_addr & mask_index) >> L[n].getBlockbits()).to_ulong();      // get current level offset

        // get index
        bitset<32> mask_tag(0); // bits of Ln cache index
        for (; i < L[n].getBlockbits() + L[n].getIndexbits() + L[n].getTagbits(); i++)
            mask_tag.set((size_t) i);
        tag[n] = (uint32_t) ((_addr & mask_tag)
                >> (L[n].getBlockbits() + L[n].getIndexbits())).to_ulong();      // get current level offset

    }

    // read Ln cache, return block of data, length of block equals to previous level cache blocksize
    vector<uint8_t> read(bitset<32> _addr, int n = 1) {
        int ret;                    // returned block of data
        vector<uint8_t> ret_block;
        vector<uint8_t> recv_block;
        bitset<32> ev_addr(0);

        if (n > maxLevel) {              // if read miss in all cache level
            vector<uint8_t> mem;
            mem.assign((unsigned long) pow(2, L[n - 1].getBlockbits()), 0x55);
            return mem;                     // return block of data, length : previous level cache blocksize

        } else {                                  // access cache level
            L[n].resetState('r');
            resolveAddr(_addr, n);

            // read access
            ret = L[n].read(index[n], tag[n]);
            if (ret == RH) {     // if read hit in current level, return block
                recv_block = L[n].getBlock();

            } else {                              // if read miss, access next level and allocate when returned
                recv_block = read(_addr, n + 1);   //
                ret = L[n].allocate(index[n], tag[n], recv_block);          //
                if (ret == EV) {        // if data evicted, write back to next level
                    ev_addr = offset[n];
                    ev_addr |= bitset<32>(index[n] << L[n].getBlockbits());
                    ev_addr |= bitset<32>(L[n].getEVTag() << L[n].getBlockbits() + L[n].getIndexbits());
                    write(ev_addr, L[n].getBlock(), n + 1);
                }
                // if no data evicted, nothing to do
            }
            vector<uint8_t>::iterator it_first = recv_block.begin() + offset[n] - offset[n - 1];
            vector<uint8_t>::iterator it_last = it_first + pow(2, L[n - 1].getBlockbits());
            ret_block.assign(it_first, it_last);

            return ret_block;
        }
    }

    void write(bitset<32> _addr, vector<uint8_t> _data, int n = 1) {
        int ret;                    // returned block of data

        if (n > maxLevel) {              // if write miss in all cache level
            return;                     // write back into memory, return

        } else {
            L[n].resetState('w');
            resolveAddr(_addr, n);

            // write access
            ret = L[n].write(index[n], tag[n], offset[n], _data);
            if (ret == WM) {            // if write miss, nothing to do
                write(_addr, _data, n + 1);
            }
            // if write hit, nothing to do
            return;
        }
    }
};

int main(int argc, char *argv[]) {

    config cacheconfig;
    ifstream cache_params;
    string dummyLine;
    cache_params.open(argv[1]);
    while (!cache_params.eof())  // read config file
    {
        cache_params >> dummyLine;
        cache_params >> cacheconfig.L1blocksize;
        cache_params >> cacheconfig.L1setsize;
        cache_params >> cacheconfig.L1size;
        cache_params >> dummyLine;
        cache_params >> cacheconfig.L2blocksize;
        cache_params >> cacheconfig.L2setsize;
        cache_params >> cacheconfig.L2size;
    }



    // Implement by you:
    // initialize the hirearch cache system with those configs
    // probably you may define a Cache class for L1 and L2, or any data structure you like
    Cache myCache(cacheconfig, 2);
    vector<uint8_t> dummydata, retdata;
    dummydata.assign(1, 0x12);
    int debug_cnt = 1;

    int L1AcceState = 0; // L1 access state variable, can be one of NA, RH, RM, WH, WM;
    int L2AcceState = 0; // L2 access state variable, can be one of NA, RH, RM, WH, WM;


    ifstream traces;
    ofstream tracesout;
    string outname;
    outname = string(argv[2]) + ".out";

    traces.open(argv[2]);
    tracesout.open(outname.c_str());

    string line;
    string accesstype;  // the Read/Write access type from the memory trace;
    string xaddr;       // the address from the memory trace store in hex;
    unsigned int addr;  // the address from the memory trace store in unsigned int;        
    bitset<32> accessaddr; // the address from the memory trace store in the bitset;

    if (traces.is_open() && tracesout.is_open()) {

        if (myCache.isConfigValid()) {
            while (getline(traces, line)) {   // read mem access file and access Cache

                istringstream iss(line);
                if (!(iss >> accesstype >> xaddr)) { break; }
                stringstream saddr(xaddr);
                saddr >> std::hex >> addr;
                accessaddr = bitset<32>(addr);

#ifdef DEBUG
                debug_cnt++;
                if (debug_cnt == 1586)
                    debug_cnt = debug_cnt;
#endif

                // access the L1 and L2 Cache according to the trace;
                if (accesstype.compare("R") == 0) {
                    // Implement by you:
                    // read access to the L1 Cache,
                    // and then L2 (if required),
                    // update the L1 and L2 access state variable;
                    retdata = myCache.read(accessaddr);
                    L1AcceState = myCache.L[1].getState('r');
                    L2AcceState = myCache.L[2].getState('r');

                } else {
                    // Implement by you:
                    // write access to the L1 Cache,
                    // and then L2 (if required),
                    // update the L1 and L2 access state variable;
                    myCache.write(accessaddr, dummydata);
                    L1AcceState = myCache.L[1].getState('w');
                    L2AcceState = myCache.L[2].getState('w');

                }


                tracesout << L1AcceState << " " << L2AcceState
                          << endl;  // Output hit/miss results for L1 and L2 to the output file;
            }
        }
        else {
            tracesout << "configuration invalid"
                      << endl;
        }
        traces.close();
        tracesout.close();
    } else cout << "Unable to open trace or traceout file ";


    return 0;
}
