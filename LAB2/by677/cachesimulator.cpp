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
#include <bitset>

using namespace std;
//access state:
#define NA 0 // no action
#define RH 1 // read hit
#define RM 2 // read miss
#define WH 3 // Write hit
#define WM 4 // write miss




struct config{
    int L1blocksize;
    int L1setsize;
    int L1size;
    int L2blocksize;
    int L2setsize;
    int L2size;
};

/* you can define the cache class here, or design your own data structure for L1 and L2 cache */
/**
  * type default
  **/
typedef unsigned char uchar;
typedef u_int32_t u32;

/**
  * Helper function declaration
  **/
u32 addressBits(u32 address, int position, int nBits);

/**
 * class declaration
 **/

/* Abstract DataStorage, base class of Cache and Mem. Its subclasses must implement readAccess and writeAccess. */
/* This is only suitable for the emulator, not really read or write */
class DataStorage {
public:
    virtual void readAccess(u32 address)=0;
    virtual void writeAccess(u32 address)=0;
    int status(){ return m_status;}
    void reset(){ m_status=NA;}
protected:
    int m_status;
};

/* A way of the Cache */
class CacheWay: public DataStorage{
public:
    ~CacheWay();
    void initCacheWay(int blockBytes, int blockCounts);
    bool empty(u32 address);
    virtual void readAccess(u32 address);
    virtual void writeAccess(u32 address);
    bool canHit(u32 address);

    // Get specific part of address
    u32 addressTag(u32 address);
    u32 addressIndex(u32 address);
    u32 addressOffset(u32 address);

protected:
    // primitive property
    int m_blockBytes;
    int m_blockCounts;

    // address related vars, calc from primitive property
    int m_tagBitsStart;
    int m_tagBitsCount;
    int m_indexBitsStart;
    int m_indexBitsCount;
    int m_offsetBitsStart;
    int m_offsetBitsCount;

    //   We don't really need to read or write, so don't need to keep data array.
    //   uchar **m_dataArray;
    // arrays
    unsigned *m_dirty;
    unsigned *m_tagArray;
    unsigned *m_validArray;

    // Helper functions for public interface
    void setupAddressRelatedVars();
    void writeBack(u32 index);
    void updateData(u32 index);
};

/* Cache class */
class Cache: public DataStorage{
public:
    Cache(int blockSize, int setSize, int size, DataStorage *nextLevel=NULL);
    ~Cache(){ delete[] m_ways;}
    int counter(int index){
        int count=m_counter[index];
        m_counter[index]=((m_counter[index]==m_setSize-1)?0:m_counter[index]+1);
        return count;
    }
    void resetNextLevel() { m_nextLevel->reset();}
    virtual void readAccess(u32 address);
    virtual void writeAccess(u32 address);

protected:
    int m_blockSize;
    int m_setSize;
    int m_size;
    DataStorage *m_nextLevel;
    CacheWay *m_ways;
    int* m_counter; // round-robin counter,

    void createAndInitWays(int ways, int wayBlockBytes, int wayBlockCounts);
    int addressWayShared(u32 address);
    int readAddressWay(u32 address);
    int writeAddressWay(u32 address);
    void emitNextLevelRead(u32 address);
    void emitNextLevelWrite(u32 address);
};

/* Mem class */
class Mem: public DataStorage{
public:
    Mem(){ reset();}
    virtual void readAccess(u32 address);
    virtual void writeAccess(u32 address);
};

/**
 * Methods
**/
void CacheWay::setupAddressRelatedVars(){
    m_offsetBitsCount=int(log2(m_blockBytes));
    m_indexBitsCount=int(log2(m_blockCounts));
    m_tagBitsCount=32-m_offsetBitsCount-m_indexBitsCount;
    m_tagBitsStart=31;
    m_indexBitsStart=m_tagBitsStart-m_tagBitsCount;
    m_offsetBitsStart=m_offsetBitsCount-1;
}

void CacheWay::initCacheWay(int blockBytes, int blockCounts){
    reset();
    m_blockBytes=blockBytes;
    m_blockCounts=blockCounts;
    m_dirty=new unsigned[blockCounts];
    m_tagArray=new unsigned[blockCounts];
    m_validArray=new unsigned[blockCounts];
    for(int i=0;i<blockCounts;i++){
        m_dirty[i]=0;
        m_tagArray[i]=0;
        m_validArray[i]=0;
    }
    setupAddressRelatedVars();
}

CacheWay::~CacheWay(){
    delete [] m_dirty;
    delete [] m_tagArray;
    delete [] m_validArray;
}

u32 CacheWay::addressTag(u32 address){
    return addressBits(address, m_tagBitsStart, m_tagBitsCount);
}

u32 CacheWay::addressIndex(u32 address){
    return addressBits(address, m_indexBitsStart, m_indexBitsCount);
}

u32 CacheWay::addressOffset(u32 address){
    return addressBits(address, m_offsetBitsStart, m_offsetBitsCount);
}

bool CacheWay::empty(u32 address){
    u32 index=addressIndex(address);
    return !bool(m_validArray[index]);
}

/* Write back data and mark as not dirty.
 Since no actual read and write, only mark*/
void CacheWay::writeBack(u32 index){
    m_dirty[index]=0;
}

/* Update data in cache when write hit.
 Since no actual read and write, only mark dirty*/
void CacheWay::updateData(u32 index){
    m_dirty[index]=1;
}

void CacheWay::readAccess(u32 address){
    u32 tag=addressTag(address);
    u32 index=addressIndex(address);
    if(m_validArray[index]){
        // already has data
        if(m_tagArray[index]==tag){
            // match the tag, whether dirty or not we can just read, it's the newest
            m_status=RH;
        }else{
            // doesn't match the tag
            if(m_dirty[index]){
                // if dirty, we need to take up this place, so write back the data in cache way
                writeBack(index); // this actually only marks index as not dirty
            }
            // now is ready to update the cache way
            m_tagArray[index]=tag;
            m_status=RM;
        }
    }else{
        // doesn't have data
        m_validArray[index]=1;
        m_tagArray[index]=tag;
        m_status=RM;
    }
}

void CacheWay::writeAccess(u32 address){
    u32 tag=addressTag(address);
    u32 index=addressIndex(address);
    if(m_validArray[index]){
        // already has data
        if(m_tagArray[index]==tag){
            // write hit
            updateData(index);
            m_status=WH;
        }else{
            // doesn't match tag, miss and direct forward
            m_status=WM;
        }
    }else{
        // write miss, direct forward (by next level cache via Cache::writeAccess
        m_status=WM;
    }
}

bool CacheWay::canHit(u32 address){
    u32 tag=addressTag(address);
    unsigned index=addressIndex(address);
    if(m_validArray[index] && (m_tagArray[index]==tag)){
        return true;
    }else{
        return false;
    }
}


void Cache::createAndInitWays(int ways, int wayBlockBytes, int wayBlockCounts)
{
    reset();
    m_counter=new int[wayBlockCounts];
    m_ways=new CacheWay[ways];
    for(int i=0;i<ways;i++){
        m_counter[i]=0;
        m_ways[i].initCacheWay(wayBlockBytes,wayBlockCounts);
    }
}
int Cache::addressWayShared(u32 address){
    // if hit, directly return the way contains data
    for(int i=0;i<m_setSize;i++){
        if(m_ways[i].canHit(address)){
            return i;
        }
    }
    // if not hit and has empty way, return the next empty way
    for(int i=0;i<m_setSize;i++){
        if(m_ways[i].empty(address)){
            return i;
        }
    }
    return -1;
}

inline int Cache::writeAddressWay(u32 address){
    int way=addressWayShared(address);
    if (way>=0) {
        return way;
    }  else {
        return 0;
    }
}

inline int Cache::readAddressWay(u32 address){
    int way=addressWayShared(address);
    // not hit and no empty way, return way indicate by round-rubin counter
    if (way>=0) {
        return way;
    }  else {
        return counter(m_ways[0].addressIndex(address));
    }
}

Cache::Cache(int blockSize, int setSize, int size, DataStorage *nextLevel):
    m_blockSize(blockSize), m_setSize(setSize), m_size(size), m_nextLevel(nextLevel){
    int wayBlockCount = size*1024/(setSize*blockSize);
    createAndInitWays(setSize, blockSize, wayBlockCount);
}

void Cache::readAccess(u32 address){
    int way=readAddressWay(address);
    m_ways[way].readAccess(address);
    m_status=m_ways[way].status();
    if(m_status==RM){
        emitNextLevelRead(address);
    }else if(m_status==RH){
        resetNextLevel();
    }
}

void Cache::writeAccess(u32 address){
    int way=writeAddressWay(address);
    m_ways[way].writeAccess(address);
    m_status=m_ways[way].status();
    if(m_status==WM){
        emitNextLevelWrite(address);
    }else if(m_status==WH){
        resetNextLevel();
    }
}

inline void Cache::emitNextLevelRead(u32 address){
    m_nextLevel->readAccess(address);
}

inline void Cache::emitNextLevelWrite(u32 address){
    m_nextLevel->writeAccess(address);
}

inline void Mem::readAccess(u32 address){
    m_status = RH; // Mem read always hit.
}

inline void Mem::writeAccess(u32 address){
    m_status = WH; // Mem write always hit.
}

/**
 * Helper Functions
 **/

/* is int a the power of 2 */
inline bool is2pow(unsigned a){
    return (a>0)&&!(a&(a-1));
}

/* Ensure the config parameters are valid */
bool cacheParamCheck(int blockSize, int setSize, int size)
{
    bool correct=true;
    if(!(blockSize>0 && is2pow(blockSize))){
        cerr<<"Block size must be positive and power of 2."<<endl;
        correct=false;
    }
    if(!((setSize==0) || ((setSize>0) && is2pow(setSize)))){
        cerr<<"Set size must be 0 or a postive integer that is power of 2."<<endl;
        correct=false;
    }
    if(!(size>0)){
        cerr<<"Cache size (in KB) must be positive integer."<<endl;
        correct=false;
    }
    return correct;
}

/* Cache constructor */
Cache makeCache(int blockSize, int setSize, int size, DataStorage* nextLevel){
    if(cacheParamCheck(blockSize,setSize,size)){ // valid config
        if(setSize==0){ // fully associative
            setSize=size*1024/blockSize;
        }
        return Cache(blockSize, setSize, size, nextLevel);
    }else{
        exit(1);
    }
}

/* Get n bits from position p*/
inline u32 addressBits(u32 address, int p, int n){
    return (address>>(p+1-n))&~(~0<<n);
}

int main(int argc, char* argv[]){

    config cacheconfig;
    ifstream cache_params;
    string dummyLine;
    cache_params.open(argv[1]);
    while(!cache_params.eof())  // read config file
    {
        cache_params>>dummyLine;
        cache_params>>cacheconfig.L1blocksize;
        cache_params>>cacheconfig.L1setsize;
        cache_params>>cacheconfig.L1size;
        cache_params>>dummyLine;
        cache_params>>cacheconfig.L2blocksize;
        cache_params>>cacheconfig.L2setsize;
        cache_params>>cacheconfig.L2size;
    }

    // Implement by you:
    // initialize the hirearch cache system with those configs
    // probably you may define a Cache class for L1 and L2, or any data structure you like
    Mem mem;
    Cache cacheL2=makeCache(cacheconfig.L2blocksize,cacheconfig.L2setsize,cacheconfig.L2size,&mem);
    Cache cacheL1=makeCache(cacheconfig.L1blocksize,cacheconfig.L1setsize,cacheconfig.L1size,&cacheL2);

    int L1AcceState =0; // L1 access state variable, can be one of NA, RH, RM, WH, WM;
    int L2AcceState =0; // L2 access state variable, can be one of NA, RH, RM, WH, WM;

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
    unsigned accessaddr; // the address from the memory trace store in the bitset;

    if (traces.is_open()&&tracesout.is_open()){
        while (getline (traces,line)){   // read mem access file and access Cache

            istringstream iss(line);
            if (!(iss >> accesstype >> xaddr)) {break;}
            stringstream saddr(xaddr);
            saddr >> std::hex >> addr;
            accessaddr = unsigned (addr);

            // access the L1 and L2 Cache according to the trace;
            if (accesstype.compare("R")==0)
            {
                //Implement by you:
                // read access to the L1 Cache,
                //  and then L2 (if required),
                //  update the L1 and L2 access state variable;
                cacheL1.readAccess(accessaddr);
                L1AcceState=cacheL1.status();
                L2AcceState=cacheL2.status();
            }
            else
            {
                //Implement by you:
                // write access to the L1 Cache,
                //and then L2 (if required),
                //update the L1 and L2 access state variable;
                cacheL1.writeAccess(accessaddr);
                L1AcceState=cacheL1.status();
                L2AcceState=cacheL2.status();
            }
            tracesout<< L1AcceState << " " << L2AcceState << endl;  // Output hit/miss results for L1 and L2 to the output file;
        }
        traces.close();
        tracesout.close();
    }
    else cout<< "Unable to open trace or traceout file ";

    return 0;
}
