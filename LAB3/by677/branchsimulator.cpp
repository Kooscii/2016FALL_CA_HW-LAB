#include <iostream>
#include <fstream>
#include <stdlib.h>

#define STRONGLY_TAKEN 0x3
#define STRONGLY_NOT_TAKEN 0x0
#define WEAKLY_TAKEN 0x2
#define WEAKLY_NOT_TAKEN 0x1
#define TAKEN 0x1
#define NOT_TAKEN 0x0

using namespace std;

void predicate(int m, unsigned inst, unsigned counters[], ofstream &outFile);
void update(int m, unsigned inst, unsigned counters[], int taken);

int main(int argc, char *argv[])
{

    int m=0;
    ifstream configFile;
    ifstream traceFile;
    ofstream outFile;
    unsigned inst = 0;
    int taken = 0;
    unsigned * counters;

    if(argc!=3){
        cerr << "Usage: branchsimulator.out config.txt trace.txt" << endl;
        exit(1);
    }

    configFile.open(argv[1]);
    if(configFile.is_open()){
        configFile>>m;
    }else{
        cerr << "Unable to open config file."<<endl;
        exit(2);
    }
    unsigned counts=1u<<m;
    counters = new unsigned[counts];
    for(unsigned i=0;i<counts;i++){
        counters[i]=STRONGLY_TAKEN;
    }

    traceFile.open(argv[2]);
    outFile.open("trace.txt.out");
    if(traceFile.is_open() && outFile.is_open()){
        while(!traceFile.eof()){
	    traceFile>>hex>>inst>>taken;
	    // without this, the last line will be process twice
	    if(traceFile.eof()){
	        break;
	    }
            predicate(m, inst, counters, outFile);
            update(m, inst, counters, taken);
        }
    }else if(!traceFile.is_open()){
        cerr << "Unable to open trace file."<<endl;
        exit(3);
    }else{
        cerr << "Unable to open output trace file."<<endl;
        exit(4);
    }

    delete[] counters;
    return 0;
}

void predicate(int m, unsigned inst, unsigned counters[], ofstream &outFile)
{
    unsigned index = inst & ((1u<<m)-1);
    unsigned counter = counters[index];
    switch(counter){
    case STRONGLY_TAKEN:
    case WEAKLY_TAKEN:
        outFile<<TAKEN<<endl;
        break;
    case STRONGLY_NOT_TAKEN:
    case WEAKLY_NOT_TAKEN:
        outFile<<NOT_TAKEN<<endl;
        break;
    default:
        cerr<<"Error: invalid in counters."<<endl;
        exit(6);
    }
}

void update(int m, unsigned inst, unsigned counters[], int taken)
{
    unsigned index = inst & ((1u<<m)-1);
    unsigned counter = counters[index];
    if(taken==TAKEN){
        switch(counter){
        case STRONGLY_TAKEN:
        case WEAKLY_TAKEN:
        case WEAKLY_NOT_TAKEN:
            counters[index]=STRONGLY_TAKEN;
            break;
        case STRONGLY_NOT_TAKEN:
            counters[index]=WEAKLY_NOT_TAKEN;
            break;
        default:
            cerr<<"Error: invalid in counters."<<endl;
            exit(6);
        }
    }else if(taken==NOT_TAKEN){
        switch(counter){
        case STRONGLY_TAKEN:
            counters[index]=WEAKLY_TAKEN;
            break;
        case STRONGLY_NOT_TAKEN:
        case WEAKLY_TAKEN:
        case WEAKLY_NOT_TAKEN:
            counters[index]=STRONGLY_NOT_TAKEN;
            break;
        default:
            cerr<<"Error: invalid in counters."<<endl;
            exit(6);
        }
    }else{
        cerr<<"Error: invalid taken/not taken in trace file."<<endl;
        exit(7);
    }
}
