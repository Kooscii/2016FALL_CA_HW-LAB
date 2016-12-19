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
#include <map>

using namespace std;

class SC {
public:
    map<int,string> counter;
    SC() {
    }

    void update(int _idx, int _branch) {
        if (counter[_idx] == "ST") {
            counter[_idx] = _branch? "ST" : "WT";
        }
        else if (counter[_idx] == "WT") {
            counter[_idx] = _branch? "ST" : "SN";
        }
        else if (counter[_idx] == "SN") {
            counter[_idx] = _branch? "WN" : "SN";
        }
        else if (counter[_idx] == "WN") {
            counter[_idx] = _branch? "ST" : "SN";
        }
    }
};

int main(int argc, char *argv[]) {
    // get config m
    ifstream config;
    int m;

    config.open(argv[1]);
    config >> m;

    ifstream traces;
    ofstream tracesout;
    string line;
    unsigned int addr;
    string addr_hex;

    string branch_str;
    int branch;
    vector<int> branch_practice;
    vector<int> branch_prediction;
    int m_index;

    SC prediction;
    map<int,string>::iterator it;

    string outname;
    outname = string(argv[2]) + ".out";
    traces.open(argv[2]);
    tracesout.open(outname.c_str());

    if (traces.is_open() && tracesout.is_open()) {
        while (getline(traces, line)) {
            istringstream iss(line);
            if (!(iss >> addr_hex >> branch_str)) { break; }
            stringstream addr_str(addr_hex);
            addr_str >> std::hex >> addr;
            branch = atoi(branch_str.c_str());

            // get branch state in practice
            branch_practice.insert(branch_practice.end(), branch);

            // get branch state in prediction
            m_index = addr % (1 << m);
            // check if counter is initialized
            it = prediction.counter.find(m_index);
            if (it == prediction.counter.end()) {
                // initialize
                prediction.counter[m_index] = "ST";
                branch_prediction.insert(branch_prediction.end(), 1);
            }
            else {
                branch_str = prediction.counter[m_index];
                branch = (branch_str=="ST"||branch_str=="WT")?1:0;
                branch_prediction.insert(branch_prediction.end(), branch);
            }

            //tracesout << m_index << "," << prediction.counter[m_index] << "," << branch_practice.back() << ",";

            // update counter
            prediction.update(m_index, branch_practice.back());

            //tracesout << prediction.counter[m_index] << endl;
            tracesout << branch_prediction.back() << endl;
        }

        traces.close();
        tracesout.close();
    }
}