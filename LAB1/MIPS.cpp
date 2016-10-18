#include <iostream>
#include <string>
#include <vector>
#include <bitset>
#include <fstream>
using namespace std;
#define ADDU 1
#define SUBU 3
#define AND 4
#define OR  5
#define NOR 7
#define LOAD 35
#define STORE 43
#define JTYPE 2
#define RTYPE 0
#define BRANCH 4
#define MemSize 65536 // memory size, in reality, the memory size should be 2^32, but for this lab, for the space resaon, we keep it as this large number, but the memory is still 32-bit addressable.

uint32_t getBits(uint32_t, uint8_t, uint8_t);
bitset<32> SignExtImm(bitset<16>);
bitset<32> ZeroExtImm(bitset<16>);
bitset<32> BranchAddr(bitset<16>);
bitset<32> JumpAddr(bitset<26>);

class RF
{
public:
    bitset<32> ReadData1, ReadData2;
    RF()
    {
        Registers.resize(32);
        Registers[0] = bitset<32> (0);
        remove( "RFresult.txt" );
    }

    void ReadWrite(bitset<5> RdReg1, bitset<5> RdReg2, bitset<5> WrtReg, bitset<32> WrtData, bitset<1> WrtEnable)
    {
        // implement the funciton by you.
        if (WrtEnable.all()) {
            Registers[WrtReg.to_ulong()] = WrtData;
        }
        else {
            ReadData1 = Registers[RdReg1.to_ulong()];
            ReadData2 = Registers[RdReg2.to_ulong()];
        }
    }

    void OutputRF()
    {
        ofstream rfout;
        rfout.open("RFresult.txt",std::ios_base::app);
        if (rfout.is_open())
        {
            rfout<<"A state of RF:"<<endl;
            for (int j = 0; j<32; j++)
            {
                rfout << Registers[j]<<endl;
            }

        }
        else cout<<"Unable to open file";
        rfout.close();
    }
private:
    vector<bitset<32> >Registers;

};

class ALU
{
public:
    bitset<32> ALUresult;
    bitset<32> ALUOperation (bitset<3> ALUOP, bitset<32> oprand1, bitset<32> oprand2)
    {
        // implement the ALU operations by you.
        switch (ALUOP.to_ulong()) {
            case ADDU:
                ALUresult = bitset<32> (oprand1.to_ulong()+oprand2.to_ulong());
                break;
            case SUBU:
                ALUresult = bitset<32> (oprand1.to_ulong()-oprand2.to_ulong());
                break;
            case AND:
                ALUresult = oprand1&oprand2;
                break;
            case OR:
                ALUresult = oprand1|oprand2;
                break;
            case NOR:
                ALUresult = ~(oprand1&oprand2);
                break;
            default:
                break;
        }
        return ALUresult;
    }
};

class INSMem
{
public:
    bitset<32> Instruction;
    // add some outputs
    bitset<6> opcode;      // 31:26
    bitset<5> rs;          // 25:21
    bitset<5> rt;          // 20:16
    bitset<5> rd;          // 15:11
    bitset<5> sha;         // 10:6
    bitset<6> fun;         // 5:0
    bitset<16> imm;       // 15:0, SignExtend
    bitset<26> addr;      // 25:0

    INSMem()
    {       IMem.resize(MemSize);
        ifstream imem;
        string line;
        int i=0;
        imem.open("imem.txt");
        if (imem.is_open())
        {
            while (getline(imem,line))
            {
                IMem[i] = bitset<8>(line);
                i++;
            }

        }
        else cout<<"Unable to open file";
        imem.close();
    }

    bitset<32> ReadMemory (bitset<32> ReadAddress)
    {
        // implement by you. (Read the byte at the ReadAddress and the following three byte).
        Instruction = bitset<32>(0);

        for (int i = 0; i < 4; ++i) {
            Instruction <<= 8;
            Instruction |= bitset<32>(IMem[ReadAddress.to_ulong()+i].to_ulong());
        }
        opcode = bitset<6> (getBits(Instruction.to_ulong(), 31, 26));
        rs = bitset<5> (getBits(Instruction.to_ulong(), 25, 21));
        rt = bitset<5> (getBits(Instruction.to_ulong(), 20, 16));
        rd = bitset<5> (getBits(Instruction.to_ulong(), 15, 11));
        sha = bitset<5> (getBits(Instruction.to_ulong(), 10, 6));
        fun = bitset<6> (getBits(Instruction.to_ulong(), 5, 0));
        imm = bitset<16> (getBits(Instruction.to_ulong(), 15, 0));
        addr = bitset<26> (getBits(Instruction.to_ulong(), 25, 0));

        return Instruction;
    }

private:
    vector<bitset<8> > IMem;

};

class DataMem
{
public:
    bitset<32> readdata;
    DataMem()
    {
        DMem.resize(MemSize);
        ifstream dmem;
        string line;
        int i=0;
        dmem.open("dmem.txt");
        if (dmem.is_open())
        {
            while (getline(dmem,line))
            {
                DMem[i] = bitset<8>(line);
                i++;
            }
        }
        else cout<<"Unable to open file";
        dmem.close();

    }
    bitset<32> MemoryAccess (bitset<32> Address, bitset<32> WriteData, bitset<1> readmem, bitset<1> writemem)
    {
        // implement by you.
        if (writemem.all()) {
            for (uint8_t i = 0; i < 4; ++i) {
                DMem[Address.to_ulong()+i] = bitset<8> (getBits(WriteData.to_ulong(), (uint8_t)31, (uint8_t)24));
                WriteData <<= 8;
            }
        }
        else {
            readdata = bitset<32> (0);

            for (uint8_t i = 0; i < 4; ++i) {
                readdata <<= 8;
                readdata |= bitset<32> (DMem[Address.to_ulong()+i].to_ulong());
            }
        }
        return readdata;
    }

    void OutputDataMem()
    {
        ofstream dmemout;
        remove( "dmemresult.txt" );
        dmemout.open("dmemresult.txt");
        if (dmemout.is_open())
        {
            for (int j = 0; j< 1000; j++)
            {
                dmemout << DMem[j]<<endl;
            }

        }
        else cout<<"Unable to open file";
        dmemout.close();

    }

private:
    vector<bitset<8> > DMem;

};

class PC
{
public:
    bitset<32> current_PC;
    PC() {
        current_PC = bitset<32>(0);
    };
    bitset<32> nextPC(bitset<32> _JumpAddr, bitset<32> _BranchAddr, bitset<2> isNext, bitset<1> isEq) {
        if (!isNext.test(0) && isEq.all()) {     // Branch
            current_PC = bitset<32> (current_PC.to_ulong()+4+_BranchAddr.to_ulong());
        }
        else if (!isNext.test(1)) {             // Jump
            current_PC = bitset<32> (current_PC.to_ulong()+4) & bitset<32>(15<<28) | _JumpAddr;
        }
        else {
            current_PC = bitset<32> (current_PC.to_ulong()+4);
        }

        return current_PC;
    };
};

class Decoder
{
public:
    bitset<1> isLoad, isStore, isI_type, isWrite;
    bitset<2> isNextPC;
    bitset<3> ALUop;
    Decoder() {
        isLoad = bitset<1> (0);
        isStore = bitset<1> (0);
        isI_type = bitset<1> (0);
        isWrite = bitset<1> (0);
        isNextPC = bitset<2> (0);
        ALUop = bitset<3> (0);
    }
    void run(bitset<6> _opcode, bitset<6> _fun) {
        isLoad = _opcode.to_ulong()==LOAD? bitset<1>(1): bitset<1>(0);
        isStore = _opcode.to_ulong()==STORE? bitset<1>(1): bitset<1>(0);
        isI_type = (_opcode.to_ulong()!=JTYPE && _opcode.to_ulong()!=RTYPE && _opcode.to_ulong()!=63)? bitset<1>(1): bitset<1>(0);
        isWrite = (isStore.none() && _opcode.to_ulong()!=JTYPE && _opcode.to_ulong()!=BRANCH)? bitset<1>(1): bitset<1>(0);
        isNextPC = bitset<2>("11");                                                     // if nextpc->pc,       isNextPC = 2b'11
        isNextPC &= (_opcode.to_ulong()==JTYPE)? bitset<2>("01"): bitset<2>("11");      // if jumpaddr->pc,     isNextPC = 2b'01
        isNextPC &= (_opcode.to_ulong()==BRANCH)? bitset<2>("10"): bitset<2>("11");     // if branchaddr->pc,   isNextPC = 2b'10
        if (_opcode.to_ulong()==RTYPE) {
            ALUop = bitset<3> (getBits(_fun.to_ulong(), 2, 0));
        }
        else if (isLoad.all() || isStore.all()) {
            ALUop = bitset<3> (ADDU);
        }
        else {
            ALUop = bitset<3> (getBits(_opcode.to_ulong(), 2, 0));
        }
    }
};


int main()
{
    PC myPC;
    RF myRF;
    ALU myALU;
    INSMem myInsMem;
    DataMem myDataMem;
    Decoder myDecoder;

    while (1)
    {
        // Fetch
        myInsMem.ReadMemory(myPC.current_PC);

        // If current insturciton is "11111111111111111111111111111111", then break;
        if (myInsMem.Instruction.all()) break;

        // decode(Read RF)
        myDecoder.run(myInsMem.opcode,
                      myInsMem.fun);

        myRF.ReadWrite(myInsMem.rs,
                       myInsMem.rt,
                       myDecoder.isI_type.all()? myInsMem.rt: myInsMem.rd,
                       myDecoder.isLoad.all()? myDataMem.readdata: myALU.ALUresult,
                       bitset<1>(0));

        // Execute
        myALU.ALUOperation(myDecoder.ALUop,
                           myRF.ReadData1,
                           myDecoder.isI_type.all()? SignExtImm(myInsMem.imm): myRF.ReadData2);

        // Read/Write Mem
        myDataMem.MemoryAccess(myALU.ALUresult,
                               myRF.ReadData2,
                               myDecoder.isLoad,
                               myDecoder.isStore);

        // Write back to RF
        myRF.ReadWrite(myInsMem.rs,
                       myInsMem.rt,
                       myDecoder.isI_type.all()? myInsMem.rt: myInsMem.rd,
                       myDecoder.isLoad.all()? myDataMem.readdata: myALU.ALUresult,
                       myDecoder.isWrite);

        myPC.nextPC(JumpAddr(myInsMem.addr),
                    BranchAddr(myInsMem.imm),
                    myDecoder.isNextPC,
                    myRF.ReadData1==myRF.ReadData2? bitset<1>(1): bitset<1>(0));

        myRF.OutputRF(); // dump RF;
    }
    myDataMem.OutputDataMem(); // dump data mem

    return 0;

}

bitset<32> SignExtImm(bitset<16> Imm) {
    bitset<32> bit32_SignExtImm(Imm.to_ulong());

    for (int i = 16; i < 32; ++i) {
        bit32_SignExtImm.set(i, Imm.test(15));
    }
    return bit32_SignExtImm;
}
bitset<32> ZeroExtImm(bitset<16> Imm) {
    bitset<32> bit32_ZeroExtImm(Imm.to_ulong());

    return bit32_ZeroExtImm;
}
bitset<32> BranchAddr(bitset<16> Imm) {
    bitset<32> bit32_BranchAddr((Imm<<2).to_ulong());

    for (int i = 18; i < 32; ++i) {
        bit32_BranchAddr.set(i, Imm.test(15));
    }
    return bit32_BranchAddr;
}
bitset<32> JumpAddr(bitset<26> Addr) {
    bitset<32> bit32_JumpAddr((Addr<<2).to_ulong());

    return bit32_JumpAddr;
}

uint32_t getBits(uint32_t _input, uint8_t _ub, uint8_t _lb) {
    bitset<32> bit32_input(_input);
    bitset<32> bit32_mask(0);

    for (size_t i = _lb; i <= _ub; ++i) {
        bit32_mask.set(i);
    }
    bit32_input &= bit32_mask;
    bit32_input >>= _lb;

    return bit32_input.to_ulong();
}
