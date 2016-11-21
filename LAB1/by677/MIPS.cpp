#include<iostream>
#include<string>
#include<vector>
#include<bitset>
#include<fstream>
using namespace std;

/* Function codes for R-Type Instructions. */
#define ADDU 1
#define SUBU 3
#define AND 4
#define OR  5
#define NOR 7
#define MemSize 65536 // memory size, in reality, the memory size should be 2^32, but for this lab, for the space resaon, we keep it as this large number, but the memory is still 32-bit addressable.

#define OPCODE_R_TYPE 0x00
#define OPCODE_ADDIU 0x09
#define OPCODE_BEQ 0x04
#define OPCODE_J 0x02
#define OPCODE_LW 0x23
#define OPCODE_SW 0x2B
#define OPCODE_HALT 0x3F

class RF
{
public:
    bitset<32> ReadData1, ReadData2;
    RF()
    {
        Registers.resize(32);
        Registers[0] = bitset<32> (0);
    }

    void ReadWrite(bitset<5> RdReg1, bitset<5> RdReg2, bitset<5> WrtReg, bitset<32> WrtData, bitset<1> WrtEnable)
    {
        if (WrtEnable.to_ulong() == 1)
        {


            Registers[WrtReg.to_ulong()] = WrtData;
        }
        else
        {



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
        switch (ALUOP.to_ulong())
        {
        case ADDU:
            ALUresult = bitset<32> ((oprand1.to_ulong() + oprand2.to_ulong()) & 0xfffffffful);
            break;
        case SUBU:
            ALUresult = bitset<32> ((oprand1.to_ulong() - oprand2.to_ulong()) & 0xfffffffful);
            break;
        case AND:
            for (int i=0; i<32; i++)
            {
                ALUresult[i] = oprand1[i] & oprand2[i];
            }
            break;
        case OR:
            for (int i=0; i<32; i++)
            {
                ALUresult[i] = oprand1[i] | oprand2[i];
            }
            break;
        case NOR:
            for (int i=0; i<32; i++)
            {
                ALUresult[i] = !(oprand1[i] | oprand2[i]);
            }
            break;
        default:
            cout << "ALU Error: Unknown instruction "<<ALUOP<<endl;
            break;
        }
        return ALUresult;
    }
};

class INSMem
{
public:
    bitset<32> Instruction;
    INSMem()
    {
        IMem.resize(MemSize);
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
        int read_address = ReadAddress.to_ulong();
        Instruction = (IMem[read_address].to_ulong() << 24) + (IMem[read_address+1].to_ulong() << 16)
                      + (IMem[read_address+2].to_ulong() << 8) + IMem[read_address+3].to_ulong();
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

        if (writemem == bitset<1>(1))
        {
            unsigned long write_data=WriteData.to_ulong();
            int address=Address.to_ulong();
            DMem[address] = ((write_data>>24) & 0xffu);
            DMem[address+1] = ((write_data>>16) & 0xffu);
            DMem[address+2] = ((write_data>>8) & 0xffu);
            DMem[address+3] = (write_data & 0xffu);
        }

        if (readmem == bitset<1>(1))
        {
            int read_address = Address.to_ulong();
            readdata =  (DMem[read_address].to_ulong() << 24) + (DMem[read_address+1].to_ulong() << 16)
                        + (DMem[read_address+2].to_ulong() << 8) + DMem[read_address+3].to_ulong();
        }
        return readdata;
    }

    void OutputDataMem()
    {
        ofstream dmemout;
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
bitset<6> opCode(bitset<32> ins)
{
    return bitset<6>((ins.to_ulong()>>26) & 0x3fu);
}
bool opCodeRTypeP(bitset<6> opCode)
{
    return (opCode.to_ulong()==OPCODE_R_TYPE);
}
bool opCodeITypeP(bitset<6> opCode)
{
    unsigned op_code = opCode.to_ulong();
    return (op_code == OPCODE_ADDIU) ||
           (op_code == OPCODE_BEQ) ||
           (op_code == OPCODE_LW) ||
           (op_code == OPCODE_SW);
}
bool opCodeJTypeP(bitset<6> opCode)
{
    unsigned op_code = opCode.to_ulong();
    return (op_code == OPCODE_J) ||
           (op_code == OPCODE_HALT);
}
bitset<5> readReg1(bitset<32> ins)
{
    return bitset<5>((ins.to_ulong()>>21)&0x1fu);
}
bitset<5> readReg2(bitset<32> ins)
{
    return bitset<5>((ins.to_ulong()>>16)&0x1fu);
}
bitset<5> readReg3(bitset<32> ins)
{
    return bitset<5>((ins.to_ulong()>>11)&0x1fu);
}
bitset<16> immediate(bitset<32> ins)
{
    return bitset<16>(ins.to_ulong()&0xffffu);
}
bitset<26> address(bitset<32> ins)
{
    return bitset<26>(ins.to_ulong()&0x3ffffffu);
}
bitset<3> aluOP(bitset<32> ins)
{
    return bitset<3>(ins.to_ulong()&0x7u);
}
void decodeIns(bitset<32> ins, RF & rf, bitset<16> & imm, bitset<26> & addr)
{
    bitset<6> op_code = opCode(ins);
    if (opCodeRTypeP(op_code))
    {
        rf.ReadWrite(readReg1(ins),readReg2(ins),0,0,0);
    }
    else if(opCodeITypeP(op_code))
    {
        rf.ReadWrite(readReg1(ins),readReg2(ins),0,0,0);
        imm = immediate(ins);
    }
    else if(opCodeJTypeP(op_code))
    {
        addr = address(ins);
    }
    else
    {
        cout << "decodeIns error: unsupported instruction"<<ins<<endl;
    }
}
void executeIns(bitset<32> ins, RF & rf, ALU & alu, bitset<16> & imm, bitset<26> & addr, bitset<32> & pc, bitset<32> & itype_rst)
{
    bitset<6> op_code = opCode(ins);
    bitset<32> next_pc(pc);
    if (opCodeRTypeP(op_code))
    {
        alu.ALUOperation(aluOP(ins), rf.ReadData1, rf.ReadData2);
    }
    else
    {
        switch(op_code.to_ulong())
        {
        case OPCODE_ADDIU:
            itype_rst = bitset<32>((long(rf.ReadData1.to_ulong()) + short(imm.to_ulong()))&0xffffffffu);
            break;
        case OPCODE_BEQ:
            if(rf.ReadData1==rf.ReadData2)
            {
                next_pc=bitset<32>(pc.to_ulong()+4+((imm.to_ulong())<<2));
            }
            break;
        case OPCODE_LW:
        case OPCODE_SW:
            // This will be processed in readwriteMem
            break;
        case OPCODE_J:
            next_pc=addr.to_ulong();
            break;
        case OPCODE_HALT:
            // it's impossible that opcode is halt, it will break while(1) before enter here
            cout<<"executeIns error: malformed halt instruction"<<endl;
            break;
        default:
            cout<<"executeIns error: unsupported opcode"<<op_code<<endl;
            break;
        }
    }
    if (next_pc == pc)
    {
        pc=bitset<32>(pc.to_ulong()+4);
    }
    else
    {
        pc=next_pc;
    }
}

void readWriteMem(bitset<32> ins, RF & rf, bitset<16> & imm, DataMem & myDataMem)
{
    bitset<6> op_code = opCode(ins);
    switch(op_code.to_ulong())
    {
    case OPCODE_LW:
        rf.ReadData2 = myDataMem.MemoryAccess(int(readReg1(ins).to_ulong())+short(imm.to_ulong()),0,1,0);
        break;
    case OPCODE_SW:
        myDataMem.MemoryAccess(int(readReg1(ins).to_ulong())+short(imm.to_ulong()),rf.ReadData2,0,1);
        break;
    }
}

void writeBackRF(bitset<32> ins, RF & rf, ALU & alu, bitset<32> & itype_rst)
{
    bitset<6> op_code = opCode(ins);
    if(opCodeRTypeP(op_code))
    {
        rf.ReadWrite(0,0,readReg3(ins),alu.ALUresult,1);
    }
    else
    {
        switch(op_code.to_ulong())
        {
        case OPCODE_ADDIU:
            rf.ReadWrite(0,0,readReg2(ins),itype_rst,1);
            break;
        case OPCODE_LW:
            rf.ReadWrite(0,0,readReg2(ins),rf.ReadData2,1);
            break;
        }
    }
}

int main()
{
    RF myRF;
    ALU myALU;
    INSMem myInsMem;
    DataMem myDataMem;
    bitset<16> imm(0);
    bitset<26> addr(0);
    bitset<32> PC(0), INS(0), ITypeResult(0);

    while (1)
    {
        // Fetch
        INS = myInsMem.ReadMemory(PC);
        // If current insturciton is "11111111111111111111111111111111", then break;
        if (INS == bitset<32>(0xfffffffful))
        {
            break;
        }
        // decode(Read RF)
        decodeIns(INS, myRF, imm,addr);
        // Execute
        executeIns(INS, myRF, myALU, imm, addr, PC, ITypeResult);
        // Read/Write Mem
        readWriteMem(INS, myRF, imm, myDataMem);
        // Write back to RF
        writeBackRF(INS, myRF, myALU, ITypeResult);
        myRF.OutputRF(); // dump RF;
    }
    myDataMem.OutputDataMem(); // dump data mem

    return 0;

}
