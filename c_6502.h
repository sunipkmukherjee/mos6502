#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif
    typedef uint16_t word;
    typedef uint8_t byte;

    const uint32_t CPU_CYCLE_USEC = 1; // 1 microseconds per cycle

#define MAX_MEM_SZ 1024 * 64

    const word V_NMI = 0xfffa; // mem loc for NMI handler

    const word V_RESET = 0xfffc; // mem loc for reset

    const word V_IRQ_BRK = 0xfffe; // mem loc for IRQ

    const byte CYC_IN = 0;
    const byte CYC_T0 = 1;
    const byte CYC_T1 = 2;
    const byte CYC_T2 = 3;
    const byte CYC_T3 = 4;
    const byte CYC_T4 = 5;
    const byte CYC_T5 = 6;
    const byte CYC_T6 = 7;
    const byte CYC_T7 = 8;

    extern const char *CYCLE_NAME_6502[];

    typedef struct __attribute__((packed))
    {
        uint16_t pc; // program counter
        uint8_t sp;  // stack pointer

        uint8_t a, x, y; // registers

        union
        {
            struct __attribute__((packed))
            {
                unsigned c : 1; // carry
                unsigned z : 1; // zero
                unsigned i : 1; // interrupt disable, this bit is set on power up
                unsigned d : 1; // decimal
                // https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
                unsigned b : 1; // break, not physical bit. Set only when PHP/BRK occurs when pushing to SP, cleared when NMI/IRQ pushes SR to SP
                unsigned rsvd : 1;
                unsigned v : 1; // overflow
                unsigned n : 1; // negative
            };
            byte sr;
        };

        byte mem[MAX_MEM_SZ]; // memory region

        byte clk;        // clock signal
        byte nmi;        // NMI signal
        byte irq;        // IRQ signal
        byte rst;        // RST signal
        byte rw;         // Read/~Write signal (active low write)

        // internal registers and cycles
        byte instr;      // current instruction
        char cycle;      // current cycle
        char irq_cycle;  // interrupt cycle
        word infer_addr; // inferred address, temporary storage only
        word instr_ptr;  // last fetch PC, for debug
        byte tmp;        // temporary register
    } cpu_6502;

    /**
 * @brief Enumerator to translate mnemonic to machine code.
 * 
 * A -> Accumulator
 * 
 * MEM -> absolute address (OP LL HH, operand address is $HHLL)
 * 
 * MEM_X -> operand is address, effective address is address incremeted by X with carry (means high byte is affected, not the carry status)
 * 
 * MEM_Y -> operand is address, effective address is address incremeted by X with carry (means high byte is affected, not the carry status)
 * 
 * IMM -> Immediate, next byte is the operand
 * 
 * IND -> Indirect, operand is address (OP LL HH), effective address is contents of word at address
 * 
 * X_IND -> operand is zeropage address, effective address is word at LL + X, LL + X + 1, increment without carry: C.w($00LL + X)
 * 
 * Y_IND -> opr is zero page address, effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
 * 
 * REL -> branch target is PC + signed offset $BB (operand byte)
 * 
 * ZPG -> operand is zero page address
 * 
 * ZPG_X -> operand is zeropage address, effective address is ZPG incremented by X without carry
 * 
 * ZPG_Y -> operand is zeropage address, effective address is ZPG incremented by Y without carry
 * 
 *
 * MOS6502 complete instruction set, ref http://www.oxyron.de/html/opcodes02.html
 * 
 */
    typedef enum
    {
        /************ ARITHMATIC ************/
        /**
     * @brief OR Accumulator, A:= A | {adr}
     * N-----Z-
     */
        /**
     * ORA  OR Memory with Accumulator
     * 
     * A OR M -> A                      N Z C I D V
     *                                  + + - - - -
     * 
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * immidiate     ORA #oper     09    2     2
     * zeropage      ORA oper      05    2     3
     * zeropage,X    ORA oper,X    15    2     4
     * absolute      ORA oper      0D    3     4
     * absolute,X    ORA oper,X    1D    3     4*
     * absolute,Y    ORA oper,Y    19    3     4*
     * (indirect,X)  ORA (oper,X)  01    2     6
     * (indirect),Y  ORA (oper),Y  11    2     5*
     */
        ORA_IMM = 0x09,
        ORA_ZP = 0x05,
        ORA_ZPX = 0x15,
        ORA_IZX = 0x01,
        ORA_IZY = 0x11,
        ORA_ABS = 0x0d,
        ORA_ABX = 0x1d,
        ORA_ABY = 0x19,
        /**
     * @brief AND Accumulaotor, A:= A & {adr}
     * N-----Z-
     */
        AND_IMM = 0x9 + 0x20,
        AND_ZP = 0x5 + 0x20,
        AND_ZPX = 0x15 + 0x20,
        AND_IZX = 0x1 + 0x20,
        AND_IZY = 0x11 + 0x20,
        AND_ABS = 0x0d + 0x20,
        AND_ABX = 0x1d + 0x20,
        AND_ABY = 0x19 + 0x20,
        /**
     * @brief EOR, A:= A ^ {adr}
     * N-----Z-
     */
        EOR_IMM = 0x09 + 0x40,
        EOR_ZP = 0x05 + 0x40,
        EOR_ZPX = 0x15 + 0x40,
        EOR_IZX = 0x01 + 0x40,
        EOR_IZY = 0x11 + 0x40,
        EOR_ABS = 0x0d + 0x40,
        EOR_ABX = 0x1d + 0x40,
        EOR_ABY = 0x19 + 0x40,
        /**
     * @brief ADC Accumulator, A:= A + {adr}
     * NV----ZC
     */
        ADC_IMM = 0x09 + 0x60,
        ADC_ZP = 0x05 + 0x60,
        ADC_ZPX = 0x15 + 0x60,
        ADC_IZX = 0x01 + 0x60,
        ADC_IZY = 0x11 + 0x60,
        ADC_ABS = 0x0d + 0x60,
        ADC_ABX = 0x1d + 0x60,
        ADC_ABY = 0x19 + 0x60,
        /**
     * @brief SBC Accumulator, A:= A - {adr}
     * NV----ZC
     */
        SBC_IMM = 0x09 + 0xe0,
        SBC_ZP = 0x05 + 0xe0,
        SBC_ZPX = 0x15 + 0xe0,
        SBC_IZX = 0x01 + 0xe0,
        SBC_IZY = 0x11 + 0xe0,
        SBC_ABS = 0x0d + 0xe0,
        SBC_ABX = 0x1d + 0xe0,
        SBC_ABY = 0x19 + 0xe0,
        /**
     * @brief CMP Accumulator, A - {adr}
     * N-----ZC
     */
        CMP_IMM = 0x09 + 0xc0,
        CMP_ZP = 0x05 + 0xc0,
        CMP_ZPX = 0x15 + 0xc0,
        CMP_IZX = 0x01 + 0xc0,
        CMP_IZY = 0x11 + 0xc0,
        CMP_ABS = 0x0d + 0xc0,
        CMP_ABX = 0x1d + 0xc0,
        CMP_ABY = 0x19 + 0xc0,
        /**
     * @brief CPX X, X - {adr}
     * N-----ZC
     */
        CPX_IMM = 0xe0,
        CPX_ZP  = 0xe4,
        CPX_ABS = 0xec,
        /**
     * @brief CPY Y, Y - {adr}
     * N-----ZC
     */
        CPY_IMM = 0xc0,
        CPY_ZP  = 0xc4,
        CPY_ABS = 0xcc,
        /**
     * @brief DEC {adr}:= {adr} - 1
     * N-----Z-
     */
        DEC_ZP = 0xc6,
        DEC_ZPX = 0xd6,
        DEC_ABS = 0xce,
        DEC_ABX = 0xde,
        /**
     * @brief DEX X:= X - 1
     * N-----Z-
     */
        DEX = 0xca,
        /**
     * @brief DEY Y:= Y - 1
     * N-----Z-
     */
        DEY = 0x88,
        /**
     * @brief INC {adr}:= {adr} + 1
     * N-----Z-
     */
        INC_ZP = 0xc6 + 0x20,
        INC_ZPX = 0xd6 + 0x20,
        INC_ABS = 0xce + 0x20,
        INC_ABX = 0xde + 0x20,
        /**
     * @brief INX X:= X + 1
     * N-----Z-
     */
        INX = 0xe8,
        /**
     * @brief INY Y:= Y + 1
     * N-----Z-
     */
        INY = 0xc8,
        /**
     * @brief ASL, {adr}:= {adr} << 2 (shift left)
     * N-----ZC
     */
        ASL = 0x0a,
        ASL_ZP = 0x06,
        ASL_ZPX = 0x16,
        ASL_ABS = 0x0e,
        ASL_ABX = 0x1e,
        /**
     * @brief ROL, {adr}:= {adr} << 2 + C (rotate left)
     * N-----ZC
     */
        ROL = 0x0a + 0x20,
        ROL_ZP = 0x06 + 0x20,
        ROL_ZPX = 0x16 + 0x20,
        ROL_ABS = 0x0e + 0x20,
        ROL_ABX = 0x1e + 0x20,
        /**
     * @brief LSR, {adr}:= {adr} >> 2 (shift right)
     * N-----ZC
     */
        LSR = 0x0a + 0x40,
        LSR_ZP = 0x06 + 0x40,
        LSR_ZPX = 0x16 + 0x40,
        LSR_ABS = 0x0e + 0x40,
        LSR_ABX = 0x1e + 0x40,
        /**
     * @brief ROL, {adr}:= {adr} << 2 + C (rotate left)
     * N-----ZC
     */
        ROR = 0x0a + 0x60,
        ROR_ZP = 0x06 + 0x60,
        ROR_ZPX = 0x16 + 0x60,
        ROR_ABS = 0x0e + 0x60,
        ROR_ABX = 0x1e + 0x60,
        /************ ARITHMATIC ************/

        /*************** MOVE ***************/
        /**
     * @brief LDA, A:= {adr}
     * N-----Z-
     */
        LDA_IMM = 0xa9,
        LDA_ZP = 0xa5,
        LDA_ZPX = 0xb5,
        LDA_IZX = 0xa1,
        LDA_IZY = 0xb1,
        LDA_ABS = 0xad,
        LDA_ABX = 0xbd,
        LDA_ABY = 0xb9,
        /**
     * @brief STA, {adr}:= A
     * --------
     */
        STA_ZP = 0xa5 - 0x20,
        STA_ZPX = 0xb5 - 0x20,
        STA_IZX = 0xa1 - 0x20,
        STA_IZY = 0xb1 - 0x20,
        STA_ABS = 0xad - 0x20,
        STA_ABX = 0xbd - 0x20,
        STA_ABY = 0xb9 - 0x20,
        /**
     * @brief LDX, X:= {adr}
     * N-----Z-
     */
        LDX_IMM = 0xa2,
        LDX_ZP = 0xa6,
        LDX_ZPY = 0xb6,
        LDX_ABS = 0xae,
        LDX_ABY = 0xbe,
        /**
     * @brief STX, {adr}:= X
     * --------
     */
        STX_ZP = 0xa6 - 0x20,
        STX_ZPY = 0xb6 - 0x20,
        STX_ABS = 0xae - 0x20,
        /**
     * @brief LDY, Y:= {adr}
     * N-----Z-
     */
        LDY_IMM = 0xa2 - 0x02,
        LDY_ZP = 0xa6 - 0x02,
        LDY_ZPX = 0xb6 - 0x02,
        LDY_ABS = 0xae - 0x02,
        LDY_ABX = 0xbe - 0x02,
        /**
     * @brief STY, {adr}:= Y
     * --------
     */
        STY_ZP = 0xa6 - 0x22,
        STY_ZPX = 0xb6 - 0x22,
        STY_ABS = 0xae - 0x22,
        /**
     * @brief Transfer commands, T{1}{2}, {2}:= {1}
     * N-----Z-
     */
        TAX = 0xaa,       // X:= A
        TXA = TAX - 0x20, // A:= X
        TAY = 0xa8,       // Y:= A
        TYA = TAY - 0x10, // A:= Y
        TSX = 0xba,       // X:= S
        TXS = TSX - 0x20, // S:= X, no flags affected
        PLA = 0x68,       // A:= +(S)
        PHA = PLA - 0x20, // (S)-:= A, no flags affected
        PLP = 0x28,       // P:= +(S), all flags affected (push to stack)
        PHP = 0x08,       // (S) -:= P
        /*************** MOVE ***************/

        /*************** JUMP ***************/
        // Branch flags
        // no flags affected
        BPL = 0x10, // branch on N = 0
        BMI = 0x30, // branch on N = 1
        BVC = 0x50, // branch on V = 0
        BVS = 0x70, // branch on V = 1
        BCC = 0x90, // branch on C = 0
        BCS = 0xb0, // branch on C = 1
        BNE = 0xd0, // branch on Z = 0
        BEQ = 0xf0, // branch on Z = 1
        /**
     * @brief BRK, software interrupt
     * (S)-:=PC,P
     * PC:=($FFFE)
     */
        BRK = 0x00,
        /**
     * @brief RTI, return from subroutine call
     * P, PC:=+(S) [affects all flags]
     */
        RTI = 0x40,
        /**
     * @brief JSR, jump to subroutine
     * S-:=PC, PC:={adr}
     */
        JSR = 0x20,
        /**
     * @brief RTS, return from subroutine
     * PC:=+(S)
     */
        RTS = 0x60,
        /**
     * @brief JMP, jump to address
     * PC:= {adr}
     */
        /**
     * @brief JMP  Jump to New Location
     * 
     * (PC+1) -> PCL                    N Z C I D V
     * (PC+2) -> PCH                    - - - - - -
     * 
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * absolute      JMP oper      4C    3     3
     * indirect      JMP (oper)    6C    3     5
     * 
     */
        JMP_ABS = 0x4c, // absolute address
        JMP_IND = 0x6c, // indirect address
        /**
     * @brief N:=b7 V:=b6 Z:=A & {adr}
     * NV----Z-
     */
        BIT_ZP = 0x24,
        BIT_ABS = 0x2c,
        CLC = 0x18, // C:= 0
        SEC = 0x38, // C:= 1
        CLD = 0xd8, // D:= 0
        SED = 0xf8, // D:= 1
        CLI = 0x58, // I:= 0
        SEI = 0x78, // I:= 1
        CLV = 0xb8, // V:= 0
        NOP = 0xea, // no operations
        /*************** JUMP ***************/

        /************* ILLEGAL **************/

        /**
     * @brief SLO, {adr}:={adr} << 2, A:=A | {adr}
     * N-----ZC
     */
        SLO_ZP = 0x07,
        SLO_ZPX = 0x17,
        SLO_IZX = 0x03,
        SLO_IZY = 0x13,
        SLO_ABS = 0x0f,
        SLO_ABX = 0x1f,
        SLO_ABY = 0x1b,

        /**
     * @brief RLA, {adr}:={adr} rotate left, A:=A & {adr}
     * N-----ZC
     */
        RLA_ZP = 0x07 + 0x20,
        RLA_ZPX = 0x17 + 0x20,
        RLA_IZX = 0x03 + 0x20,
        RLA_IZY = 0x13 + 0x20,
        RLA_ABS = 0x0f + 0x20,
        RLA_ABX = 0x1f + 0x20,
        RLA_ABY = 0x1b + 0x20,

        /**
     * @brief SRE, {adr}:={adr} >> 2, A:=A ^ {adr}
     * N-----ZC
     */
        SRE_ZP = 0x07 + 0x40,
        SRE_ZPX = 0x17 + 0x40,
        SRE_IZX = 0x03 + 0x40,
        SRE_IZY = 0x13 + 0x40,
        SRE_ABS = 0x0f + 0x40,
        SRE_ABX = 0x1f + 0x40,
        SRE_ABY = 0x1b + 0x40,

        /**
     * @brief RRA, {adr}:={adr} rotate right, A:=A (ADC) {adr}
     * NV----ZC
     */
        RRA_ZP = 0x07 + 0x60,
        RRA_ZPX = 0x17 + 0x60,
        RRA_IZX = 0x03 + 0x60,
        RRA_IZY = 0x13 + 0x60,
        RRA_ABS = 0x0f + 0x60,
        RRA_ABX = 0x1f + 0x60,
        RRA_ABY = 0x1b + 0x60,

        /**
     * @brief SAX, {adr}:= A & X
     * --------
     */
        SAX_ZP = 0x87,
        SAX_ZPY = 0x97,
        SAX_IZX = 0x83,
        SAX_ABS = 0x8f,

        /**
     * @brief SAX, {adr}:= A & X
     * N-----Z-
     */
        LAX_ZP = 0x87 + 0x20,
        LAX_ZPY = 0x97 + 0x20,
        LAX_IZX = 0x83 + 0x20,
        LAX_IZY = 0x93 + 0x20,
        LAX_ABS = 0x8f + 0x20,
        LAX_ABY = 0x9f + 0x20,

        /**
     * @brief DCP, {adr}:={adr} - 1, A-{adr}
     * N-----ZC
     */
        DCP_ZP = 0xc7,
        DCP_ZPX = 0xd7,
        DCP_IZX = 0xc3,
        DCP_IZY = 0xd3,
        DCP_ABS = 0xcf,
        DCP_ABX = 0xdf,
        DCP_ABY = 0xdb,

        /**
     * @brief DCP, {adr}:={adr} + 1, A:= A-{adr}
     * NV----ZC
     */
        ISC_ZP = 0xc7 + 0x20,
        ISC_ZPX = 0xd7 + 0x20,
        ISC_IZX = 0xc3 + 0x20,
        ISC_IZY = 0xd3 + 0x20,
        ISC_ABS = 0xcf + 0x20,
        ISC_ABX = 0xdf + 0x20,
        ISC_ABY = 0xdb + 0x20,

        /**
     * @brief ANC, A:= A & #{imm}
     * N-----ZC
     */
        ANC = 0x0b,
        ANC_ = 0x2b,

        /**
     * @brief ALR, A:= (A & #{imm}) << 2
     * N-----ZC
     */
        ALR = 0x4b,

        /**
     * @brief ARR, A:= (A & #{imm}) >> 2
     * NV----ZC
     */
        ARR = 0x6b,

        /**
     * @brief XAA (highly unstable), A:= X & #{imm}
     * N-----Z-
     */
        XAA = 0x8b,

        /**
     * @brief LAX (highly unstable), A, X:= #{imm}
     * N-----Z-
     */
        LAX = 0xab,

        /**
     * @brief AXS, X:= A & X  - #{imm}
     * N-----ZC, CMP and DEX at same time, C set like CMP
     */
        AXS = 0xcb,

        /**
     * @brief SBC, A:= A - #{imm}
     * NV-----ZC
     */
        SBC = 0xeb,

        /**
     * @brief AHX, {adr}:= A & X & H
     * no flags affected, unstable in certain conditions
     */
        AHX_IZY = 0x93,
        AHX_ABY = 0x9f,

        /**
     * @brief SHY, {adr}:= Y & H
     * no flags affected, unstable in certain conditions
     */
        SHY = 0x9c,

        /**
     * @brief SHX, {adr}:= X & H
     * no flags affected, unstable in certain conditions
     */
        SHX = 0x9e,

        /**
     * @brief TAS, S:= A & X, {adr}:= S & H
     * 
     */
        TAS = 0x9b,

        /**
     * @brief LAS, A, X, S:= {adr} & S
     * N-----Z-
     */
        LAS = 0xbb,
        /************* ILLEGAL **************/
    } MOS6502_INSTR;

    /**
 * @brief Execute the current instruction in memory at address PC
 * 
 * @param cpu 6502 CPU with memory programmed
 * 
 * @return int Status flag of the instruction, including error codes for debug
 */
    int cpu_exec(cpu_6502 *cpu);
    /**
 * @brief Reset the 6502 CPU. Clears all registers and flags, and sets
 * PC to contents of V_RESET.
 * 
 */
    void cpu_reset(cpu_6502 *cpu);
    /**
 * @brief Assert a non-maskable interrupt (in hardware: NMI line goes high to low)
 * 
 * @param cpu pointer to 6502 struct
 */
    void cpu_nmi(cpu_6502 *cpu);
    /**
 * @brief Assert or deassert IRQ
 * 
 * @param cpu pointer to 6502 struct
 * @param val IRQ pin value (high -> no interrupt, low -> interrupt)
 */
    void cpu_irq(cpu_6502 *cpu, byte val);
#ifdef __cplusplus
}
#endif