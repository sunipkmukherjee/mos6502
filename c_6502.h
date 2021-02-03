#include <stdint.h>

typedef uint16_t word;
typedef uint8_t byte;

const uint32_t CPU_CYCLE_USEC = 1; // 1 microseconds per cycle

const uint32_t MAX_MEM_SZ = 1024 * 64;

const word V_NMI = 0xfffa; // mem loc for NMI handler

const word V_RESET = 0xfffc; // mem loc for reset

const word V_IRQ_BRK = 0xfffe; // mem loc for IRQ



typedef struct __attribute__((packed))
{
    uint16_t pc; // program counter
    uint16_t sp; // stack pointer

    uint8_t a, x, y; // registers

    union
    {
        struct __attribute__((packed))
        {
            unsigned c : 1; // carry
            unsigned z : 1; // zero
            unsigned i : 1; // interrupt disable
            unsigned d : 1; // decimal
            unsigned b : 1; // break
            unsigned rsvd : 1;
            unsigned v : 1; // overflow
            unsigned n : 1; // negative
        };
        byte sr;
    };

    byte mem[MAX_MEM_SZ];

    byte last_jmp; // indicate if last instruction was a jump
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
 */
typedef enum
{
    /**
     * @brief Force break
     * 
     *  interrupt,                       N Z C I D V
     *  push PC+2, push SR               - - - 1 - -
     *
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * implied       BRK           00    1     7
     * 
     */
    BRK = 0x00,
    /**
     * @brief OR Memory with Accumulator
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
     * 
     * * -> Add one cycle if page boundary is crossed
     */
    ORA_X_IND = 0x01,
    ORA_ZPG = 0x05,
    ORA_IMM = 0x09,
    ORA_MEM = 0x0d,
    /**
     * @brief Shift Left One Bit (Memory or Accumulator)
     * 
     * C <- [76543210] <- 0             N Z C I D V
     *                                  + + + - - -
     *
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * accumulator   ASL A         0A    1     2
     * zeropage      ASL oper      06    2     5
     * zeropage,X    ASL oper,X    16    2     6
     * absolute      ASL oper      0E    3     6
     * absolute,X    ASL oper,X    1E    3     7
     */
    ASL_ZPG = 0x06,
    ASL_A = 0x0a,
    ASL_MEM = 0x0e,
    /**
     * @brief Push Processor Status on Stack
     * 
     * push SR                          N Z C I D V
     *                                  - - - - - -
     * 
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * implied       PHP           08    1     3
     * 
     */
    PHP = 0x08,
    /**
     * @brief ADC  Add Memory to Accumulator with Carry
     * 
     * A + M + C -> A, C                N Z C I D V
     *                                  + + + - - +
     * 
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * immidiate     ADC #oper     69    2     2
     * zeropage      ADC oper      65    2     3
     * zeropage,X    ADC oper,X    75    2     4
     * absolute      ADC oper      6D    3     4
     * absolute,X    ADC oper,X    7D    3     4*
     * absolute,Y    ADC oper,Y    79    3     4*
     * (indirect,X)  ADC (oper,X)  61    2     6
     * (indirect),Y  ADC (oper),Y  71    2     5*
     * 
     */
    ADC_IMM = 0x69,
    ADC_ZPG = 0x65,
    ADC_ZPG_X = 0x75,
    ADC_MEM = 0x6d,
    ADC_MEM_X = 0x7d,
    ADC_MEM_Y = 0x79,
    ADC_X_IND = 0x61,
    ADC_Y_IND = 0x71,
    /**
     * @brief NOP  No Operation
     * 
     *  ---                              N Z C I D V
     *                                  - - - - - -
     * 
     * addressing    assembler    opc  bytes  cyles
     * --------------------------------------------
     * implied       NOP           EA    1     2
     * 
     */
    NOP = 0xea,
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
    JMP_IMM = 0x4c,
    JMP_INDIRECT = 0x6c,

} cpu_instr;

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