#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include "c_6502.h"

#ifdef UNIT_TEST
int main()
{
    cpu_6502 cpu;
    // Point reset vector to 0x8000
    cpu.mem[V_RESET] = 0x00;
    cpu.mem[V_RESET + 1] = 0x80;
    // store your program at 0x8000
    cpu.mem[0x8000] = NOP;
    cpu.mem[0x8001] = JMP_INDIRECT;
    cpu.mem[0x8002] = 0x00;
    cpu.mem[0x8003] = 0x90;
    cpu.mem[0x9000] = 0x00;
    cpu.mem[0x9001] = 0xa0;
    cpu.mem[0xa000] = ADC_IMM;
    cpu.mem[0xa001] = 0x09;
    cpu.mem[0xa002] = ADC_IMM;
    cpu.mem[0xa003] = 0x05;
    cpu.mem[0xa004] = JMP_IMM;
    cpu.mem[0xa005] = 0x00;
    cpu.mem[0xa006] = 0x80;
    printf("Press enter to reset CPU: ");
    getchar();
    cpu_reset(&cpu);
    while (1)
    {
        printf("CPU Registers:\nA = 0x%02x X = 0x%02x Y = 0x%02x\nNV-BDIZCJ\n%d%d%d%d%d%d%d%d%d\nPC: 0x%04x SP: 0x%02x Next OP: 0x%02x\n", cpu.a, cpu.x, cpu.y, cpu.n, cpu.v, cpu.rsvd, cpu.b, cpu.d, cpu.i, cpu.z, cpu.c, cpu.last_jmp, cpu.pc, cpu.sp, cpu.mem[cpu.pc]);
        printf("Press enter to execute instruction: ");
        getchar();
        printf("\n\n");
        cpu_exec(&cpu);
    }
    return 0;
}
#endif

static inline void impl_adc(cpu_6502 *cpu, byte val)
{
    if (!cpu->d) // not decimal
    {
        // ref: http://teaching.idallen.com/cst8214/08w/notes/overflow.txt
        // take care of overflow flag
        byte a = cpu->a;
        printf("Operands: A = 0x%02x Imm = 0x%02x C = 0x%02x Res = ", a, val, cpu->c);
        word tmp = a + val + cpu->c;
        printf("0x%04x\n", tmp);
        byte sgna = (a & 0x80);
        byte sgn = sgna ^ (val & 0x80); // 0 if same, 1 if not
        if (!sgn)                       // same
        {
            if (sgna ^ (tmp & 0x80)) // sign of both is not the same as sign of result
                cpu->v = 1;
        }
        if (tmp & 0x100)
            cpu->c = 1;
        if (tmp & 0x80)
            cpu->n = 1;
        if (!(tmp & 0xff))
            cpu->z = 1;
        cpu->a = tmp & 0xff;
    }
    else
    {
        byte a = cpu->a;
        int num_six = (a >> 4) & 0xf;
        num_six += (val >> 4) & 0xf;
        printf("Operands: A = 0x%02x Imm = 0x%02x C = 0x%02x Six = %d Res = ", a, val, cpu->c, num_six);
        word tmp = a + val + cpu->c; //- num_six * 6; // adjust for hex addition
        int res = ((val & 0xf) + (a & 0xf) + cpu->c);
        tmp -= (res / 10) * 10;
        tmp += 0x10 * (res / 10);
        printf("0x%04x\n", tmp);
        if (tmp > 99 || tmp < 0)
            cpu->c = 1;
        if ((tmp & 0xff) == 0)
            cpu->z = 0;
        if (tmp & 0x80)
            cpu->n = 1;
        cpu->a = tmp & 0xff;
    }
}

int cpu_exec(cpu_6502 *cpu)
{
    switch (cpu->mem[cpu->pc])
    {
    case ADC_IMM:
    {
        printf("Instruction: ADC_IMM ");
        int num_cycles = 2;
        cpu->pc++; // prepare for next operand
        byte val = cpu->mem[cpu->pc];
        // do the addition, take care of BCD
        impl_adc(cpu, val);
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ADC_ZPG:
    {
        printf("Instruction: ADC_ZPG ");
        int num_cycles = 3;
        cpu->pc++; // prepare for next operand
        byte val = cpu->mem[cpu->mem[cpu->pc]];
        // do the addition, take care of BCD
        impl_adc(cpu, val);
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ADC_MEM:
    {
        printf("Instruction: ADC_MEM ");
        int num_cycles = 4;
        cpu->pc++; // prepare for next operand
        word addr = cpu->mem[cpu->pc];
        cpu->pc++;
        addr |= ((word)cpu->mem[cpu->pc]) << 8;
        byte val = cpu->mem[addr];
        // do the addition, take care of BCD
        impl_adc(cpu, val);
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ASL_ZPG:
    {
        int num_cycles = 5;
        cpu->pc++;                    // prepare for next operand
        byte val = cpu->mem[cpu->pc]; // from ZPG address
        cpu->c = (val & 0x80) >> 7;   // get bit 7 and store it in carry
        cpu->mem[cpu->pc] <<= 1;      // shift the value to left by 1
        if (!(cpu->mem[cpu->pc]))
            cpu->z = 1;
        if (cpu->mem[cpu->pc] & 0x80)
            cpu->n = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ASL_A:
    {
        int num_cycles = 2;
        cpu->c = (cpu->a & 0x80) >> 7; // get bit 7 and store it in carry
        cpu->a <<= 1;                  // shift accumulator to left by 1
        if (!(cpu->a))
            cpu->z = 1;
        if (cpu->a & 0x80)
            cpu->n = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ASL_MEM:
    {
        int num_cycles = 6;
        cpu->pc++;                     // prepare for next operand
        word addr = cpu->mem[cpu->pc]; // LO addr
        cpu->pc++;
        addr += cpu->mem[cpu->pc] * 0x16; // HI addr
        cpu->c = 0x80 & cpu->mem[addr];   // set carry
        cpu->mem[addr] <<= 1;             // shift left by 1
        byte val = cpu->mem[addr];
        if (!val) // check zero flag
            cpu->z = 1;
        if (val & 0x80) // check negative flag
            cpu->n = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case BRK:
    {
        int num_cycles = 7;
        cpu->i = 1;                                    // indicate interrupt
        word tmp = cpu->pc + 2;                        // cycle 1
        cpu->mem[cpu->sp | 0x100] = tmp & 0xff;        // 2
        cpu->sp--;                                     // 3
        cpu->mem[cpu->sp | 0x100] = (tmp >> 8) & 0xff; // 4
        cpu->sp--;                                     // 5
        cpu->mem[cpu->sp | 0x100] = cpu->sr;           // 6
        cpu->sp--;                                     // 7
        usleep(num_cycles * CPU_CYCLE_USEC);           // hacky way to do it but assume that the host CPU is much faster
        break;
    }

    case JMP_IMM:
    {
        printf("Instruction: JMP_IMM ");
        int num_cycles = 3;
        cpu->pc++;                     // prepare for next operand
        word addr = cpu->mem[cpu->pc]; // low byte of immediate addr
        cpu->pc++;
        addr += cpu->mem[cpu->pc] * 256; // high byte of immediate addr
        cpu->pc = addr;
        printf("Loc: 0x%04x\n", addr);
        cpu->last_jmp = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case JMP_INDIRECT:
    {
        printf("Instruction: JMP_INDIRECT ");
        int num_cycles = 5;
        cpu->pc++;                     // prepare for next operand
        word addr = cpu->mem[cpu->pc]; // low byte of immediate addr
        cpu->pc++;
        addr += cpu->mem[cpu->pc] * 256; // high byte of immediate addr
        printf("Addr: 0x%04x ", addr);
        addr = cpu->mem[addr] + cpu->mem[addr + 1] * 256;
        cpu->pc = addr;
        printf("Loc: 0x%04x\n", addr);
        cpu->last_jmp = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case NOP:
    {
        printf("Instruction: NOP\n");
        int num_cycles = 2;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ORA_X_IND:
    {
        int num_cycles = 6;
        cpu->pc++;                                      // prepare for next operand
        byte ll = cpu->mem[cpu->pc];                    // obtain the index
        ll += cpu->x;                                   // without carry
        word addr = cpu->mem[ll] & 0x00ff;              // get LO
        addr |= (((word)cpu->mem[ll++]) << 8) & 0xff00; // get HI, remember it is in zpg so it wraps around!
        cpu->a |= cpu->mem[addr];                       // OR the data
        if ((cpu->a) & 0x80)                            // negative flag, 2's compliment
            cpu->n = 1;
        if (!(cpu->a)) // zero flag
            cpu->z = 1;
        usleep(num_cycles * CPU_CYCLE_USEC); // hacky way to do it but assume that the host CPU is much faster
        break;
    }

    case ORA_ZPG:
    {
        int num_cycles = 3;
        cpu->pc++; // prepare for next operand
        cpu->a |= cpu->mem[cpu->pc];
        if ((cpu->a) & 0x80) // negative flag, 2's compliment
            cpu->n = 1;
        if (!(cpu->a)) // zero flag
            cpu->z = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ORA_MEM:
    {
        int num_cycles = 0;
        cpu->pc++;                     // prepare for next operand
        word addr = cpu->mem[cpu->pc]; // LO addr
        cpu->pc++;
        addr += cpu->mem[cpu->pc] * 0x16; // HI addr
        cpu->a |= cpu->mem[addr];
        if (!(cpu->a))
            cpu->z = 1;
        if (cpu->a & 0x80)
            cpu->n = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case ORA_IMM:
    {
        int num_cycles = 2;
        cpu->pc++; // prepare for next operand
        cpu->a |= cpu->mem[cpu->pc];
        if ((cpu->a) & 0x80) // negative flag, 2's compliment
            cpu->n = 1;
        if (!(cpu->a)) // zero flag
            cpu->z = 1;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    case PHP:
    {
        int num_cycles = 3;
        cpu->mem[cpu->sp | 0x100] = cpu->sr;
        cpu->sp--;
        usleep(num_cycles * CPU_CYCLE_USEC);
        break;
    }

    default:
    {
        printf("Abort trap! Instruction invalid/not implemented!\n");
        break;
    }
    }
    if (!(cpu->last_jmp))
    {
        cpu->pc++; // prepare for next instruction/operand
    }
    cpu->last_jmp = 0; // clear the jump
    return 0;
}

// set program counter to RESET vector address
void cpu_reset(cpu_6502 *cpu)
{
    cpu->sr = 0;
    cpu->sp = 0xff; // stack pointer starts at the bottom
    cpu->a = 0;
    cpu->x = 0;
    cpu->y = 0;
    cpu->last_jmp = 0;
    cpu->pc = cpu->mem[V_RESET] + cpu->mem[V_RESET + 1] * 256;
}

/*
case INSTR:
{
    int num_cycles = ;
    cpu->pc++; // prepare for next operand

    usleep(num_cycles * CPU_CYCLE_USEC);
    break;
}
*/