#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
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

// ref: http://forum.6502.org/viewtopic.php?f=3&t=3083&p=35119&hilit=mike+chambers#p35119, verilog code
static inline void impl_adc_sbc(cpu_6502 *cpu, byte val, bool sbc)
{
    // setup
    byte a = cpu->a;
    byte b = sbc ? ~val : val;
    byte c = cpu->c; // copy over carry flag

    // temporary storage
    byte tmp = 0x0, num = 0x0;

    if (cpu->d) // decimal mode
    {
        if (sbc) // subtraction
        {
            for (int i = 0; i < 2; i++) // 4 bit ALU
            {
                // LSN[4:0] = A[3:0] + B[3:0] + Ci;
                num = (a >> (4 * i)) + (b >> (4 * i)) + c;

                // LSN[4] & ~(LSN[3] & (LSN[2] | LSN[1]));
                c = ((num & 0x2) == 0x2);          // num[1]
                c |= ((num & 0x4) == 0x4);         // num[2]
                c &= ((num & 0x8) == 0x8);         // num[3]
                c = ((num & 0x10) == 0x10) & (~c); // num[4]

                num &= 0x0f;                                    // lower nibble only
                tmp |= (c ? (num + 0) : (num + 10)) << (4 * i); // put the nibble in proper place
            }
        }
        else // addition
        {
            for (int i = 0; i < 2; i++) // 4 bit ALU
            {
                // LSN[4:0] = A[3:0] + B[3:0] + Ci;
                num = (a >> (4 * i)) + (b >> (4 * i)) + c;

                // LSN[4] | (LSN[3] & (LSN[2] | LSN[1]));
                c = ((num & 0x2) == 0x2);    // num[1]
                c |= ((num & 0x4) == 0x4);   // num[2]
                c &= ((num & 0x8) == 0x8);   // num[3]
                c |= ((num & 0x10) == 0x10); // num[4]

                num &= 0x0f;                                   // lower nibble only
                tmp |= (c ? (num + 6) : (num + 0)) << (4 * i); // put the nibble in proper place
            }
        }
        cpu->v = (sbc ? !c : c);
    }
    else // binary mode
    {
        short tmp2 = a + b + c;
        c = tmp2 & 0x100;
        tmp = tmp2 & 0xff;
        if (tmp2 < -128 || tmp2 > 127)
            cpu->v = 1;
        else
            cpu->v = 0;
    }
    cpu->z = (tmp == 0x0);
    cpu->n = ((tmp & 0x80) == 0x80);
    cpu->c = c;
    cpu->a = tmp;
}

static inline void impl_ora(cpu_6502 *cpu, byte val)
{
    byte tmp = cpu->a | val; // perform the OR
    if (tmp & 0x80)          // top bit set
        cpu->n = 1;          // indicate negative
    else
        cpu->n = 0;
    if (tmp < cpu->a || tmp < val) // indicate result smaller than operands
        cpu->v = 1;                // overflow
    else
        cpu->v = 0;
    cpu->a = tmp;
}

static inline void impl_and(cpu_6502 *cpu, byte val)
{
    byte tmp = cpu->a & val; // perform the OR
    if (tmp & 0x80)          // top bit set
        cpu->n = 1;          // indicate negative
    else
        cpu->n = 0;
    if (tmp < cpu->a || tmp < val) // indicate result smaller than operands
        cpu->v = 1;                // overflow
    else
        cpu->v = 0;
    cpu->a = tmp;
}

static inline void impl_eor(cpu_6502 *cpu, byte val)
{
    byte tmp = cpu->a ^ val; // perform XOR
    if (tmp & 0x80)          // top bit set
        cpu->n = 1;          // indicate negative
    else
        cpu->n = 0;
    if (tmp < cpu->a || tmp < val) // indicate result smaller than operands
        cpu->v = 1;                // overflow
    else
        cpu->v = 0;
    cpu->a = tmp;
}

static inline byte impl_cmp(cpu_6502 *cpu, byte v1, byte v2)
{
    byte tmp = v1 - v2;
    cpu->c = v1 >= v2 ? 1 : 0;
    cpu->z = v1 == v2 ? 1 : 0;
    cpu->n = ((tmp & 0x80) == 0x80);
    return tmp;
}

static inline void impl_cma(cpu_6502 *cpu, byte val)
{
    cpu->a = impl_cmp(cpu, cpu->a, val);
}

static inline void impl_cmx(cpu_6502 *cpu, byte val)
{
    cpu->x = impl_cmp(cpu, cpu->x, val);
}

static inline void impl_cmy(cpu_6502 *cpu, byte val)
{
    cpu->y = impl_cmp(cpu, cpu->y, val);
}

static inline byte impl_dec(cpu_6502 *cpu, byte val)
{
    val--;
    cpu->z = (val == 0);
    cpu->n = ((val & 0x80) == 0x80);
    return val;
}

static inline byte impl_inc(cpu_6502 *cpu, byte val)
{
    val++;
    cpu->z = (val == 0);
    cpu->n = ((val & 0x80) == 0x80);
    return val;
}

static inline byte impl_ld(cpu_6502 *cpu, byte val)
{
    cpu->n = ((val & 0x80) == 0x80);
    cpu->z = (val == 0);
    return val;
}

static inline void impl_lda(cpu_6502 *cpu, byte val)
{
    cpu->a = impl_ld(cpu, val);
}

static inline void impl_ldx(cpu_6502 *cpu, byte val)
{
    cpu->x = impl_ld(cpu, val);
}

static inline void impl_ldy(cpu_6502 *cpu, byte val)
{
    cpu->y = impl_ld(cpu, val);
}

static inline byte impl_asl(cpu_6502 *cpu, byte val)
{
    cpu->c = ((val & 0x80) == 0x80);
    val = (val << 1) & 0xfe;
    cpu->n = ((val & 0x80) == 0x80);
    cpu->z = (val == 0);
    return val;
}

static inline byte impl_lsr(cpu_6502 *cpu, byte val)
{
    cpu->c = (val & 0x1);
    val = (val >> 1) & 0x7f;
    cpu->n = 0;
    cpu->z = (val == 0);
    return val;
}

static inline byte impl_rol(cpu_6502 *cpu, byte val)
{
    byte c = ((val & 0x80) == 0x80);
    val = (val << 1) & 0xfe;
    val |= cpu->c;
    cpu->c = c;
    cpu->n = ((val & 0x80) == 0x80);
    cpu->z = (val == 0);
    return val;
}

static inline byte impl_ror(cpu_6502 *cpu, byte val)
{
    byte c = (val & 0x1);
    val = (val >> 1) & 0x7f;
    val |= (cpu->c) << 7;
    cpu->n = cpu->c;
    cpu->c = c;
    cpu->z = (val == 0);
    return val;
}

int cpu_exec(cpu_6502 *cpu)
{
    if (cpu->cycle == 0) // at t = 0, fetch new instruction
    {
        if (cpu->nmi) // non maskable interrupt
        {
            if (0 == cpu->irq_cycle++)
            {
                return 1;
            }
            else if (1 == cpu->irq_cycle++)
            {
                return 1;
            }
            else if (2 == cpu->irq_cycle++)
            {
                cpu->mem[0x100 + cpu->sp] = cpu->pc; // lo byte
                cpu->sp--;
                return 1;
            }
            else if (3 == cpu->irq_cycle++)
            {
                cpu->mem[0x100 + cpu->sp] = cpu->pc >> 8; // hi byte
                cpu->sp--;
                return 1;
            }
            else if (4 == cpu->irq_cycle++)
            {
                cpu->b = 0;
                cpu->mem[0x100 + cpu->sp] = cpu->sr; // push status register
                cpu->sp--;
                return 1;
            }
            else if (5 == cpu->irq_cycle++)
            {
                cpu->pc = 0x0 | cpu->mem[V_NMI];
                return 1;
            }
            else if (6 == cpu->irq_cycle++)
            {
                cpu->pc |= ((word)cpu->mem[V_NMI + 1]) << 8;
                return 1;
            }
            else if (7 == cpu->irq_cycle)
            {
                cpu->i = 1; // disable IRQ while in NMI ISR
                cpu->b = 0; // forget break
                cpu->nmi = 0; // clear the nmi
                cpu->irq_cycle = 0; // clear interrupt cycle and fetch ISR instruction
            }
        }
        else if ((!cpu->i && cpu->irq) || cpu->b) // level triggered
        {
            if (0 == cpu->irq_cycle++)
            {
                return 1;
            }
            else if (1 == cpu->irq_cycle++)
            {
                return 1;
            }
            else if (2 == cpu->irq_cycle++)
            {
                cpu->mem[0x100 + cpu->sp] = cpu->pc; // lo byte
                cpu->sp--;
                return 1;
            }
            else if (3 == cpu->irq_cycle++)
            {
                cpu->mem[0x100 + cpu->sp] = cpu->pc >> 8; // hi byte
                cpu->sp--;
                return 1;
            }
            else if (4 == cpu->irq_cycle++)
            {
                if (!cpu->i && cpu->irq)
                    cpu->b = 0; // break bit is cleared, causes brk to be lost
                cpu->mem[0x100 + cpu->sp] = cpu->sr; // push status register
                cpu->sp--;
                return 1;
            }
            else if (5 == cpu->irq_cycle++)
            {
                cpu->pc = 0x0 | cpu->mem[V_IRQ_BRK];
                return 1;
            }
            else if (6 == cpu->irq_cycle++)
            {
                cpu->pc |= ((word)cpu->mem[V_IRQ_BRK + 1]) << 8;
                return 1;
            }
            else if (7 == cpu->irq_cycle)
            {
                cpu->i = 1; // interrupt is disabled for servicing
                cpu->b = 0; // break bit is cleared
                cpu->irq_cycle = 0; // clear interrupt cycle and fetch ISR instruction
            }
        }
        cpu->instr = cpu->mem[cpu->pc]; // fetch current instruction
    }
    switch (cpu->instr)
    {
    // KIL
    case 0x02:
    case 0x12:
    case 0x22:
    case 0x32:
    case 0x42:
    case 0x52:
    case 0x62:
    case 0x72:
    case 0x92:
    case 0xb2:
    case 0xd2:
    case 0xf2:
        cpu->halt = 1; // indicate KILL
        return -1;
        break;
    // NOP or NOP IMM
    case 0x80:
    case 0x82:
    case 0x89:
    case 0x1a:
    case 0x3a:
    case 0x5a:
    case 0x7a:
    case 0xda:
    case 0xfa:
    case 0xc2:
    case 0xe2:
    case NOP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        if (1 == cpu->cycle++)
        {
            cpu->cycle = 0;
        }
        break;
    }
    // TODO: NOP 3, 4, 4*
    /************ ARITHMATIC ************/
    case ORA_IMM:
    {
        if (0 == cpu->cycle++) // cycle 0, increment program counter, cycle counter
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_ora(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case ORA_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_ora(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ORA_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_ora(cpu, val);            // perform OR
            cpu->cycle = 0;                // clear cycles
        }
        break;
    }
    case ORA_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_ora(cpu, val);                   // perform OR
            cpu->cycle = 0;                       // clear cycles
        }
        break;
    }
    case ORA_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_ora(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_ora(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case ORA_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_ora(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_ora(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case ORA_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_ora(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case ORA_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_ora(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_ora(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }

    case AND_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_and(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case AND_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_and(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case AND_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_and(cpu, val);            // perform OR
            cpu->cycle = 0;                // clear cycles
        }
        break;
    }
    case AND_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_and(cpu, val);                   // perform OR
            cpu->cycle = 0;                       // clear cycles
        }
        break;
    }
    case AND_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_and(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_and(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case AND_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_and(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_and(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case AND_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_and(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case AND_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_and(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_and(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }

    case EOR_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_eor(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case EOR_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_eor(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case EOR_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_eor(cpu, val);            // perform OR
            cpu->cycle = 0;                // clear cycles
        }
        break;
    }
    case EOR_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_eor(cpu, val);                   // perform OR
            cpu->cycle = 0;                       // clear cycles
        }
        break;
    }
    case EOR_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_eor(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_eor(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case EOR_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_eor(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_eor(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case EOR_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_eor(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case EOR_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_eor(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_eor(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }

    case ADC_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0;
        }
        break;
    }
    case ADC_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ADC_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ADC_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ADC_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_adc_sbc(cpu, val, 0);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0;
        }
        break;
    }
    case ADC_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_adc_sbc(cpu, val, 0);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0;
        }
        break;
    }
    case ADC_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0;
        }
        break;
    }
    case ADC_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_adc_sbc(cpu, val, 0);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = 0;
        }
        break;
    }

    case SBC_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0;
        }
        break;
    }
    case SBC_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case SBC_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case SBC_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case SBC_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_adc_sbc(cpu, val, 1);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0;
        }
        break;
    }
    case SBC_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_adc_sbc(cpu, val, 1);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0;
        }
        break;
    }
    case SBC_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0;
        }
        break;
    }
    case SBC_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_adc_sbc(cpu, val, 1);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = 0;
        }
        break;
    }

    case CMP_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_cma(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case CMP_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_cma(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case CMP_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_cma(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case CMP_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_cma(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case CMP_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_cma(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_cma(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case CMP_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_cma(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_cma(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case CMP_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_cma(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case CMP_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_cma(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_cma(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }

    case CPX_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_cmx(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case CPX_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_cmx(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case CPX_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_cmx(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }

    case CPY_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_cmy(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case CPY_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_cmy(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case CPY_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_cmy(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }

    case DEC_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (4 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case DEC_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case DEC_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case DEC_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (5 == cpu->cycle++)
        {
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (6 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }

    case DEX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->x = impl_dec(cpu, cpu->x);
            cpu->cycle = 0;
        }
        break;
    }

    case DEY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->y = impl_dec(cpu, cpu->y);
            cpu->cycle = 0;
        }
        break;
    }

    case INC_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (4 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case INC_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case INC_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case INC_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (5 == cpu->cycle++)
        {
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (6 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }

    case INX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->x = impl_inc(cpu, cpu->x);
            cpu->cycle = 0;
        }
        break;
    }

    case INY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->y = impl_inc(cpu, cpu->y);
            cpu->cycle = 0;
        }
        break;
    }

    case ASL:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->a = impl_asl(cpu, cpu->a);
            cpu->cycle = 0;
        }
        break;
    }
    case ASL_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (4 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ASL_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ASL_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ASL_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (5 == cpu->cycle++)
        {
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (6 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }

    case LSR:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->a = impl_lsr(cpu, cpu->a);
            cpu->cycle = 0;
        }
        break;
    }
    case LSR_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (4 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case LSR_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case LSR_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case LSR_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (5 == cpu->cycle++)
        {
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (6 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }

    case ROL:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->a = impl_rol(cpu, cpu->a);
            cpu->cycle = 0;
        }
        break;
    }
    case ROL_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (4 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROL_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROL_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROL_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (5 == cpu->cycle++)
        {
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (6 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROR:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->a = impl_rol(cpu, cpu->a);
            cpu->cycle = 0;
        }
        break;
    }
    case ROR_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (4 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROR_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROR_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case ROR_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->infer_addr]; // obtain value from memory
        }
        else if (5 == cpu->cycle++)
        {
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (6 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    /************ ARITHMATIC ************/

    /*************** MOVE ***************/
    case LDA_IMM:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->pc++];
            impl_lda(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case LDA_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_lda(cpu, val);
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case LDA_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->tmp]; // obtain value from memory
            impl_lda(cpu, val);            // perform OR
            cpu->cycle = 0;                // clear cycles
        }
        break;
    }
    case LDA_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr]; // obtain value from inferred location
            impl_lda(cpu, val);                   // perform OR
            cpu->cycle = 0;                       // clear cycles
        }
        break;
    }
    case LDA_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_lda(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_lda(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case LDA_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_lda(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_lda(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case LDA_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->mem[cpu->infer_addr];
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->tmp;
            impl_lda(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }
    case LDA_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = cpu->mem[cpu->infer_addr];
                impl_lda(cpu, val);
                cpu->cycle = 0;
            }
        }
        else if (5 == cpu->cycle++)
        {
            byte val = cpu->mem[cpu->infer_addr];
            impl_lda(cpu, val);
            cpu->cycle = 0;
        }
        break;
    }

    case STA_ZP:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = cpu->mem[cpu->pc++]; // obtain zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->a;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case STA_ZPX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // origin zero page address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (3 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->a;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case STA_ABS:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->a;
            cpu->cycle = 0; // clear cycles
        }
        break;
    }
    case STA_ABX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            cpu->mem[cpu->infer_addr] = cpu->a;
            cpu->cycle = 0;
        }
        break;
    }
    case STA_ABY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= cpu->mem[cpu->pc++]; // obtain low byte of address
        }
        else if (2 == cpu->cycle++)
        {
            cpu->infer_addr |= ((word)cpu->mem[cpu->pc++]) << 8; // obtain high byte and calculate address
        }
        else if (3 == cpu->cycle++)
        {
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (4 == cpu->cycle++) // page boundary crossed
        {
            cpu->mem[cpu->infer_addr] = cpu->a;
            cpu->cycle = 0;
        }
        break;
    }
    case STA_IZX:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (4 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->tmp;
            cpu->cycle = 0;
        }
        break;
    }
    case STA_IZY:
    {
        if (0 == cpu->cycle++)
        {
            cpu->pc++;
        }
        else if (1 == cpu->cycle++)
        {
            cpu->tmp = cpu->mem[cpu->pc++]; // get immediate
        }
        else if (2 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = cpu->mem[cpu->tmp]; // store the lo address
        }
        else if (3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)cpu->mem[cpu->tmp++]) << 8; // store the hi address, note zero page is not crossed
        }
        else if (4 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (5 == cpu->cycle++)
        {
            cpu->mem[cpu->infer_addr] = cpu->a;
            cpu->cycle = 0;
        }
        break;
    }

        /*************** MOVE ***************/
    }
}

// int cpu_exec(cpu_6502 *cpu)
// {
//     switch (cpu->mem[cpu->pc])
//     {
//     case ADC_IMM:
//     {
// #ifdef MOS_DEBUG
//         printf("Instruction: ADC_IMM ");
// #endif
//         int num_cycles = 2;
//         cpu->pc++; // prepare for next operand
//         byte val = cpu->mem[cpu->pc];
//         // do the addition, take care of BCD
//         impl_adc(cpu, val);
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ADC_ZP:
//     {
// #ifdef MOS_DEBUG
//         printf("Instruction: ADC_ZPG ");
// #endif
//         int num_cycles = 3;
//         cpu->pc++; // prepare for next operand
//         byte val = cpu->mem[cpu->mem[cpu->pc]];
//         // do the addition, take care of BCD
//         impl_adc(cpu, val);
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ADC_ABS:
//     {
// #ifdef MOS_DEBUG
//         printf("Instruction: ADC_MEM ");
// #endif
//         int num_cycles = 4;
//         cpu->pc++; // prepare for next operand
//         word addr = cpu->mem[cpu->pc];
//         cpu->pc++;
//         addr |= ((word)cpu->mem[cpu->pc]) << 8;
//         byte val = cpu->mem[addr];
//         // do the addition, take care of BCD
//         impl_adc(cpu, val);
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ASL_ZPG:
//     {
//         int num_cycles = 5;
//         cpu->pc++;                    // prepare for next operand
//         byte val = cpu->mem[cpu->pc]; // from ZPG address
//         cpu->c = (val & 0x80) >> 7;   // get bit 7 and store it in carry
//         cpu->mem[cpu->pc] <<= 1;      // shift the value to left by 1
//         if (!(cpu->mem[cpu->pc]))
//             cpu->z = 1;
//         if (cpu->mem[cpu->pc] & 0x80)
//             cpu->n = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ASL_A:
//     {
//         int num_cycles = 2;
//         cpu->c = (cpu->a & 0x80) >> 7; // get bit 7 and store it in carry
//         cpu->a <<= 1;                  // shift accumulator to left by 1
//         if (!(cpu->a))
//             cpu->z = 1;
//         if (cpu->a & 0x80)
//             cpu->n = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ASL_MEM:
//     {
//         int num_cycles = 6;
//         cpu->pc++;                     // prepare for next operand
//         word addr = cpu->mem[cpu->pc]; // LO addr
//         cpu->pc++;
//         addr += cpu->mem[cpu->pc] * 0x16; // HI addr
//         cpu->c = (0x80 & cpu->mem[addr]) >> 7;   // set carry
//         cpu->mem[addr] <<= 1;             // shift left by 1
//         byte val = cpu->mem[addr];
//         if (!val) // check zero flag
//             cpu->z = 1;
//         if (val & 0x80) // check negative flag
//             cpu->n = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case BRK:
//     {
//         int num_cycles = 7;
//         cpu->i = 1;                                    // indicate interrupt
//         word tmp = cpu->pc + 2;                        // cycle 1
//         cpu->mem[cpu->sp | 0x100] = tmp & 0xff;        // 2
//         cpu->sp--;                                     // 3
//         cpu->mem[cpu->sp | 0x100] = (tmp >> 8) & 0xff; // 4
//         cpu->sp--;                                     // 5
//         cpu->mem[cpu->sp | 0x100] = cpu->sr;           // 6
//         cpu->sp--;                                     // 7
//         usleep(num_cycles * CPU_CYCLE_USEC);           // hacky way to do it but assume that the host CPU is much faster
//         break;
//     }

//     case JMP_IMM:
//     {
// #ifdef MOS_DEBUG
//         printf("Instruction: JMP_IMM ");
// #endif
//         int num_cycles = 3;
//         cpu->pc++;                     // prepare for next operand
//         word addr = cpu->mem[cpu->pc]; // low byte of immediate addr
//         cpu->pc++;
//         addr += cpu->mem[cpu->pc] * 256; // high byte of immediate addr
//         cpu->pc = addr;
// #ifdef MOS_DEBUG
//         printf("Loc: 0x%04x\n", addr);
// #endif
//         cpu->last_jmp = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case JMP_INDIRECT:
//     {
// #ifdef MOS_DEBUG
//         printf("Instruction: JMP_INDIRECT ");
// #endif
//         int num_cycles = 5;
//         cpu->pc++;                     // prepare for next operand
//         word addr = cpu->mem[cpu->pc]; // low byte of immediate addr
//         cpu->pc++;
//         addr += cpu->mem[cpu->pc] * 256; // high byte of immediate addr
// #ifdef MOS_DEBUG
//         printf("Addr: 0x%04x ", addr);
// #endif
//         addr = cpu->mem[addr] + cpu->mem[addr + 1] * 256;
//         cpu->pc = addr;
// #ifdef MOS_DEBUG
//         printf("Loc: 0x%04x\n", addr);
// #endif
//         cpu->last_jmp = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case NOP:
//     {
// #ifdef MOS_DEBUG
//         printf("Instruction: NOP\n");
// #endif
//         int num_cycles = 2;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ORA_X_IND:
//     {
//         int num_cycles = 6;
//         cpu->pc++;                                      // prepare for next operand
//         byte ll = cpu->mem[cpu->pc];                    // obtain the index
//         ll += cpu->x;                                   // without carry
//         word addr = cpu->mem[ll] & 0x00ff;              // get LO
//         addr |= (((word)cpu->mem[ll++]) << 8) & 0xff00; // get HI, remember it is in zpg so it wraps around!
//         cpu->a |= cpu->mem[addr];                       // OR the data
//         if ((cpu->a) & 0x80)                            // negative flag, 2's compliment
//             cpu->n = 1;
//         if (!(cpu->a)) // zero flag
//             cpu->z = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC); // hacky way to do it but assume that the host CPU is much faster
//         break;
//     }

//     case ORA_ZPG:
//     {
//         int num_cycles = 3;
//         cpu->pc++; // prepare for next operand
//         cpu->a |= cpu->mem[cpu->pc];
//         if ((cpu->a) & 0x80) // negative flag, 2's compliment
//             cpu->n = 1;
//         if (!(cpu->a)) // zero flag
//             cpu->z = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ORA_MEM:
//     {
//         int num_cycles = 0;
//         cpu->pc++;                     // prepare for next operand
//         word addr = cpu->mem[cpu->pc]; // LO addr
//         cpu->pc++;
//         addr += cpu->mem[cpu->pc] * 0x16; // HI addr
//         cpu->a |= cpu->mem[addr];
//         if (!(cpu->a))
//             cpu->z = 1;
//         if (cpu->a & 0x80)
//             cpu->n = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case ORA_IMM:
//     {
//         int num_cycles = 2;
//         cpu->pc++; // prepare for next operand
//         cpu->a |= cpu->mem[cpu->pc];
//         if ((cpu->a) & 0x80) // negative flag, 2's compliment
//             cpu->n = 1;
//         if (!(cpu->a)) // zero flag
//             cpu->z = 1;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     case PHP:
//     {
//         int num_cycles = 3;
//         cpu->mem[cpu->sp | 0x100] = cpu->sr;
//         cpu->sp--;
//         usleep(num_cycles * CPU_CYCLE_USEC);
//         break;
//     }

//     default:
//     {
//         printf("Abort trap! Instruction invalid/not implemented!\n");
//         break;
//     }
//     }
//     if (!(cpu->last_jmp))
//     {
//         cpu->pc++; // prepare for next instruction/operand
//     }
//     cpu->last_jmp = 0; // clear the jump
//     return 0;
// }

// // set program counter to RESET vector address
// void cpu_reset(cpu_6502 *cpu)
// {
//     cpu->sr = 0;
//     cpu->sp = 0xff; // stack pointer starts at the bottom
//     cpu->a = 0;
//     cpu->x = 0;
//     cpu->y = 0;
//     cpu->last_jmp = 0;
//     cpu->pc = cpu->mem[V_RESET] + cpu->mem[V_RESET + 1] * 256;
// }

/*
case INSTR:
{
    int num_cycles = ;
    cpu->pc++; // prepare for next operand

    usleep(num_cycles * CPU_CYCLE_USEC);
    break;
}
*/