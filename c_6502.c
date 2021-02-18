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
    cpu.mem[0x8000] = LDA_IMM;
    cpu.mem[0x8001] = 0x0;
    cpu.mem[0x8002] = NOP;
    cpu.mem[0x8003] = JMP_IND;
    cpu.mem[0x8004] = 0x00;
    cpu.mem[0x8005] = 0x90;
    cpu.mem[0x9000] = 0x00;
    cpu.mem[0x9001] = 0xa0;
    cpu.mem[0xa000] = ADC_IMM;
    cpu.mem[0xa001] = 0x09;
    cpu.mem[0xa002] = ADC_IMM;
    cpu.mem[0xa003] = 0x05;
    cpu.mem[0xa004] = JMP_ABS;
    cpu.mem[0xa005] = 0x02;
    cpu.mem[0xa006] = 0x80;
    printf("Press enter to reset CPU: ");
    getchar();
    cpu_reset(&cpu);
    while (1)
    {
        printf("CPU Cycle: %d | IRQ Cycle: %d\nCPU Registers:\nA = 0x%02x X = 0x%02x Y = 0x%02x\nNV-BDIZC\n%d%d%d%d%d%d%d%d\nPC: 0x%04x SP: 0x%02x OP: 0x%02x\n", cpu.cycle, cpu.irq_cycle, cpu.a, cpu.x, cpu.y, cpu.n, cpu.v, cpu.rsvd, cpu.b, cpu.d, cpu.i, cpu.z, cpu.c, cpu.pc, cpu.sp, cpu.instr);
        printf("Press enter to execute instruction: ");
        getchar();
        printf("\n\n");
        cpu_exec(&cpu);
    }
    return 0;
}
#endif

const char *CYCLE_NAME_6502[] =
    {
        "INVALID",
        "T0",
        "T1",
        "T2",
        "T3",
        "T4",
        "T5",
        "T6",
        "T7"};

static inline byte impl_fetch(cpu_6502 *cpu, word addr)
{
    return cpu->mem[addr];
}

static inline word impl_fetch_word(cpu_6502 *cpu, word addr)
{
    word val = impl_fetch(cpu, addr + 1);
    val <<= 8;
    val |= impl_fetch(cpu, addr);
    return val;
}

static inline void impl_write(cpu_6502 *cpu, word addr, byte val)
{
    cpu->mem[addr] = val;
}

static inline void instr_fetch(cpu_6502 *cpu)
{
    if (CYC_T0 == cpu->cycle)
    {
        cpu->cycle++;
        cpu->instr = impl_fetch(cpu, cpu->pc);
        cpu->instr_ptr = cpu->pc;
    }
    // printf("%s: PC: 0x%04x | INSTR: 0x%02x | Cycle: %d\n", __func__, cpu->pc, cpu->instr, cpu->cycle);
}
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
        byte lsn = ((a & 0xf) + (b & 0xf) + c);
        byte c3 = (lsn >> 4) & 0x1;
        tmp = lsn & 0xf;

        byte msn = ((a & 0x7) + (b & 0x7) + c3);
        byte a7 = ((a & 0x80) == 0x80);
        byte b7 = ((b & 0x80) == 0x80);
        byte msn3 = ((msn & 0x8) == 0x8);
        byte c7 = (msn3 & ((a7 ^ b7) | (a7 & b7)));
        tmp |= msn & 0x70; // top three bits
        tmp |= (((a7 ^ b7 ^ msn3) << 7) & 0x80);
        c = c7;
        cpu->v = (lsn >> 4) & 0x1;
        cpu->v ^= msn3;
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

static inline word impl_sgn_ofst(word v1, byte v2)
{
    return v1 + v2 + (((v2 & 0x80) == 0x80) ? 0xff00 : 0x0); // signed addition
}

void cpu_reset(cpu_6502 *cpu)
{
    cpu->irq_cycle = 0;
    cpu->rst = 1;
}

void cpu_nmi(cpu_6502 *cpu)
{
    cpu->nmi = 1;
}

void cpu_irq(cpu_6502 *cpu, byte val)
{
    cpu->irq = (val == 0);
}

int cpu_exec(cpu_6502 *cpu)
{
    if (cpu->rst) // reset line
    {
        if (7 == ++cpu->irq_cycle) // 7th cycle
        {
            cpu->nmi = 0;
            cpu->irq = 0;
            cpu->a = rand();
            cpu->x = 0x0;
            cpu->y = 0x0;
            cpu->sp = 0xfd;
            cpu->sr = 0;
            cpu->rsvd = 1;
            cpu->pc = impl_fetch_word(cpu, V_RESET);
            cpu->cycle = CYC_T0;
            cpu->irq_cycle = 0;
            cpu->rst = 0; // clear reset
            return 1;
        }
        else
            return 1;
    }
    if (cpu->cycle > CYC_T7)
        cpu->cycle = CYC_IN;  // this is KIL
    if (cpu->cycle == CYC_T0) // at t = 0, fetch new instruction
    {
        if (cpu->nmi) // non maskable interrupt
        {
            if (0 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                return 1;
            }
            else if (1 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                impl_write(cpu, 0x100 + cpu->sp, cpu->pc); // lo byte
                cpu->sp--;
                return 1;
            }
            else if (2 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                impl_write(cpu, 0x100 + cpu->sp, cpu->pc >> 8); // hi byte
                cpu->sp--;
                return 1;
            }
            else if (3 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                impl_write(cpu, 0x100 + cpu->sp, cpu->sr);
                cpu->sp--;
                return 1;
            }
            else if (4 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                cpu->pc = 0x0 | cpu->mem[V_IRQ_BRK];
                return 1;
            }
            else if (5 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                cpu->pc |= ((word)cpu->mem[V_IRQ_BRK + 1]) << 8;
                return 1;
            }
            else if (6 == cpu->irq_cycle)
            {
                cpu->i = 1;         // interrupt is disabled for servicing
                cpu->b = 0;         // break bit is cleared
                cpu->nmi = 0;       // clear nmi indicator
                cpu->irq_cycle = 0; // clear interrupt cycle and fetch ISR instruction
            }
        }
        else if ((!cpu->i && cpu->irq)) // level triggered
        {
            if (0 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                return 1;
            }
            else if (1 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                impl_write(cpu, 0x100 + cpu->sp, cpu->pc); // lo byte
                cpu->sp--;
                return 1;
            }
            else if (2 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                impl_write(cpu, 0x100 + cpu->sp, cpu->pc >> 8); // hi byte
                cpu->sp--;
                return 1;
            }
            else if (3 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                impl_write(cpu, 0x100 + cpu->sp, cpu->sr);
                cpu->sp--;
                return 1;
            }
            else if (4 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                cpu->pc = 0x0 | cpu->mem[V_IRQ_BRK];
                return 1;
            }
            else if (5 == cpu->irq_cycle)
            {
                cpu->irq_cycle++;
                cpu->pc |= ((word)cpu->mem[V_IRQ_BRK + 1]) << 8;
                return 1;
            }
            else if (6 == cpu->irq_cycle)
            {
                cpu->i = 1;         // interrupt is disabled for servicing
                cpu->b = 0;         // break bit is cleared
                cpu->irq_cycle = 0; // clear interrupt cycle and fetch ISR instruction
            }
        }
        instr_fetch(cpu); // fetch current instruction
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
    {
        cpu->cycle++; // after 7 cycles, cpu->cycle will become 0, which is invalid
        break;
    }
    // NOP 2
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
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    // NOP 3
    case 0x04:
    case 0x44:
    case 0x64:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    // NOP 4
    case 0x14:
    case 0x34:
    case 0x54:
    case 0x74:
    case 0xd4:
    case 0xf4:
    case 0x0c:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    // NOP 4*
    case 0x1c:
    case 0x3c:
    case 0x5c:
    case 0x7c:
    case 0xdc:
    case 0xfc:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            cpu->cycle = CYC_T0;
        }
        break;
    }
    /************ ARITHMATIC ************/
    case ORA_IMM:
    {
        if (CYC_T1 == cpu->cycle++) // cycle 0, increment program counter, cycle counter
        {
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_ora(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ORA_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ora(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ORA_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_ora(cpu, val);            // perform OR
            cpu->cycle = CYC_T0;           // clear cycles
        }
        break;
    }
    case ORA_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_ora(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case ORA_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_ora(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ora(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ORA_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_ora(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ora(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ORA_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_ora(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ORA_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_ora(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ora(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case AND_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_and(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case AND_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_and(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case AND_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_and(cpu, val);            // perform OR
            cpu->cycle = CYC_T0;           // clear cycles
        }
        break;
    }
    case AND_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_and(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case AND_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_and(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_and(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case AND_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_and(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_and(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case AND_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_and(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case AND_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_and(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_and(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case EOR_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_eor(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case EOR_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_eor(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case EOR_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_eor(cpu, val);            // perform OR
            cpu->cycle = CYC_T0;           // clear cycles
        }
        break;
    }
    case EOR_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_eor(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case EOR_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_eor(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_eor(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case EOR_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_eor(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_eor(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case EOR_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_eor(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case EOR_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_eor(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_eor(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case ADC_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ADC_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ADC_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ADC_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ADC_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_adc_sbc(cpu, val, 0);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ADC_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_adc_sbc(cpu, val, 0);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ADC_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ADC_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_adc_sbc(cpu, val, 0);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 0);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case SBC_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SBC_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SBC_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SBC_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SBC_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_adc_sbc(cpu, val, 1);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SBC_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_adc_sbc(cpu, val, 1);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SBC_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SBC_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_adc_sbc(cpu, val, 1);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_adc_sbc(cpu, val, 1);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case CMP_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case CMP_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case CMP_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case CMP_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case CMP_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_cma(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case CMP_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_cma(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case CMP_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case CMP_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_cma(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_cma(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case CPX_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_cmx(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case CPX_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_cmx(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case CPX_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_cmx(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case CPY_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_cmy(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case CPY_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_cmy(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case CPY_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_cmy(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case DEC_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case DEC_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case DEC_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case DEC_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_dec(cpu, cpu->tmp);
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case DEX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->x = impl_dec(cpu, cpu->x);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case DEY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->y = impl_dec(cpu, cpu->y);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case INC_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case INC_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case INC_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case INC_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_inc(cpu, cpu->tmp);
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case INX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->x = impl_inc(cpu, cpu->x);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case INY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->y = impl_inc(cpu, cpu->y);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case ASL:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_asl(cpu, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ASL_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ASL_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ASL_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ASL_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_asl(cpu, cpu->tmp);
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case LSR:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_lsr(cpu, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LSR_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LSR_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LSR_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LSR_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_lsr(cpu, cpu->tmp);
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case ROL:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_rol(cpu, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ROL_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROL_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROL_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROL_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_rol(cpu, cpu->tmp);
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROR:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_rol(cpu, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ROR_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROR_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROR_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ROR_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_ror(cpu, cpu->tmp);
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    /************ ARITHMATIC ************/

    /*************** MOVE ***************/
    case LDA_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_lda(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDA_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_lda(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LDA_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->tmp); // obtain value from memory
            impl_lda(cpu, val);            // perform OR
            cpu->cycle = CYC_T0;           // clear cycles
        }
        break;
    }
    case LDA_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_lda(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case LDA_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_lda(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_lda(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDA_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_lda(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_lda(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDA_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = cpu->tmp;
            impl_lda(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDA_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_lda(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_lda(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case STA_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STA_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STA_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STA_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case STA_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case STA_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->tmp);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case STA_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case LDX_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_ldx(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDX_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ldx(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LDX_ZPY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->y; // increment by Y without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
            impl_ldx(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case LDX_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_ldx(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case LDX_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_ldx(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ldx(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDY_IMM:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->pc++);
            impl_ldy(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LDY_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ldy(cpu, val);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LDY_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from memory
            impl_ldy(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case LDY_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr); // obtain value from inferred location
            impl_ldy(cpu, val);                   // perform OR
            cpu->cycle = CYC_T0;                  // clear cycles
        }
        break;
    }
    case LDY_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
            if (offset < 0xff)         // page boundary not crossed
            {
                byte val = impl_fetch(cpu, cpu->infer_addr);
                impl_ldy(cpu, val);
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            byte val = impl_fetch(cpu, cpu->infer_addr);
            impl_ldy(cpu, val);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case STX_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->x);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STX_ZPY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->y; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->x);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STX_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->x);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case STY_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->y);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STY_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->y);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case STY_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->y);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case TAX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_ldx(cpu, cpu->a);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }
    case TXA:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_lda(cpu, cpu->x);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }

    case TAY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_ldy(cpu, cpu->a);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }
    case TYA:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_lda(cpu, cpu->y);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }

    case TSX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_ldx(cpu, cpu->sp);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }
    case TXS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp = impl_ld(cpu, cpu->x);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }

    case PLA:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++; // pre-increment
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // fetch
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_lda(cpu, cpu->tmp);
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }

    case PHA:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, 0x100 + cpu->sp, cpu->a);
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp--;           // post-decrement
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }

    case PLP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++; // pre-increment
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // fetch
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sr = cpu->tmp;  // we do not want to modify flags!
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }

    case PHP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, 0x100 + cpu->sp, cpu->sr);
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp--;           // post-decrement
            cpu->cycle = CYC_T0; // EOS
        }
        break;
    }
        /*************** MOVE ***************/

        /*************** JUMP ***************/
    case BPL:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->n == 0)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->n == 0)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BMI:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->n == 1)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->n == 1)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BVC:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->v == 0)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->v == 0)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BVS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->v == 1)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->v == 1)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BCC:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->c == 0)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->c == 0)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BCS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->c == 1)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->c == 1)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BNE:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->z == 0)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->z == 0)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case BEQ:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
           cpu->tmp = impl_fetch(cpu, cpu->pc++); // fetch offset
            cpu->infer_addr = impl_sgn_ofst(cpu->pc, cpu->tmp);
            if ((cpu->pc & 0xff00) == (cpu->infer_addr & 0xff00)) // same page
            {
                if (cpu->z == 1)
                    cpu->pc = cpu->infer_addr;
                cpu->cycle = CYC_T0;
            }
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            if (cpu->z == 1)
                cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case BRK:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = 0;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, 0x100 + cpu->sp, cpu->pc); // lo byte
            cpu->sp--;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, 0x100 + cpu->sp, cpu->pc >> 8); // hi byte
            cpu->sp--;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, 0x100 + cpu->sp, cpu->sr);
            cpu->sp--;
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc = 0x0 | cpu->mem[V_IRQ_BRK];
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc |= ((word)cpu->mem[V_IRQ_BRK + 1]) << 8;
        }
        else if (CYC_T7 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->i = 1;          // interrupt is disabled for servicing
            cpu->b = 1;          // break bit is set
            cpu->cycle = CYC_T0; // clear interrupt cycle and fetch ISR instruction
        }
        break;
    }

    case RTI:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // post increment SP
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sr = cpu->tmp; // store
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // post increment SP
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc = cpu->tmp; // store hi byte
            cpu->pc <<= 8;
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // post increment SP
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc |= cpu->tmp; // store hi byte
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case RTS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // post increment SP
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = cpu->tmp; // store hi byte
            cpu->infer_addr <<= 8;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->sp++;
            cpu->tmp = impl_fetch(cpu, 0x100 + cpu->sp); // post increment SP
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr |= cpu->tmp; // store hi byte
            cpu->cycle = CYC_T0;
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc = cpu->infer_addr;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case JMP_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // lo byte
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // hi byte
            cpu->infer_addr |= ((word)cpu->tmp) << 8;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc = cpu->infer_addr;
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case JMP_IND:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // lo byte
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // hi byte
            cpu->infer_addr |= ((word)cpu->tmp) << 8;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc = impl_fetch(cpu, cpu->infer_addr);
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr++;
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc |= ((word)impl_fetch(cpu, cpu->infer_addr)) << 8;
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case BIT_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            byte val = impl_fetch(cpu, cpu->infer_addr);
            cpu->tmp = cpu->a & val;
            if (cpu->a == 0)
                cpu->z = 1;
            else
                cpu->z = 0;
            cpu->sr &= 0x3f; // unset top bits
            cpu->sr |= cpu->tmp & 0xc0;
            cpu->a = cpu->tmp;
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case BIT_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->infer_addr |= impl_fetch(cpu, cpu->pc++); // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr |= ((word)impl_fetch(cpu, cpu->pc++)) << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->infer_addr);
            cpu->tmp &= cpu->a;
            if (cpu->a == 0)
                cpu->z = 1;
            else
                cpu->z = 0;
            cpu->sr &= 0x3f; // unset top bits
            cpu->sr |= cpu->tmp & 0xc0;
            cpu->a = cpu->tmp;
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case CLC:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->c = 0;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SEC:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->c = 1;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case CLD:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->d = 0;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SED:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->d = 1;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case CLI:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->i = 0;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SEI:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->i = 1;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }

    case CLV:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->v = 0;          // obtain low byte of address
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
        /*************** JUMP ***************/

        /************* ILLEGAL **************/
    case SLO_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SLO_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SLO_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SLO_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SLO_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SLO_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SLO_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_asl(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_ora(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case RLA_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case RLA_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case RLA_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case RLA_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case RLA_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case RLA_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case RLA_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_rol(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_and(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case SRE_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SRE_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SRE_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SRE_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SRE_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SRE_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case SRE_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_lsr(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_eor(cpu, impl_fetch(cpu, cpu->infer_addr));
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case RRA_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case RRA_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case RRA_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case RRA_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case RRA_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case RRA_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case RRA_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_ror(cpu, impl_fetch(cpu, cpu->infer_addr)));
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), false);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case SAX_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a & cpu->x);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SAX_ZPY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->y; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a & cpu->x);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SAX_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a & cpu->x);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case SAX_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, cpu->a & cpu->x);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case LAX_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            cpu->x = impl_fetch(cpu, cpu->infer_addr);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LAX_ZPY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->y; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            cpu->x = impl_fetch(cpu, cpu->infer_addr);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LAX_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            cpu->x = impl_fetch(cpu, cpu->infer_addr);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case LAX_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            cpu->x = impl_fetch(cpu, cpu->infer_addr);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LAX_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            cpu->x = impl_fetch(cpu, cpu->infer_addr);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LAX_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            cpu->x = impl_fetch(cpu, cpu->infer_addr);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case DCP_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case DCP_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case DCP_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case DCP_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case DCP_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case DCP_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case DCP_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) - 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case ISC_ZP:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = impl_fetch(cpu, cpu->pc++); // obtain zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ISC_ZPX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // origin zero page address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp += cpu->x; // increment by X without carry, always in zero page
            cpu->infer_addr = cpu->tmp;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ISC_ABS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0; // clear cycles
        }
        break;
    }
    case ISC_ABX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->x;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ISC_ABY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ISC_IZX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = 0x0;
            byte tmp = cpu->tmp + cpu->x; // without carry
            cpu->infer_addr += tmp;
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            byte tmp = cpu->tmp + cpu->x + 1;    // without carry
            cpu->infer_addr += ((word)tmp) << 8; // without carry
        }
        else if (CYC_T5 == cpu->cycle++) // get word at inferred address
        {
            cpu->tmp = cpu->a;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ISC_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
        }
        else if (CYC_T3 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = impl_fetch(cpu, cpu->tmp); // store the lo address
        }
        else if (CYC_T4 == cpu->cycle++) // get low byte address
        {
            cpu->infer_addr = ((word)impl_fetch(cpu, cpu->tmp++)) << 8;; // store the hi address, note zero page is not crossed
        }
        else if (CYC_T5 == cpu->cycle++) // get calculate Y offset address
        {
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            impl_write(cpu, cpu->infer_addr, impl_fetch(cpu, cpu->infer_addr) + 1);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->infer_addr), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case ANC:
    case ANC_:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
            impl_and(cpu, cpu->tmp);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case ALR:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
            impl_and(cpu, cpu->tmp);
            impl_asl(cpu, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case ARR:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
            impl_and(cpu, cpu->tmp);
            impl_lsr(cpu, cpu->a);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case XAA:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_lda(cpu, cpu->x);
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
            impl_and(cpu, cpu->tmp);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case LAX:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = impl_fetch(cpu, cpu->pc++); // get immediate
            impl_lda(cpu, cpu->tmp);
            impl_ldx(cpu, cpu->tmp);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case AXS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = cpu->a;
            impl_and(cpu, cpu->x);
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->pc++), true);
            impl_ldx(cpu, cpu->a);
            cpu->a = cpu->tmp;
            cpu->cycle = CYC_T0;
        }
        break;
    }

    case SBC:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            impl_adc_sbc(cpu, impl_fetch(cpu, cpu->pc++), true);
            cpu->cycle = CYC_T0;
        }
        break;
    }

    // following are implemented as NOP
    case AHX_IZY:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T6 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case AHX_ABY:
    case SHY:
    case SHX:
    case TAS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            break;
        }
        else if (CYC_T5 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->cycle = CYC_T0;
        }
        break;
    }
    case LAS:
    {
        if (CYC_T1 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->pc++;
        }
        else if (CYC_T2 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->infer_addr = 0x0;
            cpu->tmp = impl_fetch(cpu, cpu->pc++);
            cpu->infer_addr |= cpu->tmp; // obtain low byte of address
        }
        else if (CYC_T3 == cpu->cycle)
        {
            cpu->cycle++;
            cpu->tmp = ((word)impl_fetch(cpu, cpu->pc++));
            cpu->infer_addr |= cpu->tmp << 8; // obtain high byte and calculate address
        }
        else if (CYC_T4 == cpu->cycle)
        {
            cpu->cycle++;
            // calculate effective address
            word offset = (cpu->infer_addr & 0xff) + cpu->y;
            cpu->infer_addr &= 0xfe00; // clear lo byte, and last bit of hi byte
            cpu->infer_addr += offset; // add offset to the address
        }
        else if (CYC_T5 == cpu->cycle++) // page boundary crossed
        {
            cpu->a = impl_fetch(cpu, cpu->infer_addr);
            impl_and(cpu, cpu->sp);
            cpu->x = cpu->a;
            cpu->sp = cpu->a;
            cpu->cycle = CYC_T0;
        }
        break;
    }
        /************* ILLEGAL **************/
    }
    return 1;
}