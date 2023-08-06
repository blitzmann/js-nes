/**
 * CPU class. Documentation:
 * https://www.masswerk.at/6502/6502_instruction_set.html
 * https://www.pagetable.com/c64ref/6502/?tab=3
 */

import { Memory } from './memory';

// const NMI_ADDRESS = 0xfffa;
const RESET_ADDRESS = 0xfffc;
const IRQ_ADDRESS = 0xfffe;

export class CPU {
    program = [];

    cycles = 0;
    stopped = false;
    ip = 0;
    memory = new Memory(0xffff);
    _log = '';

    log(msg) {
        this._log += msg;
    }

    flush_log() {
        console.log(this._log);
        this._log = '';
    }

    /**
     * We use a Uint8Array to force 8-bit unsigned numbers.
     * This helps in situations where increments or decrements overflow.
     */
    registers = new Uint8Array(5);

    get a() {
        return this.registers[0];
    }
    set a(val) {
        this.registers[0] = val;
    }
    get x() {
        return this.registers[1];
    }
    set x(val) {
        this.registers[1] = val;
    }
    get y() {
        return this.registers[2];
    }
    set y(val) {
        this.registers[2] = val;
    }
    get sr() {
        return this.registers[3];
    }
    set sr(val) {
        this.registers[3] = val;
    }
    get sp() {
        return this.registers[4];
    }
    set sp(val) {
        this.registers[4] = val;
    }

    stack_offset = 0x0100;

    // transient
    mode;
    addr;

    opcodes = new Map<number, [(data?: number) => void, () => number, number]>([
        [0xa9, [this.lda, this.mode_imm, 2]],
        [0xad, [this.lda, this.mode_abs, 4]],
        [0xbd, [this.lda, this.mode_abx, 4]],
        [0xb9, [this.lda, this.mode_aby, 4]],
        [0xa5, [this.lda, this.mode_zp0, 3]],
        [0xb5, [this.lda, this.mode_zpx, 4]],
        [0xa1, [this.lda, this.mode_izx, 6]],
        [0xb1, [this.lda, this.mode_izy, 5]],

        [0xa2, [this.ldx, this.mode_imm, 2]],
        [0xae, [this.ldx, this.mode_abs, 4]],
        [0xbe, [this.ldx, this.mode_aby, 4]],
        [0xa6, [this.ldx, this.mode_zp0, 3]],
        [0xb6, [this.ldx, this.mode_zpy, 4]],

        [0xa0, [this.ldy, this.mode_imm, 2]],
        [0xac, [this.ldy, this.mode_abs, 4]],
        [0xbc, [this.ldy, this.mode_abx, 4]],
        [0xa4, [this.ldy, this.mode_zp0, 3]],
        [0xb4, [this.ldy, this.mode_zpx, 4]],

        [0x8d, [this.sta, this.mode_abs, 4]],
        [0x9d, [this.sta, this.mode_abx, 5]],
        [0x99, [this.sta, this.mode_aby, 5]],
        [0x85, [this.sta, this.mode_zp0, 3]],
        [0x95, [this.sta, this.mode_zpx, 4]],
        [0x81, [this.sta, this.mode_izx, 6]],
        [0x91, [this.sta, this.mode_izy, 6]],

        [0x8e, [this.stx, this.mode_abs, 4]],
        [0x86, [this.stx, this.mode_zp0, 3]],
        [0x96, [this.stx, this.mode_zpy, 4]],

        [0x8c, [this.sty, this.mode_abs, 4]],
        [0x84, [this.sty, this.mode_zp0, 3]],
        [0x94, [this.sty, this.mode_zpy, 4]],

        [0xaa, [this.tax, this.mode_imp, 2]],
        [0xa8, [this.tay, this.mode_imp, 2]],
        [0xba, [this.tsx, this.mode_imp, 2]],
        [0x8a, [this.txa, this.mode_imp, 2]],
        [0x9a, [this.txs, this.mode_imp, 2]],
        [0x98, [this.tya, this.mode_imp, 2]],

        [0x48, [this.pha, this.mode_imp, 3]],
        [0x08, [this.php, this.mode_imp, 3]],
        [0x68, [this.pla, this.mode_imp, 4]],
        [0x28, [this.plp, this.mode_imp, 4]],

        [0x0a, [this.asl, this.mode_acc, 2]],
        [0x0e, [this.asl, this.mode_abs, 6]],
        [0x1e, [this.asl, this.mode_abx, 7]],
        [0x06, [this.asl, this.mode_zp0, 5]],
        [0x16, [this.asl, this.mode_zpx, 6]],

        [0x4a, [this.lsr, this.mode_acc, 2]],
        [0x4e, [this.lsr, this.mode_abs, 6]],
        [0x5e, [this.lsr, this.mode_abx, 7]],
        [0x46, [this.lsr, this.mode_zp0, 5]],
        [0x56, [this.lsr, this.mode_zpx, 6]],

        [0x2a, [this.rol, this.mode_acc, 2]],
        [0x2e, [this.rol, this.mode_abs, 6]],
        [0x3e, [this.rol, this.mode_abx, 7]],
        [0x26, [this.rol, this.mode_zp0, 5]],
        [0x36, [this.rol, this.mode_zpx, 6]],

        [0x6a, [this.ror, this.mode_acc, 2]],
        [0x6e, [this.ror, this.mode_abs, 6]],
        [0x7e, [this.ror, this.mode_abx, 7]],
        [0x66, [this.ror, this.mode_zp0, 5]],
        [0x76, [this.ror, this.mode_zpx, 6]],

        [0x29, [this.and, this.mode_imm, 2]],
        [0x2d, [this.and, this.mode_abs, 4]],
        [0x3d, [this.and, this.mode_abx, 4]],
        [0x39, [this.and, this.mode_aby, 4]],
        [0x25, [this.and, this.mode_zp0, 3]],
        [0x35, [this.and, this.mode_zpx, 4]],
        [0x21, [this.and, this.mode_izx, 6]],
        [0x31, [this.and, this.mode_izy, 5]],

        [0x2c, [this.bit, this.mode_abs, 4]],
        [0x24, [this.bit, this.mode_zp0, 3]],

        [0x49, [this.eor, this.mode_imm, 2]],
        [0x4d, [this.eor, this.mode_abs, 4]],
        [0x5d, [this.eor, this.mode_abx, 4]],
        [0x59, [this.eor, this.mode_aby, 4]],
        [0x45, [this.eor, this.mode_zp0, 3]],
        [0x55, [this.eor, this.mode_zpx, 4]],
        [0x41, [this.eor, this.mode_izx, 6]],
        [0x51, [this.eor, this.mode_izy, 5]],

        [0x09, [this.ora, this.mode_imm, 2]],
        [0x0d, [this.ora, this.mode_abs, 4]],
        [0x1d, [this.ora, this.mode_abx, 4]],
        [0x19, [this.ora, this.mode_aby, 4]],
        [0x05, [this.ora, this.mode_zp0, 3]],
        [0x15, [this.ora, this.mode_zpx, 4]],
        [0x01, [this.ora, this.mode_izx, 6]],
        [0x11, [this.ora, this.mode_izy, 5]],

        [0x69, [this.adc, this.mode_imm, 2]],
        [0x6d, [this.adc, this.mode_abs, 4]],
        [0x7d, [this.adc, this.mode_abx, 4]],
        [0x79, [this.adc, this.mode_aby, 4]],
        [0x65, [this.adc, this.mode_zp0, 3]],
        [0x75, [this.adc, this.mode_zpx, 4]],
        [0x61, [this.adc, this.mode_izx, 6]],
        [0x71, [this.adc, this.mode_izy, 5]],

        [0xc9, [this.cmp, this.mode_imm, 2]],
        [0xcd, [this.cmp, this.mode_abs, 4]],
        [0xdd, [this.cmp, this.mode_abx, 4]],
        [0xd9, [this.cmp, this.mode_aby, 4]],
        [0xc5, [this.cmp, this.mode_zp0, 3]],
        [0xd5, [this.cmp, this.mode_zpx, 4]],
        [0xc1, [this.cmp, this.mode_izx, 6]],
        [0xd1, [this.cmp, this.mode_izy, 5]],

        [0xe0, [this.cpx, this.mode_imm, 2]],
        [0xec, [this.cpx, this.mode_abs, 4]],
        [0xe4, [this.cpx, this.mode_zp0, 3]],

        [0xc0, [this.cpy, this.mode_imm, 2]],
        [0xcc, [this.cpy, this.mode_abs, 4]],
        [0xc4, [this.cpy, this.mode_zp0, 3]],

        [0xe9, [this.sbc, this.mode_imm, 2]],
        [0xed, [this.sbc, this.mode_abs, 4]],
        [0xfd, [this.sbc, this.mode_abx, 4]],
        [0xf9, [this.sbc, this.mode_aby, 4]],
        [0xe5, [this.sbc, this.mode_zp0, 3]],
        [0xf5, [this.sbc, this.mode_zpx, 4]],
        [0xe1, [this.sbc, this.mode_izx, 6]],
        [0xf1, [this.sbc, this.mode_izy, 5]],

        [0xce, [this.dec, this.mode_abs, 6]],
        [0xde, [this.dec, this.mode_abx, 7]],
        [0xc6, [this.dec, this.mode_zp0, 5]],
        [0xd6, [this.dec, this.mode_zpx, 6]],

        [0xca, [this.dex, this.mode_imp, 2]],
        [0x88, [this.dey, this.mode_imp, 2]],

        [0xee, [this.inc, this.mode_abs, 6]],
        [0xf6, [this.inc, this.mode_abx, 7]],
        [0xe6, [this.inc, this.mode_zp0, 5]],
        [0xf6, [this.inc, this.mode_zpx, 6]],

        [0xe8, [this.inx, this.mode_imp, 2]],
        [0xc8, [this.iny, this.mode_imp, 2]],

        [0xc8, [this.iny, this.mode_imp, 2]],

        [0x00, [this.brk, this.mode_imp, 7]],
        [0x4c, [this.jmp, this.mode_abs, 3]],
        [0x6c, [this.jmp, this.mode_ind, 5]],
        [0x20, [this.jsr, this.mode_abs, 6]],
        [0x40, [this.rti, this.mode_imp, 6]],
        [0x60, [this.rts, this.mode_imp, 6]],

        [0x90, [this.bcc, this.mode_rel, 2]],
        [0xb0, [this.bcs, this.mode_rel, 2]],
        [0xf0, [this.beq, this.mode_rel, 2]],
        [0x30, [this.bmi, this.mode_rel, 2]],
        [0xd0, [this.bne, this.mode_rel, 2]],
        [0x10, [this.bpl, this.mode_rel, 2]],
        [0x50, [this.bvc, this.mode_rel, 2]],
        [0x70, [this.bvs, this.mode_rel, 2]],

        [0x18, [this.clc, this.mode_imp, 2]],
        [0xd8, [this.cld, this.mode_imp, 2]],
        [0x58, [this.cli, this.mode_imp, 2]],
        [0xb8, [this.clv, this.mode_imp, 2]],
        [0x38, [this.sec, this.mode_imp, 2]],
        [0xf8, [this.sed, this.mode_imp, 2]],
        [0x78, [this.sei, this.mode_imp, 2]],

        [0xea, [this.nop, this.mode_imp, 2]],
    ]);

    pc() {
        return (this.ip += 1);
    }

    same_page(addr1, addr2) {
        return (addr1 & 0xff00) === (addr2 & 0xff00);
    }

    load_program(bytes, offset = 0x00) {
        this.memory.load(bytes, offset);
    }

    run_program() {
        while (!this.stopped) {
            this.step();
        }
    }

    cpu_fetch() {
        const ret = this.memory.get(this.ip++);
        this.log(ret.toString(16).padStart(2, '0').toUpperCase() + ' ');
        return ret;
    }

    step() {
        this.log(
            `${this.ip.toString(16).toUpperCase()}`.padEnd(5, ' ') + ' | '
        );
        const opcode = this.cpu_fetch();

        const op_def = this.opcodes.get(opcode);
        if (!op_def) {
            console.error('unknown op code: 0x' + opcode.toString(16));
        }
        const [op, mode, cycles] = op_def;
        this.mode = mode;
        this.addr = mode.bind(this)();
        this.log(' | ' + op.name.toUpperCase() + ' ');
        this.log(
            ` | A:${this.a.toString(16).toUpperCase()} X:${this.x
                .toString(16)
                .toUpperCase()} Y: ${this.y
                .toString(16)
                .toUpperCase()} P:${this.sr
                .toString(16)
                .toUpperCase()} SP:${this.sp.toString(16).toUpperCase()} CYC:${
                this.cycles
            }`
        );
        this.flush_log();

        op.bind(this)(this.addr);
        this.cycles += cycles;
    }

    /**
     * Fetch a single byte from memory
     */
    fetch_byte(addr) {
        return this.memory.get(addr);
    }

    /**
     * Fetch a word from memory, starting at `addr`. This is helpful for getting memory addresses
     *
     * @returns $<addr+1><addr>
     */
    fetch_word(addr) {
        let low = this.memory.get(addr);
        let high = this.memory.get(addr + 1);
        return (high << 8) | low;
    }

    reset() {
        const addr = this.fetch_word(RESET_ADDRESS);
        this.sr = 0;
        this.a = 0;
        this.x = 0;
        this.y = 0;
        this.sp = 0xff;
        this.ip = addr;
        this.set_flag(Flags.I, true);
        this.cycles = 7;
    }

    peek(addr) {
        return this.memory.get(addr);
    }

    set_flag(flag: Flags, val: boolean) {
        if (val) {
            this.sr |= flag;
        } else {
            this.sr &= ~flag;
        }
    }

    stack_push(data) {
        this.memory.set(this.stack_offset + this.sp, data);
        this.sp--;
    }

    stack_pop() {
        const val = this.memory.get(this.stack_offset + this.sp);
        this.sp++;
        return val;
    }

    get_flag(flag: Flags) {
        return !!(this.sr & flag);
    }

    get_flag_bit(flag: Flags) {
        return this.get_flag(flag) ? 1 : 0;
    }

    /**
     * IMM: Immediate Addressing; #$nn
     *
     * Here, a literal operand is given immediately after the instruction. The operand is always an 8-bit value and the total instruction length is always 2 bytes. In memory, the operand is a single byte following immediately after the instruction code. In assembler, the mode is usually indicated by a "#" prefix adjacent to the operand.
     *
     * @example
     * LDA #$07; load the literal hexidecimal value "$7" into the accumulator
     * ADC #$A0; add the literal hexidecimal value "$A0" to the accumulator
     * CPX #$32; compare the X-register to the literal hexidecimal value "$32"
     */
    mode_imm() {
        const ret = this.cpu_fetch();
        return ret;
    }

    /**
     * ABS: Absolute Addressing; $nnnn
     *
     * Absolute addressing modes provides the 16-bit address of a memory location, the contents of which used as the operand to the instruction. In machine language, the address is provided in two bytes immediately after the instruction (making these 3-byte instructions) in low-byte, high-byte order (LLHH) or little-endian. In assembler, conventional numbers (HHLL order or big-endian words) are used to provide the address.
     *
     * Absolute addresses are also used for the jump instructions JMP and JSR to provide the address for the next instruction to continue with in the control flow.
     *
     * @example
     * LDA $3010; load the contents of address "$3010" into the accumulator
     * ROL $08A0; rotate the contents of address "$08A0" left by one position
     * JMP $4000; jump to (continue with) location "$4000"
     */
    mode_abs() {
        let low = this.cpu_fetch();
        let high = this.cpu_fetch();
        return (high << 8) | low;
    }

    /**
     * ZP0: Zero-Page Addressing; $nn
     *
     * The 16-bit address space available to the 6502 is thought to consist of 256 "pages" of 256 memory locations each ($00…$FF). In this model the high-byte of an address gives the page number and the low-byte a location inside this page. The very first of these pages, where the high-byte is zero (addresses $0000…$00FF), is somewhat special.
     *
     * The zero-page address mode is similar to absolute address mode, but these instructions use only a single byte for the operand, the low-byte, while the high-byte is assumed to be zero by definition. Therefore, these instructions have a total length of just two bytes (one less than absolute mode) and take one CPU cycle less to execute, as there is one byte less to fetch.
     *
     * @example
     * LDA $80; load the contents of address "$0080" into the accumulator
     * BIT $A2; perform bit-test with the contents of address "$00A2"
     * ASL $9A; arithmetic shift left of the contents of location "$009A"
     */
    mode_zp0() {
        return this.cpu_fetch();
    }

    /**
     * ABX: X-Indexed Absolute; $nnnn,X
     *
     * Indexed addressing adds the contents of either the X-register or the Y-register to the provided address to give the effective address, which provides the operand.
     *
     * These instructions are usefull to e.g., load values from tables or to write to a continuous segment of memory in a loop. The most basic forms are "absolute,X" and "absolute,X", where either the X- or the Y-register, respectively, is added to a given base address. As the base address is a 16-bit value, these are generally 3-byte instructions. Since there is an additional operation to perform to determine the effective address, these instructions are one cycle slower than those using absolute addressing mode.*
     *
     * @example
     * LDA $3120,X; load the contents of address "$3120 + X" into A
     * LDX $8240,Y; load the contents of address "$8240 + Y" into X
     * INC $1400,X; increment the contents of address "$1400 + X"
     */
    mode_abx() {
        let low = this.cpu_fetch();
        let high = this.cpu_fetch();
        return ((high << 8) | low) + this.x;
    }

    /**
     * ABY: Y-Indexed Absolute; $nnnn,Y
     *
     * Indexed addressing adds the contents of either the X-register or the Y-register to the provided address to give the effective address, which provides the operand.
     *
     * These instructions are usefull to e.g., load values from tables or to write to a continuous segment of memory in a loop. The most basic forms are "absolute,X" and "absolute,X", where either the X- or the Y-register, respectively, is added to a given base address. As the base address is a 16-bit value, these are generally 3-byte instructions. Since there is an additional operation to perform to determine the effective address, these instructions are one cycle slower than those using absolute addressing mode.*
     *
     * @example
     * LDA $3120,X; load the contents of address "$3120 + X" into A
     * LDX $8240,Y; load the contents of address "$8240 + Y" into X
     * INC $1400,X; increment the contents of address "$1400 + X"
     */
    mode_aby() {
        let low = this.cpu_fetch();
        let high = this.cpu_fetch();
        return ((high << 8) | low) + this.y;
    }

    /**
     * ZPX: X-Indexed Zero Page; $nn,X
     *
     * As with absolute addressing, there is also a zero-page mode for indexed addressing. However, this is generally only available with the X-register. (The only exception to this is LDX, which has an indexed zero-page mode utilizing the Y-register.)
     *
     * As we have already seen with normal zero-page mode, these instructions are one byte less in total length (two bytes) and take one CPU cycle less than instructions in absolute indexed mode.
     *
     * Unlike absolute indexed instructions with 16-bit base addresses, zero-page indexed instructions never affect the high-byte of the effective address, which will simply wrap around in the zero-page, and there is no penalty for crossing any page boundaries.
     *
     * @example
     * LDA $80,X; load the contents of address "$0080 + X" into A
     * LSR $82,X; shift the contents of address "$0082 + X" left
     * LDX $60,Y; load the contents of address "$0060 + Y" into X
     */
    mode_zpx() {
        return (this.cpu_fetch() + this.x) & 0x00ff; // truncate to 255. We do not care about carry.
    }

    /**
     * ZPY: Y-Indexed Zero Page; $nn,Y
     *
     * As with absolute addressing, there is also a zero-page mode for indexed addressing. However, this is generally only available with the X-register. (The only exception to this is LDX, which has an indexed zero-page mode utilizing the Y-register.)
     *
     * As we have already seen with normal zero-page mode, these instructions are one byte less in total length (two bytes) and take one CPU cycle less than instructions in absolute indexed mode.
     *
     * Unlike absolute indexed instructions with 16-bit base addresses, zero-page indexed instructions never affect the high-byte of the effective address, which will simply wrap around in the zero-page, and there is no penalty for crossing any page boundaries.
     *
     * @example
     * LDA $80,X; load the contents of address "$0080 + X" into A
     * LSR $82,X; shift the contents of address "$0082 + X" left
     * LDX $60,Y; load the contents of address "$0060 + Y" into X
     */
    mode_zpy() {
        return (this.cpu_fetch() + this.y) & 0x00ff; // truncate to 255. We do not care about carry.
    }

    /**
     * IND: Indirect Addressing; ($nnnn)
     *
     * This mode looks up a given address and uses the contents of this address and the next one (in LLHH little-endian order) as the effective address. In its basic form, this mode is available for the JMP instruction only. (Its generally use is jump vectors and jump tables.)
     *
     * Like the absolute JMP instruction it uses a 16-bit address (3 bytes in total), but takes two additional CPU cycles to execute, since there are two additional bytes to fetch for the lookup of the effective jump target.
     *
     * Generally, indirect addressing is denoted by putting the lookup address in parenthesis.
     *
     * @example
     * JMP ($FF82); jump to address given in addresses "$FF82" and "$FF83"
     */
    mode_ind() {
        let low = this.cpu_fetch();
        let high = this.cpu_fetch();
        const addr = (high << 8) | low;

        low = this.memory.get(addr);
        high = this.memory.get(addr + 1);
        return (high << 8) | low;
    }

    /**
     * IZX: Pre-Indexed Indirect, "(Zero-Page,X)"; ($nn,X)
     *
     * Indexed indirect address modes are generally available only for instructions supplying an operand to the accumulator (LDA, STA, ADC, SBC, AND, ORA, EOR, etc). The placement of the index register inside or outside of the parenthesis indicating the address lookup will give you clue what these instructions are doing.
     *
     * Pre-indexed indirect address mode is only available in combination with the X-register. It works much like the "zero-page,X" mode, but, after the X-register has been added to the base address, instead of directly accessing this, an additional lookup is performed, reading the contents of resulting address and the next one (in LLHH little-endian order), in order to determine the effective address.
     *
     * Like with "zero-page,X" mode, the total instruction length is 2 bytes, but there are two additional CPU cycles in order to fetch the effective 16-bit address. As "zero-page,X" mode, a lookup address will never overflow into the next page, but will simply wrap around in the zero-page.
     *
     * These instructions are useful, whenever we want to loop over a table of pointers to disperse addresses, or where we want to apply the same operation to various addresses, which we have stored as a table in the zero-page.
     *
     * @example
     * LDA ($70,X); load the contents of the location given in addresses "$0070+X" and "$0070+1+X" into A
     * STA ($A2,X); store the contents of A in the location given in addresses "$00A2+X" and "$00A3+X"
     * EOR ($BA,X); perform an exlusive OR of the contents of A and the contents of the location given in addresses "$00BA+X" and "$00BB+X"
     */
    mode_izx() {
        let addr = this.cpu_fetch() + this.x;

        let low = this.memory.get(addr);
        let high = this.memory.get(addr + 1);
        addr = (high << 8) | low;
        return this.memory.get(addr);
    }

    /**
     * IZY: Post-Indexed Indirect, "(Zero-Page),Y"; ($nn),Y
     *
     * Post-indexed indirect addressing is only available in combination with the Y-register. As indicated by the indexing term ",Y" being appended to the outside of the parenthesis indicating the indirect lookup, here, a pointer is first read (from the given zero-page address) and resolved and only then the contents of the Y-register is added to this to give the effective address.
     *
     * Like with "zero-page,Y" mode, the total instruction length is 2 bytes, but there it takes an additional CPU cycles to resolve and index the 16-bit pointer. As with "absolute,X" mode, the effective address may overflow into the next page, in the case of which the execution uses an extra CPU cycle.
     *
     * These instructions are useful, wherever we want to perform lookups on varying bases addresses or whenever we want to loop over tables, the base address of which we have stored in the zero-page.
     *
     * @example
     * LDA ($70),Y; add the contents of the Y-register to the pointer provided in "$0070" and "$0071" and load the contents of this address into A
     * STA ($A2),Y; store the contents of A in the location given by the pointer in "$00A2" and "$00A3" plus the contents of the Y-register
     * EOR ($BA),Y; perform an exlusive OR of the contents of A and the address given by the addition of Y to the pointer in "$00BA" and "$00BB"
     */
    mode_izy() {
        let addr = this.cpu_fetch();

        let low = this.memory.get(addr);
        let high = this.memory.get(addr + 1);
        addr = (high << 8) | low;
        return this.memory.get(addr + this.y);
    }

    /**
     * REL: Relative Addressing
     *
     * This final address mode is exlusive to conditional branch instructions, which branch in the execution path depending on the state of a given CPU flag. Here, the instruction provides only a relative offset, which is added to the contents of the program counter (PC) as it points to the immediate next instruction. The relative offset is a signed single byte value in two's complement encoding (giving a range of −128…+127), which allows for branching up to half a page forwards and backwards.
     *
     * On the one hand, this makes these instructions compact, fast and relocatable at the same time. On the other hand, we have to mind that our branch target is no farther away than half a memory page.
     *
     * Generally, an assembler will take care of this and we only have to provide the target address, not having to worry about relative addressing.
     *
     * These instructions are always of 2 bytes length and perform in 2 CPU cycles, if the branch is not taken (the condition resolving to 'false'), and 3 cycles, if the branch is taken (when the condition is true). If a branch is taken and the target is on a different page, this adds another CPU cycle (4 in total).
     *
     * @example
     * BEQ $1005; branch to location "$1005", if the zero flag is set. if the current address is $1000, this will give an offset of $03.
     * BCS $08C4; branch to location "$08C4", if the carry flag is set. if the current address is $08D4, this will give an offset of $EE (−$12).
     * BCC $084A; branch to location "$084A", if the carry flag is clear.
     */
    mode_rel() {
        return this.cpu_fetch() + this.ip;
    }

    /**
     * IMP: Implied. Nothing happens
     */
    mode_imp() {
        return;
    }

    /**
     * ACC: Accumulator
     */
    mode_acc() {
        return;
    }
    //************************ INSTRUCTIONS **************************/

    /**
     * When instruction LDA is executed by the microprocessor, data is transferred from memory to the accumulator and stored in the accumulator.
     *
     * LDA affects the contents of the accumulator, does not affect the carry or overflow flags; sets the zero flag if the accumulator is zero as a result of the LDA, otherwise resets the zero flag; sets the negative flag if bit 7 of the accumulator is a 1, other­ wise resets the negative flag.
     *
     *     M → A
     */
    lda(data) {
        this.a = data;
        this.set_flag(Flags.Z, this.a === 0);
        this.set_flag(Flags.N, !!(this.a & (1 << 7)));
    }

    /**
     * Load the index register X from memory.
     *
     * LDX does not affect the C or V flags; sets Z if the value loaded was zero, otherwise resets it; sets N if the value loaded in bit 7 is a 1; otherwise N is reset, and affects only the X register.
     *
     *     M → X
     */
    ldx(data) {
        this.x = data;
        this.set_flag(Flags.Z, this.x === 0);
        this.set_flag(Flags.N, !!(this.x & (1 << 7)));
    }

    /**
     * Load the index register Y from memory.
     *
     * LDY does not affect the C or V flags, sets the N flag if the value loaded in bit 7 is a 1, otherwise resets N, sets Z flag if the loaded value is zero otherwise resets Z and only affects the Y register.
     *
     *     M → Y
     */
    ldy(data) {
        this.y = data;
        this.set_flag(Flags.Z, this.y === 0);
        this.set_flag(Flags.N, !!(this.y & (1 << 7)));
    }

    /**
     * This instruction transfers the contents of the accumulator to memory.
     *
     * This instruction affects none of the flags in the processor status register and does not affect the accumulator.
     *
     *     A → M
     */
    sta(data) {
        this.memory.set(data, this.a);
    }

    /**
     * Transfers value of X register to addressed memory location.
     *
     * No flags or registers in the microprocessor are affected by the store operation.
     *
     *     X → M
     */
    stx(data) {
        this.memory.set(data, this.x);
    }

    /**
     * Transfer the value of the Y register to the addressed memory location.
     *
     * STY does not affect any flags or registers in the microprocessor.
     *
     *     Y → M
     */
    sty(data) {
        this.memory.set(data, this.y);
    }

    /**
     * This instruction takes the value from accumulator A and trans­fers or loads it into the index register X without disturbing the content of the accumulator A.
     *
     * TAX only affects the index register X, does not affect the carry or overflow flags. The N flag is set if the resultant value in the index register X has bit 7 on, otherwise N is reset. The Z bit is set if the content of the register X is 0 as aresult of theopera­ tion, otherwise it is reset.
     *
     *     A → X
     */
    tax(_) {
        this.x = this.a;

        this.set_flag(Flags.Z, this.x === 0);
        this.set_flag(Flags.N, !!(this.x & (1 << 7)));
    }

    /**
     * This instruction moves the value of the accumulator into index register Y without affecting the accumulator.
     *
     * TAY instruction only affects the Y register and does not affect either the carry or overflow flags. If the index register Y has bit 7 on, then N is set, otherwise it is reset. If the content of the index register Y equals 0 as a result of the operation, Z is set on, otherwise it is reset.
     *
     *     A → Y
     */
    tay(_) {
        this.y = this.a;

        this.set_flag(Flags.Z, this.y === 0);
        this.set_flag(Flags.N, !!(this.y & (1 << 7)));
    }

    /**
     * This instruction transfers the value in the stack pointer to the index register X.
     *
     * TSX does not affect the carry or overflow flags. It sets N if bit 7 is on in index X as a result of the instruction, otherwise it is reset. If index X is zero as a result of the TSX, the Z flag is set, other­ wise it is reset. TSX changes the value of index X, making it equal to the content of the stack pointer.
     *
     *     S → X
     */
    tsx(_) {
        this.x = this.sp;

        this.set_flag(Flags.Z, this.x === 0);
        this.set_flag(Flags.N, !!(this.x & (1 << 7)));
    }

    /**
     * This instruction moves the value that is in the index register X to the accumulator A without disturbing the content of the index register X.
     *
     * TXA does not affect any register other than the accumula­tor and does not affect the carry or overflow flag. If the result in A has bit 7 on, then the N flag is set, otherwise it is reset. If the resultant value in the accumulator is 0, then the Z flag is set, other­ wise it is reset.
     *
     *     X → A
     */
    txa(_) {
        this.a = this.a;

        this.set_flag(Flags.Z, this.a === 0);
        this.set_flag(Flags.N, !!(this.a & (1 << 7)));
    }

    /**
     * This instruction transfers the value in the index register X to the stack pointer.
     *
     * TXS changes only the stack pointer, making it equal to the content of the index register X. It does not affect any of the flags.
     *
     *     X → S
     */
    txs(_) {
        this.sp = this.x;
    }

    /**
     * This instruction moves the value that is in the index register Y to accumulator A without disturbing the content of the register Y.
     *
     * TYA does not affect any other register other than the accumula­ tor and does not affect the carry or overflow flag. If the result in the accumulator A has bit 7 on, the N flag is set, otherwise it is reset. If the resultant value in the accumulator A is 0, then the Z flag is set, otherwise it is reset.
     *
     *     Y → A
     */
    tya(_) {
        this.a = this.y;

        this.set_flag(Flags.Z, this.a === 0);
        this.set_flag(Flags.N, !!(this.a & (1 << 7)));
    }

    /**
     * This instruction transfers the current value of the accumulator to the next location on the stack, automatically decrementing the stack to point to the next empty location.
     *
     * The Push A instruction only affects the stack pointer register which is decremented by 1 as a result of the operation. It affects no flags.
     *
     *     A↓
     */
    pha(_) {
        this.memory.set(this.stack_offset + this.sp, this.a);
        this.sp--;
    }

    /**
     * This instruction transfers the contents of the processor status reg­ister unchanged to the stack, as governed by the stack pointer.
     *
     * The PHP instruction affects no registers or flags in the micropro­cessor.
     *
     *     P↓
     */
    php(_) {
        this.memory.set(this.stack_offset + this.sp, this.sr);
    }

    /**
     * This instruction adds 1 to the current value of the stack pointer and uses it to address the stack and loads the contents of the stack into the A register.
     *
     * The PLA instruction does not affect the carry or overflow flags. It sets N if the bit 7 is on in accumulator A as a result of instructions, otherwise it is reset. If accumulator A is zero as a result of the PLA, then the Z flag is set, otherwise it is reset. The PLA instruction changes content of the accumulator A to the contents of the memory location at stack register plus 1 and also increments the stack register.
     */
    pla(_) {
        this.sp++;
        this.a = this.memory.get(this.stack_offset + this.sp);
        this.set_flag(Flags.Z, this.a === 0);
        this.set_flag(Flags.N, !!(this.a & (1 << 7)));
    }

    /**
     * This instruction transfers the next value on the stack to the Proces­sor Status register, thereby changing all of the flags and setting the mode switches to the values from the stack.
     *
     * The PLP instruction affects no registers in the processor other than the status register. This instruction could affect all flags in the status register.
     */
    plp(_) {
        this.sp++;
        this.sr = this.memory.get(this.stack_offset + this.sp);
    }

    /**
     * The shift left instruction shifts either the accumulator or the address memory location 1 bit to the left, with the bit 0 always being set to 0 and the the input bit 7 being stored in the carry flag. ASL either shifts the accumulator left 1 bit or is a read/modify/write instruction that affects only memory.
     *
     * The instruction does not affect the overflow bit, sets N equal to the result bit 7 (bit 6 in the input), sets Z flag if the result is equal to 0, otherwise resets Z and stores the input bit 7 in the carry flag.
     *
     *     C ← /M7...M0/ ← 0
     */
    asl(addr) {
        let data = this.mode === this.mode_acc ? this.a : this.memory.get(addr);
        data = data << 1;

        this.set_flag(Flags.C, (data & 0xff00) > 0);
        this.set_flag(Flags.N, !!(data & (1 << 7)));

        data = data & 0xff;
        this.set_flag(Flags.Z, data === 0);

        if (this.mode === this.mode_acc) {
            this.a = data;
        } else {
            this.memory.set(addr, data);
        }
    }

    /**
     * This instruction shifts either the accumulator or a specified memory location 1 bit to the right, with the higher bit of the result always being set to 0, and the low bit which is shifted out of the field being stored in the carry flag.
     *
     * The shift right instruction either affects the accumulator by shift­ing it right 1 or is a read/modify/write instruction which changes a speci­fied memory location but does not affect any internal registers. The shift right does not affect the overflow flag. The N flag is always reset. The Z flag is set if the result of the shift is 0 and reset otherwise. The carry is set equal to bit 0 of the input.
     *
     *     0 → /M7...M0/ → C
     */
    lsr(addr) {
        let data = this.mode === this.mode_acc ? this.a : this.memory.get(addr);

        this.set_flag(Flags.C, (data & 1) > 0);

        data = data >> 1;

        this.set_flag(Flags.N, false);
        this.set_flag(Flags.Z, data === 0);

        if (this.mode === this.mode_acc) {
            this.a = data;
        } else {
            this.memory.set(addr, data);
        }
    }

    /**
     * The rotate left instruction shifts either the accumulator or addressed memory left 1 bit, with the input carry being stored in bit 0 and with the input bit 7 being stored in the carry flags.
     *
     * The ROL instruction either shifts the accumulator left 1 bit and stores the carry in accumulator bit 0 or does not affect the internal reg­isters at all. The ROL instruction sets carry equal to the input bit 7, sets N equal to the input bit 6 , sets the Z flag if the result of the ro­ tate is 0, otherwise it resets Z and does not affect the overflow flag at all.
     *
     *     C ← /M7...M0/ ← C
     */
    rol(addr) {
        let data = this.mode === this.mode_acc ? this.a : this.memory.get(addr);

        // set bit 0 with current carry
        data = (data << 1) | this.get_flag_bit(Flags.C);
        this.set_flag(Flags.C, (data & (1 << 8)) > 0);
        data = data & 0xff;

        this.set_flag(Flags.N, (data & (1 << 7)) > 0);
        this.set_flag(Flags.Z, data === 0);

        if (this.mode === this.mode_acc) {
            this.a = data;
        } else {
            this.memory.set(addr, data);
        }
    }

    /**
     * The rotate right instruction shifts either the accumulator or addressed memory right 1 bit with bit 0 shifted into the carry and carry shifted into bit 7.
     *
     * The ROR instruction either shifts the accumulator right 1 bit and stores the carry in accumulator bit 7 or does not affect the internal regis­ ters at all. The ROR instruction sets carry equal to input bit 0, sets N equal to the input carry and sets the Z flag if the result of the rotate is 0; otherwise it resets Z and does not affect the overflow flag at all.
     *
     *     C → /M7...M0/ → C
     */
    ror(addr) {
        let data = this.mode === this.mode_acc ? this.a : this.memory.get(addr);

        // set carry to front
        data |= this.get_flag_bit(Flags.C) << 8;
        this.set_flag(Flags.C, (data & 1) > 0);
        data = data >> 1;

        this.set_flag(Flags.N, (data & (1 << 7)) > 0);
        this.set_flag(Flags.Z, data === 0);

        if (this.mode === this.mode_acc) {
            this.a = data;
        } else {
            this.memory.set(addr, data);
        }
    }

    /**
     * The AND instruction transfer the accumulator and memory to the adder which performs a bit-by-bit AND operation and stores the result back in the accumulator.
     *
     * This instruction affects the accumulator; sets the zero flag if the result in the accumulator is 0, otherwise resets the zero flag; sets the negative flag if the result in the accumulator has bit 7 on, otherwise resets the negative flag.
     *
     *     A ∧ M → A
     */
    and(addr) {
        this.a = this.a & this.memory.get(addr);

        this.set_flag(Flags.N, (this.a & (1 << 7)) > 0);
        this.set_flag(Flags.Z, this.a === 0);
    }

    /**
     * This instruction performs an AND between a memory location and the accumulator but does not store the result of the AND into the accumulator.
     *
     * The bit instruction affects the N flag with N being set to the value of bit 7 of the memory being tested, the V flag with V being set equal to bit 6 of the memory being tested and Z being set by the result of the AND operation between the accumulator and the memory if the result is Zero, Z is reset otherwise. It does not affect the accumulator.
     *
     *     A ∧ M, M7 → N, M6 → V
     */
    bit(addr) {
        let mem = this.memory.get(addr);

        this.set_flag(Flags.N, (mem & (1 << 7)) > 0);
        this.set_flag(Flags.Z, (this.a & mem) === 0);
        this.set_flag(Flags.V, (mem & (1 << 6)) > 0);
    }

    /**
     * The EOR instruction transfers the memory and the accumulator to the adder which performs a binary "EXCLUSIVE OR" on a bit-by-bit basis and stores the result in the accumulator.
     *
     * This instruction affects the accumulator; sets the zero flag if the result in the accumulator is 0, otherwise resets the zero flag sets the negative flag if the result in the accumulator has bit 7 on, otherwise resets the negative flag.
     *
     *     A ⊻ M → A
     */
    eor(addr) {
        this.a = this.a ^ this.memory.get(addr);

        this.set_flag(Flags.N, (this.a & (1 << 7)) > 0);
        this.set_flag(Flags.Z, this.a === 0);
    }

    /**
     * The ORA instruction transfers the memory and the accumulator to the adder which performs a binary "OR" on a bit-by-bit basis and stores the result in the accumulator.
     *
     * This instruction affects the accumulator; sets the zero flag if the result in the accumulator is 0, otherwise resets the zero flag; sets the negative flag if the result in the accumulator has bit 7 on, otherwise resets the negative flag.
     *
     *     A ∨ M → A
     */
    ora(addr) {
        this.a = this.a | this.memory.get(addr);

        this.set_flag(Flags.N, (this.a & (1 << 7)) > 0);
        this.set_flag(Flags.Z, this.a === 0);
    }

    /**
     * This instruction adds the value of memory and carry from the previous operation to the value of the accumulator and stores the result in the accumulator.
     *
     * This instruction affects the accumulator; sets the carry flag when the sum of a binary add exceeds 255 or when the sum of a decimal add exceeds 99, otherwise carry is reset. The overflow flag is set when the sign or bit 7 is changed due to the result exceeding +127 or -128, otherwise overflow is reset. The negative flag is set if the accumulator result contains bit 7 on, otherwise the negative flag is reset. The zero flag is set if the accumulator result is 0, otherwise the zero flag is reset.
     *
     *     A + M + C → A, C
     */
    adc(data) {
        var fetched = this.memory.get(data);
        const tmp = this.a + fetched + this.get_flag_bit(Flags.C);

        // Overflow: https://forums.nesdev.org/viewtopic.php?t=6331
        // https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        this.set_flag(
            Flags.V,
            !!(~(this.a ^ fetched) & (this.a ^ tmp) & 0x0080)
        );

        this.a = tmp & 0xff; // truncate to 255

        this.set_flag(Flags.C, tmp > 0xff);
        this.set_flag(Flags.Z, this.a === 0);
        this.set_flag(Flags.N, !!(tmp & 0x0080));
    }

    /**
     * This instruction subtracts the contents of memory from the contents of the accumulator.
     *
     * The use of the CMP affects the following flags: Z flag is set on an equal comparison, reset otherwise; the N flag is set or reset by the result bit 7, the carry flag is set when the value in memory is less than or equal to the accumulator, reset when it is greater than the accumulator. The accumulator is not affected.
     *
     *     A - M
     */
    cmp(data) {
        let tmp = this.a - this.memory.get(data);

        this.set_flag(Flags.C, this.memory.get(data) <= this.a);
        this.set_flag(Flags.Z, this.a === this.memory.get(data));
        this.set_flag(Flags.N, (tmp & (1 << 7)) > 0);
    }

    /**
     * This instruction subtracts the value of the addressed memory location from the content of index register X using the adder but does not store the result; therefore, its only use is to set the N, Z and C flags to allow for comparison between the index register X and the value in memory.
     *
     * The CPX instruction does not affect any register in the machine; it also does not affect the overflow flag. It causes the carry to be set on if the absolute value of the index register X is equal to or greater than the data from memory. If the value of the memory is greater than the content of the index register X, carry is reset. If the results of the subtraction contain a bit 7, then the N flag is set, if not, it is reset. If the value in memory is equal to the value in index register X, the Z flag is set, otherwise it is reset.qual to the accumulator, reset when it is greater than the accumulator. The accumulator is not affected.
     *
     *     X - M
     */
    cpx(data) {
        let tmp = this.x - this.memory.get(data);

        this.set_flag(Flags.C, this.memory.get(data) <= this.x);
        this.set_flag(Flags.Z, this.x === this.memory.get(data));
        this.set_flag(Flags.N, (tmp & (1 << 7)) > 0);
    }

    /**
     * This instruction performs a two's complement subtraction between the index register Y and the specified memory location. The results of the subtraction are not stored anywhere. The instruction is strict­ly used to set the flags.
     *
     * CPY affects no registers in the microprocessor and also does not affect the overflow flag. If the value in the index register Y is equal to or greater than the value in the memory, the carry flag will be set, otherwise it will be cleared. If the results of the subtract- tion contain bit 7 on the N bit will be set, otherwise it will be cleared. If the value in the index register Y and the value in the memory are equal, the zero flag will be set, otherwise it will be cleared.
     *
     *     Y - M
     */
    cpy(data) {
        let tmp = this.y - this.memory.get(data);

        this.set_flag(Flags.C, this.memory.get(data) <= this.y);
        this.set_flag(Flags.Z, this.y === this.memory.get(data));
        this.set_flag(Flags.N, (tmp & (1 << 7)) > 0);
    }

    /**
     * This instruction subtracts the value of memory and borrow from the value of the accumulator, using two's complement arithmetic, and stores the result in the accumulator. Borrow is defined as the carry flag complemented; therefore, a resultant carry flag indicates that a borrow has not occurred.
     *
     * This instruction affects the accumulator. The carry flag is set if the result is greater than or equal to 0. The carry flag is reset when the result is less than 0, indicating a borrow. The over­flow flag is set when the result exceeds +127 or -127, otherwise it is reset. The negative flag is set if the result in the accumulator has bit 7 on, otherwise it is reset. The Z flag is set if the result in the accumulator is 0, otherwise it is reset.
     *
     *     A - M - ~C → A
     */
    sbc(data) {
        // this is the same as ADC, with the memory data ~
        var fetched = ~this.memory.get(data);
        const tmp = this.a + fetched + this.get_flag_bit(Flags.C);

        //https://stackoverflow.com/questions/29193303/6502-emulation-proper-way-to-implement-adc-and-sbc
        // Overflow: https://forums.nesdev.org/viewtopic.php?t=6331
        // https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        this.set_flag(
            Flags.V,
            !!(~(this.a ^ fetched) & (this.a ^ tmp) & 0x0080)
        );

        this.a = tmp & 0xff; // truncate to 255

        this.set_flag(Flags.C, tmp > 0xff);
        this.set_flag(Flags.Z, this.a === 0);
        this.set_flag(Flags.N, !!(tmp & 0x0080));
    }

    /**
     * This instruction subtracts 1, in two's complement, from the contents of the addressed memory location.
     *
     * The decrement instruction does not affect any internal register in the microprocessor. It does not affect the carry or overflow flags. If bit 7 is on as a result of the decrement, then the N flag is set, otherwise it is reset. If the result of the decrement is 0, the Z flag is set, other­wise it is reset.
     *
     *     M - 1 → M
     */
    dec(data) {
        // this is the same as ADC, with the memory data ~
        var fetched = this.memory.get(data) - 1;
        this.memory.set(data, fetched);

        this.set_flag(Flags.Z, fetched === 0);
        this.set_flag(Flags.N, (fetched & (1 << 7)) > 0);
    }

    /**
     * This instruction subtracts one from the current value of the index register X and stores the result in the index register X.
     *
     * DEX does not affect the carry or overflow flag, it sets the N flag if it has bit 7 on as a result of the decrement, otherwise it resets the N flag; sets the Z flag if X is a 0 as a result of the decrement, otherwise it resets the Z flag.
     *
     *     X - 1 → X
     */
    dex(_) {
        // this is the same as ADC, with the memory data ~
        this.x--;

        this.set_flag(Flags.Z, this.x === 0);
        this.set_flag(Flags.N, (this.x & (1 << 7)) > 0);
    }

    /**
     * This instruction subtracts one from the current value in the in­ dex register Y and stores the result into the index register Y. The result does not affect or consider carry so that the value in the index register Y is decremented to 0 and then through 0 to FF.
     *
     * Decrement Y does not affect the carry or overflow flags; if the Y register contains bit 7 on as a result of the decrement the N flag is set, otherwise the N flag is reset. If the Y register is 0 as a result of the decrement, the Z flag is set otherwise the Z flag is reset. This instruction only affects the index register Y.
     *
     *     Y - 1 → Y
     */
    dey(_) {
        // this is the same as ADC, with the memory data ~
        this.y--;

        this.set_flag(Flags.Z, this.y === 0);
        this.set_flag(Flags.N, (this.y & (1 << 7)) > 0);
    }

    /**
     * This instruction adds 1 to the contents of the addressed memory loca­tion.
     *
     * The increment memory instruction does not affect any internal registers and does not affect the carry or overflow flags. If bit 7 is on as the result of the increment,N is set, otherwise it is reset; if the increment causes the result to become 0, the Z flag is set on, otherwise it is reset.
     *
     *     M + 1 → M
     */
    inc(data) {
        const val = this.memory.get(data) + 1;
        this.memory.set(data, val);

        this.set_flag(Flags.Z, val === 0);
        this.set_flag(Flags.N, (val & (1 << 7)) > 0);
    }

    /**
     * Increment X adds 1 to the current value of the X register. This is an 8-bit increment which does not affect the carry operation, therefore, if the value of X before the increment was FF, the resulting value is 00.
     *
     * INX does not affect the carry or overflow flags; it sets the N flag if the result of the increment has a one in bit 7, otherwise resets N; sets the Z flag if the result of the increment is 0, otherwise it resets the Z flag.
     *
     * INX does not affect any other register other than the X register.
     *
     *     X + 1 → X
     */
    inx(_) {
        this.x++;

        this.set_flag(Flags.Z, this.x === 0);
        this.set_flag(Flags.N, (this.x & (1 << 7)) > 0);
    }

    /**
     * Increment Y increments or adds one to the current value in the Y register, storing the result in the Y register. As in the case of INX the primary application is to step thru a set of values using the Y register.
     *
     * The INY does not affect the carry or overflow flags, sets the N flag if the result of the increment has a one in bit 7, otherwise resets N, sets Z if as a result of the increment the Y register is zero otherwise resets the Z flag.
     *
     *     Y + 1 → Y
     */
    iny(_) {
        this.y++;

        this.set_flag(Flags.Z, this.y === 0);
        this.set_flag(Flags.N, (this.y & (1 << 7)) > 0);
    }

    /**
     * The break command causes the microprocessor to go through an inter­ rupt sequence under program control. This means that the program counter of the second byte after the BRK. is automatically stored on the stack along with the processor status at the beginning of the break instruction. The microprocessor then transfers control to the interrupt vector.
     *
     * Other than changing the program counter, the break instruction changes no values in either the registers or the flags.
     *
     *     PC + 2↓, [FFFE] → PCL, [FFFF] → PCH
     */
    brk(_) {
        this.set_flag(Flags.I, true);
        // push program counter high-bit first
        this.stack_push(this.ip >> 8);
        this.stack_push(this.ip & 0xff);
        // push status register with the B flag set
        this.stack_push((this.sr |= Flags.B));
        // get IRQ address
        this.ip = this.fetch_word(IRQ_ADDRESS);
    }

    /**
     * This instruction establishes a new valne for the program counter.
     *
     * It affects only the program counter in the microprocessor and affects no flags in the status register.
     *
     *     [PC + 1] → PCL, [PC + 2] → PCH
     */
    jmp(data) {
        this.ip = data;
    }

    /**
     * This instruction transfers control of the program counter to a subroutine location but leaves a return pointer on the stack to allow the user to return to perform the next instruction in the main program after the subroutine is complete. To accomplish this, JSR instruction stores the program counter address which points to the last byte of the jump instruc­ tion onto the stack using the stack pointer. The stack byte contains the program count high first, followed by program count low. The JSR then transfers the addresses following the jump instruction to the program counter low and the program counter high, thereby directing the program to begin at that new address.
     *
     * The JSR instruction affects no flags, causes the stack pointer to be decremented by 2 and substitutes new values into the program counter low and the program counter high.
     *
     *     PC + 2↓, [PC + 1] → PCL, [PC + 2] → PCH
     */
    jsr(data) {
        this.stack_push(this.ip >> 8);
        this.stack_push(this.ip & 0xff);
        this.ip = data;
    }

    /**
     * This instruction transfers from the stack into the microprocessor the processor status and the program counter location for the instruction which was interrupted. By virtue of the interrupt having stored this data before executing the instruction and thei fact that the RTI reinitializes the microprocessor to the same state as when it was interrupted, the combination of interrupt plus RTI allows truly reentrant coding.
     *
     * The RTI instruction reinitializes all flags to the position to the point they were at the time the interrupt was taken and sets the program counter back to its pre-interrupt state. It affects no other registers in the microprocessor.
     *
     *     P↑ PC↑
     */
    rti(_) {
        this.sr = this.stack_pop();
        const low = this.stack_pop();
        const high = this.stack_pop();
        this.ip = (high << 8) | low;
    }

    /**
     * This instruction loads the program count low and program count high from the stack into the program counter and increments the program counter so that it points to the instruction following the JSR. The stack pointer is adjusted by incrementing it twice.
     *
     * The RTS instruction does not affect any flags and affects only PCL and PCH.
     *
     *     PC↑, PC + 1 → PC
     */
    rts(_) {
        const low = this.stack_pop();
        const high = this.stack_pop();
        this.ip = (high << 8) | low;
    }

    /**
     * This instruction tests the state of the carry bit and takes a conditional branch if the carry bit is reset.
     *
     * It affects no flags or registers other than the program counter and then only if the C flag is not on.
     *
     *     Branch on C = 0
     */
    bcc(data) {
        if (!this.get_flag(Flags.C)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction takes the conditional branch if the carry flag is on.
     *
     * BCS does not affect any of the flags or registers except for the program counter and only then if the carry flag is on.
     *
     *      Branch on C = 1
     */
    bcs(data) {
        if (this.get_flag(Flags.C)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction could also be called "Branch on Equal."
     *
     * It takes a conditional branch whenever the Z flag is on or the previ­ous result is equal to 0.
     *
     * BEQ does not affect any of the flags or registers other than the program counter and only then when the Z flag is set.
     *
     *     Branch on Z = 1
     */
    beq(data) {
        if (this.get_flag(Flags.Z)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction takes the conditional branch if the N bit is set.
     *
     * BMI does not affect any of the flags or any other part of the machine other than the program counter and then only if the N bit is on.
     *
     *     Branch on N = 1
     */
    bmi(data) {
        if (this.get_flag(Flags.N)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction could also be called "Branch on Not Equal." It tests the Z flag and takes the conditional branch if the Z flag is not on, indicating that the previous result was not zero.
     *
     * BNE does not affect any of the flags or registers other than the program counter and only then if the Z flag is reset.
     *
     *     Branch on Z = 0
     */
    bne(data) {
        if (!this.get_flag(Flags.Z)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction is the complementary branch to branch on result minus. It is a conditional branch which takes the branch when the N bit is reset (0). BPL is used to test if the previous result bit 7 was off (0) and branch on result minus is used to determine if the previous result was minus or bit 7 was on (1).
     *
     * The instruction affects no flags or other registers other than the P counter and only affects the P counter when the N bit is reset.
     *
     *     Branch on N = 0
     */
    bpl(data) {
        if (!this.get_flag(Flags.N)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction tests the status of the V flag and takes the conditional branch if the flag is not set.
     *
     * BVC does not affect any of the flags and registers other than the program counter and only when the overflow flag is reset.
     *
     *     Branch on V = 0
     */
    bvc(data) {
        if (!this.get_flag(Flags.V)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * This instruction tests the V flag and takes the conditional branch if V is on.
     *
     * BVS does not affect any flags or registers other than the program, counter and only when the overflow flag is set.
     *
     *     Branch on V = 1
     */
    bvs(data) {
        if (this.get_flag(Flags.V)) {
            return this.branch(data) + 1;
        }
        return 0;
    }

    /**
     * Used for branching operations.
     */
    branch(addr) {
        // add an extra cycle if it's on a different page
        const ret = this.same_page(addr, this.ip) ? 0 : 1;
        this.ip = addr;
        return ret;
    }

    /**
     * This instruction initializes the carry flag to a 0. This op­ eration should normally precede an ADC loop. It is also useful when used with a R0L instruction to clear a bit in memory.
     *
     * This instruction affects no registers in the microprocessor and no flags other than the carry flag which is reset.
     *
     *     0 → C
     */
    clc(_) {
        this.set_flag(Flags.C, false);
    }

    /**
     * This instruction sets the decimal mode flag to a 0. This all subsequent ADC and SBC instructions to operate as simple operations.
     *
     * CLD affects no registers in the microprocessor and no flags other than the decimal mode flag which is set to a 0.
     *
     *     0 → D
     */
    cld(_) {
        this.set_flag(Flags.D, false);
    }

    /**
     * This instruction initializes the interrupt disable to a 0. This allows the microprocessor to receive interrupts.
     *
     * It affects no registers in the microprocessor and no flags other than the interrupt disable which is cleared.
     *
     *     0 → I
     */
    cli(_) {
        this.set_flag(Flags.I, false);
    }

    /**
     * This instruction clears the overflow flag to a 0. This com­ mand is used in conjunction with the set overflow pin which can change the state of the overflow flag with an external signal.
     *
     * CLV affects no registers in the microprocessor and no flags other than the overflow flag which is set to a 0.
     *
     *     0 → V
     */
    clv(_) {
        this.set_flag(Flags.V, false);
    }

    /**
     * This instruction initializes the carry flag to a 1. This op eration should normally precede a SBC loop. It is also useful when used with a ROL instruction to initialize a bit in memory to a 1.
     *
     * This instruction affects no registers in the microprocessor and no flags other than the carry flag which is set.
     *
     *     1 → C
     */
    sec(_) {
        this.set_flag(Flags.C, true);
    }

    /**
     * This instruction sets the decimal mode flag D to a 1. This makes all subsequent ADC and SBC instructions operate as a decimal arithmetic operation.
     *
     * SED affects no registers in the microprocessor and no flags other than the decimal mode which is set to a 1.
     *
     *     1 → D
     */
    sed(_) {
        this.set_flag(Flags.D, true);
    }

    /**
     * This instruction initializes the interrupt disable to a 1. It is used to mask interrupt requests during system reset operations and during interrupt commands.
     *
     * It affects no registers in the microprocessor and no flags other than the interrupt disable which is set.
     *
     *     1 → I
     */
    sei(_) {
        this.set_flag(Flags.I, true);
    }

    nop(_) {}
}

export enum Flags {
    /**
     * The carry flag (C) flag is used as a buffer and as a borrow in arithmetic operations. Any comparisons will update this additionally to the Z and N flags, as do shift and rotate operations.
     */
    C = 1 << 0,
    /**
     * The zero flag (Z) indicates a value of all zero bits and the negative flag (N) indicates the presence of a set sign bit in bit-position 7. These flags are always updated, whenever a value is transferred to a CPU register (A,X,Y) and as a result of any logical ALU operations. The Z and N flags are also updated by increment and decrement operations acting on a memory location.
     */
    Z = 1 << 1,
    /**
     * The interrupt inhibit flag (I) blocks any maskable interrupt requests (IRQ).
     */
    I = 1 << 2,
    /**
     * The decimal flag (D) sets the ALU to binary coded decimal (BCD) mode for additions and subtractions (ADC, SBC). This flag is not used in the NES.
     */
    D = 1 << 3,
    /**
     * The break flag (B) is not an actual flag implemented in a register, and rather appears only, when the status register is pushed onto or pulled from the stack. When pushed, it will be 1 when transfered by a BRK or PHP instruction, and zero otherwise (i.e., when pushed by a hardware interrupt). When pulled into the status register (by PLP or on RTI), it will be ignored.
     *
     * In other words, the break flag will be inserted, whenever the status register is transferred to the stack by software (BRK or PHP), and will be zero, when transferred by hardware. Since there is no actual slot for the break flag, it will be always ignored, when retrieved (PLP or RTI). The break flag is not accessed by the CPU at anytime and there is no internal representation. Its purpose is more for patching, to discern an interrupt caused by a BRK instruction from a normal interrupt initiated by hardware.
     */
    B = 1 << 4,
    /**
     * Unused
     */
    _ = 1 << 5,
    /**
     * The overflow flag (V) indicates overflow with signed binary arithmetics. As a signed byte represents a range of -128 to +127, an overflow can never occur when the operands are of opposite sign, since the result will never exceed this range. Thus, overflow may only occur, if both operands are of the same sign. Then, the result must be also of the same sign. Otherwise, overflow is detected and the overflow flag is set. (I.e., both operands have a zero in the sign position at bit 7, but bit 7 of the result is 1, or, both operands have the sign-bit set, but the result is positive.)
     */
    V = 1 << 6,
    /**
     * The zero flag (Z) indicates a value of all zero bits and the negative flag (N) indicates the presence of a set sign bit in bit-position 7. These flags are always updated, whenever a value is transferred to a CPU register (A,X,Y) and as a result of any logical ALU operations. The Z and N flags are also updated by increment and decrement operations acting on a memory location.
     */
    N = 1 << 7,
}
