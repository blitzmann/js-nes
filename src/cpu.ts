/**
 * CPU class. Documentation:
 * https://www.masswerk.at/6502/6502_instruction_set.html
 * https://www.pagetable.com/c64ref/6502/?tab=3
 */
export class CPU {
    program = [];

    program_counter = -1;
    register_a = 0x0;
    register_x = 0x0;
    register_y = 0x0;
    cycles = 0;
    stopped = false;
    status_register = 0;
    memory = new Map();

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

        [0x65, [this.adc, this.mode_zp0, 3]],

        [0xee, [this.inc, this.mode_abs, 6]],
        [0xf6, [this.inc, this.mode_zpx, 6]],
        [0xf6, [this.inc, this.mode_abx, 7]],
        [0xe6, [this.inc, this.mode_zp0, 5]],

        [0xa4, [this.ldy, this.mode_zp0, 3]],
        [0xc8, [this.iny, this.mode_imp, 2]],
        [0x00, [this.brk, this.mode_imp, 7]],

        [0x6c, [this.jmp, this.mode_ind, 5]],
    ]);

    pc() {
        return (this.program_counter += 1);
    }

    load_program(bytes) {
        this.program = bytes;
    }

    run_program() {
        while (!this.stopped) {
            this.step();
        }
    }

    step() {
        const opcode = this.program[this.pc()];
        const op_def = this.opcodes.get(opcode);
        if (!op_def) {
            console.log('unknown op code: 0x' + opcode.toString(16));
        }

        const [op, mode, cycles] = op_def;
        this.addr = mode.bind(this)();

        op.bind(this)(this.addr);
        this.cycles += cycles;
    }

    peek(addr) {
        return this.memory.get(addr);
    }

    set_flag(flag: Flags, val: boolean) {
        if (val) {
            this.status_register |= flag;
        } else {
            this.status_register &= ~flag;
        }
    }

    get_flag(flag: Flags) {
        return !!(this.status_register & flag);
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
        this.program_counter++;
        return this.program[this.program_counter];
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
        this.program_counter++;
        let low = this.program[this.program_counter];
        this.program_counter++;
        let high = this.program[this.program_counter];
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
        this.program_counter++;
        return this.program[this.program_counter];
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
        this.program_counter++;
        let low = this.program[this.program_counter];
        this.program_counter++;
        let high = this.program[this.program_counter];
        return ((high << 8) | low) + this.register_x;
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
        this.program_counter++;
        let low = this.program[this.program_counter];
        this.program_counter++;
        let high = this.program[this.program_counter];
        return ((high << 8) | low) + this.register_y;
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
        this.program_counter++;
        return (this.program[this.program_counter] + this.register_x) & 0x00ff; // truncate to 255. We do not care about carry.
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
        this.program_counter++;
        return (this.program[this.program_counter] + this.register_y) & 0x00ff; // truncate to 255. We do not care about carry.
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
        this.program_counter++;
        let low = this.program[this.program_counter];
        this.program_counter++;
        let high = this.program[this.program_counter];
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
        this.program_counter++;
        let addr = this.program[this.program_counter] + this.register_x;

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
        this.program_counter++;
        let addr = this.program[this.program_counter];

        let low = this.memory.get(addr);
        let high = this.memory.get(addr + 1);
        addr = (high << 8) | low;
        return this.memory.get(addr + this.register_y);
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
        this.program_counter++;
        return this.program[this.program_counter] + this.program_counter;
    }

    /**
     * IMP: Implied. Nothing happens
     */
    mode_imp() {
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
        this.register_a = data;
        this.set_flag(Flags.Z, this.register_a === 0);
        this.set_flag(Flags.N, !!(this.register_a & (1 << 7)));
    }

    /**
     * Load the index register X from memory.
     *
     * LDX does not affect the C or V flags; sets Z if the value loaded was zero, otherwise resets it; sets N if the value loaded in bit 7 is a 1; otherwise N is reset, and affects only the X register.
     *
     *     M → X
     */
    ldx(data) {
        this.register_x = data;
        this.set_flag(Flags.Z, this.register_x === 0);
        this.set_flag(Flags.N, !!(this.register_x & (1 << 7)));
    }

    /**
     * Load the index register Y from memory.
     *
     * LDY does not affect the C or V flags, sets the N flag if the value loaded in bit 7 is a 1, otherwise resets N, sets Z flag if the loaded value is zero otherwise resets Z and only affects the Y register.
     *
     *     M → Y
     */
    ldy(data) {
        this.register_y = data;
        this.set_flag(Flags.Z, this.register_y === 0);
        this.set_flag(Flags.N, !!(this.register_y & (1 << 7)));
    }

    /**
     * This instruction transfers the contents of the accumulator to memory.
     *
     * This instruction affects none of the flags in the processor status register and does not affect the accumulator.
     *
     *     A → M
     */
    sta(data) {
        this.memory.set(data, this.register_a);
    }

    /**
     * Transfers value of X register to addressed memory location.
     *
     * No flags or registers in the microprocessor are affected by the store operation.
     *
     *     X → M
     */
    stx(data) {
        this.memory.set(data, this.register_x);
    }

    /**
     * Transfer the value of the Y register to the addressed memory location.
     *
     * STY does not affect any flags or registers in the microprocessor.
     *
     *     Y → M
     */
    sty(data) {
        this.memory.set(data, this.register_y);
    }

    /**
     * This instruction adds the value of memory and carry from the previous operation to the value of the accumulator and stores the result in the accumulator.
     *
     * This instruction affects the accumulator; sets the carry flag when the sum of a binary add exceeds 255 or when the sum of a decimal add exceeds 99, otherwise carry is reset. The overflow flag is set when the sign or bit 7 is changed due to the result exceeding +127 or -128, otherwise overflow is reset. The negative flag is set if the accumulator result contains bit 7 on, otherwise the negative flag is reset. The zero flag is set if the accumulator result is 0, otherwise the zero flag is reset.
     */
    adc(data) {
        var fetched = this.memory.get(data);
        const tmp = this.register_a + fetched + this.get_flag_bit(Flags.C);

        // Overflow: https://forums.nesdev.org/viewtopic.php?t=6331
        // https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        this.set_flag(
            Flags.V,
            !!(~(this.register_a ^ fetched) & (this.register_a ^ tmp) & 0x0080)
        );

        this.register_a = tmp & 0x00ff; // truncate to 255

        this.set_flag(Flags.C, tmp > 255);
        this.set_flag(Flags.Z, this.register_a === 0);
        this.set_flag(Flags.N, !!(tmp & 0x0080));
    }

    /**
     * This instruction adds 1 to the contents of the addressed memory loca­tion.
     *
     * The increment memory instruction does not affect any internal registers and does not affect the carry or overflow flags. If bit 7 is on as the result of the increment,N is set, otherwise it is reset; if the increment causes the result to become 0, the Z flag is set on, otherwise it is reset.
     */
    inc(data) {
        const val = this.memory.get(data) + 1;
        this.memory.set(data, val);

        this.set_flag(Flags.Z, val === 0);
        this.set_flag(Flags.N, !!(val & 0x0080));
    }

    iny(_) {
        this.register_y += 1;
    }

    jmp(data) {
        console.warn('jmp not implemented');
    }

    brk(_) {
        this.stopped = true;
    }
}

enum Flags {
    /**
     * The carry flag (C) flag is used as a buffer and as a borrow in arithmetic operations. Any comparisons will update this additionally to the Z and N flags, as do shift and rotate operations.
     */
    C,
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
