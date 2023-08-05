import test from 'ava';
import { CPU } from './cpu';

test('simple program', (t) => {
    const cpu = new CPU();
    // prettier-ignore
    cpu.load_program([
        0xa9, 0x10, // LDA #$10     -> A = #$10 (value $10, or 16)
        0x85, 0x20, // STA $20      -> $20 = #$10 (address $20 = $10, or 16)
        0xa9, 0x01, // LDA #$1      -> A = #$1
        0x65, 0x20, // ADC $20      -> A = #$11
        0x85, 0x21, // STA $21      -> $21=#$11
        0xe6, 0x21, // INC $21      -> $21=#$12
        0xa4, 0x21, // LDY $21      -> Y=#$12
        0xc8,       // INY          -> Y=#$13
        0x00,       // BRK
    ]);

    cpu.run_program();

    t.is(cpu.peek(0x20), 0x10);
    t.is(cpu.peek(0x21), 0x12);
    t.is(cpu.register_a, 0x11);
    t.is(cpu.register_y, 0x13);
});

test('addr_mode_indirect', (t) => {
    const cpu = new CPU();
    // prettier-ignore
    cpu.load_program([
      0x6c, 0x82, 0xff // JMP ($FF82)
    ]);

    cpu.memory.set(0xff82, 0xc4);
    cpu.memory.set(0xff83, 0x80);

    cpu.pc(); // increment program counter - we don't actually need the opcode for this test
    var data = cpu.mode_ind();
    t.is(data, 0x80c4);
});

test('addr_mode_izx', (t) => {
    const cpu = new CPU();
    // prettier-ignore
    cpu.load_program([
      0xa1, 0x70 // LDA ($70,X)
    ]);

    cpu.register_x = 0x05;

    cpu.memory.set(0x75, 0x23);
    cpu.memory.set(0x76, 0x30);
    cpu.memory.set(0x3023, 0xa5);

    cpu.pc(); // increment program counter - we don't actually need the opcode for this test
    var data = cpu.mode_izx();
    t.is(data, 0xa5);
});

test('addr_mode_izy', (t) => {
    const cpu = new CPU();
    // prettier-ignore
    cpu.load_program([
      0xb1, 0x70 // ($70),Y
    ]);

    cpu.register_y = 0x10;

    cpu.memory.set(0x70, 0x43);
    cpu.memory.set(0x71, 0x35);
    cpu.memory.set(0x3553, 0x23);

    cpu.pc(); // increment program counter - we don't actually need the opcode for this test
    var data = cpu.mode_izy();
    t.is(data, 0x23);
});

test('addr_mode_rel', (t) => {
    const cpu = new CPU();
    // prettier-ignore
    cpu.load_program([
    0xea, // NOOP
    0xf0, 0x03 // ($70),Y
  ]);

    cpu.pc(); // increment program counter twice
    cpu.pc();
    var data = cpu.mode_rel();
    t.is(data, 0x05);
});
