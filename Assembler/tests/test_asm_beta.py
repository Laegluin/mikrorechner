from tests import test
import commands_to_binary_string as asm
import exceptions as errors

rules = {
    'arithmetic': {'opcode': ['+', '-', '/', '*', '&', '|', '^'], 'index_of_command': 3, 'registers?': 1, 'register_indexes?': 0,'immediate?': 0},
    'datastring': {'opcode': [], 'index_of_command': 0, 'registers?': 0, 'register_indexes?': [], 'immediate?': 1,'register_indexes': [], 'immediate_length': 32, 'immediate_word_in_command': 0, 'unsigned?': 0},
    'noop_halt': {'opcode': ['noop', 'halt'], 'index_of_command': 0, 'registers?': 0, 'register_indexes?': 0, 'immediate?': 0},
    'copy': {'opcode': ['copy'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 0, 'register_indexes': [11, 5]},
    'get_compare_information_compare': {'opcode': ['get_compare_information_compare>', 'get_compare_information_compare>=', 'get_compare_information_compare='], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 0, 'register_indexes': [11, 17]},
    'set': {'opcode': ['set'], 'index_of_command': 1, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 1, 'register_indexes': [], 'immediate_length': 21, 'immediate_word_in_command': -1, 'unsigned?': 0},
    'jump': {'opcode': ['jump', 'jump_if'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 0, 'register_indexes': [11]},
    'jump_rel': {'opcode': ['jump_rel', 'jump_rel_if'], 'index_of_command': 0, 'registers?': 0, 'register_indexes?': 0, 'immediate?': 1, 'register_indexes': [], 'immediate_length': 27, 'immediate_word_in_command': 2, 'unsigned?': 0},
    'store': {'opcode': ['store'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 1, 'register_indexes': [11, 5], 'immediate_length': 15, 'immediate_word_in_command': 5, 'unsigned?': 1},
    'load': {'opcode': ['load'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 1, 'register_indexes': [11, 5], 'immediate_length': 15, 'immediate_word_in_command': 3, 'unsigned?': 1},
    'not': {'opcode': ['~'], 'index_of_command': 1, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 0},
    'shift':{'opcode': ['<<', '>>', '>>_s'], 'index_of_command': 3, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 0},
    '\S+': {},
    '\s*': {}
}

#einige befehle müssen anders getestet werden weil der text vorher verändert wird um ein einheitliches schema zu finden
def test_get_binary_string_for_command():
    test.assertEquals(asm.get_binary_string_for_command(rules['arithmetic'], 'R14 = R1 + R31'),
                      '00000001110000001011111' +'0' * 9)
    test.assertEquals(asm.get_binary_string_for_command(rules['not'], 'R15 ~ R25'),
                      '10010001111011001' +'0' * (32-(2*6+5)))
    test.assertEquals(asm.get_binary_string_for_command(rules['shift'], 'R2 = R17 << R28 times'),
                      '10100000010010001011100' +'0' * (32-(3*6+5)))
    test.assertEquals(asm.get_binary_string_for_command(rules['copy'], 'copy R4 to R18'),
                      '00001010010000100' +'0' * (32-17))
    test.assertEquals(asm.get_binary_string_for_command(rules['set'], 'R31 set 1'),
                      '00010011111' +'0' * 20 + '1')
    test.assertEquals(asm.get_binary_string_for_command(rules['set'], 'R31 set -1'),
                      '00010011111' + '1' * 21)
    test.assertEquals(asm.get_binary_string_for_command(rules['get_compare_information_compare'], 'get_compare_information_compare= R5 = R6'),
                      '00011' +'0' * 6 + get_register_string(5) + get_register_string(6) +'0' * 9)
    test.assertEquals(asm.get_binary_string_for_command(rules['jump'], 'jump to R4'),
                      '00110' + '0' * 6 + get_register_string(4) + '0' * (32-17))
    test.assertEquals(asm.get_binary_string_for_command(rules['jump_rel'], 'jump_rel to -1'),
                      '00111' + '1' * (32-5))
    test.assertEquals(asm.get_binary_string_for_command(rules['jump_rel'], 'jump_rel to 1'),
                      '00111' + '0' * (32 - 6) + '1')
    test.assertEquals(asm.get_binary_string_for_command(rules['noop_halt'], 'noop'),
                      '01100' + '0' * (32 - 5))
    test.assertEquals(asm.get_binary_string_for_command(rules['store'], 'store R4 to R4 + 32767'),
                      '01011000100000100' +'1' * 15)
    test.assertEquals(asm.get_binary_string_for_command(rules['load'], 'load R4 + 32767 to R5'),
                      '01010000101000100' + '1' * 15)
    # R31 = 9999999999999999999999999999999999999999999999
    # ungültig
    # jump_rel to 9999999999999999999999999999999999999999999999999999999
    # ungültig
    # load R4 + -25 to R4
    # ungültig
    # load R4 + 25 to R4
    # ungültig
    # store R4 to R4 + -25
    # ungültig

def test_compare(): return 0

def test_set_command(): return 0

def test_not_command():
    command = 'R4  = ~R4'
    test.assert_not_equals(asm.not_command(command)[0], command)
    test.assertEquals(asm.not_command('R5 = ~R5')[0], 'R5 ~ R5')

def test_immediate_to_binary_signed():
    test.assertEquals(asm.immediate_to_binary_signed('0b111', 4), '0111')
    test.assertEquals(asm.immediate_to_binary_signed('-1', 4), '1' * 4)
    test.assert_exception(asm.immediate_to_binary_signed, ['0b11', 1], errors.immediate_out_of_range_exception)
    test.assert_exception(asm.immediate_to_binary_signed, ['-0b11', 2], errors.immediate_out_of_range_exception)

def test_immediate_to_binary_unsigned():
    test.assertEquals(asm.immediate_to_binary_unsigned('0b11', 2), '11')
    test.assertEquals(asm.immediate_to_binary_unsigned('0xF', 4), '1111')
    test.assertEquals(asm.immediate_to_binary_unsigned('15', 4), '1111')
    test.assert_exception(asm.immediate_to_binary_unsigned, ['-11', 20], errors.immediate_out_of_range_exception)
    test.assert_exception(asm.immediate_to_binary_unsigned, ['0b1111', 3], errors.immediate_out_of_range_exception)
    test.assert_exception(asm.immediate_to_binary_unsigned, ['0b100', 2], errors.immediate_out_of_range_exception)

def test_get_opcode():
    test.assertEquals(asm.get_opcode(['+'], '+'), '00000')
    test.assertEquals(asm.get_opcode(['+', '-', '/'], '+'), '00000')
    test.assertEquals(asm.get_opcode([], '+'), '')
    test.assert_exception(asm.get_opcode, [['+'], 'get'], errors.invalid_command_exception)
    test.assert_exception(asm.get_opcode, [['-'], '+'], errors.invalid_command_exception)

def test_write_regs():
    test.assertEquals(asm.write_regs('R11', '0' * 6), '0' * 5 + get_register_string(11))
    test.assert_exception(asm.write_regs, ['R45', '0' * 32], errors.illegal_register_number_exception)
    test.assertEquals(asm.write_regs('R15 R15', '0' * 32), '0' * 5 + get_register_string(15) + get_register_string(15) + '0' * 15)
    #fall beachten? string kleiner als anzahl der register --> man kann weiter als string groß ist schreiben
    #test.assertEquals(asm.set_registers_in_binary_string('R15 R15', '0' * 2), '')

def test_write_regs_idx():
    test.assertEquals(asm.write_regs_idx('R11 R13 hallo R13', '0' * 18, [0, 6, 12]), get_register_string(11) + get_register_string(13) + get_register_string(13))
    test.assertEquals(asm.write_regs_idx('', '0' * 32, []), '0' * 32)
    test.assert_exception(asm.write_regs_idx, ['R11', '0' * 32, [0, 7]], errors.illegal_number_of_indexes_exception)
    test.assert_exception(asm.write_regs_idx, ['R43', '0' * 32, [0]], errors.illegal_register_number_exception)
    test.assert_exception(asm.write_regs_idx, ['', '0' * 32, [0]], errors.illegal_number_of_indexes_exception)

def test_get_registers():
    test.assertEquals(asm.get_registers('R12'), [get_register_string(12)])
    test.assertEquals(asm.get_registers('0xFF'), [])
    test.assertEquals(asm.get_registers('R1'), [get_register_string(1)])
    test.assertEquals(asm.get_registers('hallo R1 hallo R2'), [get_register_string(1), get_register_string(2)])
    test.assert_exception(asm.get_registers, ['R45'], errors.illegal_register_number_exception)

def get_register_string(number):
    return '{0:{fill}6b}'.format(number, fill='0')

def test_all():
    test_immediate_to_binary_signed()
    test_immediate_to_binary_unsigned()
    test_get_registers()
    test_write_regs_idx()
    test_write_regs()
    test_get_opcode()
    test_not_command()
    test_get_binary_string_for_command()