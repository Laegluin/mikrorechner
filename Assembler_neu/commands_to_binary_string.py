import re
import exceptions as error
import immediates as imm
import opcodes as opc
import registers as reg
import trim

def process_commands(commands):
    binary_strings = []
    create_binary = 1
    commands = reg.replace_special_register_names(commands)
    commands = trim.trim_word_gap(commands)
    for line, command in enumerate(commands):
        try:
            for regex in regex_process:
                if re.match(regex,command):
                    if type(regex_process[regex]) is dict:
                        rules_for_binary_string = regex_process[regex]
                        break
                    else:
                        result = regex_process[regex](command)
                        command = result['command']
                        rules_for_binary_string = result['rules']
                        break
            binary_string = get_binary_string_for_command(rules_for_binary_string,command)
            binary_strings.append(binary_string)
        except error.is_datastring_exception:
            binary_strings.append(command)
        except Exception as err:
            boo = error.handle_exceptions(err,line+1)
            create_binary = create_binary and boo
    if create_binary:
        return binary_strings
    else:
         raise error.binary_file_not_creatable_exception('Binary file will not be created.')

def get_binary_string_for_command(rules, command):
    words = command.split()
    binary_string = '0' * 32
    op = opc.get_opcode(rules['opcode'], words[rules['index_of_command']])
    binary_string = op + binary_string[5:]
    if rules['registers?'] and rules['register_indexes?']:
        binary_string = reg.set_registers_in_binary_string_indexed(command, binary_string, rules['register_indexes'])
    elif rules['registers?']:
        binary_string = reg.set_registers_in_binary_string(command, binary_string)
    if rules['immediate?']:
        if rules['unsigned?']: a = imm.immediate_to_binary_unsigned(words[rules['immediate_word_in_command']], rules['immediate_length'])
        else: a = imm.immediate_to_binary_signed(words[rules['immediate_word_in_command']], rules['immediate_length'])
        binary_string = binary_string[:(32 - rules['immediate_length'])] + a
    return binary_string

def get_compare_information_compare(command):
    cmp_cmd = command
    cmp_cmd = cmp_cmd.replace(cmp_cmd.split()[0],cmp_cmd.split()[0] + cmp_cmd.split()[2])
    l = {'opcode': ['compare>', 'compare>=', 'compare='], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 0, 'register_indexes': [11, 17]}
    return {'command': cmp_cmd, 'rules': l}

def get_command_information_set(command):
    set_cmd = command
    set_cmd = set_cmd.replace(set_cmd.split()[1],'set')
    l = {'opcode': ['set'], 'index_of_command': 1, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 1, 'register_indexes': [], 'immediate_length': 21, 'immediate_word_in_command': -1, 'unsigned?': 0}
    return {'command': set_cmd, 'rules': l}

def not_command(command):
    not_cmd = command
    not_cmd = not_cmd.replace('~', '')
    not_cmd = not_cmd.replace(not_cmd.split()[1], '~')
    l = {'opcode': ['~'], 'index_of_command': 1, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 0}
    return {'command': not_cmd, 'rules': l}

def no_valid_format(command):
    raise error.invalid_format_exception(command)

def skip_line(command):
    raise error.empty_line_exception

def data_string(command):
    raise error.is_datastring_exception

def get_copy_command_information(command):
    copy_cmd = command
    copy_cmd = copy_cmd.replace('=', 'copy')
    l = {'opcode': ['copy'], 'index_of_command': 1, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 0}
    return {'command': copy_cmd, 'rules': l}

register_regex = 'R\d{1,2}'
immediate_regex_signed = '[-]?(0b|0x)?[0-9A-Fa-f]+'
immediate_regex_unsigned = '(0b|0x)?[0-9A-Fa-f]+'
equals_regex = '\s+(=)\s+'

regex_process = {
    '{register}{equals}{register}\s+[+-/*^&|]\s+{register}\s*$'.format(register=register_regex,equals=equals_regex):
        {'opcode': ['+', '-', '/', '*', '&', '|', '^'], 'index_of_command': 3, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 0},
    '[01]+$': 
        data_string,
    '(halt|noop)\s*$':
        {'opcode': ['noop', 'halt'], 'index_of_command': 0, 'registers?': 0, 'register_indexes?': 0, 'immediate?': 0},
    '(compare)\s+{register}\s+(>|>=|=)\s+{register}\s*$'.format(register=register_regex): 
        get_compare_information_compare,
    '{register}{equals}{immediate}\s*$'.format(register=register_regex,immediate=immediate_regex_signed,equals=equals_regex):
        get_command_information_set,
    '(jump|jump_if)\s+(to)\s+{register}\s*$'.format(register=register_regex):
        {'opcode': ['jump', 'jump_if'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 0, 'register_indexes': [11]},
    '(jump_rel|jump_rel_if)\s+(to)\s+{immediate}\s*$'.format(immediate=immediate_regex_signed):
        {'opcode': ['jump_rel', 'jump_rel_if'], 'index_of_command': 0, 'registers?': 0, 'register_indexes?': 0, 'immediate?': 1, 'register_indexes': [], 'immediate_length': 27, 'immediate_word_in_command': 2, 'unsigned?': 0},
    '(store)\s+{register}\s+(to)\s+{register}\s+[+]\s+{immediate}\s*$'.format(register=register_regex,immediate=immediate_regex_unsigned):
        {'opcode': ['store'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 1, 'register_indexes': [11, 5], 'immediate_length': 15, 'immediate_word_in_command': 5, 'unsigned?': 1},
    '(load)\s+{register}\s+[+]\s+{immediate}\s+(to)\s+{register}\s*$'.format(register=register_regex,immediate=immediate_regex_unsigned):
        {'opcode': ['load'], 'index_of_command': 0, 'registers?': 1, 'register_indexes?': 1, 'immediate?': 1, 'register_indexes': [11, 5], 'immediate_length': 15, 'immediate_word_in_command': 3, 'unsigned?': 1},
    '{register}{equals}[~]{register}\s*$'.format(register=register_regex,equals=equals_regex):
        not_command,
    '{register}{equals}{register}\s+(<<|>>)(_s)?\s+{register}\s+(times)$'.format(register=register_regex,equals=equals_regex):
        {'opcode': ['<<', '>>', '>>_s'], 'index_of_command': 3, 'registers?': 1, 'register_indexes?': 0, 'immediate?': 0},
    '{register}{equals}{register}\s*$'.format(register=register_regex,equals=equals_regex):
        get_copy_command_information,
    '\s*$': 
        skip_line,
    '(.)*$': 
        no_valid_format,
}

