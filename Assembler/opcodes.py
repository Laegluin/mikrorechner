import exceptions as error

opcode_of_commands = {"copy": '00001',
           "set": '00010',
           "+": '00000',
           "-": '10111',
           "*": '01110',
           "/": '01111',
           "&": '10000',
           "|": '10001',
           "~": '10010',
           "^": '10011',
           "<<": '10100',
           ">>": '10101',
           ">>_s": '10110',
           "compare=": '00011',
           "compare>": '00100',
           "compare>=": '00101',
           "jump": '00110',
           "jump_if": '01000',
           "jump_rel_if": '01001',
            "jump_rel": '00111',
            "halt": '01101',
            "noop": '01100',
            "load": '01010',
            "store": '01011'}

def get_opcode(valid_commands, command):
    for cmd in valid_commands:
        if (command == cmd): return opcode_of_commands[cmd]
    if not valid_commands: return ''
    else:
        raise error.invalid_command_exception(command)