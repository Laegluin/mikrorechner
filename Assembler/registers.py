import re
import exceptions as error

def set_registers_in_binary_string(command, binary_string):
    registernumbers_binary = get_registers_binary_strings(command)
    i = 0
    for x in range(5, 5 + len(registernumbers_binary) * 6, 6):
        binary_string = binary_string[:x] + registernumbers_binary[i] + binary_string[x + 6:]
        i += 1
    return binary_string

def set_registers_in_binary_string_indexed(command, binary_string, positions_in_command):
    registernumbers_binary = get_registers_binary_strings(command)
    if len(registernumbers_binary) != len(positions_in_command):
        raise error.illegal_number_of_indexes_exception('Binary string indexes do not match number of registers mentioned in command.')
    for x in range(len(registernumbers_binary)):
        binary_string = binary_string[:positions_in_command[x]] + registernumbers_binary[x] + binary_string[positions_in_command[x] + 6:]
    return binary_string

def get_registers_binary_strings(command):
    registers = []
    words = command.split()
    for word in words:
        if re.match('R\d{1,2}$',word):
            number_of_register = int(word[1:])
            if 0 <= number_of_register <= 33:
                registers.append("{0:{fill}6b}".format(number_of_register, fill='0'))
            else:
                raise error.illegal_register_number_exception(word)
    return registers

def replace_special_register_names(commands):
    return [command.replace('offset', 'R33').replace('null','R32') for command in commands]