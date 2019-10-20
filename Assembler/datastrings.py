import re
import math
import exceptions as error
import commands_to_binary_string as asm
import trim

def process_datastrings(commands):
    commands_datastrings_to_binary = commands
    for line,command in enumerate(commands):
        if is_datastring(command): commands_datastrings_to_binary[line] = datastring_to_binary_string(command)
    return commands_datastrings_to_binary

def datastring_to_binary_string(data_string):
    try:
        integer = int(data_string,0)
    except: raise error.data_string_not_convertible_error(data_string)
    if integer < 0:
        return asm.immediate_to_binary_signed(data_string, 32)
    else:
        length = max(1, math.ceil(math.ceil(math.log2(integer + 1)) / 8) * 8)
        length = min(32, length)
        return asm.immediate_to_binary_unsigned(data_string, length)

def is_datastring(datastring):
    if re.match(r'0x[0-9A-Fa-f]+\s*$', datastring): return 1
    elif re.match(r'0b[01]+\s*$', datastring): return 1
    elif re.match('(-)?[0-9]+\s*$', datastring): return 1
    else: return 0

def get_datastring_indexes(commands):
    data = []
    command_count = 0
    for line, command in enumerate(commands):
        if is_datastring(command): data.append(command_count)
        if not re.match(r'\s*$', command): command_count += 1
    return data

def necessary_byte_storage(data_string):
    data_string = trim.cut_labels([data_string])[0]
    if is_datastring(data_string) and re.match('(0x|0b)?[0-9A-Fa-f]+\s*$', data_string):
        data_string = data_string.split()[0]
        i = int(data_string, 0)
        necessary_bytes = max(1, math.ceil(math.ceil(math.log2(i + 1)) / 8))
        return necessary_bytes
    elif re.match('(0x|0b)?[0-9A-Fa-f]+\s+_[A-Za-z0-9_-]+\s*$', data_string): raise error.data_string_not_convertible_error('')
    elif not re.match(r'\s*$', data_string): return 4
    else: return 0