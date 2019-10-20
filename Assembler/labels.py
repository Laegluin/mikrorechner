import datastrings
import re
import exceptions as error

def process_labels(commands):
    try:
        return replace_labels_with_values(commands)
    except (error.label_duplicate_error,error.label_replacement_not_successful_error) as err:
        print(err.string)
        raise error.binary_file_not_creatable_exception('Binary File will not be created. Process has stoppd')

def replace_labels_with_values(commands):
    label_list = get_label_adress_dictionary(commands)
    label_values = get_label_values_dictionary(commands)
    commands_with_reljumps = commands
    labelname_options = '|'.join(label_list)
    addr = 0
    print('Input after replacing and cutting down Labels, with adresses')
    for line, command in enumerate(commands):
        if re.match('(jump)\s+(to)\s+(' + labelname_options + ')\s*(_\w+\s*)?(\s+#(.)*)?$', command):
            dest = label_list[command.split()[2]] - addr
            if dest - 4 > 0: commands_with_reljumps[line] = 'jump_rel to ' + str(dest)
            else: commands_with_reljumps[line] = 'jump_rel to ' + str(dest)
        elif re.match('(jump_if)\s+(to)\s+(' + labelname_options + ')\s*(_\w+\s*)?$', command):
            dest = label_list[command.split()[2]] - addr
            if dest - 4 > 0: commands_with_reljumps[line] = 'jump_rel_if to ' + str(dest)
            else: commands_with_reljumps[line] = 'jump_rel_if to ' + str(dest)
        elif re.match('(R)\d{1,2}\s+(=)\s+(' + labelname_options + ')\s*$', command):
            words = command.split()
            replacement = replace_register_equals_label(words[0],label_values[words[2]])
            commands_with_reljumps = commands_with_reljumps[:line] + replacement + commands_with_reljumps[line+1:]
        print(str(addr) + ':\t' + str(commands_with_reljumps[line]))
        try: addr += datastrings.necessary_byte_storage(command)
        except error.data_string_not_convertible_error: raise error.label_replacement_not_successful_error('Label konnten nicht ersetzt werden')
    print('\n')
    return commands_with_reljumps

#welches register dürfen wir zum zwischenspeichern benutzen?
def replace_register_equals_label(register, value):
    if datastrings.is_datastring(value):
        val = int(value,0)
        if val < 0: binary_string = '{0:{fill}{width}b}'.format((-124 + 2 ** 32) % 2 ** 32, fill='0', width=32)
        else: binary_string = '{0:{fill}{width}b}'.format(val, fill='0', width=32)
        replacement = [register + " = 0b" + binary_string[0:21],
                       "R3 = 11",
                       register + " = " + register + " << R3 times",
                       "R3 = 0b" + binary_string[21:],
                       register + " = " + register + " | R3"]
        return replacement
    return []

def get_label_adress_dictionary(commands):
    addr = 0
    label_duplicate = 0
    labels = {}
    for line, command in enumerate(commands):
        if(re.match(r'(.)*\s+(_)[A-Za-z0-9_-]+\s*$',command)):
            label_name = command.split()[-1][1:]
            for labelName in labels:
                if labelName == label_name: label_duplicate = 1
            if not label_duplicate:
                labels[label_name] = addr
            else:
                raise error.label_duplicate_error('Line ' + str(line+1) + ': Labelname ' + label_name + ' zweimal deklariert!')
        try: addr += datastrings.necessary_byte_storage(command)
        except error.data_string_not_convertible_error:
            print('Line ' + str(line+1) + ': datastring '+ command +'not convertible!')
    return labels

def get_label_values_dictionary(commands):
    labels_and_values = {}
    for line, command in enumerate(commands):
        if(re.match(r'(.)*\s+(_)[A-Za-z0-9_-]+\s*$',command)):
            label_name = command.split()[-1][1:]
            labels_and_values[label_name] = re.sub(r'_[A-Za-z0-9_-]+\s*$','',command)
    return labels_and_values

# if __name__ == '__main__':
#     start(sys.argv[0])