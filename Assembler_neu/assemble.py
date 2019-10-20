import labels as labels
import commands_to_binary_string as asm
import datastrings as datastrings
import exceptions as error
import asm_io
import trim
import sys

def assemble(source):
    try:
        assembler_text = asm_io.read_assembler_file(source)
        commands = assembler_text.split('\n')

        commands = trim.trim_before_label_module(commands)
        asm_io.show_titled_command_array('Trimmed assemblertext',commands)

        commands = labels.process_labels(commands)
        commands = trim.cut_labels(commands)
        #hier wird innerhalb des labelmoduls geprintet weil wir die adressen brauchen

        data = datastrings.get_datastring_indexes(commands)
        commands = datastrings.process_datastrings(commands)
        asm_io.show_titled_command_array('Input after replacing datastrings with binary string', commands)

        commands = asm.process_commands(commands)
        asm_io.show_titled_binary_array('Successful conversion to binary strings!\nBinary file: ', commands)

        asm_io.save_binary(data, commands, source)
        print('Binary saved')
    except error.binary_file_not_creatable_exception as err:
        print(err.string)
    # except Exception as err:
    #     print('Other error occured while assembling file ' + source + ':\n' + str(err))

if __name__ == "__main__":
    a = sys.argv[1]
    assemble(a)