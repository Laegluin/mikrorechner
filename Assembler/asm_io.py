import math
import os

#nochmal fürs debuggen mit adressen printen

def read_assembler_file(source):
    if(os.path.exists(source)):
        if(source.endswith('.txt')):
            file = open(source,'r')
            return file.read()
        else:
            print('Pfad muss in Textdatei enden!')
    else:
        print('Pfad existiert nicht.\n- benutze slashes anstatt backslashes\n- relative oder absolute Dateipfade sind gültig')

def save_binary(data, binary_strings, destination_file):
    try:
        with open(destination_file.replace('txt', 'bin'), 'wb+') as f:
            for (idx, w) in enumerate(binary_strings):
                i = int(w, 2)
                if idx in data:
                    x = i.to_bytes(math.ceil(len(w) / 8), 'big')
                else:
                    x = i.to_bytes(4, 'little')
                f.write(x)
            f.truncate()
    except IOError as e:
        print("Konnte Binärdatei nicht lesen oder schreiben (%s)." % e)

def show_titled_binary_array(title,array):
    print(title)
    show_binary_array(array)

#in big endian darstellung
def show_binary_array(array):
    idx = 0
    for w in array:
        print(str(idx) + ':\t', end='')
        idx += 1
        # jedes w printen sodass alle 8 Schritte was gedruckt wird
        for c in range(len(w)):
            print(w[c], end='')
            if ((c + 1) % 8 == 0):
                print(" ", end='')
        print('\n', end='')
    print('\n')

def show_titled_command_array(title, commands):
    print(title)
    show_command_array(commands)

def show_command_array(commands):
    for c in commands: print(c)
    print('\n')

def execute_function_on_asm_file(source,fn):
    assembler_text = read_assembler_file(source)
    fn(assembler_text.split('\n'))