import re
import pickle
import os
import sys


befehle = {"copy": '00001',
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
           "compare_eq": '00011',
           "compare_gt": '00100',
           "compare_ge": '00101',
           "jump": '00110',
           "jump_if": '01000',
           "jump_rel_if": '01001',
           "jump_rel": '00111',
           "halt": '01101',
           "noop": '01100',
           "load": '01010',
           "store": '01011'}

def arithmetic(s):
    reg_ops = check_registers(s)
    bin = ""
    if (len(reg_ops) > 0):  # dann ist es gültig und wir können weiter machen
        words = re.sub("[\s=]", " ", s).split()  # array alles wörter die geblieben sind
        bin += get_opcode(['+', '-', '/', '*', '&', '|', '^'], words[2])
        bin += reg_ops[0]
        bin += reg_ops[1]
        bin += reg_ops[2]
        bin += '0' * 9  # dont cares
    return bin

def format_not(s):
    s = re.sub("[~]", " ", s)
    reg_ops = check_registers(s)
    bin = '10010'
    bin += reg_ops[0]
    bin += reg_ops[1]
    bin += '0' * 15
    return bin

def halt_noop(s):
    word = s.replace(' ', '')
    bin = get_opcode(['noop', 'halt'], word)
    if (bin == ''):
        print('Befehl nicht bekannt')
        return ''
    else:
        bin = bin + '0' * 27
    return bin

# für comparisons und copy
def format_2(s):
    reg_ops = check_registers(s)
    words = re.sub("[\s]", " ", s).split()
    w = words[0]
    bin = get_opcode(['compare_eq', 'compare_gt', 'compare_ge', 'copy'], w)
    if (bin == ''):
        return ''
    if (len(w) >= 7 and w[0:7] == 'compare'):
        bin += '0' * 6
        bin += reg_ops[0]
        bin += reg_ops[1]
        bin += '0' * 9
    elif (w == 'copy'):
        bin += reg_ops[1]
        bin += reg_ops[0]
        bin += '0' * 15
    return bin

def format_set(s):
    words = re.sub("[\s]", " ", s).split()
    w = words[0]
    reg_ops = check_registers(s)
    bin = '00010'
    bin += reg_ops[0]
    bin += immediate_to_binary(words[-1], 21)
    return bin

def format_jump(s):
    words = re.sub("[\s]", " ", s).split()
    w = words[0]
    reg_ops = check_registers(s)
    bin = get_opcode(['jump', 'jump_if', 'jump_rel_if', 'jump_rel'], w)
    if (bin == ''):
        return ''
    bin += reg_ops[0]
    bin += '0' * 21
    return bin

def format_jump_rel(s):
    words = re.sub("[\s]", " ", s).split()
    w = words[0]
    reg_ops = check_registers(s)
    bin = get_opcode(['jump', 'jump_if', 'jump_rel_if', 'jump_rel'], w)
    if (bin == ''):
        return ''
    bin += immediate_to_binary(words[2], 27)
    return bin

# wie ist gültigkeitsbereich in speicher für immediate werte?
def load_store(s):
    words = re.sub("[\s]", " ", s).split()
    bin = get_opcode(['load', 'store'], words[0])
    reg_ops = check_registers(s)
    if (bin == ''):
        return ''
    bin += reg_ops[1]
    bin += reg_ops[0]
    if (words[2] == '-'):
        words[3] = '-' + words[3]
    bin += immediate_to_binary(words[3], 15)
    return bin


# vielleicht eher für vorzeichen boolean einführen wg syntax der befehle
def immediate_to_binary(s, stellen):
    # hat s da immer die richtige form? --> vielleicht vorher nochmal mit regex prüfen
    i = 0
    i_bin = ''
    if (s[0] == '-'):
        i -= int(s[1:])
        if (i < -1048576):
            return ''
        i_bin = '{0:{fill}{width}b}'.format((i + 2 ** stellen) % 2 ** stellen, fill='0', width=stellen)
    else:
        i += int(s)
        if (i > 1048576):
            return ''
        i_bin += '{0:b}'.format(i)
        i_bin = '0' * (stellen - len(i_bin)) + i_bin
    return i_bin

def get_opcode(valid_commands, w):
    for c in valid_commands:
        if (w == c):
            return befehle[c]
    #ist nicht immer unbekannter befehl!
    print('Unbekannter Befehl ' + w + ', Fehler beim finden des Opcodes!')
    return ''

# überprüft registers nach gültiger zahl und gibt opcode für alle register zurück, leere liste wenn registerzahl ungültig
def check_registers(s):
    reg_ops = []
    words = re.sub("[\s]", " ", s).split()
    for w in words:  # wenn da ein R am anfang ist dann zahl konvertieren und prüfen und in opcode verändern
        if(w == 'null'):
            reg_ops.append('100000')
        elif w[0] == 'R':
            num = int(w[1:])
            if (num >= 0 and num < 32):
                reg_ops.append("{0:{fill}6b}".format(num, fill='0'))
            else:
                print('Registerzahl ungültig! R0-R31 sind gültige Register')
                reg_ops = []
                break
    return reg_ops

def format_shift(s):
    words = re.sub("[\s]", " ", s).split()
    reg_ops = check_registers(s)
    bin = get_opcode(['>>', '<<', '>>_s'], words[3])
    bin += reg_ops[0]
    bin += reg_ops[1]
    bin += reg_ops[2]
    return bin + '0' * 9

def check_format(input,path):
    output_array = []
    input_array = input.split('\n')
    for line, s in enumerate(input_array):
        if re.match(r'(R)\d{1,2}\s+(=)\s+(R)\d{1,2}\s+[+-/*^&|]\s+(R)\d{1,2}\s*$',s):  # format für arithmetische Befehle
            a = arithmetic(s)
        elif re.match(r'\w+\s*$', s):  # format für halt und noop
            a = halt_noop(s)
        elif re.match(r'\w+\s+(R)\d{1,2}\s+(to)\s+(R)\d{1,2}\s*$', s):  # format für comparisons und copy
            a = format_2(s)
        elif re.match(r'\w+\s+(R)\d{1,2}\s+(to)\s+[-]?\d{1,7}\s*$', s):  # format für set
            a = format_set(s)
        elif re.match(r'\w+\s+(to)\s+R\d{1,2}\s*$', s):  # format für jumps ohne rel
            a = format_jump(s)
        elif re.match(r'\w+\s+(to)\s\d+\s*$', s):  # format für jumps mit rel
            a = format_jump_rel(s)
        elif re.match(r'\w+\s+(R)\d{1,2}\s+[+-]\s+\d+\s+(to)\s+(R)\d{1,2}\s*$', s):  # format fuer load und store
            a = load_store(s)
        elif re.match(r'R\d{1,2}\s+[=]\s+[~]\s*[R]\d{1,2}\s*$',s):#format für not
            a = format_not(s)
        elif re.match(r'R\d{1,2}\s+(=)\s+(R)\d{1,2}\s+[<>]{2}\s+(R)\d{1,2}\s+(times)$',s):#format für shifts
            a = format_shift(s)
        else:
            print('Zeile ' + str(line + 1) + ' enthält keine gültige Form')
            break
        if (a == ""):
            break
        else:
            output_array.append(a)
    show_output_array(output_array)
    save_binary(output_array, path)

def show_output_array(a):
    for w in a:
        for c in range(32):
            print(w[c], end='')
            if ((c + 1) % 8 == 0):
                print(" ", end='')
        print('\n', end='')

def save_binary(a,s):
    datei = open(s, "wb+")
    for w in a:
        pickle.dump(int(w, 2), datei)
    datei.close()

#source ist pfad zu aktueller Datei
def start(source):
    if(os.path.exists(source)):
        if(source.endswith('.txt')):
            f = open(source,'r')
            check_format(f.read(),source.replace('.txt','.bin'))
        else:
            print('Pfad muss in Textdatei enden!')
    else:
        print('Pfad existiert nicht.\n- benutze slashes anstatt backslashes\n- relative oder absolute Dateipfade sind gültig')