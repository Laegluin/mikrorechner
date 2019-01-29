import re
import os
import math
#register auf adresse von label setzen

#bei save binary noch markieren was in normaler reihenfolge gespeichert werden muss und was mit little endian

#store und load mit labels ohne immediate wert
#copy auch mit = anstatt set??

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

def immediate_to_binary(s, stellen,unsigned):
    i = 0
    i_bin = ''
    if (s[0] == '-'):
        if unsigned: return ''
        i -= int(s[1:])
        if (i < -(2 ** (stellen-1))):
            return ''
        i_bin = '{0:{fill}{width}b}'.format((i + 2 ** stellen) % 2 ** stellen, fill='0', width=stellen)
    else:
        i += int(s)
        if unsigned and i >= (2**stellen): return ''
        elif not (unsigned or not (i > (2 ** (stellen - 1)) - 1)):
            return ''
        i_bin += '{0:b}'.format(i)
        i_bin = '0' * (stellen - len(i_bin)) + i_bin
    return i_bin

def get_opcode(valid_commands, w):
    for c in valid_commands:
        if (w == c): return befehle[c]
    return ''

# überprüft registers nach gültiger zahl und gibt opcode für alle register zurück, leere liste wenn registerzahl ungültig
def check_registers(s):
    reg_ops = []
    words = re.sub("[\s]", " ", s).split()
    for w in words:  # wenn da ein R am anfang ist dann zahl konvertieren und prüfen und in opcode verändern
        if w[0] == 'R':
            num = int(w[1:])
            if 0 <= num <= 33:
                reg_ops.append("{0:{fill}6b}".format(num, fill='0'))
            else:
                reg_ops = []
                break
    return reg_ops

#schreibt bin string um und fügt der reihe nach register ein in den string, nachm opcode
def write_regs(s,bi):
    reg_ops = check_registers(s)
    if not reg_ops: return ''
    i = 0
    for x in range(5,5 + len(reg_ops)*6,6):
        bi = bi[:x] + reg_ops[i] + bi[x+6:]
        i += 1
    return bi

#andere version mit angegebenen indexen wo der reihe nach in assemblersyntax die register im bitcode hinsollen
def write_regs_id(s,bi,idx):
    reg_ops = check_registers(s)
    if not reg_ops: return ''
    for x in range(len(reg_ops)):
        bi = bi[:idx[x]] + reg_ops[x] + bi[idx[x]+6:]
    return bi

def check_format(input,path):
    output_array = []
    input_array = input.split('\n')
    fehler = 0
    idx_data = []
    for line, s in enumerate(input_array):
        b = '0' * 32
        s = re.sub(r'#(.)*','',s)
        s = s.replace('null','R32')
        s = s.replace('offset','R33')
        words = re.sub("[\s]", " ", s).split()
        #liste mit: [[opcodes],befehlswortIndex, registerSchreiben?, registerIndexe?, restImmediate?, [indexe], laengeImmediate,wordImmediate,unsigned?]
        if re.match(r'(R)\d{1,2}\s+(=)\s+(R)\d{1,2}\s+[+-/*^&|]\s+(R)\d{1,2}\s*$',s):  # format für arithmetische Befehle
            l = [['+', '-', '/', '*', '&', '|', '^'], 3 ,1,0,0]
        elif re.match(r'(0x|0b)?[0-9A-Fa-f]+\s*$', s):
            if re.match(r'0x[0-9A-Fa-f]+\s*$',s): i = int(s.split()[0],0)
            elif re.match(r'0b[01]+\s*$',s): i = int(s.split()[0],0)
            elif re.match('[0-9]+\s*$',s): i = int(s.split()[0])
            length = math.ceil(math.ceil(math.log2(i + 1)) / 8)*8
            fo = '{:0' + str(length) + 'b}'
            output_array.append(fo.format(i))
            idx_data.append(line)
            continue
        elif re.match(r'\w+\s*$', s):  # format für halt und noop
            l = [['noop', 'halt'],0,0,0,0]
        elif re.match(r'(copy)\s+(R)\d{1,2}\s+(to)\s+(R)\d{1,2}\s*$', s):  # format für comparisons und copy
            l = [['copy'],0,1,1,0,[11,5]]
        elif re.match(r'(compare)\s+(R)\d{1,2}\s+(>|>=|=)\s+(R)\d{1,2}\s*$',s):
            words[0] = words[0] + words[2]
            l = [['compare>','compare>=','compare='],0,1,1,0,[11,17]]
        elif re.match(r'(R)\d{1,2}\s+(=)\s+[-]?\d+\s*$', s):  # format für set
            words[0] = 'set'
            l = [['set'],0,1,0,1,[],21,-1,0]
        elif re.match(r'\w+\s+(to)\s+R\d{1,2}\s*$', s):  # format für jumps ohne rel
            l = [['jump','jump_if'],0,1,1,0,[11]]
        elif re.match(r'\w+\s+(to)\s[-]?\d+\s*$', s):  # format für jumps mit rel
            l = [['jump_rel', 'jump_rel_if'],0,0,0,1,[],27,2,0]
        elif re.match(r'(store)\s+(R)\d{1,2}\s+(to)\s+(R)\d{1,2}\s+[+]\s+\d+\s*$', s):  # format fuer load und store
            l = [['store'],0,1,1,1,[11,5],15,5,1]
        elif re.match(r'(load)\s+(R)\d{1,2}\s+[+]\s+\d+\s+(to)\s+(R)\d{1,2}\s*$', s):  # format fuer load und store
            # liste mit: [[opcodes],befehlswortIndex, registerSchreiben?, registerIndexe?, restImmediate?, [indexe], laengeImmediate,wordImmediate,unsigned?]
            l = [['load'],0,1,1,1,[11,5],15,3,1]
        elif re.match(r'R\d{1,2}\s+[=]\s+[~][R]\d{1,2}\s*$',s):#format für not
            words[1] = '~'
            s = s.replace('~','')
            l = [['~'],1,1,0,0]
        elif re.match(r'R\d{1,2}\s+(=)\s+(R)\d{1,2}\s+[<>]{2}(\w{2})?\s+(R)\d{1,2}\s+(times)$',s):#format für shifts
            l = [['<<','>>','>>_s'],3,1,0,0]
        elif s:
            print('Zeile ' + str(line + 1) + ': ' + s + ' enthält keine gültige Form')
            fehler = 1
            continue
        else: continue
        op = get_opcode(l[0],words[l[1]])
        if op:
            b = op + b[5:]
        else:
            print("Befehl " + words[l[1]] + ", Zeile " + str(line+1) + " unbekannt oder falsch geschrieben!")
            fehler = 1
            continue
        if l[2] and l[3]:
            b = write_regs_id(s,b,l[5])
            if not b:
                print("Befehl " + words[l[1]] + ", Zeile " + str(line+1) + ": Registerzahl ungültig!\nGültig sind R1-R31 und null.")
                fehler = 1
                continue
        elif l[2]:
            b = write_regs(s,b)
            if not b:
                print("Befehl " + words[l[1]] + ", Zeile " + str(line+1) + ": Registerzahl ungültig!\nGültig sind R1-R31 und null.")
                fehler = 1
                continue
        if l[4]:
            a = immediate_to_binary(words[l[7]],l[6],l[8])
            if a:
                b = b[:(32-l[6])] + a
            else:
                print("Befehl " + words[l[1]] + ", Zeile " + str(line+1) + " Immediatewert ausserhalb des gültigen Bereiches!\nBinärdatei unvollständig.")
                fehler = 1
                continue
        output_array.append(b)
    if not fehler:
        print('Binärdatei: ')
        show_output_array(output_array)
        if path: save_binary(output_array, path,idx_data)
        return output_array
    else:
        print('Fehler beim erstellen der Binärdatei')
        return []

def show_output_array(a):
    idx = 0
    for w in a:
        print(str(idx) + ':\t',end='')
        idx += 1
        #jedes w printen sodass alle 8 Schritte was gedruckt wird
        for c in range(len(w)):
            print(w[c], end='')
            if ((c + 1) % 8 == 0):
                print(" ", end='')
        print('\n', end='')

def save_binary(a,s,data):
    try:
        with open(s.replace('txt','bin'), 'wb+') as f:
            for (idx,w) in enumerate(a):
                i = int(w, 2)
                if idx in data: x = i.to_bytes(math.ceil(len(w)/8),'big')
                else: x = i.to_bytes(4,'little')
                f.write(x)
            f.truncate()
    except IOError as e:
        print("Konnte Binärdatei nicht lesen oder schreiben (%s)." % e)

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