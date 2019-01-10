import re
import os
import numpy as np

#TODO es sind globale variablen, deshalb werden sie nur gelöscht wenn ich die datei neu lade
# deshalb nicht globale variablen benutzen sondern irgendwie anders

ptr = 0
input_array = []
names = []
fehler = 0

ptrs = {'byte': 0,
        'char': 0,
        'string': 0}

offsets = {'byte': 0,
           'char': 0,
           'string':0}

#jeder eintrag der listen besteht aus 0. art 1. name der variable, 2. adresse und 3. offset bei 8 bits
vars = []

def check_dec(input):
    global ptr
    global input_array
    global fehler
    global names
    input_array = input.split('\n')
    for line, s in enumerate(input_array):
        s = re.sub(r'#(.)*','',s)
        words = re.sub("[\s]", " ", s).split()
        input_array[line] = words

        # hat eine zeile die form für deklaration? dann füge sie in vars hinzu mit
        # adresse und offset innerhalb, wert als string
        vs = ['char','byte','string']
        for var in vs:
            if re.match('(' + var + ')\s+\w+\s+(=)\s+[a-zA-Z0-9\"\']+\s*$',s):
                inc_pointers(var)
                vars.append([var,words[1],ptrs[var],offsets[var],words[3],line])
                break
    #prüfen ob keine variable den gleichen Namen hat
    vr  = np.array(vars)

    for n in vr[:,1]:
        if n in names:
            fehler = 1
            print("Variablenname " + n + " doppelt belegt, ungültiger Vorgang!")
        else: names.append(n)
    check_vals()

def check_vals():
    global fehler
    for v in vars:
        if v[0] == 'char':
            c = v
            if not re.match(r'\'[A-Za-z]\'$',c[-2]):
                fehler = 1
                print('Fehler bei Deklaration von Variable ' + c[1] + ", Zeile " + str(c[-1]+1) + " : korrekte Syntax für chars ist: \'[A-Za-z]\'")
            else: c[-2] = c[-2][1:-1]
        elif v[0] == 'byte':
            b = v
            value = b[-2]
            if re.match(r'0x[0-9a-fA-F]{2}$',value):
                b[-2] = int(value[2:],16)
            elif re.match(r'\d+$',b[-2]):
                b[-2] = int(value)
            else:
                fehler = 1
                print('Fehler bei Deklaration von Variable ' + b[1] + ", Zeile " + str(b[-1]+1) + ": korrekte Syntax für bytes sind 2 Zahlen in Hexadezimal- oder Dezimalformat")
                continue
            if not (-128 <= b[-2] <= 127):
                fehler = 1
                print('Fehler bei Deklaration von Variable ' + b[1] + ", Zeile " + str(b[-1]+1) + ': Zahlenwert ausserhalb des gültigen Bereichs.')
        elif v[0] == 'string':
            s = v
            if not re.match(r'"([^\n]*)"',s[-2]):
                fehler = 1
                print('Fehler bei Deklaration von Variable ' + s[1] + ", Zeile " + str(s[-1]+1) + ": korrekte Syntax für strings ist: \"[.]*\"")
            else: s[-2] = s[-2][1:-1]
    print(vars)
    if not fehler: exchange()
    # operationen auf variablen übersetzen

def exchange():
    global input_array
    global names
    #angelehnt an den anderen code formen überprüfen
    for line, s in enumerate(input_array):

        #wörter, die vorkommen könnten in string an variablen oder register
        opt = []
        for i in range(32): opt.append('R' + str(i))
        opt.extend(names)
        opt.append('null')
        pat = '|'.join(opt)

        strs = [x[1] for x in vars if x[0] == 'string']
        bts = [x[1] for x in vars if x[0] == 'byte']
        chrs = [x[1] for x in vars if x[0] == 'char']

        words = re.sub("[\s]", " ", s).split()
        if re.match(r'({0})\s+(=)\s+({0})\s+(+)\s+({0})\s*$'.format(pat),s): #+ abfragen für strings (oder chars)
            #wenn da ein string ist ist es konkatenation und byte und chars werden umgewandelt

        elif re.match(r'({0})\s+(=)\s+({0})\s+[-/*^&|]\s+({0})\s*$'.format(pat),s):  # format für arithmetische Befehle, nur für bytes (und integer)
        elif re.match(r'\w+\s+(R)\d{1,2}\s+(to)\s+(R)\d{1,2}\s*$', s):  # format für comparisons und copy
        elif re.match(r'\w+\s+(R)\d{1,2}\s+(to)\s+[-]?\d+\s*$', s):  # format für set
        elif re.match(r'\w+\s+(to)\s+R\d{1,2}\s*$', s):  # format für jumps ohne rel
        elif re.match(r'\w+\s+(to)\s[-]?\d+\s*$', s):  # format für jumps mit rel
            l = [['jump_rel', 'jump_rel_if'],0,0,0,1,[],27,2]
        elif re.match(r'R\d{1,2}\s+[=]\s+[~][R]\d{1,2}\s*$',s):#format für not

def get_regs(var_name):
    return 'R0'

def inc_pointers(var_type):
    global ptr
    if offsets[var_type] == 3:
        offsets[var_type] = 0
        ptrs[var_type] = ptr
        ptr += 1
    else:
        offsets[var_type] += 1
        ptr += 1

#source ist pfad zu aktueller Datei
def start(source):
    if(os.path.exists(source)):
        if(source.endswith('.txt')):
            f = open(source,'r')
            check_dec(f.read())
        else:
            print('Pfad muss in Textdatei enden!')
    else:
        print('Pfad existiert nicht.\n- benutze slashes anstatt backslashes\n- relative oder absolute Dateipfade sind gültig')