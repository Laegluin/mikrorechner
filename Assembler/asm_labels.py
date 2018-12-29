import os
import re
import asem

#ersetzt alle labels durch relative sprünge

def check_labels(input,source):
    input_array = input.split('\n')
    addr = 0
    label_list = {}
    same = 0
    for line, s in enumerate(input_array):
        if(re.match(r'(.)*\s+(_)\w+\s*$',s)):
            label = s.split()[-1][1:]
            for x in label_list:
                if x == label: same = 1
            if not same:
                label_list[label] = addr
                print(label_list)
            else:
                print('Labelnamen müssen eindeutig sein und Labels dürfen nur einmal eingeführt werden!')
                break
        if not re.match(r'\s*$', s): addr += 1
    opts_labels = '|'.join(label_list)
    if not same:
        addr = 0
        for line, s in enumerate(input_array):
            if re.match('(jump)\s+(to)\s+(' + opts_labels + ')\s*(_\w+\s*)?(\s+#(.)*)?$',s):
                input_array[line] = 'jump_rel to ' + str(label_list[s.split()[2]] - addr)
            elif re.match('(jump_if)\s+(to)\s+(' + opts_labels + ')\s*(_\w+\s*)?$', s):
                input_array[line] = 'jump_rel_if to ' + str(label_list[s.split()[2]] - addr)
            if not re.match(r'\s*$', s): addr += 1
    if not same:
        asem.check_format('\n'.join(input_array),source)

def start(source):
    if(os.path.exists(source)):
        if(source.endswith('.txt')):
            f = open(source,'r')
            check_labels(f.read(),source)
        else:
            print('Pfad muss in Textdatei enden!')
    else:
        print('Pfad existiert nicht.\n- benutze slashes anstatt backslashes\n- relative oder absolute Dateipfade sind gültig')