import os
import re
import asm
import math

#testen: R1 = label, dann dahin jumpen
#labels mit zahlen erlauben
#refactoring

def check_labels(input,source):
    label_list = {}
    input_array = input.split('\n')
    for line, s in enumerate(input_array):
        input_array[line] = re.sub(r'#(.)*','',s)
    addr = 0
    same = 0
    dataidx = 0
    data = [] #liste für indexe aller Datenstrings
    for line, s in enumerate(input_array):
        if(re.match(r'(.)*\s+(_)[A-Za-z0-9_-]+\s*$',s)):
            input_array[line] = re.sub(r'_(.)*','',s)
            label = s.split()[-1][1:]
            for x in label_list:
                if x == label: same = 1
            if not same:
                label_list[label] = addr
            else:
                print('Labelnamen müssen eindeutig sein und Labels dürfen nur einmal eingeführt werden!')
                break
        s = input_array[line]
        if re.match(r'(0x|0b)?[0-9A-Fa-f]+\s*$', s):
            if re.match(r'0x[0-9A-Fa-f]+\s*$',s): i = int(s.split()[0],0)
            elif re.match(r'0b[01]+\s*$',s): i = int(s.split()[0],0)
            elif re.match('[0-9]+\s*$',s): i = int(s.split()[0])
            else:
                print('Fehler in Zeile ' + str(line+1) + ' Datenstring konnte nicht konvertiert werden')
                same = 1
                break
            summand = math.ceil(math.ceil(math.log2(i + 1)) / 8)
            data.append(dataidx)
            addr += summand
            dataidx += 1
        elif not re.match(r'\s*$', s):
            addr += 4
            dataidx += 1
    opts_labels = '|'.join(label_list)
    print('Labels:')
    print('\n'.join(label_list))
    print('\nText nach Labelmodul:')
    if not same:
        addr = 0
        for line, s in enumerate(input_array):
            if re.match('(jump)\s+(to)\s+(' + opts_labels + ')\s*(_\w+\s*)?(\s+#(.)*)?$',s):
                dest = label_list[s.split()[2]] - addr
                if dest-4 > 0: input_array[line] = 'jump_rel to ' + str(dest)
                else: input_array[line] = 'jump_rel to ' + str(dest)
            elif re.match('(jump_if)\s+(to)\s+(' + opts_labels + ')\s*(_\w+\s*)?$', s):
                dest = label_list[s.split()[2]] - addr
                if dest-4 > 0: input_array[line] = 'jump_rel_if to ' + str(dest)
                else: input_array[line] = 'jump_rel_if to ' + str(dest)
            elif re.match('(R)\d{1,2}\s+(=)\s+(' + opts_labels + ')\s*$', s):
                words = s.split()
                input_array[line] = words[0] + ' = ' + str(label_list[words[2]])
            if re.match(r'(0x|0b)?[0-9A-Fa-f]+\s*$', s):
                if re.match(r'0x[0-9A-Fa-f]+\s*$', s):
                    i = int(s.split()[0], 0)
                elif re.match(r'0b[01]+\s*$', s):
                    i = int(s.split()[0], 0)
                elif re.match('[0-9]+\s*$',s):
                    i = int(s.split()[0])
                else:
                    break
                summand = math.ceil(math.ceil(math.log2(i + 1)) / 8)
                print(str(addr) + ':\t\t' + input_array[line])
                addr += summand
            elif not re.match(r'\s*$', s):
                print(str(addr) + ':\t\t' + input_array[line])
                addr += 4
        print('\n')
        output_array = asm.check_format('\n'.join(input_array),'')
        asm.save_binary(output_array, source,data)

def start(source):
    if(os.path.exists(source)):
        if(source.endswith('.txt')):
            f = open(source,'r')
            check_labels(f.read(),source)
        else:
            print('Pfad muss in Textdatei enden!')
    else:
        print('Pfad existiert nicht.\n- benutze slashes anstatt backslashes\n- relative oder absolute Dateipfade sind gültig')

# if __name__ == '__main__':
#     start(sys.argv[0])