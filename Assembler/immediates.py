#oder eher binary toolbox und dann auch noch binärstring format usw funktionen schreiben?

import exceptions as error

def immediate_to_binary_signed(s,stellen):
    try:
        integer = int(s,0)
    except:
        raise error.immediate_not_convertible_exception(s)
    if integer < -(2 ** (stellen - 1)):raise error.immediate_out_of_range_exception(s)
    elif integer >= 0: return immediate_to_binary_unsigned(s, stellen )
    return '{0:{fill}{width}b}'.format((integer + 2 ** stellen) % 2 ** stellen, fill='0', width=stellen)

def immediate_to_binary_unsigned(s,stellen):
    try:
        integer = int(s,0)
    except:
        raise error.immediate_not_convertible_exception(s)
    if integer >= (2 ** stellen) or integer < 0: raise error.immediate_out_of_range_exception(s)
    return '{0:{fill}{width}b}'.format(integer, fill='0', width=stellen)