
class Error(Exception): pass

class label_duplicate_error(Error):
    def __init__(self, string):
        self.string = string

class data_string_not_convertible_error(Error):
    def __init__(self, string):
        self.string = string

class label_replacement_not_successful_error(Error):
    def __init__(self, string):
        self.string = string

class empty_line_exception(Error): pass

class invalid_format_exception(Error):
    def __init__(self, string):
        self.string = string

class invalid_command_exception(Error):
    def __init__(self, string):
        self.string = string

class illegal_register_number_exception(Error):
    def __init__(self, string):
        self.string = string

class immediate_out_of_range_exception(Error):
    def __init__(self, string):
        self.string = string

class immediate_not_convertible_exception(Error):
    def __init__(self, string):
        self.string = string

class illegal_number_of_indexes_exception(Error):
    def __init__(self, string):
        self.string = string

class is_datastring_exception(Error): pass

class binary_file_not_creatable_exception(Error):
    def __init__(self, string):
        self.string = string
        

def handle_exceptions(excepti,line):
    try: raise excepti
    except empty_line_exception:
        return 1
    except invalid_command_exception as err:
        print('Line ' + str(line) + ': command ' + err.string + ' unknown.')
        return 0
    except invalid_format_exception as err:
        print('Line ' + str(line) + ': command ' + err.string + ' is not formatted correctly.')
        return 0
    except illegal_register_number_exception as err:
        print('Line ' + str(line) + ': register number ' + err.string + ' invalid')
        return 0
    except immediate_out_of_range_exception as err:
        print('Line ' + str(line) + ': value of immediate ' + err.string + ' out of range')
        return 0
    except immediate_not_convertible_exception as err:
        print('Line ' + str(line) + ': immediate ' + err.string + ' not convertible')
        return 0
    except data_string_not_convertible_error as err:
        print('Line ' + str(line+1) +' '+ err.string +' not convertible')
        return 0
    except illegal_number_of_indexes_exception as err:
        print('Line :' + str(line + 1) + ' ' + err.string)
        return 0
    # except Exception as err:
    #     print('Unknown Exception has occured, Assembler is buggy.\n' + str(err))
    #     return 0



