def assertEquals(func_call, actual_result):
    if func_call == actual_result:
        print('test successful')
        return 1
    else:
        print('Error, result of function: ' + str(func_call) + " and actual result: " + str(actual_result) + " are not equal")
        return 0

def assert_not_equals(this,that):
    if this != that:
        print('test successful')
        return 1
    else:
        print('Error, ' + str(this) + " and " + str(that) + " are not equal")
        return 0
    
def assert_equals_list(testcases):
    for number,test in enumerate(testcases):
        if not test[0] == test[1]: print('Test ' + str(number+1) + ' gone wrong, ' + str(test[0]) + ' is not equal to ' + test[1] + '.')
        else: print('Test ' + str(number+1) + 'sucessful.' )

def assertTrue(bool):
    if bool:
        print('test successful')
        return 1
    else:
        print('Error, not true')
        return 0

def assertFalse(bool):
    if not bool:
        print('test successful')
        return 1
    else:
        print('Error, not false')
        return 0

def assert_exception(func, args, exception):
    try:
        func(*args)
        print('Failure: no Exception thrown')
    except exception:
        print('test successful')
    except Exception: print('Failure: other exception thrown than expected!')