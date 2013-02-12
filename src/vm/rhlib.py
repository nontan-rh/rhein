#
# rhlib.py
#

def rhlib_print(*values):
    for i in values:
        print i,
    print

def rhlib_to_array(value):
    return list(value)

def rhlib_to_string(value):
    return str(value)

