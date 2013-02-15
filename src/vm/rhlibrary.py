#
# rhlibrary.py
#

from rhobject import *

def print_(*values):
    for i in values:
        print i,
    print

def string_to_array(value):
    return VMArray(list(value))

def array_to_string(value):
    return "".join(value.body)

