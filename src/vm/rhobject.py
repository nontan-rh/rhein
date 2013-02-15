#
# rhobject.py
#

import copy

class VMKlass(object):
    def __init__(self, parent):
        self.parent = parent

class VMObject(object):
    def __init__(self, klass):
        self.klass = klass

# Embedded classes
any_klass = VMKlass(None)
int_klass = VMKlass(any_klass)
string_klass = VMKlass(any_klass)
func_klass = VMKlass(any_klass)
array_klass = VMKlass(any_klass)
hash_klass = VMKlass(any_klass)

class VMRecordKlass(VMKlass):
    def __init__(self, parent_name, member):
        super(VMRecordKlass, self).__init__(None)
        self.parent_name = parent_name
        self.member = {}
        self.member_num = len(member)
        self.resolved = False
        for i in xrange(self.member_num):
            self.member[member[i]] = i

    def resolve(self, env, reached):
        if self.resolved:
            return
        if self in reached:
            raise Exception()
        self.parent = env.get_klass_obj(self.parent_name)
        if not isinstance(self.parent, VMRecordKlass):
            return
        # Mark reached
        reached[self] = True
        self.parent.resolve(env, reached)
        del reached[self]

    def get_slot_index(self, name):
        if not name in self.member:
            raise Exception()
        return self.member[name]

# first-class object

class VMRecord(VMObject):
    def __init__(self, klass):
        super(VMRecord, self).__init__(klass)
        self.slots = [None] * klass.member_num

    def mref(self, name):
        index = self.klass.get_slot_index(name)
        if index == None: raise Exception()
        return self.slots[index]

    def mset(self, name, value):
        index = self.klass.get_slot_index(name)
        if index == None: raise Exception()
        self.slots[index] = value

class VMArray(VMObject):
    def __init__(self, body):
        super(VMArray, self).__init__(array_klass)
        self.body = body

    @staticmethod
    def make(size):
        return VMArray([None] * size)

    def raref(self, index):
        return self.body[index]

    def raset(self, index, value):
        self.body[index] = value

    def iref(self, index):
        return self.body[index]

    def iset(self, index, value):
        self.body[index] = value

# Function and Method

class VMFunc(VMObject):
    def __init__(self, name, args, varg):
        super(VMFunc, self).__init__(func_klass)
        self.name = name
        self.args = args
        self.varg = varg
        self.closed = None

    def enclose(self, frame):
        closure = copy.copy(self)
        closure.closed = frame
        return closure

class VMNativeFunc(VMFunc):
    def __init__(self, name, args, varg, fn):
        super(VMNativeFunc, self).__init__(name, args, varg)
        self.fn = fn

class VMUserFunc(VMFunc):
    def __init__(self, name, stack_size, var_size,
        func_size, args, varg, code):
        super(VMUserFunc, self).__init__(name, args, varg)
        self.stack_size = stack_size
        self.var_size = var_size
        self.func_size = func_size
        self.code = code

class VMMethod(VMFunc):
    def __init__(self):
        super(VMMethod, self).__init__(None, None, None)
        self.dispatcher = VMMethodDispatcher(None, None)

    def add_function(self, func):
        self.dispatcher.add_function(func, 0)

    def dispatch(self, args):
        return self.dispatcher.dispatch(args, 0)

class VMMethodDispatcher():
    def __init__(self, entry, varg_entry):
        self.childs = {}
        self.entry = entry
        self.varg_entry = varg_entry
    
    def add_function(self, func, index):
        if len(func.args) == index:
            if func.varg:
                if self.varg_entry != None: raise Exception()
                self.varg_entry = func
            else:
                if self.entry != None: raise Exception()
                self.entry = func
            return
        klass = func.args[index]
        if not klass in self.childs:
            self.childs[klass] = VMMethodDispatcher(None, None)
        self.childs[klass].add_function(func, index + 1)

    def dispatch(self, args, index):
        if len(args) == index:
            if self.entry == None:
                return self.varg_entry
            return self.entry
        klass = get_klass(args[index])
        while True:
            if klass in self.childs:
                res = self.childs[klass].dispatch(args, index + 1)
                if res != None:
                    return res
            klass = klass.parent
            if klass == None:
                break
        return self.varg_entry

def get_klass(obj):
    if isinstance(obj, VMKlass):
        return obj
    if isinstance(obj, int) or isinstance(obj, long):
        return int_klass
    elif isinstance(obj, str) or isinstance(obj, unicode):
        return string_klass
    elif isinstance(obj, VMObject):
        return obj.klass
    else:
        raise Exception()

