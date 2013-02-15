#
# vm.py
#

import sys
import copy
import json
from rhobject import *
import rhlibrary

# Configure file
vmconfig_file = 'src/vm/vmconfig.json'

vmconfig = json.load(open(vmconfig_file))
insn_format = vmconfig['insn_format']
std_library = vmconfig['std_library']

# VM Global Environment
class VMEnv():
    def __init__(self):
        self.function_slots = {}
        self.variable_slots = {}
        self.klass_slots = {}
        self.klass_slots['any'] = any_klass
        self.klass_slots['int'] = int_klass
        self.klass_slots['string'] = string_klass
        self.klass_slots['array'] = array_klass
        self.load_standard_library()

    def load_standard_library(self):
        self.function_slots['new'] = VMMethod()
        self.function_slots['literal'] = VMMethod()
        for name, defs in std_library.items():
            m = VMMethod()
            for d in defs:
                fn = rhlibrary.__dict__[d['name']]
                argtype = map((lambda x: self.get_klass_obj(x)), d['argtype'])
                varg = d['varg']
                m.add_function(VMNativeFunc(name, argtype, varg, fn))
            self.function_slots[name] = m

    def load_json(self, jobj):
        def is_klass(x): return x.get('type') == 'klass'
        for j in filter(is_klass, jobj):
            self.load_obj(j)
        for j in filter((lambda x: not is_klass(x)), jobj):
            self.load_obj(j)
        for k,v in self.klass_slots.items():
            if isinstance(v, VMRecordKlass):
                v.resolve(self, {})

    def load_obj(self, jobj):
        if not 'type' in jobj:
            raise Exception()
        if jobj['type'] == 'function':
            self.load_func_obj(jobj)
        elif jobj['type'] == 'variable':
            self.load_var_obj(jobj)
        elif jobj['type'] == 'class':
            self.load_klass_obj(jobj)
        else:
            raise Exception()

    def load_func_obj(self, jobj):
        if not ('name' in jobj
            and 'stack_size' in jobj
            and 'variable_size' in jobj
            and 'function_size' in jobj
            and 'argument_type' in jobj
            and 'varg' in jobj
            and 'code' in jobj):
            raise Exception()
        func_code = code_preprocess(jobj['code'])
        func_name = jobj['name']
        if not func_name in self.function_slots:
            self.function_slots[func_name] = VMMethod()
        func_obj = VMUserFunc(jobj['name'],
            jobj['stack_size'],
            jobj['variable_size'],
            jobj['function_size'],
            map((lambda x: self.get_klass_obj(x)), jobj['argument_type']),
            jobj['varg'],
            func_code)
        self.function_slots[func_name].add_function(func_obj)

    def load_var_obj(self, jobj):
        pass

    def load_klass_obj(self, jobj):
        if not ('name' in jobj
            and 'parent' in jobj
            and 'members' in jobj):
            raise Exception()
        if jobj['name'] in self.klass_slots:
            raise Exception()
        klass = VMRecordKlass(jobj['parent'], jobj['members'])
        self.klass_slots[jobj['name']] = klass
        def kons(k):
            return VMRecord(k)
        self.function_slots['new'].add_function(VMNativeFunc('new', [klass], False, kons))

    def get_function(self, name):
        return self.function_slots[name]

    def get_klass_obj(self, name):
        return self.klass_slots[name]

# Code manipulation

def code_preprocess(code):
    if not isinstance(code, list):
        raise Exception()
    (stripped_code, label_list) = code_collect_labels(code)
    for i in stripped_code:
        insn_format_check(i)
        i[0] = intern(str(i[0]))
        if i[0] == 'jump' or i[0] == 'ifjump' or i[0] == 'nifjump':
            if len(i) != 2:
                raise Exception()
            if not i[1] in label_list:
                raise Exception()
            dest = label_list[i[1]]
            i[1] = dest
    return stripped_code

def code_collect_labels(code):
    stripped = []
    labels = {}
    count = 0
    for i in code:
        insn_format_check(i)
        if i[0] == 'label':
            if i[1] in labels:
                raise Exception()
            labels[i[1]] = count
        else:
            stripped.append(i)
            count = count + 1
    return (stripped, labels)

def insn_format_check(insn):
    if not (isinstance(insn, list)
        and len(insn) >= 1
        and insn[0] in insn_format):
        raise Exception()
    i = insn_format[insn[0]]
    if i == 'noarg':
        if len(insn) != 1:
            raise Exception()
    elif i == 'str':
        if not (len(insn) == 2 and (isinstance(insn[1], str)
                                 or isinstance(insn[1], unicode))):
            raise Exception()
    elif i == 'int':
        if not (len(insn) == 2 and isinstance(insn[1], int)):
            raise Exception()
    elif i == 'lref':
        if not (len(insn) == 3
            and isinstance(insn[1], int)
            and isinstance(insn[2], int)):
            raise Exception()

class VMFrame():
    def __init__(self, closed, parent, func):
        self.stack = []
        self.closed = closed
        self.parent = parent
        self.variable_slots = [None] * func.var_size
        self.function_slots = [None] * func.func_size
        self.func = func
        self.count = 0
    
    def fetch_insn(self):
        return self.func.code[self.count]

    def push(self, value):
        self.stack.append(value)

    def pop(self):
        return self.stack.pop()

    def top(self):
        return self.stack[-1]

    def dup(self):
        value = self.stack[-1]
        self.stack.append(value)

    def jump(self,dest):
        self.count = dest

    def incr_count(self):
        self.count = self.count + 1

    def dump(self):
        pass

    def lfref(self, depth, offset):
        if depth == 0:
            return self.function_slots[offset]
        else:
            return self.closed.lfref(depth - 1, offset)

    def lfset(self, depth, offset, value):
        if depth == 0:
            self.function_slots[offset] = value
        else:
            self.closed.lfset(depth - 1, offset, value)

    def lvref(self, depth, offset):
        if depth == 0:
            return self.variable_slots[offset]
        else:
            return self.closed.lvref(depth - 1, offset)

    def lvset(self, depth, offset, value):
        if depth == 0:
            self.variable_slots[offset] = value
        else:
            self.closed.lvset(depth - 1, offset, value)

class VMExit(Exception):
    def __init__(self, value):
        self.value = value

def execute(env, entry):
    meth = env.get_function(entry)
    func = meth.dispatch([])
    frame = VMFrame(None, None, func)
    while True:
        try:
            i = frame.fetch_insn()
            #print i
            if   i[0] is 'add': execute_add(env, i, frame)
            elif i[0] is 'sub': execute_sub(env, i, frame)
            elif i[0] is 'mul': execute_mul(env, i, frame)
            elif i[0] is 'div': execute_div(env, i, frame)
            elif i[0] is 'mod': execute_mod(env, i, frame)
            elif i[0] is 'inc': execute_inc(env, i, frame)
            elif i[0] is 'eq': execute_eq(env, i, frame)
            elif i[0] is 'ne': execute_ne(env, i, frame)
            elif i[0] is 'gt': execute_gt(env, i, frame)
            elif i[0] is 'lt': execute_lt(env, i, frame)
            elif i[0] is 'ge': execute_ge(env, i, frame)
            elif i[0] is 'le': execute_le(env, i, frame)
            elif i[0] is 'jump': execute_jump(env, i, frame)
            elif i[0] is 'ifjump': execute_ifjump(env, i, frame)
            elif i[0] is 'nifjump': execute_nifjump(env, i, frame)
            elif i[0] is 'call': frame = execute_call(env, i, frame)
            elif i[0] is 'ret': frame = execute_ret(env, i, frame)
            elif i[0] is 'ranew': execute_ranew(env, i, frame)
            elif i[0] is 'raref': execute_raref(env, i, frame)
            elif i[0] is 'raset': execute_raset(env, i, frame)
            elif i[0] is 'iref': execute_iref(env, i, frame)
            elif i[0] is 'iset': execute_iset(env, i, frame)
            elif i[0] is 'mref': execute_mref(env, i, frame)
            elif i[0] is 'mset': execute_mset(env, i, frame)
            elif i[0] is 'lfref': execute_lfref(env, i, frame)
            elif i[0] is 'lfset': execute_lfset(env, i, frame)
            elif i[0] is 'lvref': execute_lvref(env, i, frame)
            elif i[0] is 'lvset': execute_lvset(env, i, frame)
            elif i[0] is 'gfref': execute_gfref(env, i, frame)
            elif i[0] is 'gvref': execute_gvref(env, i, frame)
            elif i[0] is 'gvset': execute_gvset(env, i, frame)
            elif i[0] is 'pushundef': execute_pushundef(env, i, frame)
            elif i[0] is 'pushtid': execute_pushtid(env, i, frame)
            elif i[0] is 'pushstr': execute_pushstr(env, i, frame)
            elif i[0] is 'pushint': execute_pushint(env, i, frame)
            elif i[0] is 'pushchar': execute_pushchar(env, i, frame)
            elif i[0] is 'enclose': execute_enclose(env, i, frame)
            elif i[0] is 'dup': execute_dup(env, i, frame)
            elif i[0] is 'pop': execute_pop(env, i, frame)
            elif i[0] is 'popn': execute_popn(env, i, frame)
            else: raise Exception()
        except VMExit, e:
            return e.value

def execute_add(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl + vr)
    frame.incr_count()

def execute_sub(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl - vr)
    frame.incr_count()

def execute_mul(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl * vr)
    frame.incr_count()

def execute_div(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl / vr)
    frame.incr_count()

def execute_mod(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl % vr)
    frame.incr_count()

def execute_inc(env, i, frame):
    v = frame.pop()
    frame.push(v + 1)
    frame.incr_count()

def execute_eq(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl == vr)
    frame.incr_count()

def execute_ne(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl != vr)
    frame.incr_count()

def execute_gt(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl > vr)
    frame.incr_count()

def execute_lt(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl < vr)
    frame.incr_count()

def execute_ge(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl >= vr)
    frame.incr_count()

def execute_le(env, i, frame):
    vr = frame.pop()
    vl = frame.pop()
    frame.push(vl <= vr)
    frame.incr_count()

def execute_jump(env, i, frame):
    frame.jump(i[1])

def execute_ifjump(env, i, frame):
    b = frame.pop()
    if b:
        frame.jump(i[1])
    else:
        frame.incr_count()

def execute_nifjump(env, i, frame):
    b = frame.pop()
    if not b:
        frame.jump(i[1])
    else:
        frame.incr_count()

def execute_call(env, i, frame):
    frame.incr_count()
    func = frame.pop()
    args = []
    for j in xrange(i[1]):
        args.append(frame.pop())
    args.reverse()
    if isinstance(func, VMUserFunc):
        new_frame = VMFrame(func.closed, frame, func)
        for j in xrange(i[1]):
            new_frame.variable_slots[j] = args[j]
        return new_frame
    elif isinstance(func, VMNativeFunc):
        ret = apply(func.fn, tuple(args))
        frame.push(ret)
        return frame
    elif isinstance(func, VMMethod):
        dfunc = func.dispatch(args)
        if isinstance(dfunc, VMUserFunc):
            new_frame = VMFrame(func.closed, frame, dfunc)
            for j in xrange(i[1]):
                new_frame.variable_slots[j] = args[j]
            return new_frame
        elif isinstance(dfunc, VMNativeFunc):
            ret = apply(dfunc.fn, tuple(args))
            frame.push(ret)
            return frame
    else:
        raise Exception()

def execute_ret(env, i, frame):
    parent = frame.parent
    if parent == None:
        raise VMExit(frame.pop())
    parent.push(frame.pop())
    return parent

def execute_ranew(env, i, frame):
    size = frame.pop()
    frame.push(VMArray.make(size))
    frame.incr_count()

def execute_raref(env, i, frame):
    index = frame.pop()
    array = frame.pop()
    if not isinstance(array, VMArray):
        raise Exception()
    frame.push(array.raref(index))
    frame.incr_count()

def execute_raset(env, i, frame):
    value = frame.pop()
    index = frame.pop()
    array = frame.pop()
    if not isinstance(array, VMArray):
        raise Exception()
    array.raset(index,value)
    frame.push(array)
    frame.push(index)
    frame.incr_count()

def execute_iref(env, i, frame):
    index = frame.pop()
    obj = frame.pop()
    if not isinstance(obj, VMObject):
        raise Exception()
    frame.push(obj.iref(index))
    frame.incr_count()

def execute_iset(env, i, frame):
    index = frame.pop()
    obj = frame.pop()
    value = frame.top()
    if not isinstance(obj, VMObject):
        raise Exception()
    obj.iset(index,value)
    frame.incr_count()

def execute_mref(env, i, frame):
    obj = frame.pop()
    if not isinstance(obj, VMRecord):
        raise Exception()
    frame.push(obj.mref(i[1]))
    frame.incr_count()

def execute_mset(env, i, frame):
    obj = frame.pop()
    value = frame.top()
    if not isinstance(obj, VMRecord):
        raise Exception()
    obj.mset(i[1], value)
    frame.incr_count()

def execute_lfref(env, i, frame):
    frame.push(frame.lfref(i[1],i[2]))
    frame.incr_count()

def execute_lfset(env, i, frame):
    value = frame.top()
    frame.lfset(i[1],i[2],value)
    frame.incr_count()

def execute_lvref(env, i, frame):
    frame.push(frame.lvref(i[1],i[2]))
    frame.incr_count()

def execute_lvset(env, i, frame):
    value = frame.top()
    frame.lvset(i[1],i[2],value)
    frame.incr_count()

def execute_gfref(env, i, frame):
    value = env.get_function(i[1])
    frame.push(value)
    frame.incr_count()

def execute_gvref(env, i, frame):
    value = env.get_variable(i[1])
    frame.push(value)
    frame.incr_count()

def execute_gvset(env, i, frame):
    value = frame.top()
    env.set_variable(i[1], value)
    frame.incr_count()

def execute_pushundef(env, i, frame):
    frame.push(None)
    frame.incr_count()

def execute_pushtid(env, i, frame):
    frame.push(env.get_klass_obj(i[1]))
    frame.incr_count()

def execute_pushstr(env, i, frame):
    frame.push(i[1])
    frame.incr_count()

def execute_pushint(env, i, frame):
    frame.push(i[1])
    frame.incr_count()

def execute_pushchar(env, i, frame):
    frame.push(i[1])
    frame.incr_count()

def execute_enclose(env, i, frame):
    func = env.get_function(i[1])
    closure = func.enclose(frame)
    frame.push(closure)
    frame.incr_count()

def execute_dup(env, i, frame):
    frame.dup()
    frame.incr_count()

def execute_pop(env, i, frame):
    frame.pop()
    frame.incr_count()

def execute_popn(env, i, frame):
    value = frame.pop()
    for j in xrange(i[1]):
        frame.pop()
    frame.push(value)
    frame.incr_count()

source = json.load(sys.stdin)
global_environment = VMEnv()
global_environment.load_json(source)
execute(global_environment, 'entry')

