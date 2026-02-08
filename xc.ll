%Val = type { i8, i8* } ; type, data
%Env = type { %Val*, %Env* } ; start of list, parent
%Args = type { %Val*, i64 } ; start of list, size

%Str = type { i64, i8* } ; n, data
%Func = type { %Val(%Env, %Args)*, %Env } ; ptr, closure
%List = type { %Val, %List* } ; car, cdr
%Vec = type { i64, %Val* } ; length, data array

declare i8* @GC_malloc(i64)

declare i64 @strlen(i8*)
declare ptr @memcpy(ptr, ptr, i64)
declare i32 @memcmp(ptr, ptr, i64)

declare i32 @rand()
declare i32 @srand()
declare i64 @time(i64*)

declare void @exit(i32) noreturn
declare i32 @open(i8*, i32, i32)
declare i64 @read(i32, i8*, i64)
declare i64 @write(i32, i8*, i64)
declare i32 @close(i32)
declare i32 @dup(i32)
declare i32 @dup2(i32, i32)
declare i32 @fork()
declare i32 @execve(i8*, i8**, i8**)
declare i32 @waitpid(i32, i32*, i32)

define %Env @make_global_env(i32 %argc, i8** %argv) {
  ; create env with val in it
  %fsize = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %size = mul i64 %fsize, 51
  %valsp = call i8* @GC_malloc(i64 %size)
  %vals = bitcast i8* %valsp to %Val*

  ;; TYPES
  ;; each
  ; bool
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_boolean, i64 21)
  ; int
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_integer, i64 22)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_random, i64 41)
  ; str
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_string, i64 23)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_string_append, i64 13)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_string_list, i64 29)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_string_length, i64 28)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_string_lt, i64 42)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_substring, i64 43)
  ; list
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_list, i64 24)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_list, i64 1)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_car, i64 14)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_cdr, i64 15)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_cons, i64 16)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_nullq, i64 17)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_list_length, i64 27)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_set_car, i64 30)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_set_cdr, i64 32)
  ; func
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_function, i64 26)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_apply, i64 3)
  ; symb
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_symbol, i64 25)

  ;; conversions
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_string, i64 31)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_symbol, i64 20)

  ;; all
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_equal, i64 10)

  ;; CPUCALLS
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_slt, i64 18)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_plus, i64 2)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_sub, i64 19)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_mul, i64 34)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_div, i64 35)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_modulo, i64 33)

  ;; SYSCALLS
  ; args
  call void @store_command_line(%Val* %vals, i64 39, i32 %argc, i8** %argv)
  ; exit
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_exit, i64 0)
  ; files
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_open, i64 4)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_read, i64 8)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_write, i64 6)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_close, i64 5)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_dup, i64 37)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_dup2, i64 38)
  ; procs
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_fork, i64 9)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_execve, i64 12)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_waitpid, i64 11)

  ; vec
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_make_vector, i64 44)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_vector_ref, i64 45)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_vector_set, i64 46)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_vector_length, i64 47)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @is_vector, i64 48)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_list_to_vector, i64 49)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_vector_to_list, i64 50)

  ; set runtime
  call void @store_runtime(%Val* %vals, i64 40)

  ; construct global env with native funcs
  %e1 = insertvalue %Env zeroinitializer, %Val* %vals, 0
  %e2 = insertvalue %Env %e1, %Env* null, 1

  ; ret env
  ret %Env %e2
}

@.str.runtime = private unnamed_addr constant [1 x i8] c"c"

define void @store_runtime(%Val* %vals, i64 %i) {
  %vp = getelementptr %Val, %Val* %vals, i64 %i
  %v = call %Val @make_str_val(i64 1, i8* @.str.runtime)
  store %Val %v, %Val* %vp
  ret void
}

define void @store_native_func(%Val* %vals, %Val(%Env, %Args)* %f, i64 %i) {
  %nulle = insertvalue %Env zeroinitializer, %Val* null, 0
  %f1 = insertvalue %Func zeroinitializer, %Val(%Env, %Args)* %f, 0
  %f2 = insertvalue %Func %f1, %Env %nulle, 1
  %fv = call %Val @make_func_val(%Func %f2)
  %fvp = getelementptr %Val, %Val* %vals, i64 %i
  store %Val %fv, %Val* %fvp
  ret void
}

define void @store_command_line(%Val* %vals, i64 %i, i32 %argc, i8** %argv) {
  ; make string list value out of argc and argv
  %l = call %List* @from_string_array(i32 %argc, i8** %argv)
  %lv = call %Val @make_list_val(%List* %l)

  ; make pointer to the one value
  %size = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %lvp = call i8* @GC_malloc(i64 %size) ; single value
  store %Val %lv, %Val* %lvp

  ; make the env
  %e = insertvalue %Env zeroinitializer, %Val* %lvp, 0

  ; make the func
  %f0 = insertvalue %Func zeroinitializer, %Val(%Env,%Args)* @call_command_line, 0
  %f = insertvalue %Func %f0, %Env %e, 1

  ; box the func as a value
  %fv = call %Val @make_func_val(%Func %f)

  ; store in globals
  %fvp = getelementptr %Val, %Val* %vals, i64 %i
  store %Val %fv, %Val* %fvp
  ret void
}

define %List* @from_string_array(i32 %n, i8** %p) {
entry:
  %cmp = icmp eq i32 %n, 0
  br i1 %cmp, label %done, label %more
done:
  ret %List* null
more:
  %first = load i8*, i8** %p
  %firstn = call i64 @strlen(i8* %first)
  %sv = call %Val @make_str_val(i64 %firstn, i8* %first)
  %np = getelementptr i8**, i8** %p, i64 1
  %ni = sub i32 %n, 1
  %tail = call %List* @from_string_array(i32 %ni, i8** %np)
  %l = call %List* @cons(%Val %sv, %List* %tail)
  ret %List* %l
}

define %Val @call_command_line(%Env %env, %Args %args) {
  %out = call %Val @lookup(%Env %env, i64 0, i64 0)
  ret %Val %out
}

define %Val @lookup(%Env %env, i64 %depth, i64 %offset) {
  %vp = call %Val* @lookupp(%Env %env, i64 %depth, i64 %offset)
  %v = load volatile %Val, %Val* %vp
  ret %Val %v
}

define %Val* @lookupp(%Env %env, i64 %depth, i64 %offset) {
  %cmp = icmp eq i64 %depth, 0
  br i1 %cmp, label %access, label %next
access:
  %vs = extractvalue %Env %env, 0
  %vp = getelementptr %Val, %Val* %vs, i64 %offset
  ret %Val* %vp

next:
  %pep = extractvalue %Env %env, 1
  %pe = load %Env, %Env* %pep
  %nd = sub i64 %depth, 1
  %rv = tail call %Val* @lookupp(%Env %pe, i64 %nd, i64 %offset)
  ret %Val* %rv
}

define %Val @make_func_val(%Func %f) {
  %1 = insertvalue %Val zeroinitializer, i8 6, 0
  %size = ptrtoint %Func* getelementptr (%Func, %Func* null, i64 1) to i64
  %p = call i8* @GC_malloc(i64 %size)
  %fp = bitcast i8* %p to %Func*
  store %Func %f, %Func* %fp
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val @to_func_val(%Val(%Env, %Args)* %f, %Env %env) {
  %f1 = insertvalue %Func zeroinitializer, %Val(%Env, %Args)* %f, 0
  %f2 = insertvalue %Func %f1, %Env %env, 1
  %v = tail call %Val @make_func_val(%Func %f2)
  ret %Val %v
}

define %Val @sys_exit(%Env %env, %Args %args) {
  %v = call %Val @get_arg(%Args %args, i64 0)
  %c = call i32 @val_to_i32(%Val %v)
  call void @exit(i32 %c)
  ret %Val %v
}

define %Val @sys_open(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v1p = call i8* @to_cstring(%Val %v1)
  %v2v = call %Val @get_arg(%Args %args, i64 1)
  %v2i32 = call i32 @val_to_i32(%Val %v2v)
  %n = extractvalue %Args %args, 1
  %cmp = icmp slt i64 %n, 3
  br i1 %cmp, label %l2, label %l3
l2:
  %out2 = call i32 @open(i8* %v1p, i32 %v2i32, i32 0)
  %out2i64 = sext i32 %out2 to i64
  %outv2 = call %Val @make_int_val(i64 %out2i64)
  ret %Val %outv2
l3:
  %v3 = call %Val @get_arg(%Args %args, i64 2)
  %v3i32 = call i32 @val_to_i32(%Val %v3)
  %out3 = call i32 @open(i8* %v1p, i32 %v2i32, i32 %v3i32)
  %out3i64 = sext i32 %out3 to i64
  %outv3 = tail call %Val @make_int_val(i64 %out3i64)
  ret %Val %outv3
}

define i8* @to_cstring(%Val %v) {
  %sn = call i64 @val_to_str_n(%Val %v)
  %sd = call i8* @val_to_str_d(%Val %v)
  %size = add i64 %sn, 1
  %out = call i8* @GC_malloc(i64 %size)
  call i8* @memcpy(i8* %out, i8* %sd, i64 %sn)
  %end = getelementptr i8, i8* %out, i64 %sn
  store i8 0, i8* %end
  ret i8* %out
}

define %Val @sys_write(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %fd = call i32 @val_to_i32(%Val %v1)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %data = call i8* @val_to_str_d(%Val %v2)
  %v3 = call %Val @get_arg(%Args %args, i64 2)
  %n = call i64 @val_to_i64(%Val %v3)
  %w = call i32 @write(i32 %fd, i8* %data, i64 %n)
  %w64 = sext i32 %w to i64
  %out = tail call %Val @make_int_val(i64 %w64)
  ret %Val %out
}

define %Val @sys_read(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %fd = call i32 @val_to_i32(%Val %v1)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %n = call i64 @val_to_i64(%Val %v2)
  %a = call i8* @GC_malloc(i64 %n)
  %nr = call i64 @read(i32 %fd, i8* %a, i64 %n)
  %out = tail call %Val @make_str_val(i64 %nr, i8* %a)
  ret %Val %out
}

define %Val @sys_fork(%Env %env, %Args %args) {
  %pid = call i32 @fork()
  %pid64 = sext i32 %pid to i64
  %out = tail call %Val @make_int_val(i64 %pid64)
  ret %Val %out
}

define %Val @call_equal(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %cmp = call i1 @val_equal(%Val %v1, %Val %v2)
  %out = tail call %Val @make_bool_val(i1 %cmp)
  ret %Val %out
}

define i1 @val_equal(%Val %v1, %Val %v2) {
entry:
  %t1 = extractvalue %Val %v1, 0
  %d1 = extractvalue %Val %v1, 1
  %t2 = extractvalue %Val %v2, 0
  %d2 = extractvalue %Val %v2, 1
  %tcmp = icmp eq i8 %t1, %t2
  br i1 %tcmp, label %data, label %false
false:
  ret i1 0
data:
  switch i8 %t1, label %false [
    i8 1, label %is_bool
    i8 2, label %is_int
    i8 3, label %is_str
    i8 4, label %is_list
    i8 5, label %is_str
    i8 6, label %is_func
    i8 7, label %is_vec
  ]
is_bool:
  %b1 = call i1 @to_i1(%Val %v1)
  %b2 = call i1 @to_i1(%Val %v2)
  %cmpd = icmp eq i1 %b1, %b2
  ret i1 %cmpd
is_int:
  %i1 = call i64 @val_to_i64(%Val %v1)
  %i2 = call i64 @val_to_i64(%Val %v2)
  %cmpi = icmp eq i64 %i1, %i2
  ret i1 %cmpi
is_str:
  %sn1 = call i64 @val_to_str_n(%Val %v1)
  %sn2 = call i64 @val_to_str_n(%Val %v2)
  %ncmp = icmp eq i64 %sn1, %sn2
  br i1 %ncmp, label %str_cmp, label %no
no:
  ret i1 0
str_cmp:
  %sd1 = call i8* @val_to_str_d(%Val %v1)
  %sd2 = call i8* @val_to_str_d(%Val %v2)
  %cmpsi = call i32 @memcmp(i8* %sd1, i8* %sd2, i64 %sn1)
  %cmps = icmp eq i32 %cmpsi, 0
  ret i1 %cmps
is_list:
  %l1 = bitcast i8* %d1 to %List*
  %l2 = bitcast i8* %d2 to %List*
  %cmpl = call i1 @list_equal(%List* %l1, %List* %l2)
  ret i1 %cmpl
is_func:
  %cmpf = icmp eq i8* %d1, %d2
  ret i1 %cmpf
is_vec:
  %vp1 = bitcast i8* %d1 to %Vec*
  %vp2 = bitcast i8* %d2 to %Vec*
  %cmpv = call i1 @vec_equal(%Vec* %vp1, %Vec* %vp2)
  ret i1 %cmpv
}

define i1 @list_equal(%List* %l1, %List* %l2) {
  ; if one is null, ret t or false 
  %l1n = icmp eq %List* %l1, null
  %l2n = icmp eq %List* %l2, null
  %onen = or i1 %l1n, %l2n
  br i1 %onen, label %comp, label %val

comp:
  %ncmp = and i1 %l1n, %l2n
  ret i1 %ncmp

val:
  %v1p = getelementptr %List, %List* %l1, i64 0, i32 0
  %v1 = load %Val, %Val* %v1p
  %v2p = getelementptr %List, %List* %l2, i64 0, i32 0
  %v2 = load %Val, %Val* %v2p
  %vcmp = call i1 @val_equal(%Val %v1, %Val %v2)
  br i1 %vcmp, label %next, label %false

false:
  ret i1 0

next:
  %cdr1p = getelementptr %List, %List* %l1, i64 0, i32 1
  %cdr2p = getelementptr %List, %List* %l2, i64 0, i32 1
  %cdr1 = load %List*, %List** %cdr1p
  %cdr2 = load %List*, %List** %cdr2p
  %cdrcmp = call i1 @list_equal(%List* %cdr1, %List* %cdr2)
  ret i1 %cdrcmp
}

define %Val @sys_waitpid(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %pid = call i32 @val_to_i32(%Val %v1)
  %statusrp = call i8* @GC_malloc(i64 32)
  %statusp = bitcast i8* %statusrp to i32*
  %w = call i32 @waitpid(i32 %pid, i32* %statusp, i32 0)
  %status = load i32, i32* %statusp
  %status64 = sext i32 %status to i64
  %out = tail call %Val @make_int_val(i64 %status64)
  ret %Val %out
}

define %Val @sys_execve(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %path = call i8* @to_cstring(%Val %v1)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %argsa = call i8** @to_str_array(%Val %v2)
  %v3 = call %Val @get_arg(%Args %args, i64 2)
  %enva = call i8** @to_str_array(%Val %v3)
  %ex = call i32 @execve(i8* %path, i8** %argsa, i8** %enva)
  %ex64 = sext i32 %ex to i64
  %out = tail call %Val @make_int_val(i64 %ex64)
  ret %Val %out
}

define i8** @to_str_array(%Val %v) {
entry:
  %p = extractvalue %Val %v, 1
  %lp = bitcast i8* %p to %List*
  %n = call i64 @list_length(%List* %lp)
  %n1 = add i64 %n, 1
  %size = mul i64 %n1, 64
  %outr = call i8* @GC_malloc(i64 %size)
  %out = bitcast i8* %outr to i8**
  br label %header
header:
  %head = phi %List* [ %lp, %entry], [ %cdr, %body ]
  %i = phi i64 [ 0, %entry ], [ %ni, %body ]
  %cmp = icmp eq %List* %head, null
  br i1 %cmp, label %done, label %body
body:
  %vp = getelementptr %List, %List* %head, i64 0, i32 0
  %elv = load %Val, %Val* %vp
  %s = call i8* @to_cstring(%Val %elv)
  %sp = getelementptr i8*, i8** %out, i64 %i
  store i8* %s, i8** %sp
  %cdrp = getelementptr %List, %List* %head, i64 0, i32 1
  %cdr = load %List*, %List** %cdrp
  %ni = add i64 %i, 1
  br label %header
done:
  %tp = getelementptr i8*, i8** %out, i64 %n
  store i8* null, i8** %tp
  ret i8** %out
}

define i32 @val_to_i32(%Val %v) {
  %x = call i64 @val_to_i64(%Val %v)
  %out = trunc i64 %x to i32
  ret i32 %out
}

define i64 @val_to_i64(%Val %v) {
  %d = extractvalue %Val %v, 1
  %ip = bitcast i8* %d to i64*
  %i = load volatile i64, i64* %ip
  ret i64 %i
}

define i64 @val_to_str_n(%Val %v) {
  %rp = extractvalue %Val %v, 1
  %sp = bitcast i8* %rp to %Str*
  %snp = getelementptr %Str, %Str* %sp, i64 0, i32 0
  %n = load i64, i64* %snp
  ret i64 %n
}

define i8* @val_to_str_d(%Val %v) {
  %rp = extractvalue %Val %v, 1
  %sp = bitcast i8* %rp to %Str*
  %sdp = getelementptr %Str, %Str* %sp, i64 0, i32 1
  %d = load i8*, i8** %sdp
  ret i8* %d
}

define %Val @sys_close(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v1i32 = call i32 @val_to_i32(%Val %v1)
  %out = call i32 @close(i32 %v1i32)
  %out64 = sext i32 %out to i64
  %outv = tail call %Val @make_int_val(i64 %out64)
  ret %Val %outv
}

define %Val @sys_dup(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v1i32 = call i32 @val_to_i32(%Val %v1)
  %out = call i32 @dup(i32 %v1i32)
  %out64 = sext i32 %out to i64
  %outv = tail call %Val @make_int_val(i64 %out64)
  ret %Val %outv
}

define %Val @sys_dup2(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v1i32 = call i32 @val_to_i32(%Val %v1)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %v2i32 = call i32 @val_to_i32(%Val %v2)
  %out = call i32 @dup2(i32 %v1i32, i32 %v2i32)
  %out64 = sext i32 %out to i64
  %outv = tail call %Val @make_int_val(i64 %out64)
  ret %Val %outv
}

define %Val @call_list(%Env %env, %Args %args) {
  %vs = extractvalue %Args %args, 0
  %size = extractvalue %Args %args, 1
  %n = sub i64 %size, 1
  %l = call %List* @make_list(%Val* %vs, %List* null, i64 %n)
  %v = tail call %Val @make_list_val(%List* %l)
  ret %Val %v
}

define %List* @make_list(%Val* %vs, %List* %lp, i64 %i) {
  %cmp = icmp sge i64 %i, 0
  br i1 %cmp, label %body, label %end

body:
  %vp = getelementptr %Val, %Val* %vs, i64 %i
  %v = load %Val, %Val* %vp
  %nl = call %List* @cons(%Val %v, %List* %lp)
  %ni = sub i64 %i, 1
  %out = call %List* @make_list(%Val* %vs, %List* %nl, i64 %ni)
  ret %List* %out

end:
  ret %List* %lp
}

define %List* @cons(%Val %v, %List* %lp) {
  %1 = insertvalue %List zeroinitializer, %Val %v, 0
  %2 = insertvalue %List %1, %List* %lp, 1
  %size = ptrtoint %List* getelementptr (%List, %List* null, i64 1) to i64
  %outp = call i8* @GC_malloc(i64 %size)
  %outpc = bitcast i8* %outp to %List*
  store %List %2, %List* %outpc
  ret %List* %outpc
}

define %Val @call_plus(%Env %env, %Args %args) {
entry:
  %vs = extractvalue %Args %args, 0
  %n = extractvalue %Args %args, 1
  br label %header
header:
  %sum = phi i64 [ 0, %entry], [ %bump, %body]
  %i = phi i64 [0, %entry], [%ni, %body]
  %cmp = icmp slt i64 %i, %n 
  br i1 %cmp, label %body, label %exit
body:
  %avp = getelementptr %Val, %Val* %vs, i64 %i
  %av = load %Val, %Val* %avp
  %x = call i64 @val_to_i64(%Val %av)
  %bump = add i64 %sum, %x
  %ni = add i64 %i, 1
  br label %header
exit:
  %out = tail call %Val @make_int_val(i64 %sum)
  ret %Val %out
}

define %Val @call_apply(%Env %env, %Args %args) {
  %vs = extractvalue %Args %args, 0
  %v1p = getelementptr %Val, %Val* %vs, i64 0
  %fv = load %Val, %Val* %v1p
  %v2p = getelementptr %Val, %Val* %vs, i64 1
  %lv = load %Val, %Val* %v2p
  %lvdp = extractvalue %Val %lv, 1
  %lp = bitcast i8* %lvdp to %List*
  %fargs = call %Args @list_to_args(%List* %lp)
  %out = tail call %Val @call_func_val(%Val %fv, %Args %fargs) ;; required for CPS call/cc TCO to work!
  ret %Val %out
}

define %Args @list_to_args(%List* %lp) {
entry:
  %n = call i64 @list_length(%List* %lp)
  %args = call %Args @make_args(i64 %n)
  br label %header
header:
  %i = phi i64 [ 0, %entry], [ %ni, %body ]
  %head = phi %List* [ %lp, %entry], [ %cdr, %body]
  %cmp = icmp slt i64 %i, %n
  br i1 %cmp, label %body, label %done
body:
  %valp = getelementptr %List, %List* %head, i64 0, i32 0
  %val = load %Val, %Val* %valp
  call void @set_arg(%Args %args, i64 %i, %Val %val)
  %cdrp = getelementptr %List, %List* %head, i64 0, i32 1
  %cdr = load %List*, %List** %cdrp
  %ni = add i64 %i, 1
  br label %header
done:
  ret %Args %args
}

define i64 @list_length(%List* %lp) {
  %cmp = icmp eq %List* %lp, null
  br i1 %cmp, label %end, label %next
end:
  ret i64 0
next:
  %cdrp = getelementptr %List, %List* %lp, i64 0, i32 1
  %cdr = load %List*, %List** %cdrp
  %lr = call i64 @list_length(%List* %cdr)
  %out = add i64 %lr, 1
  ret i64 %out
}

define %Val @make_list_val(%List* %lp) {
  %1 = insertvalue %Val zeroinitializer, i8 4, 0
  %p = bitcast %List* %lp to i8*
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val @call_func_val(%Val %v, %Args %args) {
  %frp = extractvalue %Val %v, 1
  %fp = bitcast i8* %frp to %Func*
  %f = load %Func, %Func* %fp
  %cf = extractvalue %Func %f, 0
  %e = extractvalue %Func %f, 1
  %out = tail call %Val %cf (%Env %e, %Args %args)
  ret %Val %out
}

define %Val @make_bool_val(i1 %x) {
  %1 = insertvalue %Val zeroinitializer, i8 1, 0
  %p = call i8* @GC_malloc(i64 1)
  %d = zext i1 %x to i8
  store i8 %d, i8* %p
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val @make_int_val(i64 %x) {
  %1 = insertvalue %Val zeroinitializer, i8 2, 0
  %p = call i8* @GC_malloc(i64 8)
  %ip = bitcast i8* %p to i64*
  store volatile i64 %x, i64* %ip
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val @make_str_val(i64 %n, i8* %s) {
  %1 = insertvalue %Val zeroinitializer, i8 3, 0
  %s1 = insertvalue %Str zeroinitializer, i64 %n, 0
  %s2 = insertvalue %Str %s1, i8* %s, 1
  %size = ptrtoint %Val* getelementptr (%Str, %Str* null, i64 1) to i64
  %data = call i8* @GC_malloc(i64 %size)
  %sp = bitcast i8* %data to %Str*
  store %Str %s2, %Str* %sp
  %out = insertvalue %Val %1, i8* %data, 1
  ret %Val %out
}

define %Val @make_sym_val(i64 %n, i8* %s) {
  %1 = call %Val @make_str_val(i64 %n, i8* %s)
  %2 = insertvalue %Val %1, i8 5, 0
  ret %Val %2
}

define %Args @make_args(i64 %n) {
  %size = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %m = mul i64 %size, %n
  %p = call i8* @GC_malloc(i64 %m)
  %vs = bitcast i8* %p to %Val*
  %1 = insertvalue %Args zeroinitializer, %Val* %vs, 0
  %2 = insertvalue %Args %1, i64 %n, 1
  ret %Args %2
}

define %Val @get_arg(%Args %args, i64 %i) {
  %vs = extractvalue %Args %args, 0
  %vp = getelementptr %Val, %Val* %vs, i64 %i
  %v = load %Val, %Val* %vp
  ret %Val %v
}

define void @set_arg(%Args %args, i64 %i, %Val %v) {
  %vs = extractvalue %Args %args, 0
  %vp = getelementptr %Val, %Val* %vs, i64 %i
  store %Val %v, %Val* %vp
  ret void
}

define void @set(%Env %env, i64 %depth, i64 %offset, %Val %v) {
  %vp = call %Val* @lookupp(%Env %env, i64 %depth, i64 %offset)
  store volatile %Val %v, %Val* %vp
  ret void
}

define %Env @sub_env(%Env %env, i64 %n) {
  %vsize = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %total = mul i64 %vsize, %n
  %p = call i8* @GC_malloc(i64 %total) ; note: relies on GC_malloc zeroing by default
  %vs = bitcast i8* %p to %Val*
  %1 = insertvalue %Env zeroinitializer, %Val* %vs, 0
  %esize = ptrtoint %Env* getelementptr (%Env, %Env* null, i64 1) to i64
  %ep = call i8* @GC_malloc(i64 %esize)
  %epc = bitcast i8* %ep to %Env*
  store %Env %env, %Env* %epc
  %2 = insertvalue %Env %1, %Env* %epc, 1
  ret %Env %2
}

define i1 @to_i1(%Val %v) {
  %t = extractvalue %Val %v, 0
  %d = extractvalue %Val %v, 1
  %isnotbool = icmp ne i8 %t, 1
  br i1 %isnotbool, label %notb, label %b
notb:
  ret i1 1
b:
  %dv = load i8, i8* %d
  %db = trunc i8 %dv to i1
  %isnotfalse = icmp ne i1 %db, 0
  ret i1 %isnotfalse
}

define void @set_rest(%Env %env, %Args %args, i64 %n) {
entry: 
  %na = extractvalue %Args %args, 1
  %nr = sub i64 %na, %n
  %vsize = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %size = mul i64 %nr, %vsize
  %p = call i8* @GC_malloc(i64 %size)
  %vs = bitcast i8* %p to %Val*
  br label %header
header:
  %i = phi i64 [%n, %entry], [%ni, %body]
  %cmp = icmp slt i64 %i, %na
  br i1 %cmp, label %body, label %done
body:
  %av = call %Val @get_arg(%Args %args, i64 %i)
  %ri = sub i64 %i, %n
  %rp = getelementptr %Val, %Val* %vs, i64 %ri
  store %Val %av, %Val* %rp
  %ni = add i64 %i, 1
  br label %header
done:
  %nr_minus_1 = sub i64 %nr, 1
  %l = call %List* @make_list(%Val* %vs, %List* null, i64 %nr_minus_1)
  %lv = call %Val @make_list_val(%List* %l)
  call void @set(%Env %env, i64 0, i64 %n, %Val %lv)
  ret void
}

define %Val @call_string_append(%Env %env, %Args %args) {
entry:
  %n = extractvalue %Args %args, 1
  br label %length_header

length_header:
  %i = phi i64 [0, %entry], [%ni, %length_body]
  %size = phi i64 [0, %entry], [%nsize, %length_body]
  %cmp = icmp slt i64 %i, %n
  br i1 %cmp, label %length_body, label %alloc

length_body:
  %vi = call %Val @get_arg(%Args %args, i64 %i)
  %sl = call i64 @val_to_str_n(%Val %vi)
  %ni = add i64 %i, 1
  %nsize = add i64 %size, %sl
  br label %length_header

alloc:
  %outp = call i8* @GC_malloc(i64 %size)
  br label %copy_header

copy_header: 
  %loc = phi i8* [ %outp, %alloc ], [ %nloc, %copy_body ]
  %j = phi i64 [ 0, %alloc ], [ %nj, %copy_body ]
  %ccmp = icmp slt i64 %j, %n
  br i1 %ccmp, label %copy_body, label %done

copy_body:
  %jv = call %Val @get_arg(%Args %args, i64 %j)
  %jsn = call i64 @val_to_str_n(%Val %jv)
  %jsd = call i8* @val_to_str_d(%Val %jv)
  call i8* @memcpy(i8* %loc, i8* %jsd, i64 %jsn)
  %nloc = getelementptr i8, i8* %loc, i64 %jsn
  %nj = add i64 1, %j
  br label %copy_header

done:
  %out = tail call %Val @make_str_val(i64 %size, i8* %outp)
  ret %Val %out
}

define %Val @call_car(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %lp = extractvalue %Val %v0, 1
  %l = bitcast i8* %lp to %List*
  %lv = load %List, %List* %l
  %car = extractvalue %List %lv, 0
  ret %Val %car
}

define %Val @call_cdr(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %lp = extractvalue %Val %v0, 1
  %l = bitcast i8* %lp to %List*
  %lv = load %List, %List* %l
  %cdr = extractvalue %List %lv, 1
  %out = tail call %Val @make_list_val(%List* %cdr)
  ret %Val %out
}

define %Val @call_cons(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %l1p = extractvalue %Val %v1, 1
  %l1 = bitcast i8* %l1p to %List*
  %h = call %List* @cons(%Val %v0, %List* %l1)
  %out = tail call %Val @make_list_val(%List* %h)
  ret %Val %out
}

define %Val @call_nullq(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %d = extractvalue %Val %v0, 1
  %cmp = icmp eq i8* %d, null
  %out = tail call %Val @make_bool_val(i1 %cmp)
  ret %Val %out
}

define %Val @call_slt(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %i0 = call i64 @val_to_i64(%Val %v0)
  %i1 = call i64 @val_to_i64(%Val %v1)
  %cmp = icmp slt i64 %i0, %i1
  %out = tail call %Val @make_bool_val(i1 %cmp)
  ret %Val %out
}

define %Val @call_sub(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %i0 = call i64 @val_to_i64(%Val %v0)
  %i1 = call i64 @val_to_i64(%Val %v1)
  %diff = sub i64 %i0, %i1
  %out = tail call %Val @make_int_val(i64 %diff)
  ret %Val %out
}

define %Val @call_symbol(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %str = extractvalue %Val %v0, 1
  %out1 = insertvalue %Val zeroinitializer, i8 5, 0
  %out2 = insertvalue %Val %out1, i8* %str, 1
  ret %Val %out2
}

define %Val @is_boolean(%Env %env, %Args %args) {
  %out = call %Val @is_type(%Args %args, i8 1)
  ret %Val %out
}

define %Val @is_integer(%Env %env, %Args %args) {
  %out = tail call %Val @is_type(%Args %args, i8 2)
  ret %Val %out
}

define %Val @is_string(%Env %env, %Args %args) {
  %out = tail call %Val @is_type(%Args %args, i8 3)
  ret %Val %out
}

define %Val @is_list(%Env %env, %Args %args) {
  %out = tail call %Val @is_type(%Args %args, i8 4)
  ret %Val %out
}

define %Val @is_symbol(%Env %env, %Args %args) {
  %out = tail call %Val @is_type(%Args %args, i8 5)
  ret %Val %out
}

define %Val @is_function(%Env %env, %Args %args) {
  %out = tail call %Val @is_type(%Args %args, i8 6)
  ret %Val %out
}

define %Val @is_type(%Args %args, i8 %type) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %vt = extractvalue %Val %v0, 0
  %cmp = icmp eq i8 %type, %vt
  %out = tail call %Val @make_bool_val(i1 %cmp)
  ret %Val %out
}

define %Val @call_random(%Env %env, %Args %args) {
  %r = call i32 @rand()
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %n = call i64 @val_to_i64(%Val %v0)
  %r64 = sext i32 %r to i64
  %rem = urem i64 %r64, %n
  %out = tail call %Val @make_int_val(i64 %rem)
  ret %Val %out
}

define %Val @call_list_length(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %d = extractvalue %Val %v0, 1
  %l = bitcast i8* %d to %List*
  %n = call i64 @list_length(%List* %l)
  %out = tail call %Val @make_int_val(i64 %n)
  ret %Val %out
}

define %Val @call_string_length(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %n = call i64 @val_to_str_n(%Val %v0)
  %out = tail call %Val @make_int_val(i64 %n)
  ret %Val %out
}

define %Val @call_string_lt(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %sn1 = call i64 @val_to_str_n(%Val %v1)
  %sn2 = call i64 @val_to_str_n(%Val %v2)
  %sd1 = call i8* @val_to_str_d(%Val %v1)
  %sd2 = call i8* @val_to_str_d(%Val %v2)
  %lcmp = icmp ult i64 %sn1, %sn2
  %minl = select i1 %lcmp, i64 %sn1, i64 %sn2
  %scmp  = call i32 @memcmp(i8* %sd1, i8* %sd2, i64 %minl)
  %seq = icmp eq i32 %scmp, 0
  br i1 %seq, label %ret_lcmp, label %ret_scmp
ret_lcmp:
  %lout = tail call %Val @make_bool_val(i1 %lcmp)
  ret %Val %lout
ret_scmp:
  %sb = icmp slt i32 %scmp, 0
  %out = tail call %Val @make_bool_val(i1 %sb)
  ret %Val %out
}

define %Val @call_string_list(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %sn = call i64 @val_to_str_n(%Val %v0)
  %sd = call i8* @val_to_str_d(%Val %v0)
  %l = call %List* @append_string_list(i64 %sn, i8* %sd)
  %out = tail call %Val @make_list_val(%List* %l)
  ret %Val %out
}

define %List* @append_string_list(i64 %n, i8* %s) {
  %cmp = icmp eq i64 %n, 0
  br i1 %cmp, label %end, label %more
end:
  ret %List* null

more:
  %v = call %Val @make_str_val(i64 1, i8* %s)
  %nn = sub i64 %n, 1
  %ns = getelementptr i8, i8* %s, i64 1
  %tail = call %List* @append_string_list(i64 %nn, i8* %ns)
  %out = call %List* @cons(%Val %v, %List* %tail)
  ret %List* %out
}

define %Val @call_set_car(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %lp = extractvalue %Val %v0, 1
  %l = bitcast i8* %lp to %List*
  %carp = getelementptr %List, %List* %l, i64 0, i32 0
  store %Val %v1, %Val* %carp
  ret %Val %v0
}

define %Val @call_set_cdr(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %l0p = extractvalue %Val %v0, 1
  %l0 = bitcast i8* %l0p to %List*
  %l1p = extractvalue %Val %v1, 1
  %l1 = bitcast i8* %l1p to %List*
  %cdrp = getelementptr %List, %List* %l0, i64 0, i32 1
  store %List* %l1, %List** %cdrp
  ret %Val %v0
}

define %Val @call_string(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %vd = extractvalue %Val %v0, 1
  %out1 = insertvalue %Val zeroinitializer, i8 3, 0
  %out2 = insertvalue %Val %out1, i8* %vd, 1
  ret %Val %out2
}

define %Val @call_modulo(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %i0 = call i64 @val_to_i64(%Val %v0)
  %i1 = call i64 @val_to_i64(%Val %v1)
  %r = srem i64 %i0, %i1
  %out = tail call %Val @make_int_val(i64 %r)
  ret %Val %out
}

define %Val @call_mul(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %i0 = call i64 @val_to_i64(%Val %v0)
  %i1 = call i64 @val_to_i64(%Val %v1)
  %r = mul i64 %i0, %i1
  %out = tail call %Val @make_int_val(i64 %r)
  ret %Val %out
}

define %Val @call_div(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %i0 = call i64 @val_to_i64(%Val %v0)
  %i1 = call i64 @val_to_i64(%Val %v1)
  %r = sdiv i64 %i0, %i1
  %out = tail call %Val @make_int_val(i64 %r)
  ret %Val %out
}

define %Val @call_substring(%Env %env, %Args %args) {
  %vs = call %Val @get_arg(%Args %args, i64 0)
  %vi = call %Val @get_arg(%Args %args, i64 1)
  %vj = call %Val @get_arg(%Args %args, i64 2)
  %sd = call i8* @val_to_str_d(%Val %vs)
  %i = call i64 @val_to_i64(%Val %vi)
  %j = call i64 @val_to_i64(%Val %vj)
  %n = sub i64 %j, %i
  %nsd = getelementptr i8, i8* %sd, i64 %i
  %out = tail call %Val @make_str_val(i64 %n, i8* %nsd)
  ret %Val %out
}

define void @seed_random() {
  %t = call i64 @time(i64* null)
  %seed = trunc i64 %t to i32
  call void @srand(i32 %seed)
  ret void
}

define %Val @make_vec_val(i64 %n, %Val* %data) {
  %1 = insertvalue %Val zeroinitializer, i8 7, 0
  %v1 = insertvalue %Vec zeroinitializer, i64 %n, 0
  %v2 = insertvalue %Vec %v1, %Val* %data, 1
  %size = ptrtoint %Vec* getelementptr (%Vec, %Vec* null, i64 1) to i64
  %p = call i8* @GC_malloc(i64 %size)
  %vp = bitcast i8* %p to %Vec*
  store %Vec %v2, %Vec* %vp
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val @call_make_vector(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %n = call i64 @val_to_i64(%Val %v0)
  %fill = call %Val @get_arg(%Args %args, i64 1)
  %vsize = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %total = mul i64 %vsize, %n
  %p = call i8* @GC_malloc(i64 %total)
  %data = bitcast i8* %p to %Val*
  br label %header
header:
  %i = phi i64 [ 0, %0 ], [ %ni, %body ]
  %cmp = icmp slt i64 %i, %n
  br i1 %cmp, label %body, label %done
body:
  %ep = getelementptr %Val, %Val* %data, i64 %i
  store %Val %fill, %Val* %ep
  %ni = add i64 %i, 1
  br label %header
done:
  %out = tail call %Val @make_vec_val(i64 %n, %Val* %data)
  ret %Val %out
}

define %Val @call_vector_ref(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %rp = extractvalue %Val %v0, 1
  %vp = bitcast i8* %rp to %Vec*
  %dp = getelementptr %Vec, %Vec* %vp, i64 0, i32 1
  %data = load %Val*, %Val** %dp
  %idx = call i64 @val_to_i64(%Val %v1)
  %ep = getelementptr %Val, %Val* %data, i64 %idx
  %out = load %Val, %Val* %ep
  ret %Val %out
}

define %Val @call_vector_set(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %v2 = call %Val @get_arg(%Args %args, i64 2)
  %rp = extractvalue %Val %v0, 1
  %vp = bitcast i8* %rp to %Vec*
  %dp = getelementptr %Vec, %Vec* %vp, i64 0, i32 1
  %data = load %Val*, %Val** %dp
  %idx = call i64 @val_to_i64(%Val %v1)
  %ep = getelementptr %Val, %Val* %data, i64 %idx
  store %Val %v2, %Val* %ep
  ret %Val %v2
}

define %Val @call_vector_length(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %rp = extractvalue %Val %v0, 1
  %vp = bitcast i8* %rp to %Vec*
  %np = getelementptr %Vec, %Vec* %vp, i64 0, i32 0
  %n = load i64, i64* %np
  %out = tail call %Val @make_int_val(i64 %n)
  ret %Val %out
}

define %Val @is_vector(%Env %env, %Args %args) {
  %out = tail call %Val @is_type(%Args %args, i8 7)
  ret %Val %out
}

define %Val @call_list_to_vector(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %d = extractvalue %Val %v0, 1
  %lp = bitcast i8* %d to %List*
  %n = call i64 @list_length(%List* %lp)
  %vsize = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %total = mul i64 %vsize, %n
  %p = call i8* @GC_malloc(i64 %total)
  %data = bitcast i8* %p to %Val*
  br label %l2v_header
l2v_header:
  %i = phi i64 [ 0, %0 ], [ %ni, %l2v_body ]
  %head = phi %List* [ %lp, %0 ], [ %cdr, %l2v_body ]
  %cmp = icmp slt i64 %i, %n
  br i1 %cmp, label %l2v_body, label %l2v_done
l2v_body:
  %valp = getelementptr %List, %List* %head, i64 0, i32 0
  %val = load %Val, %Val* %valp
  %ep = getelementptr %Val, %Val* %data, i64 %i
  store %Val %val, %Val* %ep
  %cdrp = getelementptr %List, %List* %head, i64 0, i32 1
  %cdr = load %List*, %List** %cdrp
  %ni = add i64 %i, 1
  br label %l2v_header
l2v_done:
  %out = tail call %Val @make_vec_val(i64 %n, %Val* %data)
  ret %Val %out
}

define %Val @call_vector_to_list(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %rp = extractvalue %Val %v0, 1
  %vp = bitcast i8* %rp to %Vec*
  %np = getelementptr %Vec, %Vec* %vp, i64 0, i32 0
  %n = load i64, i64* %np
  %dp = getelementptr %Vec, %Vec* %vp, i64 0, i32 1
  %data = load %Val*, %Val** %dp
  %nm1 = sub i64 %n, 1
  %l = call %List* @make_list(%Val* %data, %List* null, i64 %nm1)
  %out = tail call %Val @make_list_val(%List* %l)
  ret %Val %out
}

define i1 @vec_equal(%Vec* %v1, %Vec* %v2) {
entry:
  %n1p = getelementptr %Vec, %Vec* %v1, i64 0, i32 0
  %n1 = load i64, i64* %n1p
  %n2p = getelementptr %Vec, %Vec* %v2, i64 0, i32 0
  %n2 = load i64, i64* %n2p
  %ncmp = icmp eq i64 %n1, %n2
  br i1 %ncmp, label %check, label %no
no:
  ret i1 0
check:
  %d1p = getelementptr %Vec, %Vec* %v1, i64 0, i32 1
  %d1 = load %Val*, %Val** %d1p
  %d2p = getelementptr %Vec, %Vec* %v2, i64 0, i32 1
  %d2 = load %Val*, %Val** %d2p
  br label %header
header:
  %i = phi i64 [ 0, %check ], [ %ni, %match ]
  %cmp = icmp slt i64 %i, %n1
  br i1 %cmp, label %body, label %yes
body:
  %e1p = getelementptr %Val, %Val* %d1, i64 %i
  %e1 = load %Val, %Val* %e1p
  %e2p = getelementptr %Val, %Val* %d2, i64 %i
  %e2 = load %Val, %Val* %e2p
  %ecmp = call i1 @val_equal(%Val %e1, %Val %e2)
  br i1 %ecmp, label %match, label %no
match:
  %ni = add i64 %i, 1
  br label %header
yes:
  ret i1 1
}
