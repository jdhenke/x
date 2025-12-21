%Val = type { i8, i8* } ; type, data
%Env = type { %Val*, %Env* } ; start of list, parent
%Args = type { %Val*, i64 } ; start of list, size

%Func = type { %Val(%Env, %Args)*, %Env } ; ptr, closure
%List = type { %Val, %List* } ; car, cdr

@nl_str = unnamed_addr constant [2 x i8] c"\0A\00"

@int_str = unnamed_addr constant [3 x i8] c"%d\00"
@str_str = unnamed_addr constant [5 x i8] c"\22%s\22\00"
@unk_str = unnamed_addr constant [10 x i8] c"unk %d %x\00"
@func_str = unnamed_addr constant [6 x i8] c"λ %x\00"

@lp_str = unnamed_addr constant [2 x i8] c"(\00"
@rp_str = unnamed_addr constant [2 x i8] c")\00"
@sp_str = unnamed_addr constant [2 x i8] c" \00"

declare i32 @printf(i8*, ...)
declare i8* @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare i8* @GC_malloc(i64)

;; syscalls
declare void @exit(i32) noreturn

declare i32 @open(i8*, i32, ...)
declare i64 @read(i32, i8*, i64)
declare i64 @write(i32, i8*, i64)
declare i32 @close(i32)

declare i32 @fork()
declare i32 @execve(i8*, i8**, i8**)
declare i32 @waitpid(i32, i32*, i32)

define %Env @make_global_env() {
  ; create env with val in it
  %size = mul i64 64, 20
  %valsp = call i8* @GC_malloc(i64 %size)
  %vals = bitcast i8* %valsp to %Val*

  ; be sure to update size accordingly
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_exit, i64 0)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_list, i64 1)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_plus, i64 2)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_apply, i64 3)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_open, i64 4)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_close, i64 5)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_write, i64 6)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_println, i64 7)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_read, i64 8)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_fork, i64 9)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_equal, i64 10)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_waitpid, i64 11)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @sys_execve, i64 12)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_string_append, i64 13)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_car, i64 14)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_cdr, i64 15)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_cons, i64 16)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_nullq, i64 17)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_slt, i64 18)
  call void @store_native_func(%Val* %vals, %Val(%Env, %Args)* @call_sub, i64 19)

  ; construct global env with native funcs
  %e1 = insertvalue %Env undef, %Val* %vals, 0
  %e2 = insertvalue %Env %e1, %Env* null, 1

  ; ret env
  ret %Env %e2
}

define void @store_native_func(%Val* %vals, %Val(%Env, %Args)* %f, i64 %i) {
  %nulle = insertvalue %Env undef, %Val* null, 0
  %f1 = insertvalue %Func undef, %Val(%Env, %Args)* %f, 0
  %f2 = insertvalue %Func %f1, %Env %nulle, 1
  %fv = call %Val @make_func_val(%Func %f2)
  %fvp = getelementptr %Val, %Val* %vals, i64 %i
  store %Val %fv, %Val** %fvp
  ret void
}

define %Val @lookup(%Env %env, i64 %depth, i64 %offset) {
  %vp = call %Val* @lookupp(%Env %env, i64 %depth, i64 %offset)
  %v = load %Val, %Val* %vp
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
  %rv = call %Val* @lookupp(%Env %pe, i64 %nd, i64 %offset)
  ret %Val* %rv
}

define %Val @make_func_val(%Func %f) {
  %1 = insertvalue %Val undef, i8 6, 0
  %size = ptrtoint %Func* getelementptr (%Func, %Func* null, i64 1) to i64
  %p = call i8* @GC_malloc(i64 %size)
  %fp = bitcast i8* %p to %Func*
  store %Func %f, %Func* %fp
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val @to_func_val(%Val(%Env, %Args)* %f, %Env %env) {
  %f1 = insertvalue %Func undef, %Val(%Env, %Args)* %f, 0
  %f2 = insertvalue %Func %f1, %Env %env, 1
  %v = call %Val @make_func_val(%Func %f2)
  ret %Val %v
}

define %Val* @val_ptr(%Val %v) {
  %size = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %p = call i8* @GC_malloc(i64 %size)
  %vp = bitcast i8* %p to %Val*
  store %Val %v, %Val* %vp
  ret %Val* %vp
}

define %Val* @empty_val_ptr() {
  %size = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %p = call i8* @GC_malloc(i64 %size)
  %vp = bitcast i8* %p to %Val*
  ret %Val* %vp
}

define %Val @sys_exit(%Env %env, %Args %args) {
  %v = call %Val @get_arg(%Args %args, i64 0)
  %p = extractvalue %Val %v, 1
  %c = ptrtoint i8* %p to i32
  call void @exit(i32 %c)
  ret %Val %v
}

define %Val @sys_open(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v1p = extractvalue %Val %v1, 1
  %v2v = call %Val @get_arg(%Args %args, i64 1)
  %v2p = extractvalue %Val %v2v, 1
  %v2i64 = ptrtoint i8* %v2p to i64
  %v2i32 = trunc i64 %v2i64 to i32
  %n = extractvalue %Args %args, 1
  %cmp = icmp slt i64 %n, 3
  br i1 %cmp, label %l2, label %l3
l2:
  %out2 = call i32 @open(i8* %v1p, i32 %v2i32)
  %out2i64 = zext i32 %out2 to i64
  %outv2 = call %Val @make_int_val(i64 %out2i64)
  ret %Val %outv2
l3:
  %v3 = call %Val @get_arg(%Args %args, i64 2)
  %v3p = extractvalue %Val %v3, 1
  %v3i64 = ptrtoint i8* %v3p to i64
  %v3i32 = trunc i64 %v3i64 to i32
  %out3 = call i32 @open(i8* %v1p, i32 %v2i32, i32 %v3i32)
  %out3i64 = zext i32 %out3 to i64
  %outv3 = call %Val @make_int_val(i64 %out3i64)
  ret %Val %outv3
}

define %Val @sys_write(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %fd = call i32 @val_to_i32(%Val %v1)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %data = extractvalue %Val %v2, 1
  %v3 = call %Val @get_arg(%Args %args, i64 2)
  %n = call i64 @val_to_i64(%Val %v3)
  %w = call i32 @write(i32 %fd, i8* %data, i64 %n)
  %w64 = zext i32 %w to i64
  %out = call %Val @make_int_val(i64 %w64)
  ret %Val %out
}

define %Val @sys_read(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %fd = call i32 @val_to_i32(%Val %v1)
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %n = call i64 @val_to_i64(%Val %v2)
  %size = add i64 %n, 1
  %a = call i8* @GC_malloc(i64 %size)
  %nr = call i32 @read(i32 %fd, i8* %a, i64 %n)
  %nr64 = zext i32 %nr to i64
  %bp = getelementptr i8, i8* %a, i64 %nr64
  store i8 0, i8* %bp
  %out = call %Val @make_str_val(i8* %a)
  ret %Val %out
}

define %Val @sys_fork(%Env %env, %Args %args) {
  %pid = call i32 @fork()
  %pid64 = zext i32 %pid to i64
  %out = call %Val @make_int_val(i64 %pid64)
  ret %Val %out
}

define %Val @call_equal(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %d1 = extractvalue %Val %v1, 1
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %d2 = extractvalue %Val %v2, 1
  %b1 = icmp eq i8* %d1, %d2
  %out = call %Val @make_bool_val(i1 %b1)
  ret %Val %out
}

define %Val @sys_waitpid(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %pid = call i32 @val_to_i32(%Val %v1)
  %statusp = call i8* @GC_malloc(i64 32)
  %status = bitcast i8* %statusp to i32*
  %w = call i32 @waitpid(i32 %pid, i32* %status, i32 0)
  %w64 = zext i32 %pid to i64
  %out = call %Val @make_int_val(i64 %w64)
  ret %Val %out
}

define %Val @sys_execve(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %path = extractvalue %Val %v1, 1
  %v2 = call %Val @get_arg(%Args %args, i64 1)
  %argsa = call i8** @to_str_array(%Val %v2)
  %v3 = call %Val @get_arg(%Args %args, i64 2)
  %enva = call i8** @to_str_array(%Val %v3)
  %ex = call i32 @execve(i8* %path, i8** %argsa, i8** %enva)
  %ex64 = zext i32 %ex to i64
  %out = call %Val @make_int_val(i64 %ex64)

  ret %Val %out
}

define i8** @to_str_array(%Val %v) {
entry:
  %p = extractvalue %Val %v, 1
  %lp = bitcast i8* %p to %List*
  %n = call i64 @list_length(%List* %lp)
  %n1 = add i64 %n, 1
  %size = mul i64 %n1, 64
  %out = call i8** @GC_malloc(i64 %size)
  br label %header
header:
  %head = phi %List* [ %lp, %entry], [ %cdr, %body ]
  %i = phi i64 [ 0, %entry ], [ %ni, %body ]
  %cmp = icmp eq %List* %head, null
  br i1 %cmp, label %done, label %body
body:
  %vp = getelementptr %List, %List* %head, i64 0, i32 0
  %elv = load %Val, %Val* %vp
  %s = extractvalue %Val %elv, 1
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
  %i = ptrtoint i8* %d to i64
  ret i64 %i
}

define %Val @sys_close(%Env %env, %Args %args) {
  %v1 = call %Val @get_arg(%Args %args, i64 0)
  %v1p = extractvalue %Val %v1, 1
  %v1i64 = ptrtoint i8* %v1p to i64
  %v1i32 = trunc i64 %v1i64 to i32
  %out = call i32 @close(i32 %v1i32)
  %out64 = zext i32 %out to i64
  %outv = call %Val @make_int_val(i64 %out64)
  ret %Val %outv
}


define %Val @call_list(%Env %env, %Args %args) {
  %vs = extractvalue %Args %args, 0
  %size = extractvalue %Args %args, 1
  %n = sub i64 %size, 1
  %l = call %List* @make_list(%Val* %vs, %List* null, i64 %n)
  %v = call %Val @make_list_val(%List* %l)
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
  %1 = insertvalue %List undef, %Val %v, 0
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
  %d = extractvalue %Val %av, 1
  %x = ptrtoint i8* %d to i64
  %bump = add i64 %sum, %x
  %ni = add i64 %i, 1
  br label %header
exit:
  %out = call %Val @make_int_val(i64 %sum)
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
  %out = call %Val @call_func_val(%Val %fv, %Args %fargs)
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
  %1 = insertvalue %Val undef, i8 4, 0
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
  %out = call %Val %cf (%Env %e, %Args %args)
  ret %Val %out
}

define %Val @call_println(%Env %env, %Args %arg) {
  %v = call %Val @get_arg(%Args %arg, i64 0)
  call void @println(%Val %v)
  ret %Val %v
}

define void @println(%Val %v) {
  call void @print(%Val %v)
  %nl = getelementptr inbounds [2 x i8], [2 x i8]* @nl_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %nl)
  ret void
}

define void @print(%Val %v) {
  %is = getelementptr inbounds [3 x i8], [3 x i8]* @int_str, i64 0, i64 0
  %ss = getelementptr inbounds [5 x i8], [5 x i8]* @str_str, i64 0, i64 0
  %us = getelementptr inbounds [10 x i8], [10 x i8]* @unk_str, i64 0, i64 0
  %fs = getelementptr inbounds [6 x i8], [6 x i8]* @func_str, i64 0, i64 0

  %type = extractvalue %Val %v, 0
  %x = extractvalue %Val %v, 1

  ; call i32 (i8*, ...) @printf(i8* %is, i8 %type)

  switch i8 %type, label %default [
    i8 1, label %is_bool
    i8 2, label %is_int
    i8 3, label %is_str
    i8 4, label %is_list
    i8 6, label %is_func
  ]
  ; symb
  ; list

is_bool:
  %b = ptrtoint i8* %x to i1
  call i32 (i8*, ...) @printf(i8* %is, i1 %b)
  ret void

is_int:
  %i = ptrtoint i8* %x to i64
  call i32 (i8*, ...) @printf(i8* %is, i64 %i)
  ret void

is_str:
  call i32 (i8*, ...) @printf(i8* %ss, i8* %x)
  ret void

is_list:
  %lp = bitcast i8* %x to %List*
  call void @print_list(%List* %lp)
  ret void

is_func:
  call i32 (i8*, ...) @printf(i8* %fs, i8* %x)
  ret void

default:
  call i32 (i8*, ...) @printf(i8* %us, i8 %type, i8* %x)
  ret void
}

define void @print_list(%List* %lp) {
entry:
  %lps = getelementptr inbounds [2 x i8], [2 x i8]* @lp_str, i64 0, i64 0
  %rps = getelementptr inbounds [2 x i8], [2 x i8]* @rp_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %lps)
  call void @print_list_vals(%List* %lp)
  call i32 (i8*, ...) @printf(i8* %rps)
  ret void
}

define void @print_list_vals(%List* %lp) {
  %sps = getelementptr inbounds [2 x i8], [2 x i8]* @sp_str, i64 0, i64 0
  %cmp = icmp eq %List* %lp, null
  br i1 %cmp, label %done, label %more

done:
  ret void

more:
  %l = load %List, %List* %lp
  %car = extractvalue %List %l, 0
  call void @print(%Val %car)
  %cdr = extractvalue %List %l, 1
  %more_el = icmp ne %List* %cdr, null
  br i1 %more_el, label %recurse, label %done

recurse:
  call i32 (i8*, ...) @printf(i8* %sps)
  call void @print_list_vals(%List* %cdr)
  ret void

}

define %Val @make_bool_val(i1 %x) {
  %1 = insertvalue %Val undef, i8 1, 0
  %d = inttoptr i1 %x to i8*
  %2 = insertvalue %Val %1, i8* %d, 1
  ret %Val %2
}

define %Val @make_int_val(i64 %x) {
  %1 = insertvalue %Val undef, i8 2, 0
  %d = inttoptr i64 %x to i8*
  %2 = insertvalue %Val %1, i8* %d, 1
  ret %Val %2
}

define %Val @make_str_val(i8* %s) {
  %1 = insertvalue %Val undef, i8 3, 0
  %2 = insertvalue %Val %1, i8* %s, 1
  ret %Val %2
}

define %Args @make_args(i64 %n) {
  %size = ptrtoint %Val* getelementptr (%Val, %Val* null, i64 1) to i64
  %m = mul i64 %size, %n
  %p = call i8* @GC_malloc(i64 %m)
  %vs = bitcast i8* %p to %Val*
  %1 = insertvalue %Args undef, %Val* %vs, 0
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
  store %Val %v, %Val* %vp
  ret void
}

define %Env @sub_env(%Env %env, i64 %n) {
  %size = mul i64 64, %n
  %p = call i8* @GC_malloc(i64 %size)
  %vs = bitcast i8* %p to %Val**
  %1 = insertvalue %Env undef, %Val** %vs, 0
  %ep = call i8* @GC_malloc(i64 64)
  %epc = bitcast i8* %ep to %Env*
  store %Env %env, %Env* %epc
  %2 = insertvalue %Env %1, %Env* %epc, 1
  ret %Env %2
}

define i1 @to_i1(%Val %v) {
  %d = extractvalue %Val %v, 1
  %out = ptrtoint i8* %d to i1
  ret i1 %out
}

define void @set_rest(%Env %env, %Args %args, i64 %n) {
entry: 
  %na = extractvalue %Args %args, 1
  %nr = sub i64 %na, %n
  %size = mul i64 %nr, 64
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
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %s0 = extractvalue %Val %v0, 1
  %s1 = extractvalue %Val %v1, 1
  %l1 = call i64 @strlen(i8* %s0)
  %l2 = call i64 @strlen(i8* %s1)
  %t = add i64 %l1, %l2
  %size = add i64 1, %t
  %p = call i8* @GC_malloc(i64 %size)
  call i8* @strcpy(i8* %p, i8* %s0)
  call i8* @strcat(i8* %p, i8* %s1)
  %out = call %Val @make_str_val(i8* %p)
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
  %out = call %Val @make_list_val(%List* %cdr)
  ret %Val %out
}

define %Val @call_cons(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %l1p = extractvalue %Val %v1, 1
  %l1 = bitcast i8* %l1p to %List*
  %h = call %List* @cons(%Val %v0, %List* %l1)
  %out = call %Val @make_list_val(%List* %h)
  ret %Val %out
}

define %Val @call_nullq(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %d = extractvalue %Val %v0, 1
  %cmp = icmp eq i8* %d, null
  %out = call %Val @make_bool_val(i1 %cmp)
  ret %Val %out
}

define %Val @call_slt(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %p0 = extractvalue %Val %v0, 1
  %p1 = extractvalue %Val %v1, 1
  %i0 = ptrtoint i8* %p0 to i64
  %i1 = ptrtoint i8* %p1 to i64
  %cmp = icmp slt i64 %i0, %i1
  %out = call %Val @make_bool_val(i1 %cmp)
  ret %Val %out
}

define %Val @call_sub(%Env %env, %Args %args) {
  %v0 = call %Val @get_arg(%Args %args, i64 0)
  %v1 = call %Val @get_arg(%Args %args, i64 1)
  %p0 = extractvalue %Val %v0, 1
  %p1 = extractvalue %Val %v1, 1
  %i0 = ptrtoint i8* %p0 to i64
  %i1 = ptrtoint i8* %p1 to i64
  %diff = sub i64 %i0, %i1
  %out = call %Val @make_int_val(i64 %diff)
  ret %Val %out
}
