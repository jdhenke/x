%Val = type { i8, i8* } ; type, data
%Env = type { %Val**, %Env* } ; start of list, parent
%Args = type { %Val*, i64 } ; start of list, size

%Func = type { %Val(%Env, %Args)*, %Env } ; ptr, closure
%List = type { %Val, %List* } ; car, cdr

@int_str = unnamed_addr constant [4 x i8] c"%d\0A\00"
@str_str = unnamed_addr constant [6 x i8] c"\22%s\22\0A\00"
@unk_str = unnamed_addr constant [5 x i8] c"unk\0A\00"
@func_str = unnamed_addr constant [7 x i8] c"λ %x\0A\00"

declare i32 @printf(i8*, ...)
declare i8* @GC_malloc(i64)

define %Env @make_global_env() {
  ; create env with val in it
  %size = mul i64 64, 2
  %valsp = call i8* @GC_malloc(i64 %size) ; x2 element
  %vals = bitcast i8* %valsp to %Val**

  ; be sure to update size accordingly
  call void @store_native_func(%Val** %vals, %Val(%Env, %Args)* @call_list, i64 0)
  call void @store_native_func(%Val** %vals, %Val(%Env, %Args)* @call_plus, i64 1)

  ; construct global env with native funcs
  %e1 = insertvalue %Env undef, %Val** %vals, 0
  %e2 = insertvalue %Env %e1, %Env* null, 1

  ; ret env
  ret %Env %e2

}

define void @store_native_func(%Val** %vals, %Val(%Env, %Args)* %f, i64 %i) {
  %nulle = insertvalue %Env undef, %Val** null, 0
  %f1 = insertvalue %Func undef, %Val(%Env, %Args)* %f, 0
  %f2 = insertvalue %Func %f1, %Env %nulle, 1
  %fv = call %Val @make_func_val(%Func %f2)
  %fvp = call %Val* @val_ptr(%Val %fv)
  %v0 = getelementptr %Val*, %Val** %vals, i64 %i
  store %Val* %fvp, %Val** %v0
  ret void
}

define %Val @lookup(%Env %env, i64 %depth, i64 %offset) {
  %cmp = icmp eq i64 %depth, 0
  br i1 %cmp, label %access, label %next
access:
  %vs = extractvalue %Env %env, 0
  %vpp = getelementptr %Val*, %Val** %vs, i64 %offset
  %vp = load %Val*, %Val** %vpp
  %v = load %Val, %Val* %vp
  ret %Val %v

next:
  %pep = extractvalue %Env %env, 1
  %pe = load %Env, %Env* %pep
  %nd = sub i64 %depth, 1
  %rv = call %Val @lookup(%Env %pe, i64 %nd, i64 %offset)
  ret %Val %rv
}

define %Val @make_func_val(%Func %f) {
  %1 = insertvalue %Val undef, i8 6, 0
  %p = call i8* @GC_malloc(i64 192)
  %fp = bitcast i8* %p to %Func*
  store %Func %f, %Func* %fp
  %2 = insertvalue %Val %1, i8* %p, 1
  ret %Val %2
}

define %Val* @val_ptr(%Val %v) {
  %p = call i8* @GC_malloc(i64 128)
  %vp = bitcast i8* %p to %Val*
  store %Val %v, %Val* %vp
  ret %Val* %vp
}

;;; FIXME
define %Val @call_list(%Env %env, %Args %args) {
  %vs = extractvalue %Args %args, 0
  %size = extractvalue %Args %args, 1
  %n = sub i64 %size, 1
  %l = call %List @make_list(%Val* %vs, %List* null, i64 %n)
  %v = call %Val @make_list_val(%List %l)
  ret %Val %v
}

define %List @make_list(%Val* %vs, %List %l, i64 %i) {
  %cmp = icmp sge i64 %i, 0
  br i1 %cmp, label %body, label %end

body:
  %vp = getelementptr %Val, %Val* %vs, i64 %i
  %v = load %Val, %Val* %vp
  %nl = call %List @cons(%Val %v, %List %l)
  %ni = sub i64 %i, 1
  %out = call %List @make_list(%Val* %vs, %List %nl, i64 %ni)
  ret %List %out

end:
  ret %List %l
}

define %List @cons(%Val %v, %List %l) {
  %1 = insertvalue %List undef, %Val %v, 0
  %lp = call i8* @GC_malloc(i64 192)
  %lpc = bitcast i8* %lp to %List*
  store %List %l, %List* %lpc
  %2 = insertvalue %List %1, %List* %lpc, 1
  ret %List %2
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


define void @print(%Val %v) {
  %is = getelementptr inbounds [4 x i8], [4 x i8]* @int_str, i64 0, i64 0
  %ss = getelementptr inbounds [6 x i8], [6 x i8]* @str_str, i64 0, i64 0
  %us = getelementptr inbounds [5 x i8], [5 x i8]* @unk_str, i64 0, i64 0
  %fs = getelementptr inbounds [7 x i8], [7 x i8]* @func_str, i64 0, i64 0
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
  ;; TODO: print list
  ret void

is_func:
  call i32 (i8*, ...) @printf(i8* %fs, i8* %x)
  ret void

default:
  call i32 (i8*, ...) @printf(i8* %us)
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

define %Val @make_list_val(%List %l) {
  %1 = insertvalue %Val undef, i8 4, 0
  %lp = call i8* @GC_malloc(i64 192)
  %lpc = bitcast i8* %lp to %List*
  store %List %l, %List* %lpc
  %2 = insertvalue %Val %1, i8* %lp, 1
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
