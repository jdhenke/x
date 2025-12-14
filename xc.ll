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
  ; create function for call_list
  %f1 = insertvalue %Func undef, %Val(%Env, %Args)* @call_list, 0
  %fe = insertvalue %Env undef, %Val** null, 0
  %f2 = insertvalue %Func %f1, %Env %fe, 1

  ; create val for function
  %vf = call %Val @make_func_val(%Func %f2)
  %vfp = call %Val* @val_ptr(%Val %vf)

  ; create env with val in it
  %valsp = call i8* @GC_malloc(i64 64) ; x1 element
  %vals = bitcast i8* %valsp to %Val**
  %v0 = getelementptr %Val*, %Val** %vals, i64 0
  store %Val* %vfp, %Val** %v0

  %e1 = insertvalue %Env undef, %Val** %vals, 0
  %e2 = insertvalue %Env %e1, %Env* null, 1

  ; ret env
  ret %Env %e2

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

define %Val @call_list(%Env %env, %Args %args) {
  ; malloc a List
  %lrp = call i8* @GC_malloc(i64 128)
  %lp = bitcast i8* %lrp to %List*
  %carp = getelementptr %List, %List* %lp, i32 0, i32 0
  %foo = call %Val @make_int_val(i64 64)
  store %Val %foo, %Val* %carp
  %cdrp = getelementptr %List, %List* %lp, i32 0, i32 1
  store %List* null, %List** %cdrp

  %1 = insertvalue %Val undef, i8 5, 0 ; type list
  %2 = insertvalue %Val %1, i8* %lp, 1 ; store pointer to malloced List as value
  ret %Val %2
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

