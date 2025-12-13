%Val = type { i8, i8* } ; type, data
%List = type { %Val*, %List* } ; car, cdr
%Vals = type { %Val**, i64 }; malloced list of vals, size
%Func = type { %Val*(%Vals*, %Vals*)*, %Vals*} ; f(env, args) -> val, env


declare i32 @printf(i8*, ...)
declare i8* @GC_malloc(i64)

; (define a 1)
; (define b 2)
; (printf "%d\n" (+ a b))

%c1 = unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @main() {
  ; stdlib
  %var_plus_envrp = call i8* @GC_malloc(i64 128)
  %var_plus_envp = bitcast i8* %var_plus_envrp to %Vals*
  %var_plus_envpl = getelementptr %Vals, %Vals* %var_plus_envp, i32 0, i32 1
  store i64 0, i64* %var_plus_envpl
  %var_plus = call %Val* @make_func_val(%Val*(%Vals*, %Vals*)* @std_plus, %Vals* %var_plus_envp)


  %var_printf

  ; every eval-ed expression should be assigned a %exp_# %Val*
  ; every define var should be assigned a %var_name %Val*
  ; every set should store into the %var_name %Val* pointer

  ; (define a 2)
  ; (define b a)
  ; (set! b 3)

  ; (define a 1)
  %ex_1 = call %Val* @make_int_val(i64 1)
  %var_a = bitcast %Val* %ex_1 to %Val*

  ;           
  ; (define b 2)
  %ex_2 = call %Val* @make_int_val(i64 2)
  %var_b = bitcast %Val* %ex_2 to %Val*

  ; (printf "%d\n" (+ a b))
  ; get f to call
  ; get arg vals to call with
  ; call

  %ex3_argp = getelementptr i8* [4 x 18], [4 x 18]* %c1, i64 0, i64 0
  %ex3 = call %Val* @make_string_val(i8* %ex3_argp)

  %l_4rp = call i8* @GC_malloc(i64 256)
  %l_4p = bitcast i8* %l_4rp to %Val**
  %l_4p0p = getelementptr %Val*, %Val** %l_4p, i64 0
  store %Val* %var_a, %Val** %l_4p0p
  %l_4p1p = getelementptr %Val*, %Val** %l_4p, i64 1
  store %Val* %var_b, %Val** %l_4p1p
  %ex_4_args_rp = call i8* @GC_malloc(i64 128)
  %ex_4_args_p = bitcast i8* %ex_4_args_rp to %Vals*
  %lp = getelementptr %Vals, %Vals* %ex_4_args_p, i32 0, i32 0
  store %Val** %l_4p, %Val*** %lp
  %np = getelementptr %Vals, %Vals* %ex_4_args_p, i32 0, i32 1
  store i64 2, i64* %np
  %ex4 = call %Val* @call_f(%Val* %var_plus, %Vals* %ex_4_args_p)

  %args_5 = call %Vals* @make_args(i64 2)
  call void @set_arg(i64 0, %Val* %ex3)
  call void @set_arg(i64 1, %Val* %ex4)

  call %Val* call_f(%Val* %var_printf, %Vals* %args_5)

  ; assemble ex4 args to call printf
  ; make_string_val from constant
  ; use %ex_3
  ; %ex_4 = call_f std_printf

  ; done
  ret i32 0
}

; f is a func val
; extra func pointer and env
; invoke func pointer with env and args
; return result
define %Val* @call_f(%Val* %f, %Vals* %args) {
  ;;; FIXME
  %out = call %Val* @make_int_val(i64 6)
  ret %Val* %out
}

define %Val* @std_plus(%Vals* %env, %Val* %args) {
  ;;; FIXME
  %out = call %Val* @make_int_val(i64 5)
  ret %Val* %out
}

;;; TODO: @std_printf

define %Val* @make_int_val(i64 %x) {
  %r = call i8* @GC_malloc(i64 128)
  %v = bitcast i8* %r to %Val*
  %tp = getelementptr %Val, %Val* %v, i32 0, i32 0
  store i8 2, i8* %tp
  %irp = getelementptr %Val, %Val* %v, i32 0, i32 1
  %ip = bitcast i8** %irp to i64*
  store i64 %x, i64* %ip
  ret %Val* %v
}

define %Val* @make_func_val(%Val*(%Vals*, %Vals*)* %f, %Vals* %env) {
  ;;; FIXME
  %out = call %Val* @make_int_val(i64 7)
  ret %Val* %out
}
