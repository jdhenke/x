%Val = type { i8, i64 } ; type, data
%Env = type { %Val**, %Env* } ; start of list, parent
%Args = type { %Val*, i64 } ; start of list, size

; data field:
; 
; bool is i1
; int is i64
; string is i8*; malloced terminated by \00
; symbol is i8*; is malloced terminated by \00
; list is %List*
%List = type { %Val, %List* } ; car, cdr
; func is %Func*
%Func = type { %Val(%Env, %Args)*, %Env } ; ptr, closure

define %Val foo(%Env parent, %Args args) {
  ;;; PREPARE

  ; create new env, parent
  ; first pass to get defs
  defs = ; list of symbols
  env = make_env(parent, args, defs)

  ;;; EMIT

  ; NOUNS

  ; literals
  make_bool_val()
  make_int_val()
  make_string_val()

  ; symbol
  load lookup(env, depth, offset)

  ; VERBS

  ; define
  ; set!
  store(eX, lookup())

  ; define
  ; lambda
  ; let
  ; let*
  create LLVM func
  create new env
  fv = make_func_val()
  append self to env ;; will this bump depth lookups for lets, args ??
  ; for let
  assemble args
  call_func_val()

  ; if
  ; cond
  ; or
  ; and
  br stuff

  ; func call
  eval f
  eval args
  call_func_val()

  ;;; CODA
  ret %e<last>
}
