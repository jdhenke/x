%Val = type {i8, i8*}

declare i32 @printf(i8*, ...)

@type_str = unnamed_addr constant [10 x i8] c"Type: %d\0A\00"
@bool_str = unnamed_addr constant [4 x i8] c"%d\0A\00"
@int_str = unnamed_addr constant [4 x i8] c"%d\0A\00"

define i32 @main() {
  ; create and print all types

  ; boolean
  %bv = alloca %Val
  %bt = getelementptr inbounds %Val, %Val* %bv, i32 0, i32 0
  store i8 1, i8* %bt
  %bd = getelementptr inbounds %Val, %Val* %bv, i32 0, i32 1
  %bdc = bitcast i8* %bd to i1*
  store i1 1, i1* %bdc
  call void @print(%Val* %bv)
  store i1 0, i1* %bdc
  call void @print(%Val* %bv)

  ; integer
  %iv = alloca %Val
  %ivt = getelementptr inbounds %Val, %Val* %iv, i32 0, i32 0
  store i8 2, i8* %ivt
  %ivd = getelementptr inbounds %Val, %Val* %iv, i32 0, i32 1
  %ivdc = bitcast i8* %ivd to i64*
  store i64 42, i64* %ivdc
  call void @print(%Val* %iv)
  store i64 43, i64* %ivdc
  call void @print(%Val* %iv)

  ; string
  ; symbol
  ; list
  ; function
  ret i32 42
}

define void @print(%Val* %v) {
  ; print type
  %st = getelementptr inbounds [10 x i8], [10 x i8]* @type_str, i64 0, i64 0
  %vtp = getelementptr inbounds %Val, %Val* %v, i32 0, i32 0
  %vtv = load i8, i8* %vtp
  call i32 (i8*, ...) @printf(i8* %st, i8 %vtv)

  %cmp1 = icmp eq i8 1, %vtv
  br i1 %cmp1, label %case_bool, label %check2

check2:
  %cmp2 = icmp eq i8 2, %vtv
  br i1 %cmp2, label %case_int, label %check3

check3:
  ret void
  
; boolean
case_bool:
  %b = getelementptr inbounds %Val, %Val* %v, i32 0, i32 1
  %bc = bitcast i8* %b to i1*
  %bcv = load i1, i1* %bc
  %bs = getelementptr inbounds [4 x i8], [4 x i8]* @bool_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %bs, i1 %bcv)

  ret void


; integer
case_int:
  %ip = getelementptr inbounds %Val, %Val* %v, i32 0, i32 1
  %ipc = bitcast i8* %ip to i64*
  %id = load i64, i64* %ipc
  %is = getelementptr inbounds [4 x i8], [4 x i8]* @int_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %is, i64 %id)
  ret void

; string
; symbol
; list
; function
}

define i64 @add5(i64 %x) {
  %sum = add i64 5, %x
  ret i64 %sum
}
