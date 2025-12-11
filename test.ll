%Val = type {i8, i8*}

declare i32 @printf(i8*, ...)
declare i8* @malloc(i64)

@type_str = unnamed_addr constant [10 x i8] c"Type: %d\0A\00"
@bool_str = unnamed_addr constant [4 x i8] c"%d\0A\00"
@int_str = unnamed_addr constant [4 x i8] c"%d\0A\00"
@str_str = unnamed_addr constant [6 x i8] c"\22%s\22\0A\00"
@sym_str = unnamed_addr constant [4 x i8] c"%s\0A\00"

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
  %sv = alloca %Val
  %svt = getelementptr inbounds %Val, %Val* %sv, i32 0, i32 0
  store i8 3, i8* %svt
  %svdp = getelementptr inbounds %Val, %Val* %sv, i32 0, i32 1
  %ptr = call i8* @malloc(i64 4)
  %p0 = getelementptr i8, i8* %ptr, i64 0
  %p1 = getelementptr i8, i8* %ptr, i64 1
  %p2 = getelementptr i8, i8* %ptr, i64 2
  %p3 = getelementptr i8, i8* %ptr, i64 3
  store i8 74, i8* %p0
  store i8 111, i8* %p1
  store i8 101, i8* %p2
  store i8 0, i8* %p3
  store i8* %ptr, i8** %svdp
  call void @print(%Val* %sv)

  ; symbol
  %yv = alloca %Val
  %yvtp = getelementptr inbounds %Val, %Val* %yv, i32 0, i32 0
  store i8 4, i8* %yvtp
  %yvdp = getelementptr inbounds %Val, %Val* %yv, i32 0, i32 1
  %yptr = call i8* @malloc(i64 4)
  %yp0 = getelementptr i8, i8* %yptr, i64 0
  %yp1 = getelementptr i8, i8* %yptr, i64 1
  %yp2 = getelementptr i8, i8* %yptr, i64 2
  %yp3 = getelementptr i8, i8* %yptr, i64 3
  store i8 74, i8* %yp0
  store i8 111, i8* %yp1
  store i8 101, i8* %yp2
  store i8 0, i8* %yp3
  store i8* %yptr, i8** %yvdp
  call void @print(%Val* %yv)

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
  %cmp3 = icmp eq i8 3, %vtv
  br i1 %cmp3, label %case_str, label %check4

check4:
  %cmp4 = icmp eq i8 4, %vtv
  br i1 %cmp4, label %case_sym, label %check5

check5:
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
case_str:
  %spp = getelementptr inbounds %Val, %Val* %v, i32 0, i32 1
  %sp = load i8*, i8** %spp
  %ss = getelementptr inbounds [6 x i8], [6 x i8]* @str_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %ss, i8* %sp)
  ret void

; symbol
case_sym:
  %ypp = getelementptr inbounds %Val, %Val* %v, i32 0, i32 1
  %yp = load i8*, i8** %ypp
  %ys = getelementptr inbounds [4 x i8], [4 x i8]* @sym_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %ys, i8* %yp)
  ret void

; list
; function
}

define i64 @add5(i64 %x) {
  %sum = add i64 5, %x
  ret i64 %sum
}
