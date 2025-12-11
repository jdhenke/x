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
  store i1 1, i1* %bdc
  call void @print(%Val* %bv)

  ; integer
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

  ; boolean
  %b = getelementptr inbounds %Val, %Val* %v, i32 0, i32 1
  %bc = bitcast i8* %b to i1*
  %bcv = load i1, i1* %bc
  %s = getelementptr inbounds [4 x i8], [4 x i8]* @bool_str, i64 0, i64 0
  call i32 (i8*, ...) @printf(i8* %s, i1 %bcv)

  ; integer
  ; string
  ; symbol
  ; list
  ; function

  ret void
}

define i64 @add5(i64 %x) {
  %sum = add i64 5, %x
  ret i64 %sum
}
