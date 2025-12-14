%Val = type { i8, i8* } ; type, data
%Env = type { %Val**, %Env* } ; start of list, parent
%Args = type { %Val*, i64 } ; start of list, size

@int_str = unnamed_addr constant [4 x i8] c"%d\0A\00"
@str_str = unnamed_addr constant [6 x i8] c"\22%s\22\0A\00"
declare i32 @printf(i8*, ...)

define void @print(%Val %v) {
  %is = getelementptr inbounds [4 x i8], [4 x i8]* @int_str, i64 0, i64 0
  %ss = getelementptr inbounds [6 x i8], [6 x i8]* @str_str, i64 0, i64 0
  %type = extractvalue %Val %v, 0
  %x = extractvalue %Val %v, 1

  switch i8 %type, label %default [
    i8 1, label %is_bool
    i8 2, label %is_int
    i8 3, label %is_str
  ]

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

default:
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

