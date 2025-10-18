; ModuleID = 'oats_aot'
source_filename = "oats_aot"
target triple = "x86_64-pc-linux-gnu"

declare ptr @malloc(i64)

declare i64 @strlen(ptr)

declare ptr @memcpy(ptr, ptr, i64)

declare void @free(ptr)

declare void @print_f64(double)

declare void @print_f64_no_nl(double)

declare void @print_str(ptr)

declare void @print_str_no_nl(ptr)

declare void @print_newline()

declare ptr @array_to_string(ptr)

declare void @rc_dec_str(ptr)

declare void @sleep_ms(double)

declare ptr @str_concat(ptr, ptr)

define double @oats_main() {
entry:
  %x = alloca i64, align 8
  %y = alloca i64, align 8
  unreachable
}

define i32 @main(i32 %0, ptr %1) {
entry:
  %call_oats_main = call double @oats_main()
  call void @executor_run()
  %f_to_i = fptosi double %call_oats_main to i32
  ret i32 %f_to_i
}

declare void @executor_run()
