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

define double @add(double %0, double %1) {
entry:
  %sum = fadd double %0, %1
  ret double %sum
}

declare void @rc_dec(ptr)

declare void @rc_weak_dec(ptr)

define double @oats_main() {
entry:
  %call_internal = call double @add(double 3.000000e+00, double 5.000000e+00)
  %num = alloca double, align 8
  store double %call_internal, ptr %num, align 8
  %num1 = load double, ptr %num, align 8
  call void @print_f64_no_nl(double %num1)
  call void @print_newline()
  ret double 0.000000e+00
}

define i32 @main(i32 %0, ptr %1) {
entry:
  %call_oats_main = call double @oats_main()
  call void @executor_run()
  %f_to_i = fptosi double %call_oats_main to i32
  ret i32 %f_to_i
}

declare void @executor_run()
