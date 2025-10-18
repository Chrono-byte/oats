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

define double @fibonacci(double %0) {
entry:
  %cmp = fcmp ole double %0, 1.000000e+00
  %i2f = sitofp i1 %cmp to double
  %neq0 = fcmp one double %i2f, 0.000000e+00
  %not_nan = fcmp oeq double %i2f, %i2f
  %num_truth = and i1 %neq0, %not_nan
  br i1 %num_truth, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  ret double %0

if.else:                                          ; preds = %entry
  br label %if.merge

if.merge:                                         ; preds = %if.else
  %sub = fsub double %0, 1.000000e+00
  %call_internal = call double @fibonacci(double %sub)
  %sub1 = fsub double %0, 2.000000e+00
  %call_internal2 = call double @fibonacci(double %sub1)
  %sum = fadd double %call_internal, %call_internal2
  ret double %sum
}

declare void @rc_dec(ptr)

declare void @rc_weak_dec(ptr)

define double @oats_main() {
entry:
  %i = alloca double, align 8
  store double 1.000000e+00, ptr %i, align 8
  br label %for.cond

for.cond:                                         ; preds = %for.incr, %entry
  %i1 = load double, ptr %i, align 8
  %cmp = fcmp ole double %i1, 2.000000e+01
  %i2f = sitofp i1 %cmp to double
  %neq0 = fcmp one double %i2f, 0.000000e+00
  %not_nan = fcmp oeq double %i2f, %i2f
  %num_truth = and i1 %neq0, %not_nan
  br i1 %num_truth, label %for.body, label %for.after

for.body:                                         ; preds = %for.cond
  %i2 = load double, ptr %i, align 8
  %call_internal = call double @fibonacci(double %i2)
  %fib_i = alloca double, align 8
  store double %call_internal, ptr %fib_i, align 8
  %fib_i3 = load double, ptr %fib_i, align 8
  call void @print_f64(double %fib_i3)
  br label %for.incr

for.incr:                                         ; preds = %for.body
  %i4 = load double, ptr %i, align 8
  %sum = fadd double %i4, 1.000000e+00
  store double %sum, ptr %i, align 8
  br label %for.cond

for.after:                                        ; preds = %for.cond
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
