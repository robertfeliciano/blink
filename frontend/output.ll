; ModuleID = 'Module'
source_filename = "Module"

define i32 @main(i32 %a) {
main_entry:
  %x = alloca i32, align 4
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  store i32 19, ptr %x, align 4
  %x2 = load i32, ptr %x, align 4
  %gttmp = icmp sgt i32 %x2, 10
  br i1 %gttmp, label %then, label %else3

then:                                             ; preds = %main_entry
  ret i32 5
  br label %finally10

else3:                                            ; preds = %main_entry
  %x4 = load i32, ptr %x, align 4
  %lttmp = icmp slt i32 %x4, 20
  br i1 %lttmp, label %then5, label %else7

then5:                                            ; preds = %else3
  ret i32 6
  br label %finally69

else7:                                            ; preds = %else3
  %x8 = load i32, ptr %x, align 4
  %addtmp = add i32 %x8, 11
  ret i32 %addtmp
  br label %finally69

finally69:                                        ; preds = %else7, %then5
  br label %finally10

finally10:                                        ; preds = %finally69, %then
}
