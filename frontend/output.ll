; ModuleID = 'Module'
source_filename = "Module"

define i32 @main(i32 %a) {
main_entry:
  %gttmp = icmp sgt i32 %a, 10
  br i1 %gttmp, label %loop, label %loop_end

loop:                                             ; preds = %loop, %main_entry
  br label %loop

loop_end:                                         ; preds = %main_entry
  ret i32 1
}
