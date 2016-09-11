toastr.options.positionClass = "toast-top-center"

if ($("#last-question").data("correct")) {
  toastr.success("答對，下一題!")
}
