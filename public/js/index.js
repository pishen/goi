toastr.options.positionClass = "toast-top-center"

if ($("#message").length > 0) {
  const text = $("#message").data("text")
  const color = $("#message").data("color")
  if (color == "green") {
    toastr.success(text)
  } else if (color == "red") {
    toastr.error(text)
  }
}
