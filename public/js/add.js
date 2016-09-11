function arrangeAnswers() {
  var full = $(".answer").toArray().every(i => i.value != '')
  if (full) {
    var lastAnswer = $(".answer").last()
    var clonedAnswer = lastAnswer.clone()
    clonedAnswer.val("")
    clonedAnswer.on("input", arrangeAnswers)
    clonedAnswer.insertAfter(lastAnswer)
  }
}

$(".answer").on("input", arrangeAnswers)
