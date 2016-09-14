$(".answer").on("input", () => {
  if ($(".answer").toArray().every(i => i.value != "")) {
    let newAnswer = $(".answer").last().clone(true)
    newAnswer.val("")
    newAnswer.insertBefore(".actions")
  }
})

let questionInputTime = new Date().getTime()
let awesomplete = new Awesomplete($(".question").get(0), {
  minChars: 1
})
$(".question").on("input", () => {
  let newQuestionInputTime = new Date().getTime()
  questionInputTime = newQuestionInputTime
  if ($(".question").val() != "") {
    $.getJSON("/suggestions", {
      q: $(".question").val()
    }, suggestions => {
      if (questionInputTime == newQuestionInputTime) {
        awesomplete.list = suggestions
        awesomplete.evaluate()
      }
    })
  }
})

let questionStr = ""
function fillAnswers() {
  let newQuestionStr = $(".question").val()
  if (newQuestionStr != "" && questionStr != newQuestionStr) {
    questionStr = newQuestionStr
    $.getJSON("/answers", {
      q: newQuestionStr
    }, answers => {
      if (questionStr == newQuestionStr && answers.length > 0) {
        let ansInputs = answers.concat("").map(ans => {
          let ansInput = $(".answer").last().clone(true)
          ansInput.val(ans)
          return ansInput
        })
        $(".answer").remove()
        ansInputs.forEach(jq => jq.insertBefore(".actions"))
      }
    })
  }
}

$(".question").on("awesomplete-selectcomplete", fillAnswers)
$(".question").change(fillAnswers)

toastr.options.positionClass = "toast-top-center"
$(".vocabs").submit((event) => {
  if ($(".answer").toArray().every(e => e.value == "")) {
    console.log(event)
    toastr.error("please fill the answers")
    event.preventDefault()
  }
})

$(".delete-btn").click(() => {
  $(".action").val("delete")
  $(".vocabs").off().submit()
})
