create_error <- function(message, header = "random") {
  showModal(modalDialog(
    footer = modalButton("Got it"),
    title = ifelse(header == "random", sample(messages, 1), header),
    message
  ))
}
