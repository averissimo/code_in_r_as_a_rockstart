# mod/another.R
a_variable <- 1

#' @export
log <- function (msg) {
  box::use(glue[glue])
  # We can now use `glue` inside the function:
  message(glue(.sep = " ", "[LOG MESSAGE]", msg, .envir = parent.frame()))
}
