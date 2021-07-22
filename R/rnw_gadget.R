#' Run R-NONMEM Workbench
#'
#' Function for rstudio add-in
#' @export
rnw_gadget <- function() {
  appDir <- system.file("rnw", package = "rnw")

  shiny::runApp(
    appDir,
    display.mode = "normal"
  )
}
