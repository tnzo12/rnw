#' Run R-NONMEM Workbench
#'
#' This function initiates the workbench on viewer in rstudio
#' @export
run <- function() {
  appDir <- system.file("rnw", package = "rnw")

  shiny::runApp(
    appDir,
    display.mode = "normal"
  )
}
