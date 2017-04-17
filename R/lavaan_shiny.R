
#' Shiny interface for lavaan
#' 
#' This function calls a shiny interface for lavaan.
#' 
#' @param launch.browser Option will be passed on to \code{\link[shiny]{runApp}}
#' @export
lavaanGUI <- function(launch.browser=TRUE){  
  shiny::runApp(system.file('lavaanGUI', package='lavaanGUI'),
                launch.browser=launch.browser)
}
