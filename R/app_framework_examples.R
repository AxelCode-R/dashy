
#' create_app_example
#'
#' @param run boolean if the shiny app should run or
#'            app with ui and server gets returned
#'
#' @return app or NULL
#' @export
#'
#' @examples \dontrun{
#' example_create_app()
#' }
create_app_example = function(run = TRUE) {
  R.utils::sourceDirectory(path = system.file("create_app_example1", package = "dashy"), modifiedOnly = FALSE)

  app <- dashy::create_app(
    app_title = "Example App",
    css_files_global = system.file("create_app_example1/www/styles_global.css", package = "dashy"),
    js_files = NULL,
    dashy::tab(
      name = "Home",
      selected = TRUE,
      R6class = TabExampleStartTab,
      css_files = NULL,
      lazy = FALSE
    ),
    dashy::tab(
      name = "Example Small",
      R6class = TabExampleSmall,
      css_files = c(
        system.file("create_app_example1/www/styles.css", package = "dashy"),
        system.file("create_app_example1/www/styles2.css", package = "dashy")
      ),
      lazy = FALSE
    ),
    dashy::subtab(
      name = "Example Subtabs",
      startExpanded = TRUE,
      css_files = NULL,
      dashy::tab(
        name = "Subtab1",
        R6class = SubTabExample1,
        css_files = NULL,
        lazy = TRUE
      ),
      dashy::tab(
        name = "Subtab2",
        R6class = SubTabExample2,
        css_files = NULL,
        lazy = TRUE
      )
    )
  )

  if (run == TRUE) {
    return(shiny::shinyApp(ui = app$ui, server = app$server))
  }
  return(app)
}
