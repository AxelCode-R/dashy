TabExampleStartTab <- R6::R6Class(
  public = list(
    initialize = function(ns, app_rv, logger) {
      private$ns <- ns
      private$app_rv <- app_rv
    },
    ui = function() {
      shiny::div(
        shiny::img(src = system.file("create_app_example1/www/ai_picture.jpg", package = "dashy"), height = "300px", width = "400px")
      )
    },

    server = function(input, output, session) {}
  ),

  private = list(
    ns = NULL,
    app_rv = NULL
  )
)
