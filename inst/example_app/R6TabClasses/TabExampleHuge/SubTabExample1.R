SubTabExample1 <- R6::R6Class(
  public = list(
    initialize = function(ns, app_rv, logger) {
      private$ns <- ns
      private$app_rv <- app_rv
    },
    ui = function() {
      shiny::div(
        shiny::p(paste0("SubTabExample1 ", Sys.time())),
        shiny::h1("h1")
      )
    },

    server = function(input, output, session) {
      # shiny::observeEvent(
      #   ignoreNULL = TRUE,
      #   eventExpr = private$app_rv$tab_selected_counter(),
      #   handlerExpr = {
      #     print("tab1 focus")
      #     private$app_rv$menuItemBadgeLabel("tab1 focus")
      #   }
      # )
    }
  ),

  private = list(
    ns = NULL,
    app_rv = NULL
  )
)
