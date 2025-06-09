SubTabExample2 <- R6::R6Class(
  public = list(
    initialize = function(ns, app_rv, logger) {
      private$ns <- ns
      private$app_rv <- app_rv
    },
    ui = function() {
      shiny::div(
        shiny::p(paste0("SubTabExample2 ", Sys.time()))
      )
    },

    server = function(input, output, session) {
      # shiny::observeEvent(
      #   ignoreNULL = TRUE,
      #   eventExpr = private$app_rv$tab_selected_counter(),
      #   handlerExpr = {
      #     print("tab2 focus")
      #     private$app_rv$menuItemBadgeLabel("tab2 focus")
      #   }
      # )
    }
  ),

  private = list(
    ns = NULL,
    app_rv = NULL
  )
)
