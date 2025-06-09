TabExampleSmall <- R6::R6Class(
  inherit = DataExampleSmall,
  public = list(
    initialize = function(ns, app_rv, logger) {
      #super$initialize()
      private$ns <- ns
      private$app_rv <- app_rv
      private$logger <- logger
    },

    ui = function() {
      shiny::div(
        shiny::p(paste0("hello example small   ", Sys.time())),
        shiny::h1("h1"),
        private$plot_ui()
      )
    },

    server = function(input, output, session) {
      private$plot_server(input, output, session)
      # private$dynamic_sidebar_server(input, output, session)
    }
  ),

  private = list(
    logger = NULL,
    app_rv = NULL,
    ns = NULL,
    timer = NULL,

    dynamic_sidebar_server = function(input, output, session) {
      shiny::observe({
        private$app_rv$menuItemBadgeLabel(
          paste0("time: ", private$timer())
        )
      })
    },

    plot_ui = function() {
      shiny::plotOutput(private$ns("plot"))
    },

    plot_server = function(input, output, session) {
      private$timer <- reactivePoll_safely(
        logger = private$logger,
        intervalMillis = 2*10^3,
        checkFunc = function() {
          if (runif(1)>0.8) {
            stop(stringi::stri_rand_lipsum(n_paragraphs = 1))
          }
          Sys.time()
        },
        valueFunc = function() {
          if (runif(1)>0.8) {
            stop(stringi::stri_rand_lipsum(n_paragraphs = 1))
          }
          format(Sys.time(), format = "%M:%S")
        },
        errorReturn = "ERROR"
      )
      output$plot <- shiny::renderPlot({
        trigger <- private$timer()
        plot(x = 1:100, y = runif(100))
      })
    }
  )
)
