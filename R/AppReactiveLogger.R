AppReactiveLogger <- R6::R6Class(
  public = list(
    initialize = function() {
      private$rv_msgs <- shiny::reactiveVal(NULL)
      private$msg_type_config <- list(
        success = list(color = "green", icon = shiny::icon("circle-check")),
        info = list(color = "skyblue", icon = shiny::icon("comment")),
        warning = list(color = "orange", icon = shiny::icon("circle-exclamation")),
        error = list(color = "red", icon = shiny::icon("triangle-exclamation"))
      )
    },

    logger_ui = function() {
      shiny::div(
        id = "logger_container",
        shiny::tags$script(system.file("logger_script.js", package = "dashy")),
        shiny::tags$style(shiny::HTML(
          readLines(system.file("logger_style.css", package = "dashy"))
        ))
      )
    },

    logger_server = function(input, output, session) {
      shiny::observeEvent(
        eventExpr = private$rv_msgs(),
        handlerExpr = {
          last_msg <- private$rv_msgs()[[1]]
          msg_config <- private$msg_type_config[last_msg$type][[1]]
          style <- NULL
          icon <- NULL
          if (!is.null(msg_config)) {
            style <- glue::glue(
              "border-left: 3px solid {msg_config$color};
              border-top: 1px solid {msg_config$color};"
            )
            icon <- msg_config$icon
          }

          shiny::insertUI(
            session = session,
            selector = "#logger_container",
            where = "beforeEnd",
            ui = {
              shiny::div(
                class = "logger_element",
                style = style,
                shiny::div(icon, last_msg$time, class = "logger_element_title"),
                shiny::div(last_msg$msg, class = "logger_element_text")
              )
            }
          )
        }
      )
    },

    log = function(msg, type = "info") {
      private$rv_msgs(
        c(
          list(list(time = format(Sys.time(), format = "%H:%M:%S"), type = type, msg = msg)),
          private$rv_msgs()
        )
      )
    }
  ),
  private = list(
    rv_msgs = NULL,
    msg_type_config = NULL
  )
)
