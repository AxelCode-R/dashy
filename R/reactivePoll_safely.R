reactivePoll_safely <- function(
    session = shiny::getDefaultReactiveDomain(),
    logger = NULL,
    intervalMillis = Inf,
    checkFunc = function() {runif(1)},
    valueFunc = function() {runif(1)},
    errorReturn = NULL
  ) {
  shiny::reactivePoll(
    intervalMillis = intervalMillis,
    session = session,
    checkFunc = function() {
      tryCatch({
        checkFunc()
      }, error = function(e) {
        if (!is.null(logger)) {
          logger$log(
            paste0("Error in reactivePoll check: ", e$message),
            type = sample(c("success", "info", "warning", "error"), 1)
          )
        }
        return(NULL)
      })
    },
    valueFunc = function() {
      tryCatch({
        valueFunc()
      }, error = function(e) {
        if (!is.null(logger)) {
          logger$log(
            paste0("Error in reactivePoll value: ", e$message),
            type = sample(c("success", "info", "warning", "error"), 1)
          )
        }
        return(errorReturn)
      })
    }
  )
}

do_safely <- function(expr, logger = NULL, errorReturn = NULL) {
  tryCatch(
    expr = expr,
    error = function(e) {
    if (!is.null(logger)) {
      logger$log(
        paste0("Error in do_safely: ", e$message),
        type = sample(c("success", "info", "warning", "error"), 1)
      )
    }
    return(errorReturn)
  })
}
