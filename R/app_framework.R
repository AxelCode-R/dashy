#' subtab
#'
#' @param ... tabs, see \code{\link{tab}}
#' @param name name to display in left-sidebar for the subtab
#' @param startExpanded should it start as expanded?
#' @param css_files css file paths
#'
#' @export
#'
subtab = function(..., name, startExpanded = FALSE, css_files = NULL) {
  list(
    name = name,
    id = create_tab_name_id(name),
    startExpanded = startExpanded,
    subtabs = lapply(
      X = list(...),
      FUN = function(l) {
        if (!is.null(css_files)) {
          l$css_files <- c(l$css_files, css_files)
        }
        return(l)
      }
    )
  )
}

#' tab
#'
#' @param name name to display in left-sidebar for the tab
#' @param R6class R6 class with ns, app_rv and logger as arguments and
#'                public ui() and server() functions
#' @param selected should the tab be selected on init?
#' @param css_files css file paths
#' @param lazy should the tabs ui and server only load if a user selects it or
#'             on start-up?
#'
#' @export
#'
tab = function(name, R6class, selected = NULL, css_files = NULL, lazy = TRUE) {
  list(
    name = name,
    id = create_tab_name_id(name),
    R6class = R6class,
    selected = selected,
    css_files = css_files,
    lazy = lazy
  )
}

#' create_app
#'
#' @param ... tabs or subtabs, see \code{\link{tab}} or \code{\link{subtab}}
#' @param app_title app title to display
#' @param css_files_global css file paths
#' @param js_files js file paths
#'
#' @export
#'
#' @examples \dontrun{
#'   dashy::create_app_example(run = FALSE)
#'
#'   # or
#'   app <- dashy::create_app_example(run = FALSE)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
create_app = function(..., app_title = "", css_files_global = NULL, js_files = NULL) {
  logger <- AppReactiveLogger$new()

  tabs <- list(...)
  tabs_flatten <- NULL
  for(i in 1:length(tabs)) {
    if (!is.null(tabs[[i]]$subtabs)) {
      tabs_flatten <- c(tabs_flatten, tabs[[i]]$subtabs)
    } else {
      tabs_flatten <- c(tabs_flatten, list(tabs[[i]]))
    }
  }

  ui <- shinydashboardPlus::dashboardPage(
    header = shinydashboardPlus::dashboardHeader(
      title = app_title,
      controlbarIcon = shiny::div(
        id = "controlbar_icon",
        class = "icon-unanimated",
        shiny::icon("gears")
      )
    ),
    sidebar = shinydashboardPlus::dashboardSidebar(
      collapsed = FALSE,
      shinydashboard::sidebarMenu(
        id = "sidebar_tabs",
        lapply(
          X = tabs,
          FUN = function(tab) {
            if (!is.null(tab$subtabs)) {
              do.call(
                what = shinydashboard::menuItem,
                args = list(
                  text = tab$name,
                  tabName = tab$id,
                  icon = tab$icon,
                  startExpanded = tab$startExpanded,
                  lapply(
                    X = tab$subtabs,
                    FUN = function(subtab) {
                      do.call(
                        what = shinydashboard::menuSubItem,
                        args = list(
                          text = subtab$name,
                          tabName = subtab$id,
                          icon = subtab$icon
                        )
                      )
                    }
                  )
                )
              )
            } else {
              do.call(
                what = shinydashboard::menuItem,
                args = list(
                  text = tab$name,
                  tabName = tab$id,
                  icon = tab$icon,
                  selected = tab$selected
                )
              )
            }
          }
        )
      )
    ),
    body = shinydashboard::dashboardBody(
      if (!is.null(css_files_global)) {
        shiny::tags$style(shiny::HTML({
          unlist(lapply(css_files_global, readLines))
        }))
      },
      if (!is.null(js_files)) {
        shiny::tags$script(shiny::HTML({
          unlist(lapply(js_files, readLines))
        }))
      },
      do.call(
        what = shinydashboard::tabItems,
        args = lapply(
          X = tabs_flatten,
          FUN = function(tab) {
            shinydashboard::tabItem(
              tabName = tab$id,
              shiny::uiOutput(
                outputId = paste0("tab_ui_", tab$id)
              )
            )
          }
        )
      )
    ),
    controlbar = shinydashboardPlus::dashboardControlbar(
      id = "controlbar",
      overlay = FALSE,
      collapsed = TRUE,
      shinydashboardPlus::controlbarMenu(
        shinydashboardPlus::controlbarItem(
          title = "Logs",
          icon = shiny::icon("envelope"),
          logger$logger_ui()
        )
      )
    )
  )

  server <- function(input, output, session) {
    logger$logger_server(input = input, output = output, session = session)

    app_rvs <- list(
      a = shiny::reactiveVal("a")
    )
    env <- new.env()
    env$loaded_tabs <- NULL

    lapply(
      X = tabs_flatten,
      FUN = function(tab) {
        if (isFALSE(tab$lazy)) {
          tab_load_backend(
            tab = tab,
            app_rvs = app_rvs,
            output = output,
            logger = logger
          )
          env$loaded_tabs <- c(env$loaded_tabs, tab$name)
        }
      }
    )

    shiny::observeEvent(
      eventExpr = input$sidebar_tabs,
      handlerExpr = {
        selected_tab <- input$sidebar_tabs
        if (!selected_tab %in% env$loaded_tabs) {
          tab_load_backend(
            tab = rlist::list.filter(tabs_flatten, id == selected_tab)[[1]],
            app_rvs = app_rvs,
            output = output,
            logger = logger
          )
          env$loaded_tabs <- c(env$loaded_tabs, selected_tab)
        }
      }
    )
  }

  return(list(ui = ui, server = server))
}

tab_load_backend = function(tab, app_rvs, output, logger) {
  obj <- tab$R6class$new(
    ns = shiny::NS(tab$id),
    app_rv = app_rvs,
    logger = logger
  )
  output[[paste0("tab_ui_", tab$id)]] <- shiny::renderUI(
    expr = {
      shiny::div(
        if (!is.null(tab$css_files)) {
          shiny::tags$style(shiny::HTML({
            css <- trimws(unlist(lapply(tab$css_files, readLines)), which = "right")
            add_ns <- grepl("\\{$", css)
            css[add_ns] <- paste0("#tab_ui_", tab$id, " ", css[add_ns])
            css
          }))
        },
        obj$ui()
      )
    }
  )
  shiny::moduleServer(
    id = tab$id,
    module = obj$server
  )
}

create_tab_name_id = function(name) {
  tolower(gsub(" ", "_", name))
}
