#' Monitor variables
#'
#' This function starts the monitoring of a numeric variable in your environment
#' in the RStudio viewer pane or an external browser window. Only one connection
#' is supported.
#'
#' @param ... variables to monitor
#' @param list optional character vector of variable names to monitor
#'
#' @export
monitor <- function(..., list) {
  if (!exists(".mtr_server", envir = .GlobalEnv)) {
    mtr_serve()
  }
  if (!exists(".mtr_vars", envir = .GlobalEnv)) {
    .GlobalEnv[[".mtr_vars"]] <- new.env()
  }
  vars <- as.character(match.call(expand.dots = FALSE)$`...`)
  if (!missing(list)) vars <- c(vars, unlist(list))
  for (v in vars) {
    val <- get(v, envir = parent.frame())
    if (!is.numeric(val)) {
      warning(paste(v, "is not numeric, skipping monitor..."))
      next
    }
    if (val %in% ls(.GlobalEnv[[".mtr_vars"]])) next
    rm(list = v, envir = parent.frame())
    makeActiveBinding(v, fun = bind_monitor(v), parent.frame())
    create_slot(v)
    assign(v, val, parent.frame())
  }
  if (Sys.getenv("RSTUDIO") == "1") {
    rstudioapi::viewer(.GlobalEnv[[".mtr_dir"]][["url"]])
  } else {
    utils::browseURL(.GlobalEnv[[".mtr_dir"]][["url"]])
  }
}

#' Stop monitoring variables
#'
#' This function stops the monitoring of a variable being monitored and removes
#' it from the active browser window or RStudio viewer pane.
#'
#' @param ... variables to monitor
#' @param list optional character vector of variable names to monitor
#'
#' @export
unmonitor <- function(..., list) {
  vars <- as.character(match.call(expand.dots = FALSE)$`...`)
  if (!missing(list)) vars <- c(vars, unlist(list))
  for (v in vars) {
    val <- get(v,  envir = .GlobalEnv[[".mtr_vars"]])
    rm(list = v,   envir = .GlobalEnv[[".mtr_vars"]])
    if (exists(v, parent.frame()) && bindingIsActive(v, parent.frame())) {
      remove_slot(v)
      rm(list = v,   envir = parent.frame())
      assign(v, val, envir = parent.frame())
    }
  }
}

#' Create a function for the active binding
#'
#' @keywords internal
bind_monitor <- function(v) {
  function(x) {
    if (missing(x)) return(.GlobalEnv[[".mtr_vars"]][[local(v)]])
    if (!is.numeric(x)) {
      warning(paste0("Value not numeric, removing monitor"), call. = FALSE)
      rm(list = v,   envir = .GlobalEnv[[".mtr_vars"]])
      rm(list = v,   envir = parent.frame())
      remove_slot(v)
      assign(v, x, envir = parent.frame())
    } else {
      assign(local(v), x, envir = .GlobalEnv[[".mtr_vars"]])
      push_data(local(v), x)
    }
  }
}

#' Serve the websocket and webpage for the monitor.
#'
#' @keywords internal
mtr_serve <- function() {
  if (exists(".mtr_server", envir = .GlobalEnv)) stop("server already running.")
  .GlobalEnv[[".mtr_server"]] <- httpuv::startServer(
    host = "0.0.0.0",
    port = 9234,
    app  = list(
      onWSOpen = function(ws) {
        .GlobalEnv[[".mtr_ws"]] <- ws
        for (v in ls(.GlobalEnv[[".mtr_vars"]])) {
          create_slot(v)
          push_data(v, get(v))
        }
      }
    )
  )
  .GlobalEnv[[".mtr_dir"]] <- servr::httd(
    dir = system.file("www", package = "varmonitor"),
    daemon = TRUE, browser = FALSE, port = 9235,
    interval = Inf, verbose = FALSE
  )
}

#' Stop serving the websocket and webpage for the monitor -- clean up
#'
#' @keywords internal
mtr_stop <- function() {
  suppressWarnings({
    try(.GlobalEnv[[".mtr_server"]]$stop(), silent = TRUE)
    try(.GlobalEnv[[".mtr_ws"]]$close(), silent = TRUE)
    try(rm(list = ".mtr_server", envir = .GlobalEnv), silent = TRUE)
    try(rm(list = ".mtr_ws",     envir = .GlobalEnv), silent = TRUE)
    try(rm(list = ".mtr_dir",    envir = .GlobalEnv), silent = TRUE)
    servr::daemon_stop()
  })
}

#' Send a datapoint for a variable via the websocket
#'
#' @keywords internal
push_data <- function(name, data) {
  li <- list(data)
  names(li) <- name
  msg <- jsonlite::toJSON(li)
  if (!is.null(.GlobalEnv[[".mtr_ws"]]))
    .GlobalEnv[[".mtr_ws"]]$send(enc2utf8(msg))
}

#' Trigger slot creation on the page via the websocket
#'
#' @keywords internal
create_slot <- function(name) {
  li <- list(name)
  names(li) <- ".__create_slot__"
  msg <- jsonlite::toJSON(li)
  if (!is.null(.GlobalEnv[[".mtr_ws"]]))
    .GlobalEnv[[".mtr_ws"]]$send(enc2utf8(msg))
}

#' Trigger slot removal on the page via the websocket
#'
#' @keywords internal
remove_slot <- function(name) {
  li <- list(name)
  names(li) <- ".__remove_slot__"
  msg <- jsonlite::toJSON(li)
  if (!is.null(.GlobalEnv[[".mtr_ws"]]))
    .GlobalEnv[[".mtr_ws"]]$send(enc2utf8(msg))
}
