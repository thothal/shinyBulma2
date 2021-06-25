setHook("rstudio.sessionInit", function(newSession) {
  .__terminal_pid__ <<- rstudioapi::terminalCreate("Grunt", FALSE)
  rstudioapi::terminalSend(.__terminal_pid__,
                           as.character(glue::glue("cd \"{here::here('tools')}\" ",
                                                         "&& grunt watch\r\n")))

  message(glue::glue("grunt started with id <{.pid}>"))
}, action = "append")

invisible(reg.finalizer(.GlobalEnv, function(e) {
  if(rstudioapi::isAvailable()) {
    rstudioapi::terminalKill(.__terminal_pid__)
  }
}, TRUE))
