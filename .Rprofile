setHook("rstudio.sessionInit", function(newSession) {
  .pid <<- rstudioapi::terminalCreate("Grunt", FALSE)
  rstudioapi::terminalSend(.pid, as.character(glue::glue("cd \"{here::here('tools')}\" ",
                                                         "&& grunt watch\r\n")))

  message(glue::glue("grunt started with id <{.pid}>"))
}, action = "append")

invisible(reg.finalizer(.GlobalEnv, function(e) rstudioapi::terminalKill(.pid), TRUE))
