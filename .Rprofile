setHook("rstudio.sessionInit", function(newSession) {
     .pid <- rstudioapi::terminalExecute("grunt watch",
                                          here::here("tools"),
                                          show = FALSE)
     message(glue::glue("grunt started with id <{.pid}>"))
}, action = "append")


