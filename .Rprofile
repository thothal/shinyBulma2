setHook("rstudio.sessionInit", function(newSession) {
     .pid <- rstudioapi::terminalExecute("grunt watch",
                                          here::here("tools"),
                                          show = TRUE)
     message(glue::glue("grunt started with id <{.pid}>"))
}, action = "append")


