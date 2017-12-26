# 1.

message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}

message2error(stop("!"))
#  Error in withCallingHandlers(code, message = function(e) stop(e)) : ! 
traceback()

message2error <- function(code) {
  tryCatch(code, message = function(e) stop(e))
}

message2error(stop("!"))
#  Error in doTryCatch(return(expr), name, parentenv, handler) : ! 
traceback()

# The error messages is called from the message argument but you can't see that
# from the tryCatch. Also, tryCatch goes deeper into the function call which produced
# the error whereas withCallingHandlers just presents the call from withCallingHandlers

# 26/12/2017
# tryCatch gives the traceback from it's context, only showing the traceback THROUGH tryCatch
# withCallingHandlers gives the traceback globally, following each function call, going deeper.
