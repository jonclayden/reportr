#' @import ore
#' @export
OL <- list(Ignore=0L, Debug=1L, Verbose=2L, Info=3L, Warning=4L, Question=5L, Error=6L, Fatal=7L)

.Defaults <- list(reportrOutputLevel=OL$Info,
                  reportrPrefixFormat="%d%L: ",
                  reportrTimeFormat="%Y-%m-%d %H:%M:%S",
                  reportrStderrLevel=OL$Warning,
                  reportrStackTraceLevel=OL$Error,
                  reportrBaseClasses=FALSE,
                  reportrMessageFilterIn=NULL,
                  reportrMessageFilterOut=NULL)

.Workspace <- new.env()

# A unique sentinel, distinguishable from any real value including NULL, used
# to detect whether a default return value was actually supplied
.noDefault <- new.env()
