#' @import ore
#' @export
OL <- list(Debug=1, Verbose=2, Info=3, Warning=4, Question=5, Error=6, Fatal=7)

.Defaults <- list(reportrOutputLevel=OL$Info,
                  reportrPrefixFormat="%d%L: ",
                  reportrStderrLevel=OL$Warning,
                  reportrStackTraceLevel=OL$Error,
                  reportrMessageFilterIn=NULL,
                  reportrMessageFilterOut=NULL,
                  reportrStackFilterIn=NULL,
                  reportrStackFilterOut=NULL)

.Workspace <- new.env()
