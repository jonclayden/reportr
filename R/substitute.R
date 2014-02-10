s <- function (string, round = NULL, signif = NULL, envir = parent.frame())
{
    if (is.null(round) && is.null(signif) && is.list(getOption("reportrNumericRepresentation")))
    {
        round <- getOption("reportrNumericRepresentation")$round
        signif <- getOption("reportrNumericRepresentation")$signif
    }
    
    while ((match <- regexpr("(?<!\\\\)\\#\\{[^\\}]+\\}", string, perl=TRUE)) != -1)
    {
        expressionString <- substr(string, match+2, match+attr(match,"match.length")-2)
        value <- eval(parse(text=expressionString), envir=envir)
        
        if (is.double(value))
        {
            if (!is.null(round))
                value <- round(value, round)
            else if (!is.null(signif))
                value <- signif(value, signif)
        }
        
        string <- paste(substr(string,1,match-1), as.character(value)[1], substr(string,match+attr(match,"match.length"),nchar(string)), sep="")
    }
    
    string <- gsub("\\#", "#", string, fixed=TRUE)
    
    return (string)
}
