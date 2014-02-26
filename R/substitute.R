s <- function (strings, round = NULL, signif = NULL, envir = parent.frame())
{
    if (is.null(round) && is.null(signif) && is.list(getOption("reportrNumericRepresentation")))
    {
        round <- getOption("reportrNumericRepresentation")$round
        signif <- getOption("reportrNumericRepresentation")$signif
    }
    
    for (i in seq_along(strings))
    {
        while ((match <- regexpr("(?<!\\\\)\\#\\{[^\\}]+\\}", strings[i], perl=TRUE)) != -1)
        {
            expressionString <- substr(strings[i], match+2, match+attr(match,"match.length")-2)
            value <- eval(parse(text=expressionString), envir=envir)
        
            if (is.double(value))
            {
                if (!is.null(round))
                    value <- round(value, round)
                else if (!is.null(signif))
                    value <- signif(value, signif)
            }
        
            strings[i] <- paste(substr(strings[i],1,match-1), as.character(value)[1], substr(strings[i],match+attr(match,"match.length"),nchar(strings[i])), sep="")
        }
    
        strings[i] <- gsub("\\#", "#", strings[i], fixed=TRUE)
    }
    
    return (strings)
}
