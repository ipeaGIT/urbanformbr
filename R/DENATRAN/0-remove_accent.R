rm_accent <- function(str,pattern="all") { #str = tmp_muni$name_muni
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")

  message("1")
  # opcao retirar todos
  if(any(c("all","al","a","todos","t","to","tod","todo") %in% pattern)){
    message("2")
    output <- chartr(paste(symbols, collapse="")
                     , paste(nudeSymbols, collapse=""), str)
    return(output)
  }
  for(i in which(accentTypes%in%pattern)){
    str <- chartr(symbols[i],nudeSymbols[i], str)
  }
  return(str)
}
