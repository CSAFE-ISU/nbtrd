
url_unreachable <- function(url) {
  opt1 <- try(xml2::read_html(url), silent = T)
  if ("RCurl" %in% installed.packages()) {
    opt2 <- RCurl::url.exists(url)
    
    return("try-error" %in% class(opt1) | !opt2)
  } else {
    return("try-error" %in% class(opt1))
  }
  
}
