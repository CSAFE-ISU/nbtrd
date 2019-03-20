
url_unreachable <- function(url) {
  opt1 <- try(xml2::read_html(url), silent = T)
  if ("curl" %in% installed.packages()) {
    res <- try(curl::curl_fetch_memory(url, handle = curl::new_handle(failonerror = T)))
    opt2 <- res$status_code == 200
    return("try-error" %in% class(opt1) | "try-error" %in% class(opt2))
  } else {
    return("try-error" %in% class(opt1))
  }
  
}
