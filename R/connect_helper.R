#' Set up connection to NBTRD
#'
#' This function connects to a pre-existing Docker firefox container running
#' on localhost:4445, and if none is found, it uses the RSelenium client option,
#' (which is deprecated) to connect.
#'
#' @param selenium_port integer port number of docker container
#' @param strict error if RSelenium connection does not succeed?
#' @return RSelenium remoteDriver object, if connection was successful, NULL otherwise.
#' @export
setup_NBTRD <- setup_NRBTD <- function(selenium_port = 4445L, strict = T) {

  # Docker option
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost", port = selenium_port,
    browserName = "firefox"
  )
  remDr$open()
  remDr$navigate(url = "https://tsapps.nist.gov/NRBTD/")

# 
#   if (!remDr$getStatus()$ready) {
#     # Client option
#     rD <- RSelenium::rsDriver(browser = "firefox")
#     remDr <- rD[["client"]]
#   }


  # Establish a wait for an element
  # remDr$setImplicitWaitTimeout(1000)

  if (length(remDr$getSession()) > 0) {
    return(remDr)
  } else {
    if (strict) {
      stop("Could not connect to Selenium server via Docker or Client")
    } else {
      warning("Could not connect to Selenium server via Docker or Client")
    }

    return(NULL)
  }
}
