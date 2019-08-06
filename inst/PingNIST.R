#!/usr/bin/Rscript
library(tidyverse)
library(RSelenium)
library(nbtrd)
library(rvest)
library(XML)


if (!exists("nbtrd_user")) nbtrd_user <- "srvander"
if (!exists("nbtrd_pwd")) nbtrd_pwd <- keyringr::decrypt_gk_pw("db csafe user srvander")

cron_ran <- httr::GET("https://hc-ping.com/b974ffc9-ff32-432d-a454-fbf76b278de8")

remDr <- setup_NBTRD()

if (str_detect(remDr$getCurrentUrl()[[1]], "nist.gov")) {
  login <- nbtrd_login(remDr, nbtrd_user, nbtrd_pwd)
  if (login) {
    res <- httr::GET("https://hc-ping.com/3d34aa94-1333-48b4-a935-d65e30efc27c")
  if (!res$status_code == 200) warning("hc-ping not updated")
  } else {
    warning("login failed")
  }
}

remDr$close()

