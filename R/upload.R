

# --- Helper functions for Selenium ----

#' Log in to NBTRD page
#' 
#' @param rd a remoteDriver object that is at the NBTRD homepage
#' @param user username
#' @param password password
#' @export
#' @import RSelenium
nbtrd_login <- function(rd, user, password) {
  
  test <- suppressMessages(try(remDr$findElement(using = "css selector", value = "#loginLink")$clickElement()))
  
  if ("try-error" %in% class(test)) {
    stop("Could not find login link element")
  }
  
  Sys.sleep(5)
  
  rd$
    findElement(using = "css selector", value = "#UserName")$
    sendKeysToElement(list(user))
  
  rd$
    findElement(using = "css selector", value = "#Password")$
    sendKeysToElement(list(password))
  
  rd$
    findElement(using = "css selector", 
                value = "#bodycontainer > div.container > div > div > div > div.panel-body > form > fieldset > input")$
    clickElement()
  
  Sys.sleep(5)
  
  res <- rd$findElements(using = "css selector",
                         value = "#logoutForm")
  
  return(length(res) > 0)
}

#' fill in a text field
#' 
#' @param rd a remoteDriver object
#' @param css_sel css selector for the text entry box
#' @param value value to fill in (character)
#' @param clear should the text box be cleared before entering the new value?
#' @import RSelenium
fill_text_field <- function(rd, css_sel, value, clear = F) {
  if (clear) {
    rd$findElement(using = "css selector", value = css_sel)$
      clearElement()
  }
  
  rd$findElement(using = "css selector", value = css_sel)$
    # clickElement()$
    sendKeysToElement(list(value))
}

#' fill in a numeric field
#' 
#' @param rd a remoteDriver object
#' @param css_sel css selector for the text entry box
#' @param value value to fill in (numeric)
#' @param clear should the text box be cleared before entering the new value?
#' @import RSelenium
fill_num_field <- function(rd, css_sel, value, clear = F) {
  if (clear) {
    rd$findElement(using = "css selector", value = css_sel)$
      clearElement()
  }
  
  rd$findElement(using = "css selector", value = css_sel)$
    setElementAttribute("value", value)
}

#' fill in a dropdown field
#' 
#' Function will check to make sure that the value specified is available (matches the beginning of an option)
#' and will issue a warning if this is not the case.
#' @param rd a remoteDriver object
#' @param css_sel css selector for the entry box
#' @param value value to fill in
#' @import RSelenium
fill_dropdown_field <- function(rd, css_sel, value) {
  value_options <- lapply(
    rd$findElement(using = "css selector", value = css_sel)$findChildElements(using = "xpath", value = "option"),
    function(x) x$getElementText()
  ) %>%
    unlist()
  
  if (sum(str_detect(value_options, paste0("^",value) )) == 0) {
    warning("specified value not found at beginning of any of the dropdown options")
  }
  
  rd$findElement(using = "css selector", value = css_sel)$
    findChildElement(using = "xpath", value = sprintf("option[starts-with(text(), '%s')]", as.character(value)))$
    clickElement()
}

#' fill in a logical dropdown field
#' 
#' Sends only the first character of value (T/F)
#' @param rd a remoteDriver object
#' @param css_sel css selector for the entry box
#' @param value value to fill in
#' @import RSelenium
fill_logical_field <- function(rd, css_sel, value) {
  rd$findElement(using = "css selector", value = css_sel)$
    sendKeysToElement(list(str_sub(as.character(value), 1, 1)))
}

#' fill in a checkbox
#' 
#' If value is true, check the box, otherwise, nothing
#' @param rd a remoteDriver object
#' @param css_sel css selector for the entry box
#' @param value value to fill in
#' @import RSelenium
fill_checkbox <- function(rd, css_sel, value) {
  if (as.logical(value)) {
    rd$findElement(using = "css selector", value = css_sel)$clickElement()
  }
}

#' Fill in a dropdown field that has an "other" option text box
#' 
#' @param rd a remoteDriver object
#' @param css_sel_init css selector for the dropdown box
#' @param css_sel_other css selector for the "other" text box entry
#' @param value_init value for the dropdown box
#' @param value_other value for the text box (NA if dropdown value doesn't trigger the text box)
#' @import RSelenium
fill_conditional_field <- function(rd, css_sel_init, css_sel_other, value_init, value_other) {
  if (is.na(value_other)) {
    fill_dropdown_field(rd, css_sel_init, value_init)
  } else {
    stopifnot(value_init == "Other")
    fill_dropdown_field(rd, css_sel_init, value_init)
    fill_text_field(remDr, css_sel_other, value = value_other)
  }
}

#' Suppress all messages and errors when running an expression
#' 
#' @param expr Expression
quiet_try <- function(expr) {
  suppressMessages(try(expr, silent = T))
}

#' Check that a link exists
#' 
#' Returns the link url if the link exists and NA otherwise
#' @param rd a remoteDriver object
#' @param xpath_sel xpath selector for the object
#' @param thing_type character describing the link object, e.g. "bullet" - used in message passing
#' @param quiet suppress messaging
#' @import RSelenium
check_link_exists <- function(rd, xpath_sel, thing_type = "", quiet = F) {
  # See if thing already exists
  tmp <- quiet_try(remDr$findElement("xpath", value = xpath_sel)$getElementAttribute("href") %>% unlist())
  
  if (!"try-error" %in% class(tmp)) {
    if (!quiet) message(sprintf("%s already exists", thing_type))
    return(tmp)
  }
  
  return(NA)
}

#' Tries to run an expression a certain number of times
#' 
#' This function executes test_expr to determine whether to enter a while loop.
#' In the while loop, the control flow is as follows: sleep(sleep_before), 
#' thing_expr(), sleep(sleep_after).
#' 
#' The while loop will exit if test_fcn(test_expr) is false or if the number of 
#' iterations exceeds n_lim.
#' 
#' @param test_expr a parameterless function that will be executed to determine 
#'          while loop control flow
#' @param test_fcn a function to evaluate the value of test_expr, with one 
#'          parameter
#' @param thing_expr the main task that must be completed by the end of the loop 
#'          (no parameters)
#' @param n_lim maximum number of iterations
#' @param warning_text warning to print if iteration limit is reached
#' @param sleep_before time to sleep before evaluating thing_expr
#' @param sleep_after time to sleep after evaluating thing_expr and before 
#'          evaluating test_expr at the end of the while loop
#' @import RSelenium
loop_test <- function(test_expr, test_fcn, thing_expr, n_lim = 15, 
                      warning_text = "", sleep_before = 0, sleep_after = 0) {
  tmp <- test_expr()
  n <- 0
  while(test_fcn(tmp) & n < n_lim) {
    n <- n + 1
    Sys.sleep(sleep_before)
    thing_expr()
    Sys.sleep(sleep_after)
    tmp <- test_expr()
  }
  
  if (n == n_lim){
    if (warning_text != "") {
      warning(warning_text)
    }
    return(FALSE)
  } 
  
  return(TRUE)
}

#' A loop test to open a modal for data upload
#' 
#' @param rd a remote driver
#' @param modal_css_sel css selector for the modal (usually ".modal-open")
#' @param btn_css_sel css selector for the button to open the modal
#' @export
#' @import RSelenium
open_modal <- function(rd, modal_css_sel, btn_css_sel) {
  return(
    loop_test(
      function() quiet_try(rd$findElement("css selector", 
                                          value = modal_css_sel)$elementId),
      function(x) "try-error" %in% class(x),
      function() rd$findElement(using = "css selector", 
                                value = btn_css_sel)$clickElement(),
      warning_text = "open_modal failed",
      sleep_after = 2
    )
  )
}

#' A loop test to close a modal and submit the data
#' 
#' @param rd a remote driver
#' @param modal_css_sel css selector for the modal (usually ".modal-open")
#' @param btn_css_sel css selector for the button to close the modal/save the 
#'          data
#' @export
#' @import RSelenium
submit_modal <- function(rd, modal_css_sel, btn_css_sel) {
  return(
    loop_test(
      function() quiet_try(rd$findElement("css selector", 
                                          value = modal_css_sel)$elementId),
      function(x) !("try-error" %in% class(x)),
      function() remDr$findElement(using = "css selector", 
                                   value = btn_css_sel)$clickElement(),
      warning_text = "submit_modal failed",
      sleep_after = 2
    )
  )
}

#' Get text for a link
#' 
#' @param rd a remote driver
#' @param css_sel CSS selector for link element
#' @import RSelenium
find_text <- function(rd, css_sel) {
  quiet_try(rd$findElement("css selector", value = css_sel)$getElementText() %>% 
              unlist())
}

#' Function to create a new firearm
#' 
#' @param rd a remote driver
#' @param df a single-row data frame with the necessary information (name, model, 
#'           comments, brand, brand_other, caliber, caliber_other, consec_manufacture, 
#'           cartridges, bullets, breech_face_class, breech_face_other, firing_pin_class,
#'           firing_pin_other, n_lands, twist_direction)
#' @param seturl URL to go to before creating the barrel (defaults to current URL)
#' @export
#' @import RSelenium
create_firearms <- function(rd, df, seturl = rd$getCurrentUrl()){
  
  stopifnot("name" %in% names(df))
  stopifnot("model" %in% names(df))
  stopifnot("comments" %in% names(df))
  stopifnot("brand" %in% names(df))
  stopifnot("brand_other" %in% names(df))
  stopifnot("caliber" %in% names(df))
  stopifnot("caliber_other" %in% names(df))
  stopifnot("consec_manufacture" %in% names(df))
  stopifnot("cartridges" %in% names(df))
  stopifnot("bullets" %in% names(df))
  stopifnot("breech_face_class" %in% names(df))
  stopifnot("breech_face_other" %in% names(df))
  stopifnot("firing_pin_class" %in% names(df))
  stopifnot("firing_pin_other" %in% names(df))
  stopifnot("n_lands" %in% names(df))
  stopifnot("twist_direction" %in% names(df))
  
  stopifnot(df$brand %in% dropdown_options$barrel_brand_options)
  stopifnot(df$caliber %in% dropdown_options$caliber_options)
  
  
  rd$navigate(seturl)
  
  tmp <- check_link_exists(rd, sprintf("//a[text()=\"%s\"]", df$name), "Barrel")
  
  if (!is.na(tmp)) {
    return(tmp)
  }
  
  try({
    open_modal(rd, ".modal-open", "#btnAddNewFirearm")
    
    # Enter text values into fields
    fill_text_field(rd, "#FirearmName", as.character(df$name), clear = T)
    fill_text_field(rd, "#Model", df$model, clear = T)
    fill_text_field(rd, "#Comment", df$comments, clear = T)
    
    # This will match the closest (alphabetical) brand to df$brand's value
    fill_conditional_field(rd, "#BrandID", "#BrandOther", df$brand, df$brand_other)
    
    # This will match the closest (alphabetical) caliber to df$caliber's value
    fill_conditional_field(rd, "#CaliberID", "#CaliberOther", df$caliber, df$caliber_other)
    
    stopifnot(as.character(as.logical(df$consec_manufacture)) %in% c("TRUE", "FALSE"))
    fill_logical_field(rd, "#IsConsecutive", df$consec_manufacture)
    
    fill_checkbox(rd, "#HasCartridge", df$cartridges)
    fill_checkbox(rd, "#HasBullet", df$bullets)
    
    if (df$cartridges) {
      stopifnot(df$breech_face_class %in% dropdown_options$breech_face_options)
      fill_conditional_field(rd, "#BreechFaceClassID", "#OtherBreechFace", df$breech_face_class, df$breech_face_other)
      fill_conditional_field(rd, "#FiringPinClassID", "#OtherFiringPin", df$firing_pin_class, df$firing_pin_other)
    }
    
    if (df$bullets) {
      stopifnot(df$df$n_lands %in% dropdown_options$n_lands_options)
      stopifnot(df$twist_direction %in% dropdown_options$twist_options)
      fill_dropdown_field(rd, "#NumberOfLandsID", df$n_lands)
      fill_dropdown_field(rd, "#TwistDirectionID", df$twist_direction)
    }
    
    submit_modal(rd, ".modal-open", "input.btn:nth-child(2)")
  })
  
  rd$refresh()
  
  # See if barrel already exists
  return(check_link_exists(rd, sprintf("//a[text()=\"%s\"]", df$name), "Barrel"))
}


#' Function to create a new bullet in a specific barrel
#' 
#' @param remDr a remote driver
#' @param df a single-row data frame with the necessary information (bullet, cartridge_des, 
#'           lot_no, firing_seq, comments, brand, brand_other, caliber, caliber_other, grain, 
#'           surface_mat, surface_mat_other, barrel, details_url)
#' @export
#' @import RSelenium
create_bullets <- function(remDr, df) {
  
  stopifnot("bullet" %in% names(df))
  stopifnot("cartridge_des" %in% names(df))
  stopifnot("comments" %in% names(df))
  stopifnot("brand" %in% names(df))
  stopifnot("brand_other" %in% names(df))
  stopifnot("caliber" %in% names(df))
  stopifnot("caliber_other" %in% names(df))
  stopifnot("lot_no" %in% names(df))
  stopifnot("firing_seq" %in% names(df))
  stopifnot("grain" %in% names(df))
  stopifnot("surface_mat" %in% names(df))
  stopifnot("surface_mat_other" %in% names(df))
  stopifnot("barrel" %in% names(df))
  stopifnot("details_url" %in% names(df))
  
  stopifnot(df$brand %in% dropdown_options$bullet_brand_options)
  stopifnot(df$caliber %in% dropdown_options$bullet_caliber_options)
  stopifnot(df$grain %in% dropdown_options$bullet_weight_options)
  stopifnot(df$surface_mat %in% dropdown_options$surface_material_options)
  
  barrel_name_sel <- paste("div.row:nth-child(4) > div:nth-child(1) > div:nth-child(1) >", 
                           "div:nth-child(2) > div:nth-child(2) > div:nth-child(1) > ", 
                           "dl:nth-child(1) > dd:nth-child(2)")
  
  # Navigate to barrel URL
  loop_test(function() find_text(remDr, barrel_name_sel), 
            function(x) x != df$barrel, 
            function() remDr$navigate(df$details_url),
            warning_text = "navigation to barrel URL failed")
  
  # Check to see if bullet already exists
  tmp <- check_link_exists(remDr, sprintf("//a[text()=\"%s\"]", df$bullet), "Bullet")
  
  if (!is.na(tmp)) {
    return(tmp)
  }
  
  try({
    # Create the bullet
    open_modal(remDr, ".modal-open", "#btnAddNewBullet")
    
    # Enter text values into fields
    fill_text_field(remDr, "#SpecimenName", df$bullet, clear = T)
    fill_text_field(remDr, "#CartridgeDesignation", df$cartridge_des, clear = T)
    fill_text_field(remDr, "#LotNumber", df$lot_no, clear = T)
    fill_text_field(remDr, "#FiringSequence", df$firing_seq, clear = T)
    fill_text_field(remDr, "#Comment", df$comments, clear = T)
    
    # This will match the closest (alphabetical) brand to df$brand's value
    fill_conditional_field(remDr, "#BrandId", "#OtherBrand", df$brand, df$brand_other)
    fill_conditional_field(remDr, "#NominalCaliberId", "#OtherNominalCaliber", df$caliber, df$caliber_other)
    fill_dropdown_field(remDr, "#BulletWeightId", df$grain)
    fill_conditional_field(remDr, "#SurfaceMaterialId", "#OtherSurfaceMaterial", 
                           df$surface_mat, df$surface_mat_other)
    
    submit_modal(remDr, ".modal-open", "input.btn:nth-child(2)")
  })
  
  remDr$navigate(df$details_url)
  return(check_link_exists(remDr, sprintf("//a[text()=\"%s\"]", df$bullet), "Bullet"))
}

#' Function to upload land scans to bullet objects
#' 
#' @param remDr a remote driver
#' @param land_df a single-row data frame with required information (bullet, bullet_link,
#'          new_filename, creator, nist_meas, measurand, lighting_dir, lighting_dir_other, 
#'          meas_type, meas_type_other, instrument_brand, instrument_model, roi, land, 
#'          lateral_res, vertical_res, obj, aperture, comment, filename)
#' @export
#' @import RSelenium
create_lands <- function(remDr, land_df) {
  bullet_sel <- paste("div.row:nth-child(9) > div:nth-child(1) > div:nth-child(1) > ", 
                      "div:nth-child(2) > div:nth-child(2) > div:nth-child(1) > ", 
                      "dl:nth-child(1) > dd:nth-child(2)")
  
  # Navigate to bullet URL
  loop_test(function() find_text(remDr, bullet_sel), 
            function(x) x != land_df$bullet, 
            function() remDr$navigate(land_df$bullet_link),
            warning_text = "navigation to bullet URL failed")
  
  
  # Check to see if land already exists
  tmp <- check_link_exists(remDr, sprintf("//a[text()=\"%s\"]", basename(land_df$new_filename)), "Land")
  
  if (!is.na(tmp)) {
    return(tmp)
  }
  
  try({
    
    # Create the land
    open_modal(remDr, ".modal-open", "input.btn")
    
    fill_dropdown_field(remDr, "#CreatorID", value = land_df$creator)
    fill_dropdown_field(remDr, "#IsNISTMeasurement", value = land_df$nist_meas)
    fill_dropdown_field(remDr, "#MeasurandID", value = land_df$measurand)
    
    if (land_df$measurand != "3D Topography" & !is.na(land_df$lighting_dir)) {
      fill_conditional_field(remDr, "#LightingTypeID", "#OtherLightingType", land_df$lighting_dir, land_df$lighting_dir_other)
    }
    fill_conditional_field(remDr, "#MeasurementTypeID", "#OtherMeasurementType", land_df$meas_type, land_df$meas_type_other)
    
    fill_text_field(remDr, "#InstrumentBrand", value = land_df$instrument_brand, clear = T)
    fill_text_field(remDr, "#InstrumentModel", value = land_df$instrument_model, clear = T)
    
    fill_dropdown_field(remDr, "#RegionOfInterestID", value = land_df$roi)
    
    fix_land <- land_df$land %>% str_replace_all("\\D", "")
    fill_text_field(remDr, "#LeaOrGeaNumber", value = fix_land, clear = T)
    fill_num_field(remDr, "#LateralResolution", value = land_df$lateral_res)
    fill_num_field(remDr, "#VerticalResolution", value = land_df$vertical_res, clear = T)
    
    fill_num_field(remDr, "#ObjectiveMagnification", value = land_df$obj)
    
    if (!is.na(land_df$aperture)) {
      fill_text_field(remDr, "#ObjectiveNA", value = land_df$aperture)
    }
    
    fill_text_field(remDr, "#Comment", value = land_df$comment)
    
    file.copy(land_df$filename, file.path(getwd(), basename(land_df$new_filename)), overwrite = T)
    
    # Ensure file gets uploaded
    loop_test(
      function() remDr$findElement("css selector", "#ImageFile")$getElementAttribute("value") %>% unlist(),
      function(x) nchar(x) == 0,
      function() remDr$findElement("css selector", "#ImageFile")$sendKeysToElement(list(file.path(getwd(), basename(land_df$new_filename))))
    )
    
    submit_modal(remDr, ".modal-open", "input.btn:nth-child(2)")
    
    # Remove renamed file once it's uploaded
    file.remove(file.path(getwd(), basename(land_df$new_filename)))
  })
  
  
  remDr$navigate(land_df$bullet_link)
  return(check_link_exists(remDr, sprintf("//a[text()=\"%s\"]", basename(land_df$new_filename)), "Land"))
}

#' Parse a string containing magnification value `dddX' into a number `ddd'
#' @param x string
#' @return number extracted from x
parse_objective <- function(x) {
  x %>%
    str_extract("\\d{1,} ?[xX]") %>%
    parse_number()
}

#' Read metadata from x3p and fill in land information
#' 
#' x3p header information is concatenated into a long comment (containing serial 
#' number, version, calibration date, probing system, creator, creation date, 
#' and md5), and other relevant information (microscope brand, model, 
#' resolution) is stored in individual fields corresponding to fields on NBTRD.
#' @param path path to x3p file
#' @return tibble with land information
#' @export
x3p_land_info <- function(path) {
  tmp <- read_x3p(path)
  
  stopifnot("x3p" %in% class(tmp))
  
  full_comment <- paste(
    c(paste0("Scan Comment: ", tmp$general.info$Comment[[1]]),
      paste0("Instrument Serial Number: ", tmp$general.info$Instrument$Serial[[1]]),
      paste0("Instrument Version: ",       tmp$general.info$Instrument$Version[[1]]),
      paste0("Instrument Calibration Date: ", unlist(tmp$general.info$CalibrationDate)),
      paste0("Probing System: ", tmp$general.info$ProbingSystem$Type[[1]]),
      paste0("Scanned by: ", tmp$general.info$Creator[[1]]),
      paste0("Creation Date: ", tmp$general.info$Date[[1]]),
      paste0("binary_md5checksum: ", tmp$matrix.info$DataLink$MD5ChecksumPointData[[1]])
    ),
    collapse = "\n")
  
  
  tibble(
    filename = path, 
    instrument_brand = tmp$general.info$Instrument$Manufacturer[[1]],
    instrument_model = tmp$general.info$Instrument$Model[[1]],
    lateral_res = tmp$header.info$incrementX,
    vertical_res = tmp$header.info$incrementY,
    obj = parse_objective(tmp$general.info$ProbingSystem$Identification[[1]]),
    aperture = NA,
    comment = full_comment
  )
}