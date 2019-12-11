# ---- Steps ----
# 0. Start a docker container with the datapath on the host machine shared with the container
# 1. Assemble data: Ideally, structured as 
#    Barrel_X/Bullet_Y/Study-Barrel_X-Bullet_Y-Land*.x3p
# 2. Create metadata csv containing columns barrel, bullet, land, and path, 
#    with one row for each x3p file.
# 3. Log on to NIST and create study manually. Note the URL of the study: 
#    https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/<study_id>
# 4. Run this script



datapath <- "/media/Raven/LAPD-NIST"

# Set up metadata file
metadata <- tibble(path = list.files(datapath, "*.x3p", full.names = T, recursive = T)) %>%
  tidyr::extract(path, into = c("barrel", "bullet", "land"), "(FAU\\d{3})-(B[ABCD])-(L[1-6])", remove = F)
write_csv(metadata, file.path(datapath, "meta.csv"))


docker_port <- 4444L # Docker port is needed to tell selenium where to look

# ---- Data parameters ----
# These change with every set

# Inputting this stuff isn't automated because it's only done once per set and 
# it didn't seem worth it. But the information is needed to pull links out, and 
# I tried to structure this so that you had to collect all of the information 
# first before running the script.

setInfo <- list(
  name = "LAPD",
  description = "NIJ/LAPD Joint Project exemplars from 626 Beretta 92 F/FS firearms (626 barrels x 4 bullets each = 2504 bullets) from Srini Rathinam (LAPD), Principle Investigator and Project Manager.",
  reference = "Rathinam, S. (May 2014). “Stabilized Relative Frequencies of Random Striae and Their Contribution to Firearm Individualization: An Empirical Study Using 625 Beretta model F/FS Semi-automatic Pistols”, paper presented at the 45th annual AFTE training seminar, Seattle, USA",
  abstract = "",
  creator = "Iowa State University, CSAFE",
  persistence = FALSE,
  consecutive_manufacture = FALSE,
  different_ammo = TRUE,
  data_path = "/media/Raven/LAPD-NIST",
  meta_path = "/media/Raven/LAPD-NIST/meta.csv",
  file_regex = "(FAU\\d{3})-(B[abcdABCD])-(L[1-6])"
)

metadata <- read_csv(setInfo$meta_path) %>%
  mutate(fileid = paste(barrel, bullet, land, sep = "-"))

# Assumes all barrels are the same
barrelInfo <- tibble(
  brand = "Beretta",
  brand_other = NA,
  model = "92 F/FS",
  caliber = "9 mm Luger",
  caliber_other = NA,
  consec_manufacture = FALSE,
  comments = "Land impressions are not indexed from bullet to bullet.", 
  cartridges = FALSE,
  bullets = TRUE,
  breech_face_class = NA,
  breech_face_other = NA,
  firing_pin_class = NA,
  firing_pin_other = NA,
  n_lands = "6",
  twist_direction = "Right"
)

# Assumes all bullets are the same
ammoInfo <- tibble(
  brand = "Winchester", 
  brand_other = NA,
  caliber = "9 mm",
  caliber_other = NA,
  grain = "101-150",
  cartridge_des = "9 mm Luger",
  surface_mat = "Copper",
  surface_mat_other = NA,
  firing_seq = "",
  lot_no = "",
  comments = ""
)

scanInfo <- tibble(
  creator = "Hofmann, Heike",
  nist_meas = "F",
  measurand = "3D Topography",
  lighting_dir = NA,
  lighting_dir_other = NA,
  meas_type = "Other",
  meas_type_other = "Confocal Light Microscope",
  roi = "Land Engraved Area",
  othercomment = "Scanned by Bill Henderson, downsampled by Heike Hofmann"
)

upload_file_list <- list.files(setInfo$data_path, pattern = "*.x3p", 
                               full.names = T, recursive = T)

# ---- Set up docker image and start it up ----
# Parts of the docker command
selenium_string <- sprintf("-p %d:4444", docker_port) # map chosen host port to port 4444 on docker image
vnc_string <- "-p 5901:5900" # This allows you to connect to localhost:5901 using vnc remote desktop to see what the browser is doing
selenium_docker_img <- "selenium/standalone-firefox-debug:2.53.1"
file_system_mapping <- paste0("-v ", datapath, ":", datapath) # map datapath on host to datapath in docker
docker_img <- system(paste("docker run -d -P", selenium_string, vnc_string, file_system_mapping, selenium_docker_img)) # run docker image start
rm(selenium_string, vnc_string, selenium_docker_img, file_system_mapping)

# ---- Actual Execution ----

remDr <- setup_NBTRD(selenium_port = docker_port, strict = T)
login <- nbtrd_login(remDr, user = "srvander", 
                     password = keyringr::decrypt_gk_pw("db csafe user srvander"))

# Find set link using set name
# I'm pretending $ is a pipe here because otherwise it's hard to read.
set_link <- remDr$
  findElement(using = "link text", value = setInfo$name)$
  getElementAttribute("href")[[1]]

remDr$navigate(set_link)

# ---- Create barrels ----

# Get 
firearms <- read_csv(setInfo$meta_path) %>%
  magrittr::extract2("barrel") %>%
  unique() %>%
  na.omit()

# Partially fill in function arguments with remoteDriver and seturl - easier to map
create_study_barrels <- partial(create_firearms, rd = remDr, seturl = set_link)

# Initial barrel creation
firearms_info <- tibble(
  idx = 1:length(firearms),
  name = as.character(firearms),
  df = list(barrelInfo)
) %>%
  unnest(df)
  

# JS Script to change barrel list to full length
barrel_length_script_js <- "
  $('.form-control option')[0].value = arguments[0]; 
  $('.form-control').trigger('change')
  return $('#tableFirearms tbody tr').length;"
# Execute the script
nrows <- remDr$executeScript(barrel_length_script_js, 
                             list(nrow(firearms_info))) %>% 
  unlist() %>% as.numeric()

# Get all the links on the page
page_of_links <- remDr$getPageSource() %>%
  unlist() %>%
  read_html()

# Fill in missing sets if necessary
if (nrows == 0) {
  firearms_info <- firearms_info  %>%
    nest(-idx, .key = "df") %>%
    mutate(url = map(df, create_study_barrels)) %>%
    unnest()
} else if (nrows != nrow(firearms_info)) {
  firearm_names <- page_of_links %>%
    rvest::html_nodes("#tableFirearms tbody tr td:first-of-type a") %>%
    html_text()
  missing <- firearms_info %>%
    filter(!name %in% firearm_names) %>%
    nest(-idx, .key = "df") %>%
    mutate(url = map(df, create_study_barrels)) %>%
    unnest()
}

# Get the whole page of links to firearms
page_of_links <- remDr$getPageSource() %>%
  unlist() %>%
  read_html()

# Create a tbl of links w/ relevant info
firearms_links <- tibble(
  firearm_name = page_of_links %>%
    rvest::html_nodes("#tableFirearms tbody tr td:first-of-type a") %>%
    html_text(),
  details_url = page_of_links %>%
    rvest::html_nodes("#tableFirearms tbody tr td:first-of-type a") %>%
    html_attr("href") %>%
    paste0("https://tsapps.nist.gov", .),
  edit_url = page_of_links %>% 
    rvest::html_nodes("#tableFirearms a[href*='Firearm'][href*='Edit']") %>%
    html_attr("href") %>%
    paste0("https://tsapps.nist.gov", .),
  id = str_remove(details_url, 
                  fixed("https://tsapps.nist.gov/NRBTD/Studies/Firearm/Details/"))
) %>%
  unique() %>%
  filter(firearm_name != "Bullet / CC") # Get rid of extra links and crud

remDr$navigate(set_link)

# ---- Create bullets ----
# Create a partially filled in function so we don't have to have rd as an argument in map
create_study_bullets <- safely(partial(create_bullets, rd = remDr))

# Initial bullet creation - get all metadata collected to be filled in to the form
bullet_info <- read_csv(setInfo$meta_path) %>%
  left_join(select(firearms_links, barrel = firearm_name, id, details_url)) %>%
  merge(ammoInfo) %>%
  mutate_at(vars(barrel, bullet), str_remove_all, "[[:punct:]]") %>% 
  select(-land, -path) %>%
  unique() %>%
  mutate(idx = 1:n()) %>%
  # Get rid of problem entries
  filter(!is.na(barrel), !is.na(bullet), !is.na(details_url)) 
# Actually create the bullets
bullet_info <- bullet_info %>%
  nest(bullet_only_info = -idx) %>%
  mutate(bullet_link_res = purrr::map(bullet_only_info, create_study_bullets)) 

# This function handles safely() type output, which is a list
# It first checks to see if entries are NULL and replaces them with NA
# If they are non-null, they are converted to character.
# Then it unlists them into a character vector
# purrr::map_chr doesn't handle NULL values that well, hence the workaround
modify_to_NA <- function(x, depth = 1) {
  purrr::map_chr(x, ~ifelse(is.null(.), NA, as.character(.)))
}

# Undo the results of safely()
bullet_info <- bullet_info %>%
  mutate(bullet_errs = purrr::map(bullet_link_res, "error") %>% modify_to_NA(),
         bullet_link = purrr::map(bullet_link_res, "result") %>% modify_to_NA()) 

# Errors - fix manually  or figure out why it failed.
filter(bullet_info, is.na(bullet_link))  %>%
  mutate(bullet_link_res = purrr::map(bullet_only_info, create_study_bullets)) 

# This function retries a few times to ensure the issues aren't because of a 
# flaky selenium connection or internet issue
modify_if_NA <- function(link, info) {
  times <- 5
  while (times > 0 & is.na(link)) {
    link <- create_study_bullets(info)$result %>% modify_to_NA(depth = 1)
    times <- times - 1
  }

  link
}

bullet_info$bullet_link <- modify2(bullet_info$bullet_link, 
                                   bullet_info$bullet_only_info, 
                                   modify_if_NA)
  

# ---- Create lands ----
indiv_land_info <- upload_file_list %>%
  map_df(x3p_land_info) %>%
  tidyr::extract(filename, into = c("barrel", "bullet", "land"), 
                 regex = setInfo$file_regex, remove = F) %>%
  mutate(fileid = paste(barrel, bullet, land, sep = "-")) %>%
  left_join(select(unnest(bullet_info, "bullet_only_info"), bullet, barrel, bullet_link)) %>%
  merge(scanInfo) %>%
  unique() %>%
  mutate(comment = paste(comment, othercomment, sep = "\n"),
         new_filename = filename
         # new_filename = file.path(getwd(), basename(filename))
         ) %>%
  mutate(idx = 1:n()) 

# the last bit took so long remDr is probably not still functional, so refresh it
remDr <- setup_NBTRD(selenium_port = docker_port, strict = T)
login <- nbtrd_login(remDr, user = "srvander", 
                     password = keyringr::decrypt_gk_pw("db csafe user srvander"))
remDr$navigate(set_link)

# Create a partially filled in function so we don't have to have remDr as an argument in map
create_study_lands <- partial(create_lands, rd = remDr, copy = T)

# I'm too chicken to overwrite indiv_land_info because it took so long to generate...
indiv_land_info2 <- indiv_land_info %>%
  nest(land_df = -idx) %>%
  mutate(land_link = purrr::map(land_df, create_study_lands)) %>%
  unnest()



# ---- Clean Up ----
remDr$close()

system(paste("docker stop", docker_img))

save(firearm_info, bullet_info, indiv_land_info, 
     file = paste0(setInfo$name, "_Upload.Rdata"))

  
