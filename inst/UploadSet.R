# ---- Steps ----
# 1. Assemble data: Ideally, structured as 
#    Barrel_X/Bullet_Y/Study-Barrel_X-Bullet_Y-Land*.x3p
# 2. Create metadata csv containing columns barrel, bullet, land, and path, 
#    with one row for each x3p file.
# 3. Log on to NIST and create study manually. Note the URL of the study: 
#    https://tsapps.nist.gov/NRBTD/Studies/Studies/Details/<study_id>
# 4. Run this script

datapath <- "/media/Raven/LAPD-NIST"
metadata <- tibble(path = list.files(datapath, "*.x3p", full.names = T, recursive = T)) %>%
  tidyr::extract(path, into = c("barrel", "bullet", "land"), "(FAU\\d{3})-(B[ABCD])-(L[1-6])", remove = F)
write_csv(metadata, file.path(datapath, "meta.csv"))

# ---- Data parameters ----

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

# ---- Actual Execution ----

remDr <- setup_NBTRD(strict = T)
login <- nbtrd_login(remDr, user = "srvander", 
                     password = keyringr::decrypt_gk_pw("db csafe user srvander"))

# Find set link using set name
set_link <- remDr$
  findElement(using = "link text", value = setInfo$name)$
  getElementAttribute("href")[[1]]

remDr$navigate(set_link)

# ---- Create barrels ----

firearms <- read_csv(setInfo$meta_path) %>%
  magrittr::extract2("barrel") %>%
  unique() 

# Partially fill in function arguments with remoteDriver and seturl - easier to map
create_study_barrels <- partial(create_firearms, rd = remDr, seturl = set_link)

# Initial barrel creation
firearms_info <- tibble(
  idx = 1:length(firearms),
  name = as.character(firearms),
  df = list(barrelInfo)
) %>%
  unnest()

# JS Script to change barrel list to full length
barrel_length_script_js <- "
  $('.form-control option')[0].value = arguments[0]; 
  $('.form-control').trigger('change')
  return $('#tableFirearms tbody tr').length;
"

nrows <- remDr$executeScript(barrel_length_script_js, 
                             list(nrow(firearms_info))) %>% 
  unlist() %>% as.numeric()

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

page_of_links <- remDr$getPageSource() %>%
  unlist() %>%
  read_html()

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
  filter(firearm_name != "Bullet / CC")

remDr$navigate(set_link)

# ---- Create bullets ----
create_study_bullets <- safely(partial(create_bullets, rd = remDr))

bullet_info <- read_csv(setInfo$meta_path) %>%
  left_join(select(firearms_links, barrel = firearm_name, id, details_url)) %>%
  merge(ammoInfo) %>%
  mutate_at(vars(barrel, bullet), str_remove_all, "[[:punct:]]") %>% 
  select(-land, -path) %>%
  unique() %>%
  mutate(idx = 1:n()) %>%
  nest(-idx, .key = "bullet_only_info") %>%
  mutate(bullet_link_res = purrr::map(bullet_only_info, create_study_bullets)) %>%
  mutate(bullet_errs = purrr::map(bullet_link_res, "error"),
         bullet_link = purrr::map_chr(bullet_link_res, "result")) %>%
  unnest(-bullet_link_res)

# ---- Create lands ----
create_study_lands <- partial(create_lands, remDr = remDr)

indiv_land_info <- upload_file_list %>%
  map_df(x3p_land_info) %>%
  tidyr::extract(filename, into = c("barrel", "bullet", "land"), 
                 regex = setInfo$file_regex, remove = F) %>%
  mutate(fileid = paste(barrel, bullet, land, sep = "-")) %>%
  left_join(select(bullet_info, bullet, barrel, bullet_link)) %>%
  merge(scanInfo) %>%
  unique() %>%
  mutate(comment = paste(comment, othercomment, sep = "\n"),
         new_filename = sprintf("%s/%s/%s.x3p", datapath, rename, fileid)) %>%
  mutate(idx = 1:n()) %>%
  nest(-idx, .key = "land_df") %>%
  mutate(land_link = purrr::map_chr(land_df, create_study_lands)) %>%
  unnest()



# ---- Clean Up ----
remDr$close()

save(firearm_info, bullet_info, indiv_land_info, 
     file = paste0(setInfo$name, "_Upload.Rdata"))

  
