library(RSelenium)
library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

source("scripts/scraping_fcts.R")

# https://www.betreut.de/de-de/profiles?sort=bestMatch&order=asc&isRefinedSearch=true&verticalId=childCare&radius=20&geoRegionId=POSTCODE-DE-10117&geoRegionSearch=10117+Mitte
# https://www.betreut.de/de-de/profiles?sort=bestMatch&verticalId=childCare&radius=20&geoRegionId=POSTCODE-DE-10117

# set variables to change for the search query
query_values <- list(
  "job" = c(
    "Kinderbetreuung" = "childCare",
    "Seniorenbetreuung" = "seniorCare",
    "Reinigung" = "homeCare&subVerticalIdList=housekeeping"
  ),
  "plz" = "37355",
  "radius" = 500
)

base_url <- "https://www.betreut.de"

login_url <- paste0(base_url, "/login")


elements <- c(
  cookie_accept = "#onetrust-reject-all-handler",
  login_username = "#j_username",
  login_password = "#j_password",
  login_button = "//button[contains(text(), 'Einloggen')]",
  filter_head = ".body-3",
  profile_link = ".profileLink",
  next_page = ".nextLink",
  show_more = "//button[contains(text(), 'Mehr anzeigen')]"
)

# load login credentials
login_data <- readLines("data/betreut/logins.txt")


# define user agent by creating firefox profile
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/135.0"
# user_agent <- "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_8; like Mac OS X) AppleWebKit/536.13 (KHTML, like Gecko)  Chrome/50.0.2440.333 Mobile Safari/600.1"
# Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1
fprof <- makeFirefoxProfile(list(general.useragent.override = user_agent))


# start remote driver
rD <- rsDriver(
  browser = "firefox",
  port = 4445L,
  extraCapabilities = fprof,
  phantomver = NULL,
)

remDr <- rD$client

remDr$setTimeout(type = "implicit", milliseconds = 3000)
remDr$setTimeout(type = "page load", milliseconds = 5000)

# navigate to login page
remDr$navigate(login_url)


# remove cookie banner
try(remDr$findElement("css", elements["cookie_accept"])$clickElement(), TRUE)


# enter username and password
remDr$findElement(
  "css",
  elements["login_username"]
)$sendKeysToElement(list(login_data[1]))

remDr$findElement(
  "css",
  elements["login_password"]
)$sendKeysToElement(list(login_data[2]))

remDr$findElement(value = elements["login_button"])$clickElement()

Sys.sleep(3)

# start loop here

search_url <- sprintf(
  "%s/de-de/profiles?sort=bestMatch&verticalId=%s&radius=%s&geoRegionId=POSTCODE-DE-%s&max=50",
  base_url,
  query_values[["job"]][["Reinigung"]],
  query_values["radius"],
  query_values["plz"]
)

# define output file
file_output <- paste0(
  "data/betreut/profiles/profiles_overview_",
  sub(
    "&subVerticalIdList=housekeeping",
    "",
    query_values[[1]][["Reinigung"]]
  ),
  ".csv"
)


# navigate to parametrized search page
remDr$navigate(search_url)


# Anzahl Anbieter:
# Kinderbetreuung: 34.830
# Seniorenbetreuung: 13.102
# Reinigung: 23.388
profiles_n <- remDr$findElement("css", ".body-3")$getElementText()[[1]] |>
  str_extract("^[0-9.]{1,6}") |>
  str_remove_all("\\.") |>
  as.integer()


# check if file allready exists, than start from last scraped page
if (!file.exists(file_output)) {
  message("File ", file_output, " does not exist. Start from page 1.")
  profiles_scraped <- 0

  page <- 1
  offset <- 0
} else {
  message("File ", file_output, " already exists. Start from last page.")
  profiles_overview_all <- read_csv2(file_output)

  profiles_scraped <- nrow(profiles_overview_all)

  page <- max(profiles_overview_all$page)
  offset <- (page - 1) * 50

  search_url <- sub(
    "offset=0",
    paste0("offset=", offset),
    search_url
  )

  remDr$navigate(search_url)
}


pages_list <- remDr$findElements("css", ".step")
last_page <- pages_list[[length(pages_list)]]$getElementText()[[1]] |>
  as.integer()


while (page <= last_page) {
  message("Page: ", page, " from ", last_page, ": ", appendLF = FALSE)

  # Get all profile links from this page
  profiles <- remDr$getPageSource()[[1]] |>
    read_html() |>
    html_elements(css = ".providerSnippet")

  # check if all 50 profiles loaded (if not last page), otherwise wait
  if (length(profiles) < 50 && page < last_page) {
    message("Not all profiles loaded. Wait 5 seconds.")
    Sys.sleep(5)
    profiles <- remDr$getPageSource()[[1]] |>
      read_html() |>
      html_elements(css = ".providerSnippet")
  }

  profiles_overview_df <- data.frame(
    profile_url = profiles |>
      html_elements(".profileLink") |>
      html_attr('href'),
    name = profiles |> html_node(".name") |> html_text(),
    short_location = profiles |>
      html_node(".hidden-xs") |>
      html_text(trim = TRUE) |>
      str_remove("\\|") |>
      str_squish(),
    short_ratings = profiles |>
      html_node(".stars-text") |>
      html_text() |>
      as.integer(),
    short_verification = profiles |>
      html_node(".verification") |>
      html_text() |>
      str_extract("\\d+") |>
      as.integer(),
    short_hires = profiles |>
      html_node(".hireCount") |>
      html_text() |>
      str_extract("\\d+") |>
      as.integer(),
    short_oneliner = profiles |> html_node("b") |> html_text(),
    img_src = profiles |>
      html_elements(".photo.img-responsive") |>
      html_attr("style") |>
      str_extract("url\\((.+?)\\)", group = 1),
    page = page,
    query_date = format(Sys.time(), "%Y%m%d_%H%M")
  )

  readr::write_csv2(
    profiles_overview_df,
    file = file_output,
    append = TRUE
  )

  profiles_scraped <- profiles_scraped + nrow(profiles_overview_df)

  message(profiles_scraped, " profiles scraped in total.")

  page <- page + 1

  # click next page and wait 5 secconds (to load page and b/c robots.txt)
  remDr$findElement(using = "css", value = elements["next_page"])$clickElement()

  Sys.sleep(5)

  # check if next page is already loaded, otherwise wait for 4 seconds
  offset <- remDr$getCurrentUrl()[[1]] |>
    str_extract("offset=(\\d+)", group = 1) |>
    as.integer()

  if (offset / 50 + 1 < page) {
    message("Next page not yet loaded. Wait 4 seconds.")
    Sys.sleep(4)
  }
}


# close the browser
try(remDr$close(), FALSE)

# stop the selenium server
rD[["server"]]$stop()
