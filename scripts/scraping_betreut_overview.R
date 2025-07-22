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

page_size <- 50

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
  "%s/de-de/profiles?sort=bestMatch&verticalId=%s&radius=%s&geoRegionId=POSTCODE-DE-%s&max=%s&offset=0",
  base_url,
  query_values[["job"]][["Reinigung"]],
  query_values["radius"],
  query_values["plz"],
  page_size
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


# check if file allready exists, than start from last scraped page
if (!file.exists(file_output)) {
  message("File ", file_output, " does not exist. Start from page 1.")

  profiles_scraped <- 0

  page_start <- 1
} else {
  message("File ", file_output, " already exists. Start from last page.")

  profiles_on_disk <- read_csv2(file_output)

  profiles_scraped <- nrow(profiles_on_disk)

  page_start <- max(profiles_on_disk$page) + 1
}


# navigate to parametrized search page
remDr$navigate(search_url)

pages_list <- remDr$findElements("css", ".step")
page_last <- pages_list[[length(pages_list)]]$getElementText()[[1]] |>
  as.integer()


for (page in seq(page_start, page_last)) {
  message("Open page: ", page, " from ", page_last, ": ", appendLF = FALSE)

  # 1. update search url with offset calculated by page number and open page
  offset <- (page - 1) * page_size

  search_url <- str_replace(
    search_url,
    "offset=([0-9]+)",
    paste0("offset=", offset)
  )

  remDr$navigate(search_url)

  Sys.sleep(3)

  # 2. check if page is loaded correctly, otherwise, wait up to 15 seconds

  # 2.1 correct offset of current page URL
  t0 <- Sys.time()
  repeat {
    current_url <- remDr$getCurrentUrl()[[1]]
    if (grepl(paste0("offset=", offset), current_url)) {
      break
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 15) {
      stop("Timed out waiting for offset=", offset)
    }
    Sys.sleep(0.5)
  }

  # 2.2 all 50 profiles loaded (if not last page)
  t0 <- Sys.time()
  repeat {
    profiles <- remDr$getPageSource()[[1]] |>
      read_html() %>%
      html_elements(css = ".providerSnippet")
    if (length(profiles) >= page_size || page == page_last) {
      break
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 15) {
      warning("Only found ", length(profiles), " profiles after ", 15, "s")
      break
    }
    Sys.sleep(0.5)
  }

  # 3. get the information for all profiles from the current page
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

  # 4. write profile data to disk (append if not first page)
  readr::write_csv2(
    profiles_overview_df,
    file = file_output,
    col_names = page == 1,
    append = page > 1
  )

  profiles_scraped <- profiles_scraped + nrow(profiles_overview_df)

  message(
    nrow(profiles_overview_df),
    " new profiles, add up to ",
    profiles_scraped,
    " in total."
  )

  page <- page + 1
}


# close the browser
try(remDr$close(), FALSE)

# stop the selenium server
rD[["server"]]$stop()
