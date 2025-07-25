library(RSelenium)
library(readr)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)


source("scripts/scraping_fcts.R")

base_url <- "https://www.betreut.de"

login_url <- paste0(base_url, "/login")

# load login credentials
login_data <- readLines("data/betreut/logins.txt")

files_overview <- list.files(
  "data/betreut/profiles/",
  pattern = "profiles_overview_",
  full.names = TRUE
)

df_overview <- lapply(files_overview, read_csv2) |> bind_rows()


profile_urls <- df_overview$profile_url |> na.omit() |> unique()


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

Sys.sleep(5)

remDr <- rD$client

remDr$setTimeout(type = "implicit", milliseconds = 3000)
remDr$setTimeout(type = "page load", milliseconds = 5000)


# navigate to login page
remDr$navigate(login_url)

# remove cookie banner
try(remDr$findElement("css", elements["cookie_accept"])$clickElement(), TRUE)


# login and enter username and password
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

# define output files
file_profiles <- "data/betreut/profiles.csv"
file_reviews <- "data/betreut/reviews.csv"
file_errors <- "data/betreut/errors.csv"

# check if file already exists, if so load already scraped profile urls
file_profiles_exists <- file.exists(file_profiles)

if (file_profiles_exists) {
  message("Load already scraped profile urls.")

  profile_urls_scraped <- read_csv2(file_profiles) |> pull(profile_url)

  profile_urls <- profile_urls[!(profile_urls %in% profile_urls_scraped)]

  if (file.exists(file_errors)) {
    profile_urls_errors <- read_lines(file_errors) |> str_remove(base_url)
    profile_urls <- profile_urls[!(profile_urls %in% profile_urls_errors)]
  }
}

# loop over list of all profile urls to scrape profile and review data
for (profile_num in seq(length(profile_urls))) {
  message(profile_num, " of ", length(profile_urls), ". ", appendLF = FALSE)

  profile_url <- paste0(base_url, profile_urls[profile_num])

  success <- tryCatch(
    {
      remDr$navigate(profile_url)
      TRUE
    },
    error = function(e) {
      message(" (Cannot open profile URL)")
      FALSE
    }
  )

  if (!success) {
    next
  }

  Sys.sleep(3)

  current_url <- remDr$getCurrentUrl()[[1]] |>
    str_remove(base_url)

  # check if redirected to wrong page -> write to error file and skip
  if (!grepl("source=providerSnippet", current_url)) {
    message(
      "Redirect to wrong page. Write to error file and skip: ",
      profile_url
    )

    write(profile_url, file_errors, append = TRUE)
    next
  }

  # expand text description
  suppressMessages(try(
    remDr$findElement(value = elements["show_more"])$clickElement(),
    TRUE
  ))

  current_url <- remDr$getCurrentUrl()[[1]] |>
    str_remove(base_url)

  # read static html for extraction
  profile_html <- remDr$getPageSource()[[1]] |>
    read_html()

  # prepare info from profile overview for extraction in data.frame
  headbox_infoline <- profile_html |>
    html_elements(xpath = "//div[@id='section-header']/div[2]/div/div/span") |>
    html_text()

  about_infoline <- get_text(".MuiTypography-h3", profile_html)

  # extract profile data
  profile_df <- data.frame(
    name = headbox_infoline[1],
    online_status = get_text(
      "#section-header > div > div > span",
      profile_html
    ),
    premium = !is.na(html_element(profile_html, ".MuiIconButton-label")),
    stars = profile_html |>
      html_element(
        css = ".MuiRating-root.MuiRating-sizeLarge.MuiRating-readOnly"
      ) |>
      html_attr("aria-label") |>
      str_extract("([0-5]) Stars", group = 1) |>
      as.integer(),
    headbox_experience = str_extract(
      paste(headbox_infoline, collapse = " "),
      "([0-9]{1,2}) Jahre Erf.",
      group = 1
    ) |>
      as.integer(),
    headbox_price = str_extract(
      paste(headbox_infoline, collapse = " "),
      "([0-9]{1,2})€",
      group = 1
    ) |>
      as.integer(),
    headbox_responserate = str_extract(
      paste(headbox_infoline, collapse = " "),
      "([0-9]{1,3})%",
      group = 1
    ) |>
      as.integer(),
    headbox_checks = profile_html |>
      html_elements(
        xpath = "//div[@id='section-header']/div[2]/div[3]/div/li"
      ) |>
      html_text() |>
      paste(collapse = ":|;"),
    about_age = str_extract(
      about_infoline,
      "([1-9][0-9]) Jahre",
      group = 1
    ) |>
      as.integer(),
    about_language = str_extract(about_infoline, "spricht (.*)$", group = 1),
    about_text = get_text("#about-section", profile_html),
    about_tags = profile_html |>
      html_elements(xpath = "//div[@id='section-about']/div/div") |>
      html_nodes("span") |>
      html_text() |>
      paste(collapse = ":|;"),
    checks_main = get_text(
      "#section-header .MuiTypography-displayBlock",
      profile_html,
      TRUE
    ) |>
      paste0(collapse = ":|;"),
    checks_experience = get_text(
      "#section-experience .MuiTypography-displayBlock",
      profile_html,
      TRUE
    ) |>
      paste0(collapse = ":|;"),
    checks_service = get_text(
      "#section-services .MuiTypography-displayBlock",
      profile_html,
      TRUE
    ) |>
      paste0(collapse = ":|;"),
    checks_qualification = get_text(
      "#section-qualifications .MuiTypography-displayBlock",
      profile_html,
      TRUE
    ) |>
      paste0(collapse = ":|;"),
    profile_url = current_url
  )

  message(
    "Write profile data of ",
    profile_df$name,
    " to csv file. ",
    appendLF = FALSE
  )

  write_csv2(
    profile_df,
    file_profiles,
    col_names = !file_profiles_exists,
    append = file_profiles_exists
  )

  file_profiles_exists <- TRUE

  # get and store reviews
  reviews <- profile_html |>
    html_elements(
      css = ".MuiGrid-root.MuiGrid-container.MuiGrid-spacing-xs-1.MuiGrid-item"
    )

  if (length(reviews) == 0) {
    message("No reviews found.")
    next
  }

  reviews_df <- lapply(seq_along(reviews), function(review) {
    review_infos <- reviews[review] |> html_nodes("span") |> html_text()

    data.frame(
      review_name = review_infos[1],
      review_date = review_infos[3],
      review_rating = reviews[review] |>
        html_elements(".MuiRating-icon.MuiRating-iconFilled") |>
        length(),
      review_text = get_text(
        ".MuiGrid-root.MuiGrid-item.MuiGrid-grid-xs-12",
        reviews[review]
      ),
      profile_url = current_url
    )
  }) |>
    bind_rows() |>
    mutate(
      review_answer = ifelse(
        lead(review_name == profile_df$name),
        lead(review_text),
        NA
      )
    ) |>
    filter(review_rating > 0)

  message("Write ", nrow(reviews_df), " reviews to csv file.")

  write_csv2(
    reviews_df,
    file_reviews,
    col_names = !file_profiles_exists,
    append = file_profiles_exists
  )
}


# close the browser
try(remDr$close(), FALSE)

# stop the selenium server
rD[["server"]]$stop()
