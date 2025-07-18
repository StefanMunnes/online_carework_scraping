library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)


source("scripts/scraping_fcts.R")

# set variables to change for the search query
query_values <- list(
  plz = c("Leipzig" = "04315", "Bremen" = "28211"),
  frequency = "Einmalig",
  duration = "2 Stunden",
  date = "22"
)

query_date <- format(Sys.time(), "%Y%m%d_%H%M")

# define constant objects for scraping
target_url <- "https://www.helpling.de/"

elements <- c(
  cookie_accept = "#cookiescript_accept",
  popup_close = "//button[@title = 'Close']",
  frequency_choice = paste0(
    "//div[contains(text(),'",
    query_values[["frequency"]],
    "')]"
  ),
  date_field = "//div[contains(text(),'Datum wählen')]",
  date_input = paste0("//div[text()='", query_values[["date"]], "']"),
  duration_field = "//div[text()='Dauer auswählen']",
  duration_input = paste0(
    "//div[contains(text(),'",
    query_values[["duration"]],
    "')]"
  ),
  show_results = "//div[text() = 'Reinigungskräfte anzeigen']",
  cleaners = "//div[contains(text(), 'Reinigungskräfte verfügbar')]",
  profile_rating = "//button[text() = 'Bewertungen']",
  next_arrow = ".profile__next-arrow",
  profile_cards = "//div[@data-testid='CandidateCard']",
  profile_details = "//div[text() = 'Siehe Details']",
  profile_close = "//div[@data-testid='CandidateModal']/div/div/div/div"
)

# define user agent by creating firefox profile
user_agents <- c(
  # iPhone with iOS 17 (Latest):
  "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
  # iPhone with iOS 16:
  "Mozilla/5.0 (iPhone; CPU iPhone OS 16_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.6 Mobile/15E148 Safari/604.1",
  # Google Pixel 8 Pro (Android 14):
  "Mozilla/5.0 (Linux; Android 14; Pixel 8 Pro) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36",
  # Google Pixel 7 (Android 13):
  "Mozilla/5.0 (Linux; Android 13; Pixel 7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Mobile Safari/537.36",
  # Samsung Galaxy S24 Ultra (Android 14):
  "Mozilla/5.0 (Linux; Android 14; SM-S928B) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36",
  # Samsung Galaxy S23 (Android 13):
  "Mozilla/5.0 (Linux; Android 13; SM-S911B) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Mobile Safari/537.36"
)

fprof <- makeFirefoxProfile(list(
  general.useragent.override = user_agents[1],
  browser.download.dir = "C:/Users/munnes/Documents/R_selenium_firefox"
))

# start remote driver
rD <- rsDriver(
  browser = "firefox",
  port = 4445L,
  extraCapabilities = fprof,
  phantomver = NULL,
)


remDr <- rD$client

# set timeout
remDr$setTimeout(type = "implicit", milliseconds = 3000)
remDr$setTimeout(type = "page load", milliseconds = 5000)

# set window size for mobile (otherwise page is not loading correct)
remDr$setWindowSize(width = 700, height = 900)


# navigate to target page
remDr$navigate(target_url)

# enter PLZ and click enter
remDr$findElement('css', '#bid_postcode')$sendKeysToElement(list(
  query_values[["plz"]][["Bremen"]],
  key = "enter"
))

# accept cookie banner
try(remDr$findElement("css", elements["cookie_accept"])$clickElement(), TRUE)


# check for wrong redirect if no results for PLZ
# if (!grepl("step-zero", remDr$getCurrentUrl())) {
#   message("Redirect to wrong page. Move to previous page.")
#   remDr$goBack()
# } else {
#   message("Redirect to correct page.")
# }

# Close popup explanation
remDr$findElement("xpath", "//div[text() = 'Überspringen']")$clickElement()

# Wait and close Rabatt popup
Sys.sleep(10)
try(
  remDr$findElement(
    "xpath",
    "//button[contains(@class, 'CloseButton')]"
  )$clickElement(),
  TRUE
)

# Choos eone of three frequency Button
remDr$findElement(value = elements["frequency_choice"])$clickElement()

# Close popup: "regelmäßige Buchungen"
remDr$findElement("xpath", "//div[text() = 'Schließen']")$clickElement()

# Move to next page
remDr$findElement("xpath", "//div[text() = 'Fortfahren']")$clickElement()

# Move to next Month
# remDr$findElement("xpath", "//*[@id='arrow-left']")$clickElement()

# Pick date
remDr$findElement("xpath", elements["date_input"])$clickElement()

# Move to next page (keep time (12:00) constant)
remDr$findElement("xpath", "//div[text() = 'Fortfahren']")$clickElement()

# Pick duration
remDr$findElement(value = elements["duration_input"])$clickElement()


Sys.sleep(2)

# Extract number of results from html page
number_cleaners <- remDr$findElement(
  "xpath",
  elements["cleaners"] # "//div[contains(text(), 'Reinigungskräfte verfügbar')]"
)$getElementText() |>
  str_extract("\\d+") |>
  as.integer()


# ---- Scrape profiles ----

# create empty data frames for profiles and reivews (append in loop)
df_profiles_all <- data.frame()
df_reviews_all <- data.frame()

index <- 1 # # index of the next profile to click
seen_ids <- character(0) # store all scraped profile ids


while (length(seen_ids) < number_cleaners) {
  # 1.1 load most recent list of profile cards and create IDs
  profile_cards <- remDr$findElements("xpath", elements["profile_cards"])
  profile_ids <- sapply(profile_cards, function(card) card$getElementText()) |>
    str_remove_all("[^A-Za-z0-9]")

  # 1.2 load most recent list of profiles and keep if not seen
  profiles_elements <- remDr$findElements("xpath", elements["profile_details"])
  profiles_not_seen <- profiles_elements[!(profile_ids %in% seen_ids)]

  # 2. scrape new profiles: loop over profile elements and extract data
  message(length(profiles_not_seen), " new Profiles to scrape ...")
  for (profile in profiles_not_seen) {
    # 2.1 open profile page
    profile$clickElement()
    Sys.sleep(sample(c(0.5, 1, 1.5), 1))

    # 2.2 ---- Extract profile data ----
    profile_html <- remDr$getPageSource()[[1]] |>
      read_html() |>
      html_element("div[data-testid='CandidateModal']")

    df_profile <- data.frame(
      name = profile_html |>
        html_node(xpath = "div/div/div[2]") |>
        html_text(),
      place = profile_html |>
        html_node(xpath = "div/div[3]/div/div/div[1]/div[1]") |>
        html_text(),
      price = profile_html |>
        html_node(xpath = "div/div[3]/div/div/div[1]/div[2]/div") |>
        html_text() |>
        str_extract("[0-9.]+") |>
        as.numeric(),
      rating = profile_html |>
        html_node(xpath = "div/div[3]/div/div/div[2]/div[1]/div") |>
        html_text() |>
        as.numeric(),
      ratings = profile_html |>
        html_node(xpath = "div/div[3]/div/div/div[2]/div[3]/div/div[1]/div") |>
        html_text() |>
        str_extract("\\d+") |>
        as.integer(),
      hires = profile_html |>
        html_node(xpath = "div/div[3]/div/div/div[2]/div[2]/div") |>
        html_text() |>
        str_extract("\\d+") |>
        as.integer(),
      checklist = profile_html |>
        html_element(
          xpath = "//div[contains(text(), 'Checkliste:')]/following-sibling::div[2]"
        ) |>
        html_nodes("div") |>
        html_text() |>
        unique() |>
        paste(collapse = ":|;"),
      about_text = html_node(
        profile_html,
        xpath = "//div[contains(text(), 'Über mich:')]/following-sibling::div[2]"
      ) |>
        html_text(),
      img_src = profile_html |>
        html_node("img") |>
        html_attr("src"),
      query_date = query_date,
      query_plz = query_values[["plz"]][["Bremen"]]
    ) |>
      mutate(
        id = paste(
          str_replace_all(name, " ", "_"),
          as.character(price),
          as.character(rating),
          as.character(hires),
          sep = "_"
        )
      )

    # 2.1 add profile data to df
    df_profiles_all <- rbind(df_profiles_all, df_profile)

    # 2.3 ---- Extract reviews ----
    # 2.3.1 Get elements from review list
    rev_elements <- profile_html |>
      html_elements(
        xpath = "//div[contains(text(), 'Bewertungen:')]/following-sibling::div"
      )

    # 2.3.2 Extract review data
    df_reviews <- tibble(
      id = df_profile$id,
      rev_name = rev_elements |>
        html_nodes(xpath = "div[1]/div[1]") |>
        html_text(),
      rev_rating = rev_elements |>
        html_nodes(xpath = "div[1]/div[2]/div[6]") |>
        html_text(),
      revtext = rev_elements |>
        html_nodes(xpath = "div[2]") |>
        html_text(),
      query_date = query_date,
      query_plz = query_values[["plz"]][["Bremen"]]
    )

    # 2.3.3 add review data to df
    df_reviews_all <- bind_rows(df_reviews_all, df_reviews)

    message(
      "Profile ",
      index,
      ": ",
      df_profile$name,
      " with ",
      nrow(df_reviews),
      " reviews"
    )

    index = index + 1

    # 2.4close/leave profile page
    remDr$findElement("xpath", elements["profile_close"])$clickElement()
    Sys.sleep(sample(c(0.5, 1, 1.5), 1))
  }

  # 3. add ids of new profiles to seen ids
  seen_ids <- c(seen_ids, profile_ids) |> unique()

  # 4. scroll down to load new profiles
  message(
    length(seen_ids),
    " Profiles of ",
    number_cleaners,
    " scraped, scroll down for more ..."
  )
  element_body <- remDr$findElement("css", "body")
  element_body$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
}


# 5. save data
dataframes <- list(
  "profiles" = df_profiles_all,
  "reviews" = df_reviews_all
)

for (data in names(dataframes)) {
  message("Saving ", data)
  dataframes[[data]] |>
    write.csv(
      file = sprintf(
        "data/scraping/helpling/%s_%s_%s.csv",
        data,
        query_values[["plz"]][["Bremen"]],
        query_date
      ),
      row.names = FALSE
    )
}


# 6. go back
for (i in 1:5) {
  remDr$goBack()
  Sys.sleep(sample(c(0.5, 1, 1.5), 1))
}

# close the browser
try(remDr$close(), FALSE)

# stop the selenium server
rD[["server"]]$stop()
