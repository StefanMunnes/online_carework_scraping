library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)


source("scripts/scraping_fcts.R")

# set variables to change for the search query
query_values <- c(
  plz = "10117",
  repetition = "Einmalig",
  duration = "2 Stunden",
  date = "30"
)

query_date <- format(Sys.time(), "%Y%m%d_%H%M")

# define constant objects for scraping
target_url <- "https://www.helpling.de/"

elements <- c(
  cookie_accept = "#cookiescript_accept",
  popup_close = "//button[@title = 'Close']",
  repetition_choice = paste0(
    "//div[contains(text(),'",
    query_values["repetition"],
    "')]"
  ),
  date_field = "//div[contains(text(),'Datum wählen')]",
  date_input = paste0("//div[text()='", query_values["date"], "']"),
  duration_field = "//div[text()='Dauer auswählen']",
  duration_input = paste0(
    "//div[contains(text(),'",
    query_values["duration"],
    "')]"
  ),
  show_results = "//div[text() = 'Reinigungskräfte anzeigen']",
  cleaners = ".cleaners-count",
  profile_rating = "//button[text() = 'Bewertungen']",
  next_arrow = ".profile__next-arrow"
)

# define user agent by creating firefox profile
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/135.0"
fprof <- makeFirefoxProfile(list(general.useragent.override = user_agent))


# start remote driver
rD <- rsDriver(browser = "firefox", port = 4445L, extraCapabilities = fprof)

remDr <- rD$client


# navigate to target page
remDr$navigate(target_url)
Sys.sleep(1)


# enter PLZ and click enter
remDr$findElement('css', '#bid_postcode')$sendKeysToElement(list(
  query_values["plz"],
  key = "enter"
))

Sys.sleep(2)


# check for wrong redirect if no results for PLZ
if (!grepl("step-zero", remDr$getCurrentUrl())) {
  message("Redirect to wrong page. Move to previous page.")
  remDr$goBack()
} else {
  message("Redirect to correct page.")
}


# accept cookie banner
Sys.sleep(2)
try(remDr$findElement("css", elements["cookie_accept"])$clickElement(), TRUE)


# Chooseone of three Repetition Button
remDr$findElement(value = elements["repetition_choice"])$clickElement()
Sys.sleep(1)

# Move one page down to get the next fields visible
element_body <- remDr$findElement("css", "body")

element_body$sendKeysToElement(list(key = "page_down"))

Sys.sleep(1)


# Choose duration
remDr$findElement(value = elements["duration_field"])$clickElement()
remDr$findElement(value = elements["duration_input"])$clickElement()
Sys.sleep(1)

# Move one page down to get the next fields visible
element_body$sendKeysToElement(list(key = "page_down"))
Sys.sleep(1)

# Choose date
remDr$findElement(value = elements["date_field"])$clickElement()
remDr$findElement(value = elements["date_input"])$clickElement()
Sys.sleep(1)

# click to show results
remDr$findElement(value = elements["show_results"])$clickElement()

Sys.sleep(5)


# Extract number of results from html page
number_cleaners <- remDr$getPageSource()[[1]] |>
  read_html() |>
  html_element(css = elements["cleaners"]) |>
  html_text(trim = TRUE) |>
  as.integer()


# Extract first profile elements
elements_profiles <- remDr$findElements(value = elements["profile_rating"])
# TODO check if correct profile information, otherwise: break and reload

# Scroll down to the bottom and wait until all elements are loaded
while (length(elements_profiles) < number_cleaners) {
  message(
    "Only ",
    length(elements_profiles),
    " of ",
    number_cleaners,
    " elements loaded. Scrolling down and waiting."
  )

  # Scroll to the bottom of the page and wait for profile elements to load
  element_body$sendKeysToElement(list(key = "end"))

  Sys.sleep(2)

  # Update the list of elements
  elements_profiles <- remDr$findElements(value = elements["profile_rating"])
}

element_body$sendKeysToElement(list(key = "home"))


# --- Start: Loop through profiles to extract data ---
file_output <- paste0("data/helpling/scraping/profiles_", query_date, ".csv")

if (file.exists(file_output)) file.remove(file_output)


# Click the profile review button to open the profile popup
elements_profiles[[1]]$clickElement()
Sys.sleep(2)


all_profiles <- lapply(seq_along(elements_profiles), function(x) {
  # Extract HTML and parse profile data
  profile_html <- read_html(remDr$getPageSource()[[1]])

  # Extract information from last 90 days box
  if (is.na(get_text(".about-me", profile_html))) {
    div_pos <- 1
  } else {
    div_pos <- 2
  }

  tmp_l90 <- html_elements(
    profile_html,
    xpath = paste0(
      "//div[@class='reviews__wrapper']/div[1]/div[",
      div_pos,
      "]/div[2]/div/div/div"
    )
  ) |>
    html_text()

  # if just 8 elements in vector, add NA in second position
  if (length(tmp_l90) == 8) {
    tmp_l90 <- c(tmp_l90[1], NA, tmp_l90[2:8])
  }

  # Extract profile data and store in data frame
  profile <- data.frame(
    name = get_text(".card-header__name", profile_html),
    price = get_text(".price-value", profile_html) |> str_remove("€ /Std"),
    rating = get_text(
      ".short-rating > .short-rating__wrapper.short-rating__wrapper--rating > .short-rating__number",
      profile_html
    ),
    cleanings = get_text(
      ".short-rating > .short-rating__wrapper.short-rating__wrapper--counter > .short-rating__number",
      profile_html
    ),
    info = get_text(".card-info__text", profile_html, TRUE) |>
      paste(collapse = ":|;"),
    text_headline = get_text(".about-me > h4", profile_html) |>
      str_replace_all("\\n", " ") |>
      str_squish(),
    text_body = get_text(".about-me > .subheader", profile_html) |>
      str_replace_all("\\n", " ") |>
      str_squish(),
    l90_reliability = tmp_l90[1], # |> str_remove(" Zuverlässigkeit"),
    l90_bookingprob = tmp_l90[2], # |> str_remove(" Buchungswahrscheinlichkeit"),
    l90_longtermcust = tmp_l90[3], # |> str_remove(" Langzeitkunden"),
    l90_cleanings = tmp_l90[5],
    l90_cancelshort = tmp_l90[7],
    l90_cancelprov = tmp_l90[9],
    rev_names = get_text(
      ".medium-6.shrink.columns.name",
      profile_html,
      TRUE
    ) |>
      paste(collapse = ":|;"),
    rev_ratings = get_text(".card-header__rating-text", profile_html, TRUE) |>
      str_remove("/.+") |>
      as.numeric() |>
      paste(collapse = ":|;"),
    rev_texts = get_text(
      ".reviews__wrapper > div > div > div > .row > .medium-12.columns",
      profile_html,
      TRUE
    ) |>
      str_replace_all("\\n", " ") |>
      str_squish() |>
      paste(collapse = ":|;"),
    # TODO exgterne Bewertungen erfassen
    query_plz = query_values["plz"],
    query_repetition = query_values["repetition"],
    query_duration = query_values["duration"],
    query_date = query_values["date"]
  ) |>
    mutate(
      rev_n = str_count(rev_ratings, ":|;") + 1,
      .before = rev_names
    )

  # Print profile number and name
  message("Profile: ", x, " of ", length(elements_profiles), ": ", profile$name)

  write.table(
    profile,
    file = file_output,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(file_output),
    append = file.exists(file_output)
  )

  # Click the next arrow
  remDr$findElement("css", elements["next_arrow"])$clickElement()
  Sys.sleep(sample(c(1, 1.5, 2), 1))

  # Close Popup Rabatt
  try(remDr$findElement(value = elements["popup_close"])$clickElement(), TRUE)

  return(profile)
})

# Combine all profiles into one data frame
if (length(all_profiles) > 0) {
  final_profiles_df <- dplyr::bind_rows(all_profiles)
} else {
  message("No profiles were successfully extracted.")
}

save(final_profiles_df, file = "data/helpling/scraping/final_profiles_df.RData")

#View(final_profiles_df)

# close the browser
try(remDr$close(), FALSE)

# stop the selenium server
rD[["server"]]$stop()
