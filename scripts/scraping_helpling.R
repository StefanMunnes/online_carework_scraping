library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)


source("scripts/scraping_fcts.R")

# set variables to change for the search query
query_values <- c(
  plz = "10117",
  frequency = "Einmalig",
  duration = "2 Stunden",
  date = "30"
)

query_date <- format(Sys.time(), "%Y%m%d_%H%M")

# define constant objects for scraping
target_url <- "https://www.helpling.de/"

elements <- c(
  cookie_accept = "#cookiescript_accept",
  popup_close = "//button[@title = 'Close']",
  frequency_choice = paste0(
    "//div[contains(text(),'",
    query_values["frequency"],
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
user_agent <- "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_8; like Mac OS X) AppleWebKit/536.13 (KHTML, like Gecko)  Chrome/50.0.2440.333 Mobile Safari/600.1"

fprof <- makeFirefoxProfile(list(
  general.useragent.override = user_agent,
  browser.download.dir = "C:/Users/munnes/Documents/R_selenium_firefox"
))


# start remote driver
rD <- rsDriver(browser = "firefox", port = 4445L, extraCapabilities = fprof)

remDr <- rD$client

# set timeout
remDr$setTimeout(type = "implicit", milliseconds = 3000)
remDr$setTimeout(type = "page load", milliseconds = 5000)

# set window size for mobile (otherwise page is not loading correct)
remDr$setWindowSize(width = 700, height = 900)

# navigate to target page
remDr$navigate(target_url)

# validate cloudfare
# TODO remDr$findElement("css", "input[type = 'checkbox']")$clickElement()

# enter PLZ and click enter
remDr$findElement('css', '#bid_postcode')$sendKeysToElement(list(
  query_values["plz"],
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
remDr$findElement("xpath", "//*[@id='arrow-left']")$clickElement()

# Pick date
remDr$findElement("xpath", "//div[text() = '21']")$clickElement()

# Move to next page (keep time (12:00) constant)
remDr$findElement("xpath", "//div[text() = 'Fortfahren']")$clickElement()

# Pick duration
remDr$findElement(value = elements["duration_input"])$clickElement()

# Move to profile overview page
remDr$findElement(
  "xpath",
  "//div[text() = 'Jetzt Profile vergleichen']"
)$clickElement()

Sys.sleep(3)

page_content <- remDr$getPageSource()[[1]] |>
  read_html()

# Extract number of results from html page
number_cleaners <- page_content |>
  html_element(
    xpath = "//div[contains(text(), 'Reinigungskräfte verfügbar')]"
  ) |>
  html_text(trim = TRUE) |>
  str_extract("\\d+") |>
  as.integer()


# --- Extract Profile Information from Overview page (Gemini) ---

element_body <- remDr$findElement("css", "body")
element_body$clickElement()

# Scroll down to the bottom and wait until all elements are loaded
profiles_df <- data.frame()

repeat {
  page_source <- remDr$getPageSource()[[1]]

  if (!is.null(page_source) && nchar(page_source) > 0) {
    page_html <- read_html(page_source)
  } else {
    Sys.sleep(2) # wait briefly and retry once
    page_source <- remDr$getPageSource()[[1]]
    if (!is.null(page_source) && nchar(page_source) > 0) {
      page_html <- read_html(page_source)
    } else {
      stop("Page source is still empty after retrying.")
    }
  }

  profile_cards <- page_html |>
    html_elements("div[data-testid='CandidateCard']")

  profile_texts <- profile_cards |> html_text()

  avatar_urls <- profile_cards %>%
    html_node("img") |>
    html_attr("src")

  profiles_df_new <- data.frame(
    name = str_extract(profile_texts, "(.+?)[0-9]+", 1),
    price = str_extract(profile_texts, "([0-9.]+)€", 1) |> as.numeric(),
    rating = str_extract(profile_texts, "Std([0-9.]+)", 1) |> as.numeric(),
    ratings = str_extract(profile_texts, "\\(([0-9]+)\\)", 1) |> as.integer(),
    cleanings = str_extract(profile_texts, "([< ]\\d+) Reinigung", 1),
    image = grepl("uploads/profile/picture", avatar_urls),
    avatar_gender = str_extract(avatar_urls, "avatar_(her|him)", 1),
    avatar_url = avatar_urls
  )

  profiles_df <- rbind(profiles_df, profiles_df_new) |> distinct()

  # Check if all profiles have been loaded, otherwise scroll down
  if (nrow(profiles_df) < number_cleaners) {
    message(sprintf(
      "Only %d of %d profiles loaded. Scrolling down...",
      nrow(profiles_df),
      number_cleaners
    ))

    element_body$sendKeysToElement(list(key = "end"))

    Sys.sleep(sample(c(1, 1.5, 2), 1))
  } else {
    message("All ", number_cleaners, " profiles loaded.")
    break
  }
}


page_html <- read_html(page_source)

element_candidate <- page_html |>
  html_element("div[data-testid='CandidateModal']")

profile_df <- data.frame(
  name = element_candidate |>
    html_node(xpath = "div/div/div[2]") |>
    html_text(),
  img_src = element_candidate |>
    html_node("img") |>
    html_attr("src"),
  place = element_candidate |>
    html_node(xpath = "div/div[3]/div/div/div[1]/div[1]") |>
    html_text(),
  price = element_candidate |>
    html_node(xpath = "div/div[3]/div/div/div[1]/div[2]/div") |>
    html_text(),
  rating = element_candidate |>
    html_node(xpath = "div/div[3]/div/div/div[2]/div[1]/div") |>
    html_text(),
  hires = element_candidate |>
    html_node(xpath = "div/div[3]/div/div/div[2]/div[2]/div") |>
    html_text(),
  ratings = element_candidate |>
    html_node(xpath = "div/div[3]/div/div/div[2]/div[3]/div/div[1]/div") |>
    html_text(),
  checklist = element_candidate |>
    html_element(
      xpath = "//div[contains(text(), 'Checkliste:')]/following-sibling::div[2]"
    ) |>
    html_nodes("div") |>
    html_text() |>
    unique() |>
    paste(collapse = ":|;"),
  about_text = html_node(
    element_candidate,
    xpath = "//div[contains(text(), 'Über mich:')]/following-sibling::div[2]"
  ) |>
    html_text()
)


rev_elements <- element_candidate |>
  html_elements(
    xpath = "//div[contains(text(), 'Bewertungen:')]/following-sibling::div"
  )

rev_data <- tibble(
  name = rev_elements |>
    html_nodes(xpath = "div[1]/div[1]") |>
    html_text(),
  ratings = rev_elements |>
    html_nodes(xpath = "div[1]/div[2]/div[6]") |>
    html_text(),
  text = rev_elements |>
    html_nodes(xpath = "div[2]") |>
    html_text()
)


# Path Name   /html/body/div[14]/div/div[2]/div/div[2]/div[2]
# Path review /html/body/div[14]/div/div[2]/div/div[3]/div/div/div[5]/div/div[4]

# Download and save image locally
remDr$navigate(profiles_df$avatar_url[1])

current_img_url <- remDr$getCurrentUrl()[[1]]

httr::GET(
  current_img_url,
  httr::write_disk("tmp/helpling_avatar_1.jpg", overwrite = TRUE)
)


# --- Function to Extract Data from a Single Card ---
extract_profile_data <- function(card_node) {
  # Extract Name (Find the specific div containing the name)
  # This selector targets the div holding the name based on its structure relative to the image and price
  # It looks for a div that's a sibling of the image container and precedes the price container
  name_node <- card_node |>
    html_element(
      "div > div > div:nth-child(1) > div:nth-child(2) > div[dir='auto']"
    )
  name <- name_node |> html_text(trim = TRUE)

  # Extract Price
  price_node <- card_node |>
    html_element("div[data-testid='CandidateCard:HourPrice'] > div[dir='auto']")
  price_text <- price_node |> html_text(trim = TRUE)
  # Clean price (remove currency symbol and '/ Std', convert comma to dot)
  price <- price_text |>
    str_remove_all("[€€ / Std]") |> # Adjusted regex slightly for robustness
    str_replace(",", ".") |>
    str_trim() |>
    as.numeric()

  # --- Revised Rating and Cleanings Extraction ---
  star_rating <- NA_real_
  review_count <- NA_integer_
  cleanings_count <- NA_integer_

  # Find the main container holding rating, cleanings, details link etc.
  # This seems to be the second div within the card's main clickable area
  details_container <- card_node |>
    html_element("div > div > div:nth-child(2)")

  if (!is.na(details_container)) {
    # Get all direct child divs within this container
    sub_containers <- details_container |>
      html_elements(xpath = "./div")

    # Iterate through the direct children divs to find rating and cleanings
    for (sub_container in sub_containers) {
      # Try to find the text node within the common inner structure
      text_node <- sub_container |> html_element("div > div > div[dir='auto']")

      if (!is.na(text_node)) {
        current_text <- text_node |> html_text(trim = TRUE)

        # Check for Rating pattern (e.g., "5 (12)") and ensure not already found
        if (
          is.na(star_rating) &&
            str_detect(current_text, "^[\\d\\.]+\\s*\\(\\d+\\)$")
        ) {
          star_rating <- current_text |>
            str_extract("^[\\d\\.]+") |>
            as.numeric()
          review_count <- current_text |>
            str_extract("\\(\\d+\\)") |>
            str_remove_all("[\\(\\)]") |>
            as.integer()
        }

        # Check for Cleanings pattern (text containing "Reinigung") and ensure not already found
        if (is.na(cleanings_count) && str_detect(current_text, "Reinigung")) {
          cleanings_count <- current_text |>
            str_extract("^\\d+") |>
            as.integer()
        }
      }
      # Optimization: If we've found both, we can stop looping
      if (!is.na(star_rating) && !is.na(cleanings_count)) {
        break
      }
    }
  }

  # Return data as a list
  list(
    Name = ifelse(is.na(name), "N/A", name),
    Price_EUR_per_Hour = ifelse(is.na(price), NA_real_, price),
    Star_Rating = star_rating, # Will be NA if not found
    Review_Count = review_count, # Will be NA if not found
    Cleanings_Count = cleanings_count # Will be NA if not found
  )
}


safe_extract <- possibly(
  extract_profile_data,
  otherwise = list(
    Name = "Error",
    Price_EUR_per_Hour = NA_real_,
    Star_Rating = NA_real_,
    Review_Count = NA_integer_,
    Cleanings_Count = NA_integer_
  )
)

all_profiles_data <- map_dfr(profile_cards, safe_extract)


# Extract first profile elements
elements_profiles <- remDr$findElements(
  "xpath",
  "//div[text() = 'Siehe Details']"
)


element_body <- remDr$findElement("css", "body")
element_body$clickElement()

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

  Sys.sleep(sample(c(1, 1.5, 2), 1))

  # Update the list of elements
  elements_profiles <- remDr$findElements(
    "xpath",
    "//div[text() = 'Siehe Details']"
  )
}

element_body$sendKeysToElement(list(key = "home"))


elements_profiles[[2]]$clickElement()

remDr$goBack()

element_body$sendKeysToElement(list(key = "end"))


# Initialize counter and loop parameters
i <- 1
batch_size <- 14

repeat {
  # Refresh the list of profiles each time due to DOM updates after scrolling
  elements_profiles <- remDr$findElements(
    "xpath",
    "//div[text() = 'Siehe Details']"
  )

  # Break the loop if no more profiles are found
  if (i > length(elements_profiles)) {
    message("No more profiles found. Scraping completed.")
    break
  }

  # Click the profile, then go back
  message(sprintf(
    "Clicking profile number %d out of %d",
    i,
    length(elements_profiles)
  ))
  elements_profiles[[i]]$clickElement()
  Sys.sleep(1) # wait for profile to load (adjust as needed)

  # Extract HTML and parse profile data
  profile_html <- read_html(remDr$getPageSource()[[1]])

  avatar_gender <- profile_html |>
    html_elements(
      xpath = "/html/body/div[14]/div/div[2]/div/div[2]/div[1]/div/img"
    ) |>
    html_attr("src") |>
    str_extract("avatar_(her|him)", group = 1)

  message("Avatar gender: ", avatar_gender)

  remDr$goBack()
  Sys.sleep(1) # wait for main page to reload completely

  # After every batch of profiles, scroll down to load new profiles
  if (i %% batch_size == 0) {
    message("Scrolling down to load more profiles...")
    element_body$sendKeysToElement(list(key = "end"))
    Sys.sleep(2) # wait for new profiles to load after scrolling
  }

  # Increment the counter
  i <- i + 1
}


# extract src of image with xpath with rvest
avatar_gender <- profile_html |>
  html_elements(
    xpath = "/html/body/div[13]/div/div[2]/div/div[2]/div[1]/div/img"
  ) |>
  html_attr("src") |>
  str_extract("avatar_(her|him)", group = 1)

avatar_image <- is.na(avatar_gender)


# --- Start: Loop through profiles to extract data ---
file_output <- paste0("data/helpling/scraping/profiles_", query_date, ".csv")


if (file.exists(file_output)) file.remove(file_output)


# Click the profile review button to open the profile popup
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
  # TODO externe Bewertungen erfassen
  query_plz = query_values["plz"],
  query_frequency = query_values["frequency"],
  query_duration = query_values["duration"],
  query_date = query_values["date"]
) |>
  mutate(
    rev_n = str_count(rev_ratings, ":|;") + 1,
    .before = rev_names
  )


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
    query_frequency = query_values["frequency"],
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
