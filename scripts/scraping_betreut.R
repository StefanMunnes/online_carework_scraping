library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)

source("scripts/scraping_fcts.R")

# https://www.betreut.de/de-de/profiles?sort=bestMatch&order=asc&isRefinedSearch=true&verticalId=childCare&radius=20&geoRegionId=POSTCODE-DE-10117&geoRegionSearch=10117+Mitte
# https://www.betreut.de/de-de/profiles?sort=bestMatch&verticalId=childCare&radius=20&geoRegionId=POSTCODE-DE-10117

# set variables to change for the search query
query_values <- c(
  plz = "10117"
)

query_date <- format(Sys.time(), "%Y%m%d_%H%M")

base_url <- "https://www.betreut.de"

login_url <- "https://www.betreut.de/de-de/login"

search_url <- paste0(
  base_url,
  "/de-de/profiles?sort=bestMatch&verticalId=childCare&radius=20&geoRegionId=POSTCODE-DE-",
  query_values["plz"]
)


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
login_data <- readLines("scripts/logins.txt")


# define user agent by creating firefox profile
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/135.0"
# user_agent <- "Mozilla/5.0 (iPhone; CPU iPhone OS 8_0_8; like Mac OS X) AppleWebKit/536.13 (KHTML, like Gecko)  Chrome/50.0.2440.333 Mobile Safari/600.1"
# Mozilla/5.0 (iPhone; CPU iPhone OS 10_3 like Mac OS X) AppleWebKit/602.1.50 (KHTML, like Gecko) CriOS/56.0.2924.75 Mobile/14E5239e Safari/602.1
fprof <- makeFirefoxProfile(list(general.useragent.override = user_agent))


# start remote driver
rD <- rsDriver(browser = "firefox", port = 4445L, extraCapabilities = fprof)

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


# navigate to parametrized search page
remDr$navigate(search_url)


# Anzahl Anbieter: 2.937 Kinderbetreuung Ergebnisse (+20km)
number_profiles <- remDr$getPageSource()[[1]] |>
  read_html() |>
  html_element(css = elements["filter_head"]) |>
  html_text(trim = TRUE) |>
  str_extract("^[0-9.]{1,6}") |>
  str_remove_all("\\.") |>
  as.integer()


# get all profile urls from all pages
file_out_profiles <- "data/scraping/betreut/profiles_20250428_1330.csv"

if (file.exists(file_out_profiles)) {
  file.remove(file_out_profiles)
}

file_out_reviews <- "data/scraping/betreut/reviews_20250428_1330.csv"

if (file.exists(file_out_reviews)) {
  file.remove(file_out_reviews)
}

page <- 1

repeat {
  # Get all profile links from this page
  page_links <- remDr$getPageSource()[[1]] |>
    read_html() |>
    html_elements(css = elements["profile_link"])

  profile_urls <- html_attr(page_links, 'href')

  # loop through profiles: enter, get data, go back
  profiles_ls <- lapply(seq_along(profile_urls), function(profile) {
    message("Profile: ", profile, " of ", length(profile_urls))

    profile_url <- paste0("https://www.betreut.de", profile_urls[profile])

    remDr$navigate(profile_url)

    # expand text description
    try(remDr$findElement(value = elements["show_more"])$clickElement(), TRUE)

    # read static html for extraction
    profile_html <- remDr$getPageSource()[[1]] |>
      read_html()

    # prepare info from profile overview for extraction in data.frame
    headbox_infoline <- profile_html |>
      html_elements(
      xpath = "//div[@id='section-header']/div[2]/div/div/span"
    ) |>
      html_text() |>
      paste(collapse = " ")
    
    about_infoline <- get_text(".MuiTypography-h3", profile_html)

    # extract profile data
    profile <- data.frame(
      name = profile_html |>
        html_elements(
          xpath = "//div[@id='section-header']/div[2]/div/div/span"
        ) |>
        html_text() |>
        pluck(1),
      picture = ifelse(
        # TODO check if this code works
        profile_html |>
          html_element(css = "#section-header") |>
          html_node("img") |>
          length() >
          0,
        TRUE,
        FALSE
      ),
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
      headbox_experience = str_extract(headbox_infoline, "([0-9]{1,2}) Jahre Erf.", group = 1) |> as.integer(),
      headbox_price = str_extract(headbox_infoline, "([0-9]{1,2})€", group = 1) |> as.integer(),
      headbox_responserate = str_extract(headbox_infoline, "([0-9]{1,2})%", group = 1) |> as.integer(),
      about_age = str_extract(about_infoline, "([1-9][0-9]) Jahre", group = 1) |> as.integer(),
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
        paste0(collapse = ":|;")
    )

    # get and store reviews
    reviews <- profile_html |>
      html_elements(
        css = ".MuiGrid-root.MuiGrid-container.MuiGrid-spacing-xs-1.MuiGrid-item"
      )

    review_data <- lapply(seq_along(reviews), function(review) {
      review_infos <- reviews[review] |> html_nodes("span") |> html_text()

      review_df <- data.frame(
        review_name = review_infos[1],
        review_date = review_infos[3],
        review_rating = reviews[review] |>
          html_elements(".MuiRating-icon.MuiRating-iconFilled") |>
          length(),
        review_text = get_text(
          ".MuiGrid-root.MuiGrid-item.MuiGrid-grid-xs-12",
          reviews[review]
        ),
        url = profile_url
      )

      return(review_df)
    }) |>
      bind_rows() |>
      filter(review_rating > 0)

    if (nrow(review_data) > 0) {
      message("Write ", nrow(review_data), " reviews to csv file: ", file_out_reviews)
      write.table(
        review_data,
        file = file_out_reviews,
        sep = ",",
        row.names = FALSE,
        col.names = !file.exists(file_out_reviews),
        append = file.exists(file_out_reviews)
      )
    }
    

    # got back
    remDr$goBack()

    return(profile)
  })

  profiles_df <- bind_rows(profiles_ls)

  # add oerview information to profile data frame
  page_profiles_df <- lapply(page_links, function(profile) {
    data.frame(
      short_location = profile |> html_node(".hidden-xs") |> html_text(trim = TRUE) |> str_remove("\\|") |> str_squish(),
      short_ratings = profile |> html_node(".stars-text") |> html_text() |> as.integer(),
      short_verification = profile |> html_node(".verification") |> html_text() |> str_extract("\\d+") |> as.integer(),
      short_hires = profile |> html_node(".hireCount") |> html_text() |> str_extract("\\d+") |> as.integer(),
      short_oneliner = profile |> html_node("b") |> html_text()
    )
  }) |>
    bind_rows()

  profiles_all_df <- bind_cols(page_profiles_df, profiles_df)

  # add url (as ID), query plz, and number of reviews
  profiles_all_df$url <- profile_urls
  profiles_all_df$query_plz <- query_values["plz"]
  profiles_all_df$query_profiles_n <- number_profiles


  # write profiles data.frame to csv
  message(nrow(profiles_all_df), " profiles on page ", page, " found.")

  message("Write profiles to csv file: ", file_out_profiles)

  write.table(
    profiles_all_df,
    file = file_out_profiles,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(file_out_profiles),
    append = file.exists(file_out_profiles)
  )

  # Look for next page arrow and click if available, else break
  # tryCatch({
  #     next_arrow <- remDr$findElement(using = "css", value = elements["next_page"])
  #     next_arrow$clickElement()
  #     message("Move to next page: ", page)
  #   page <- page + 1
  # }, error = function(e) {
  #     message("Last page reached.")
  #     break
  #   })
  next_arrow <- remDr$findElement(using = "css", value = elements["next_page"])

  # Jump to next page if available
  if (page > 2) {
    message("Last page reached.")
    break
  } else {
    page <- page + 1
    message("Move to next page: ", page)
    next_arrow$clickElement()
  }
}


################################################################################

# close the browser
try(remDr$close(), FALSE)

# stop the selenium server
rD[["server"]]$stop()
