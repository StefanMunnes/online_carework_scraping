library(dplyr)
library(readr)
library(stringr)

# Set seed for reproducibility of random sampling
set.seed(161312)

# ---- 1. betreut (download sampled images) ----
# list CSV files containing profile information from overview page
files_overview <- list.files(
  "data/betreut/profiles/",
  pattern = "profiles_overview_",
  full.names = TRUE
)

# read and combine all overview csv files into one data frame
betreut_profiles <- lapply(files_overview, read_csv2) |> bind_rows()

# extract unique non-missing image URLs and randomly sample 40 for downloading
betreut_urls <- betreut_profiles |>
  filter(!is.na(img_src)) |>
  pull(img_src) |>
  unique() |>
  sample(40)

# download each sampled image with a constructed file name.
for (url in betreut_urls) {
  # Extract numeric ID from URL to construct file name.
  file <- str_extract(url, "(\\d+)/profile", group = 1)
  file <- paste0("betreut_", file, ".jpg")

  message("Download file: ", file)

  # Download the image file in binary mode.
  download.file(
    url = url,
    destfile = paste0("image_classifier_eval/images/", file),
    mode = "wb"
  )

  # Pause for 3 seconds between downloads.
  Sys.sleep(3)
}

# ---- 2. helpling (load downloaded images for sample) ----
# List image files with avatar pattern from helpling directory.
helpling_files <- list.files(
  "data/helpling/images/",
  pattern = "avatar-",
  full.names = TRUE
) |>
  unique() |>
  sample(20)

# Copy sampled helpling images into the samples directory with a new prefix.
lapply(helpling_files, function(file) {
  file.copy(
    from = file,
    to = paste0("image_classifier_eval/images/helpling_", basename(file))
  )
})
