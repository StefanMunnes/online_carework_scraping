library(dplyr)
library(readr)
library(stringr)


files_overview <- list.files(
  "data/betreut/profiles/",
  pattern = "profiles_overview_",
  full.names = TRUE
)

image_urls <- lapply(files_overview, read_csv2) |>
  bind_rows() |>
  filter(!is.na(img_src)) |>
  distinct(img_src) |>
  mutate(
    image_file_name = str_extract(img_src, "member/(.+?)/profile", group = 1) |>
      str_replace_all(pattern = "/", replacement = "_") |>
      str_c(".jpg")
  ) |>
  select(image_file_name, img_src)


path_imgs <- "data/betreut/images/"

# check for
existing_imgs <- list.files(path_imgs)

if (length(existing_imgs) > 0) {
  image_urls <- image_urls[!(image_urls$image_file_name %in% existing_imgs), ]
}


for (row in seq(nrow(image_urls))) {
  message(
    row,
    "/",
    nrow(image_urls),
    ": ",
    image_urls$img_src[row],
    appendLF = FALSE
  )

  tryCatch(
    {
      download.file(
        url = image_urls$img_src[row],
        destfile = paste0(path_imgs, image_urls$image_file_name[row]),
        mode = "wb",
        quiet = TRUE
      )

      message(" (Downloaded)")
    },
    error = function(e) {
      message(" (Download failed)")
    }
  )

  Sys.sleep(3)
}
