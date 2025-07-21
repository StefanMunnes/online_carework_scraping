profiles_clean <- read.csv2(
  "data/helpling/profiles_clean.csv",
  stringsAsFactors = FALSE
)


# get image urls (without default avatars)
file_names <- unique(profiles_clean$img_file[profiles_clean$img_picture])

for (file in file_names) {
  file_path <- paste0("data/helpling/images/", file)

  message("Process image: ", file, appendLF = FALSE)

  if (file.exists(file_path)) {
    message(" (File already exists)")
    next
  }

  tryCatch(
    {
      download.file(
        url = url,
        destfile = paste0("data/helpling/images/", file),
        mode = "wb"
      )

      message(" (Downloaded)")
    },
    error = function(e) {
      message(" (Download failed)")
    }
  )

  Sys.sleep(sample(c(0.5, 1, 1.5), 1))
}


download.file(
  url = "https://h2-production-de.s3.eu-west-1.amazonaws.com/uploads/profile/picture/251832/avatar-1734971643620.png",
  destfile = "tmp/avatar.png",
  mode = "wb"
)
