library(dplyr)
library(stringr)

files_profiles <- list.files(
  "data/helpling/",
  pattern = "profiles_",
  full.names = TRUE
)


profiles_clean <- lapply(files_profiles, read.csv, stringsAsFactors = FALSE) |>
  bind_rows() |>
  group_by(name, place, about_text, img_src) |>
  mutate(
    query_plz_n = n(),
    query_plz = paste(query_plz, collapse = ";")
  ) |>
  arrange(price) |> # filter by price, to keep just lowest one per person
  filter(row_number() == 1) |>
  ungroup() |>
  mutate(
    checklist_ausweis = str_detect(checklist, "Ausweis"),
    checklist_haustiere = str_detect(checklist, "Haustiere"),
    checklist_fenster = str_detect(checklist, "Fenster"),
    checklist_gewerbeschein = str_detect(checklist, "Gewerbeschein"),
    checklist_fuehrungszeugnis = str_detect(checklist, "Führungszeugnis"),
    checklist_agency = str_detect(checklist, "agency"),
    checklist_industry_trained = str_detect(checklist, "Industry"),
    img_picture = str_detect(img_src, "uploads/profile/picture"),
    img_file = ifelse(img_picture, basename(img_src), NA),
    gender = case_when(
      str_detect(img_src, "avatar_him") ~ "male",
      str_detect(img_src, "avatar_her") ~ "female",
      .default = NA_character_
    ),
    gender_src = case_when(
      str_detect(img_src, "avatar_him") ~ "avatar",
      str_detect(img_src, "avatar_her") ~ "avatar",
      .default = NA_character_
    ),
    place = str_remove(place, ", Deutschland")
  )

write.csv2(
  profiles_clean,
  file = "data/helpling/profiles_clean.csv",
  row.names = FALSE
)
