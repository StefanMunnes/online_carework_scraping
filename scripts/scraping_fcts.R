get_text <- function(selector, node, multi = FALSE) {
  if (multi) {
    element <- node %>% html_elements(css = selector)
  } else {
    element <- node %>% html_element(css = selector)
  }
  if (any(is.na(element))) {
    return(NA_character_)
  } else {
    return(html_text(element, trim = TRUE))
  }
}
