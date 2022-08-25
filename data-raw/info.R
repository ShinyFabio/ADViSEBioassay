library(dplyr)
library(readxl)
library(shiny)

info = readxl::read_xlsx("C:\\Users\\fabio\\Desktop\\ADViSEBioassay\\data-raw\\info organismi marini_rev.xlsx") %>% group_by(Chemical_code) %>% 
  dplyr::mutate(Info = dplyr::case_when(!is.na(Chemical_code) ~ as.character(shiny::actionButton(
    label = "Click here", 
    inputId = paste0("butt_", Chemical_code),
    onclick = 'Shiny.setInputValue(\"infoprod_select_button\", this.id, {priority: \"event\"})')))) %>%
  dplyr::ungroup()

usethis::use_data(info, overwrite = TRUE)