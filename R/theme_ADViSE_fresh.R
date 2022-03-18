# Theme: bs4Dash theme with fresh package (ADViSE theme)------------------------------------------------------------------------------
#' @title theme_ADViSE_fresh
#' @description Poor Man's Flatly theme for a shinydashboard application edited
#' @import fresh
#'
#' @return Object produced by fresh
#' @seealso \code{\link{fresh}}
#' @export
#' 
#' 

theme_ADViSE_fresh = create_theme(
  
  bs4dash_status(primary = "#2aaae2", light = "#2aaae2"),
  
  bs4dash_sidebar_light(
    bg = "#2c2f76", #sfondo sidebar
    color = "#FFF", #colore testo
    hover_color = "#2aaae2", #colore testo quando evidenziato
    submenu_bg = "#2c2f76", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#2aaae2"
  ),
  
  bs4dash_layout(main_bg = "#FFF"),
  
  bs4dash_vars(
    
    navbar_light_color = "#2c2f76", #colore icone navbar 
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  )
)