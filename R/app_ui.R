#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(actionButton, column, insertTab, tabsetPanel))
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @import shinydashboard
#' @importFrom fresh use_theme
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shinyBS bsModal
#' 
#' @noRd
#' 


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    tags$link(rel = "stylesheet", type="text/css", href="www/custom_notifications.css"),
    #tags$link(rel = "stylesheet", type="text/css", href="www/custom_sidebarpanels.css"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/custom_dashboardheader_title.css")),

    dashboardPage(
      #title = "ADViSEBioassay",
 
      # header = bs4DashNavbar(
      #   skin = "light", status = "primary",
      #   title = dashboardBrand("ADViSEBioassay", color = "primary", 
      #                          href = "https://github.com/ShinyFabio/ADViSEBioassay", image = "www/logo_ADViSEBioassay.png"),
      #   
      #   rightUi = tags$li(class = "dropdown", actionButton(inputId  = "jumptohome", label =NULL, icon = icon("home"),status = "primary")) #
      # ),
      
      dashboardHeader(title = span(
        tagList(tags$img(src = "www/logo_ADViSEBioassay.png", width = "32px",style="margin-right: 4px;"), "ADViSEBioassay")),
        tags$li(class = "dropdown", actionBttn("jumptohome", icon = icon("home"), style = "stretch", size = "lg"))),
      

      sidebar = dashboardSidebar(
        #skin = "light", 
        #inputId = "sidebarState",
        
        sidebarUserPanel(name = textOutput("nome"),
                         subtitle = actionButton('change','Change', style='padding:0px; height:18px; font-size:90%'),
                         image = "www/userimage.png"
        ),
        # tags$div(
        #   class = "user-panel mt-3 pb-3 mb-3",
        #   fluidRow(
        #     column(3, tags$img(src = "www/userimage.png",style = "width:45px;", class = "img-circle elevation-2")),
        #     column(8, 
        #            fluidRow(textOutput("nome"), style = "color:white;"),
        #            fluidRow(actionButton('change','Change', 
        #                         style='padding:0px; height:18px; font-size:90%;background-color: #2c2f76;border-color: #2c2f76;color: white;'))
        #   ))),
        
        sidebarMenu(
          id = "sidebarmenu",
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("D1", tabName = "d1tab", icon = icon("table")),
          menuItem("Cytotoxicity", tabName = "cytotab",icon = icon("file-import"))
        )
      ),
      
      #controlbar = dashboardControlbar(),  #il controlbar a destra
      #footer = dashboardFooter(), ##aggiunge una barra sotto a tutto dove posso scrivere qualcosa

      body = dashboardBody(
        #fresh::use_theme(theme_ADViSE_fresh),
        theme_ADViSE,
        
        #####menu home ####
        tabItems(
          tabItem(
            tabName = "home",
            fluidRow(
              shinydashboard::box(width = 12, status = "primary",
                                  column(3, br(), tags$img(src = "www/advise_logo.png", width = "400px")),
                                  column(7,br(), br(), br(), 
                                         HTML("<h1 style = 'text-align: center;font-size: 53px;color: #0e3d51;'>
                     <strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Welcome to ADViSEBioassay!</strong>
                     </h1>")),
                     column(2, tags$img(src = "www/logo_ADViSEBioassay.png", width = "140px"), style = "text-align: right"))
              ),
            br(),
            fluidRow(column(12,wellPanel(
              h3(strong("ADViSEBioassay")," is a Shiny app for..........",style = "color: #0e3d51;")
            ))),
            br(),
            fluidRow(
              
              
              box(width = 3, status = "primary", title = h3(strong("Database"), style = "color: white; display:inline; margin-top: 0px;margin-bottom: 0px;"), solidHeader = T,
                  uiOutput("valbox_cyto"),
                  uiOutput("valbox_D1"),
                  div(actionButton("loaddatabase", label = HTML("&nbsp;Load database!"), icon("rocket"), class = "btn btn-primary btn-lg", style='padding:10px; font-size:140%; font-weight: bold;'),style = "text-align: center;")
                  #hr(),
                  
                  #actionButton("upaziendedata", HTML("&nbsp;Update database"), icon("file-upload"),  style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:110%; font-weight: bold;'),
                  #actionButton("download_cyto", "Download database", icon("download"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:110%; font-weight: bold;')
              )
              
              
              
            )

          ),
          
          
          #### menu cytotab ####
          tabItem(
            tabName = "cytotab",
            #tabBox(id = "tabsetcyto", width = 12, status = "primary",
            tabsetPanel(id = "tabsetcyto",

              tabPanel(
                "Data",
                box(width = 12, status = "primary",
                    fluidRow(column(
                      width = 1, style="width: 7rem;",
                      dropdownButton(
                        conditionalPanel(condition = "output.check_data == false",
                                         h5(strong("Summarized data")),
                                         materialSwitch("summ_viewtable", value = TRUE, status = "primary")
                        ),
                        circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                        tooltip = tooltipOptions(title = "Click to see options"))
                    )),
                    
                    shinycssloaders::withSpinner(DT::DTOutput("dtdata"))),
                fluidRow(
                  
                  # column(
                  #   2, style = "text-align:center;",
                  #   actionButton("upcyto_modalbutton", HTML("&nbsp;Add new data"), icon("file-upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;'),
                  #   strong(h3("or", style = "margin-top: 10px;")),
                  #   actionButton("upload_updated_cyto", "Upload database", icon("upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  # ),
                  
                  
                  column(
                    1, style = "text-align:right;padding-right: 2rem; width: 25rem;",
                    actionButton("upcyto_modalbutton", HTML("&nbsp;Add new data"), icon("file-upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                    ),
                  column(1, style = "width: 3rem; padding: 0px;text-align:center;", strong(h3("or", style = "margin-top: 10px;"))),
                  column(
                    1, style = "padding-right: 30rem; padding-left: 2rem;",
                    actionButton("upload_updated_cyto", "Upload database", icon("upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  column(
                    2, style = "text-align:center;",
                    actionButton("save_update", HTML("&nbsp;Save database"), icon("save"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:140%; font-weight: bold;')
                    ),
                  conditionalPanel(
                    condition = "output.checkupdated_cyto_fordownload == true",
                    column(
                      3, style = "text-align:center;",
                      downloadButton("download_updated_cyto", "Download database", style='padding:10px; font-size:140%; font-weight: bold;')
                      )
                  ),
                  column(
                    2, style = "text-align:center;",
                    actionButton("remove_update_cyto", HTML("&nbsp;Restore database"),icon("undo"), style='background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:140%; font-weight: bold;')
                  )
                  
                ),

                tags$head(tags$style("#upcyto_tab .modal-dialog{ min-width:170rem}")),
                tags$head(tags$style("#upcyto_tab .modal-body{ min-height:80rem}")),
                shinyBS::bsModal(
                  "upcyto_tab", "Update database", trigger = "upcyto_modalbutton", size = "large",
                  sidebarLayout(
                    sidebarPanel(width = 3,
                                 mod_load_cyto_ui("load_cyto_mod"),
                                 hr(),
                                 conditionalPanel(
                                   condition = "output.check_data_updated == false",
                                   div(actionButton("update_cyto_bttn", "Merge!",icon("edit"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:110%; font-weight: bold;')), style = "text-align: center;")
                                 
                    ),
                    mainPanel(
                      width = 9,
                      box(width = NULL, status = "primary",
                          fluidRow(
                            column(width = 1, style="width: 7rem;",
                                   dropdownButton(
                                     conditionalPanel(condition = "output.check_data_updated == false",
                                                      h5(strong("Summarized data")),
                                                      materialSwitch("summ_viewtable_updated", value = TRUE, status = "primary")
                                     ),
                                     circle = TRUE, status = "danger", icon = icon("cog"), width = "300px",
                                     tooltip = tooltipOptions(title = "Click to see options"))
                            )),
                          
                          shinycssloaders::withSpinner(DTOutput("newdata_cyto_DT")))
                      
                    )
                  )
                )
                
              ),
              
              
              ##### informative graphs #####
              tabPanel(
                "Data exploration",
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    awesomeRadio("seltype_infograph", "Plot type", choices = c("Data overview", "Model family", "Model type", "Product family", "Heatmap")),
                    conditionalPanel(
                      condition = "input.seltype_infograph == 'Product family'",
                      awesomeCheckbox("first50_prodfam", "Shows first 50 product family", value = TRUE)
                    )#,
                    # conditionalPanel(
                    #   condition = "input.seltype_infograph == 'Heatmap'",
                    #   selectInput("scale_heatinform", "Scale data", choices = c("None", "By row", "By column"))
                    # )
                  ),
                  mainPanel(
                    width = 10,
                    conditionalPanel(
                      condition = "input.seltype_infograph == 'Data overview'",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("countbarplot"))
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph == 'Model type'",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("modtype_barplto"))
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph == 'Model family'",
                      shinycssloaders::withSpinner(uiOutput("piemodfamilyplotUI")),
                      uiOutput("back"),
                      uiOutput("back1")
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph == 'Product family'",
                      shinycssloaders::withSpinner(uiOutput("prodfam_barplotUI"))
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph == 'Heatmap'",
                      InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_inform_output", layout = "1|(2-3)", width1 = 1000, height1 = 800)
                      
                    )
                    
                  )
                )

              ),
              
              ##### Barplot #####
              tabPanel(
                "Barplot",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("typeeval_bar", "Select a measure", choices = c("Cytotoxicity", "Vitality")),
                    fluidRow(
                      column(6, selectInput("model_filt_bar", "Filter Model type", choices = "")),
                      column(6, selectInput("family_filt_bar", "Filter Product Family", choices = ""))),
                    selectInput("purif_filt_bar", "Select a purification", choices = ""),
                    fluidRow(
                      column(6, awesomeCheckbox("addpoints_barplot", "Add points", value = TRUE)),
                      column(6, conditionalPanel(condition = "output.check_multID_bar1 == true",
                                                 awesomeCheckbox("facet_bar", "Faceting expID", value = FALSE)))
                    ),
                    div(actionButton("add_barplot", "Add another barplot", icon("plus")), style = "text-align:center;"),
                    conditionalPanel(
                      condition = "output.show_barplot2 == true",
                      br(),
                      selectInput("typeeval_bar2", "Select a measure", choices = c("Cytotoxicity", "Vitality")),
                      fluidRow(
                        column(6, selectInput("model_filt_bar2", "Filter Model type", choices = "")),
                        column(6,selectInput("family_filt_bar2", "Filter Product Family", choices = ""))),
                      selectInput("purif_filt_bar2", "Select a purification", choices = ""),
                      fluidRow(
                        column(6, awesomeCheckbox("addpoints_barplot2", "Add points", value = TRUE)),
                        column(6, conditionalPanel(condition = "output.check_multID_bar2 == true",
                                                   awesomeCheckbox("facet_bar2", "Faceting expID", value = FALSE)))
                      )
                    )
                  ),
                  
                  mainPanel(
                    width = 9,
                    shinycssloaders::withSpinner(plotly::plotlyOutput("barplot")),
                    conditionalPanel(
                      condition = "output.show_barplot2 == true",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("barplot2"))
                    )
                  )
                )
              ),
              
              
              ##### spiderplot ####
              tabPanel(
                "Spider Plot",
                mod_spiderplot_ui("spiderplot_cyto")
              ),
              
              
              ##### BubblePlot #####
              tabPanel(
                "Bubble Plot",
                mod_bubble_plot_ui("bubbleplot_cyto",  c("Corrected_value", "CV"))
              ),
              
              #### Heatmap ####
              tabPanel("Heatmap",
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           div(actionButton("makeheatmap", label = "Make Heatmap", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:130%; font-weight: bold;'), align= "center"),
                           br(),
                           h4(strong("Data filtering")),
                           fluidRow(
                             column(6,selectInput("typeeval_heat", "Select a measure", choices = c("Cytotoxicity", "Vitality"))),
                             column(6, selectInput("prod_filt_heatmap", "Select a Product Family", choices = ""))
                           ),
                           selectInput("purif_filt_heat", "Select a purification", choices = "", multiple = FALSE),
                           fluidRow(
                             column(6, selectInput("mod_filt_heatmap", "Filter Model type (rows)", choices = "",multiple = TRUE)),
                             column(6, selectInput("column_filt_heatmap", "Filter Product (columns)", choices = "",multiple = TRUE))
                           ),
                           
                           
                           fluidRow(
                             column(6, selectInput("dose_op_heatmap", "Operation with doses", choices = c("filter", "mean", "subtract"))),
                             
                             column(
                               6,
                               conditionalPanel(
                                 condition = "input.dose_op_heatmap == 'filter'",
                                 radioButtons("filt_dose", "Filter dose", choices = "",inline = TRUE)
                               ),
                               
                               conditionalPanel(
                                 condition = "input.dose_op_heatmap == 'subtract'",
                                 fluidRow(
                                   column(6, selectInput("subdose_heatmap", "Subtract:", choices = c("30-5"))),
                                   column(6, style="padding-top: 5px;",br(), actionButton("revdose_heat", icon("exchange-alt"))))
                               )
                             )
                             
                             
                             
                           ),
                           #awesomeCheckbox("logheat", "Log2 scale", value = FALSE),
                           # fluidRow(
                           #   column(6,
                           #          selectInput("selscaleheat", "Standardize data:", 
                           #                      choices = c("None" = "none", "By row" = "row", "By column" = "column"), 
                           #                      selected = "column")
                           #   ),
                           #   conditionalPanel(condition = "input.selscaleheat == 'none'",
                           #                    column(6,textInput("unitlegend_ht", "Unit measure", value = "ug/ml"))
                           #   )
                           # ),
                           hr(),
                           h4(strong("Data heatmap")),
                           fluidRow(
                             column(5, style="padding-top: 5px;", br(), awesomeCheckbox("show_valheat", "Show cell values")),
                             column(7, 
                                    conditionalPanel(
                                      condition = "input.show_valheat == true", 
                                      sliderInput("range_showvalheat", "Threshold", value = 0, min = 0, max = 100)))),
                           fluidRow(column(5, actionButton("view_dataheat", "Check Data", icon("eye"))),
                                    column(7, downloadButton("download_heat", "Download data heatmap"), style = "text-align:right")),
                           tags$head(tags$style("#viewdt_heatmap .modal-dialog{ width:1300px}")),
                           shinyBS::bsModal(
                             "viewdt_heatmap", trigger = "view_dataheat", title = "Data Table Heatmap",
                             fluidRow(
                               conditionalPanel(condition = "input.dose_op_heatmap == 'filter'",
                                                column(2, selectInput("dose_dtheatmap", "Select a dose", choices = ""))),
                               column(10, div(DT::DTOutput("dt_heatmap"), style = "overflow-x: scroll;")))
                           ),
                           hr(),
                           h4(strong("Annotations")),
                           fluidRow(
                             column(6, selectInput("selectannot_row", "Row annotation:", choices = c("Model_Family","Experiment_id", "Corrected_value"), multiple = TRUE)),
                             column(6, selectInput("selectannot_col", "Column annotation:", choices = "Product_Family"))
                           ),
                           
                           hr(),
                           h4(strong("Dendrogramm options")),
                           ###dendrogramm on column or row?
                           h5(strong("Where to show dendrogramm")),
                           fluidRow(
                             column(6, materialSwitch(inputId = "rowdend", label = "Row",  value = FALSE, status = "primary", width = "90%")),
                             column(6, materialSwitch(inputId = "columndend", label = "Column",  value = FALSE, status = "primary", width = "90%"))
                           ),
                           
                           conditionalPanel(condition = "input.rowdend == 1 || input.columndend == 1",
                                            fluidRow(
                                              column(6,
                                                     selectInput("seldistheat", "Distance function:", choices = c("euclidean", "maximum", "canberra"), selected = "euclidean") #, "minkowski","manhattan",
                                              ),
                                              column(6,
                                                     selectInput("selhclustheat", "Clustering method:", choices = c("ward.D2", "complete", "average" , "median"), selected = "complete") #, "centroid","mcquitty","ward.D2", "single", 
                                              )
                                            )
                           ),
                           
                           conditionalPanel(condition = "input.rowdend == 0",
                                            h5(strong("Order data by annotation?")),
                                            awesomeCheckbox("heatsort", label = "Order", value = TRUE)
                           ),
                           
                           conditionalPanel(condition = "input.rowdend == 1",
                                            hr(),
                                            sliderInput("sliderrowheat", "Column cluster number:", min=2, max = 10, value=2, step = 1)
                           ),
                           
                           conditionalPanel(condition = "input.columndend == 1",
                                            hr(),
                                            sliderInput("slidercolheat", "Column cluster number:", min=2, max = 10, value=2, step = 1)
                           )
                         ), #end of sidebarpanel
                         
                         mainPanel(width = 9, 
                                   InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_output", layout = "1|(2-3)", width1 = 1000, height1 = 600)
                         )
                       )
              )
              
              
            )
            
          ), #end of tabitem cytotab
          
          
          ####### D1 #######
          tabItem(
            tabName = "d1tab",
            #tabBox(id = "tabsetd1", width = 12, status = "primary",
            tabsetPanel(id = "tabsetd1",
              tabPanel(
                "Data",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    fluidRow(
                      column(10, fileInput("exp_list_file_D1","Select the Experiment list file (.xlsx)")),
                      column(
                        2, style="padding-left: 9px; padding-top: 4px;",br(), 
                        mod_edit_data_ui("edit_exp_list_D1"))
                    ),
                    conditionalPanel(
                      condition = "output.check_explist_D1 == false",
                      fluidRow(
                        column(10, fileInput("target_file_D1","Select the Target file (.xlsx)")),
                        column(
                          2, br(), style="padding-left: 9px; padding-top: 4px;",
                          mod_edit_data_ui("edit_target_D1"))
                      )
                    ),
                    conditionalPanel(
                      condition = "output.check_target_D1 == false",
                      div(actionButton("gocyto_D1", "Evaluate cytotoxicity", icon("cogs")), style = "text-align: center;")
                    ),
                    conditionalPanel(
                      condition = "output.check_data_D1 == false",
                      hr(),
                      materialSwitch("summ_viewtable_D1", label = "Summarize data", value = TRUE, status = "primary")
                    )
                    
                    
                  ),
                  mainPanel(
                    width = 9,
                    div(DT::DTOutput("dtdata_D1"), style = "overflow-x: scroll;")
                  )
                )
                
              ), #end of tabpanel D1
              
              #### Data exploration D1 ####
              tabPanel(
                "Data exploration",
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    awesomeRadio("seltype_infograph_D1", "Plot type", choices = c("Data overview", "Product family")),
                    conditionalPanel(
                      condition = "input.seltype_infograph_D1 == 'Product family'",
                      awesomeCheckbox("first50_prodfam_D1", "Shows first 50 product family", value = TRUE)
                    )
                  ),
                  mainPanel(
                    width = 10,
                    conditionalPanel(
                      condition = "input.seltype_infograph_D1 == 'Data overview'",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("countbarplot_D1"))
                    ),

                    conditionalPanel(
                      condition = "input.seltype_infograph_D1 == 'Product family'",
                      shinycssloaders::withSpinner(uiOutput("prodfam_barplotUI_D1"))
                    )
                    
                  )
                )
                
              ),
              
              ##### Barplot D1 #####
              tabPanel(
                "Barplot",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("typeeval_bar_D1", "Select a measure", choices = c("Cytotoxicity", "Vitality")),
                    selectInput("model_filt_bar_D1", "Filter Model type", choices = ""),
                    selectInput("family_filt_bar_D1", "Filter Product Family", choices = ""),
                    fluidRow(
                      column(6, awesomeCheckbox("addpoints_barplot_D1", "Add points", value = TRUE)),
                      column(6, conditionalPanel(condition = "output.check_multID_bar1_D1 == true",
                                                 awesomeCheckbox("facet_bar_D1", "Faceting expID", value = FALSE)))
                    ),
                    div(actionButton("add_barplot_D1", "Add another barplot", icon("plus")), style = "text-align:center;"),
                    conditionalPanel(
                      condition = "output.show_barplot2_D1 == true",
                      br(),
                      selectInput("typeeval_bar2_D1", "Select a measure", choices = c("Cytotoxicity", "Vitality")),
                      selectInput("model_filt_bar2_D1", "Filter Model type", choices = ""),
                      selectInput("family_filt_bar2_D1", "Filter Product Family", choices = ""),
                      fluidRow(
                        column(6, awesomeCheckbox("addpoints_barplot2_D1", "Add points", value = TRUE)),
                        column(6, conditionalPanel(condition = "output.check_multID_bar2_D1 == true",
                                                   awesomeCheckbox("facet_bar2_D1", "Faceting expID", value = FALSE)))
                      )
                    )
                  ),
                  
                  mainPanel(
                    width = 9,
                    shinycssloaders::withSpinner(plotly::plotlyOutput("barplot_D1")),
                    conditionalPanel(
                      condition = "output.show_barplot2_D1 == true",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("barplot2_D1"))
                    )
                  )
                )
              ), #end of tabpanel barplot D1
              
              ##### spiderplot ####
              tabPanel(
                "Spider Plot",
                mod_spiderplot_ui("spiderplot_D1")
              ),
              
              tabPanel(
                "Bubble Plot",
                mod_bubble_plot_ui("bubbleplot_D1", c("CV"))
              ),
              
              tabPanel(
                "Heatmap",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    div(actionButton("makeheatmap_D1", label = "Make Heatmap", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:130%; font-weight: bold;'), align= "center"),
                    br(),
                    h4(strong("Data filtering")),
                    fluidRow(
                      column(6,selectInput("typeeval_heat_D1", "Select a measure", choices = c("Cytotoxicity", "Vitality"))),
                      column(6, selectInput("prodfam_heat_D1", "Select a Product Family", choices = ""))
                    ),
                    selectInput("purif_filt_heat_D1", "Select a purification", choices = "", multiple = FALSE),
                    fluidRow(
                      column(6, selectInput("dose_op_heatmap_D1", "Operation with doses", choices = c("filter", "mean", "subtract"))),
                      
                      column(
                        6,
                        conditionalPanel(
                          condition = "input.dose_op_heatmap_D1 == 'filter'",
                          radioButtons("filt_dose_D1", "Filter dose", choices = "",inline = TRUE)
                        ),
                        
                        conditionalPanel(
                          condition = "input.dose_op_heatmap_D1 == 'subtract'",
                          fluidRow(
                            column(6, selectInput("subdose_heatmap_D1", "Subtract:", choices = c("30-5"))),
                            column(6, style="padding-top: 5px;",br(), actionButton("revdose_heat_D1", icon("exchange-alt"))))
                        )
                      )

                    ),
                    
                    hr(),
                    h4(strong("Data heatmap")),
                   # fluidRow(
                     # column(5, style="padding-top: 5px;", br(), 
                   awesomeCheckbox("show_valheat_D1", "Show cell values"),
                      # column(7, 
                      #        conditionalPanel(
                      #          condition = "input.show_valheat_D1 == true", 
                      #          sliderInput("range_showvalheat_D1", "Threshold", value = 0, min = 0, max = 100)))
                      #),
                    
                    h4(strong("Dendrogramm options")),
                    ###dendrogramm on column or row?
                    fluidRow(
                      column(6, materialSwitch(inputId = "rowdend_D1", label = "Row",  value = FALSE, status = "primary", width = "90%"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.rowdend_D1 == 1",
                      fluidRow(
                        column(6,
                               selectInput("seldistheat_D1", "Distance function:", choices = c("euclidean", "maximum", "canberra"), selected = "euclidean")
                        ),
                        column(6,
                               selectInput("selhclustheat_D1", "Clustering method:", choices = c("ward.D2", "complete", "average" , "median"), selected = "complete")
                        )
                      ),
                      fluidRow(
                        hr(),
                        sliderInput("sliderrowheat_D1", "Row cluster number:", min=2, max = 10, value=2, step = 1)
                      )
                    )
                    
                    # conditionalPanel(condition = "input.rowdend == 0",
                    #                  h5(strong("Order data by annotation?")),
                    #                  awesomeCheckbox("heatsort", label = "Order", value = TRUE)
                    # ),
                    
       
                  ),
                  
                  mainPanel(
                    width = 9,
                    InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput("heatmap_D1_output", layout = "1|(2-3)", width1 = 1000, height1 = 800)
                  )
                )
              )
              
              
            )
          ) #end of tabitem D1
          
        ) #end of tabItemS
        
      ) #end of dashboard body
    ),
    
    
    # Your application UI logic 

  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ADViSEBioassay'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

