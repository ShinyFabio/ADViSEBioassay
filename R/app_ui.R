#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @import shinydashboard
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shinyBS bsModal
#' @importFrom shinycssloaders withSpinner
#' @import tmap
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

      dashboardHeader(title = span(
        tagList(tags$img(src = "www/logo_ADViSEBioassay.png", width = "32px",style="margin-right: 4px;"), "ADViSEBioassay")),
        tags$li(class = "dropdown", actionBttn("jumptohome", icon = icon("house"), style = "stretch", size = "lg"))),
      

      sidebar = dashboardSidebar(

        sidebarUserPanel(name = textOutput("nome"),
                         subtitle = actionButton('change','Change', style='padding:0px; height:18px; font-size:90%'),
                         image = "www/userimage.png"
        ),

        sidebarMenu(
          id = "sidebarmenu",
          menuItem("Home", tabName = "home", icon = icon("house")),
          menuItem("D1", tabName = "d1tab", icon = icon("circle"),
                   menuSubItem("Data summary", tabName = "d1summtab", icon = icon("clipboard-list")),
                   menuSubItem("Search", tabName = "d1querytab", icon = icon("magnifying-glass")),
                   menuSubItem("Explore", tabName = "d1plottab", icon = icon("chart-bar"))
                   ),
          menuItem("Cytotoxicity", tabName = "cytotab",icon = icon("circle"),
                   menuSubItem("Data summary", tabName = "cytosummtab", icon = icon("clipboard-list")),
                   menuSubItem("Search", tabName = "cytoquerytab", icon = icon("magnifying-glass")),
                   menuSubItem("Explore", tabName = "cytoplottab", icon = icon("chart-bar"))
                   ),
          menuItem("Reporter", tabName = "repotab", icon = icon("circle"),
                   selectInput("sel_reporter", tags$span(style="color: white;","Select Data Type"), choices = c("TREM2", "SEAP")),
                   menuSubItem("Data summary", tabName = "reposummtab", icon = icon("clipboard-list")),
                   menuSubItem("Search", tabName = "repoquerytab", icon = icon("magnifying-glass")),
                   menuSubItem("Explore", tabName = "repoplottab", icon = icon("chart-bar"))
                   ),
          menuItem("Integration", tabName = "inttab", icon = icon("vials"),
                   menuSubItem("Overview", tabName = "overinttab", icon = icon("clipboard-list")),
                   menuSubItem("Search", tabName = "intquerytab", icon = icon("magnifying-glass"))
                   )
          )
      ),

      body = dashboardBody(
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
              h3(strong("ADViSEBioassay")," is a Shiny app for..........PAGE UNDER CONSTRUCTION",style = "color: #0e3d51;")
            ))),
            br(),
            fluidRow(
              
              column(
                10, offset = 1,
                box(width = NULL, status = "primary", title = h3(strong("Database"), style = "color: white; display:inline; margin-top: 0px;margin-bottom: 0px;"), solidHeader = T,
                    fluidRow(
                      column(4, uiOutput("valbox_cyto")),
                      column(4, uiOutput("valbox_D1")),
                      column(4, uiOutput("valbox_repo"))
                    ),
                    fluidRow(
                      div(actionButton("loaddatabase", label = HTML("&nbsp;Load database!"), icon("rocket"), 
                                       class = "btn btn-primary btn-lg", style='padding:10px; font-size:200%; font-weight: bold;'),
                          style = "text-align: center;")
                    )
                )
                
                )

              
            )

          ),
          
          
          #### tabitem cyto summ ####
          
          tabItem(
            tabName = "cytosummtab",
            tabsetPanel(
              
              ###### Data table cyto ####
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
                        circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                        tooltip = tooltipOptions(title = "Click to see options"))
                    )),
                    
                    shinycssloaders::withSpinner(DT::DTOutput("dtdata"))),
                
                fluidRow(
                  column(
                    1, style = "text-align:right;padding-right: 2rem; width: 25rem;",
                    actionButton("upcyto_modalbutton", HTML("&nbsp;Add new data"), icon("file-arrow-up"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  column(1, style = "width: 3rem; padding: 0px;text-align:center;", strong(h3("or", style = "margin-top: 10px;"))),
                  column(
                    1, style = "padding-right: 30rem; padding-left: 2rem;",
                    actionButton("upload_updated_cyto", "Upload database", icon("upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  conditionalPanel(
                    condition = "output.check_ifsave_cyto == true",
                    column(
                      2, style = "text-align:center;",
                      actionButton("save_update", HTML("&nbsp;Save database"), icon("floppy-disk"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:140%; font-weight: bold;')
                    )
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
                    actionButton("remove_update_cyto", HTML("&nbsp;Restore database"),icon("arrow-rotate-left"), style='background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:140%; font-weight: bold;')
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
                                   div(actionButton("update_cyto_bttn", "Merge!",icon("pen-to-square"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:110%; font-weight: bold;')), style = "text-align: center;")
                                 
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
                                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                     tooltip = tooltipOptions(title = "Click to see options"))
                            )),
                          
                          shinycssloaders::withSpinner(DTOutput("newdata_cyto_DT")))
                      
                    )
                  )
                )
                
              ),
              
              ####### informative graphs #####
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
                      htmlOutput("heatmap_inform_output")
                    )
                    
                  )
                )
              )

            )
          ),
          
          
          
          ##### tabitem cyto query ####
          tabItem(
            tabName = "cytoquerytab",
            
            column(
              9,
              
              ###first query
              box(
                width = NULL, status = "primary",
                fluidRow(
                  column(3,
                         selectInput("filtmod_query_cyto", "Filter Model_type", choices = "", multiple = TRUE),
                         radioButtons("andor_query_cyto", label = NULL, choices = c("AND", "OR"), selected = "OR",inline = TRUE)
                  ),
                  column(3,
                         selectInput("selcol_query_cyto", "Column", choices = "")
                  ),
                  column(2,
                         selectInput("selop_query_cyto", "Operator", choices = c("max", "min", "greater than", "greater than or equal", "equal", "less than or equal", "less than"))
                  ),
                  column(2,
                    conditionalPanel(
                      condition = "input.selop_query_cyto != 'max' && input.selop_query_cyto != 'min'",
                      numericInput("thresh_query_cyto", "Threshold", value = 1))
                  ),
                  column(2, style = "text-align:center;",br(),shinyBS::bsButton("add2query_cyto", label = HTML("&nbsp;Add"), style="success", icon("plus")))
                )
              ),
              
              
              ###second query
              conditionalPanel(
                condition = "output.checkadd2query_cyto == 'twoquery'",
                box(width = NULL, status = "primary",
                    fluidRow(
                      # column(3,
                      #   selectInput("filtmod_query_cyto2", "Filter Model_type", choices = "", multiple = TRUE),
                      #   radioButtons("andor_query_cyto2", label = NULL, choices = c("AND", "OR"), selected = "OR",inline = TRUE)
                      # ),
                      column(3,
                             selectInput("selcol_query_cyto2", "Column", choices = "")
                      ),
                      column(2,
                             selectInput("selop_query_cyto2", "Operator", choices = c("max", "min", "greater than", "greater than or equal", "equal", "less than or equal", "less than"))
                      ),
                      conditionalPanel(
                        condition = "input.selop_query_cyto2 != 'max' && input.selop_query_cyto2 != 'min'",
                        column(2, numericInput("thresh_query_cyto2", "Threshold", value = 1))
                      )
                    )
                    
                )
              )
              
            ),

            
            #button launch query
            box(width = 3, status = "primary",
                div(style = "text-align: center;", br(), br(), 
                    conditionalPanel("output.checkadd2query_cyto == 'twoquery'", br(), br(),br()),
                    actionButton("go_querycyto", "Search", icon("magnifying-glass"), style = "font-size:25px;"), 
                    br(), br(),
                    conditionalPanel("output.checkadd2query_cyto == 'twoquery'", br(), br(),br())
                )),

            
            
            fluidRow(column(12,
              box(
                width = 4, status = "primary",
                shinycssloaders::withSpinner(DTOutput("querydt_cyto"))
                ),
              box(
                width = 8, status = "primary",
                shinycssloaders::withSpinner(DTOutput("query2dt_cyto"))
              )
            ))

            
            
          ),
          
          
          #### tabitem cyto plot ####
          tabItem(
            tabName = "cytoplottab",
            #tabBox(id = "tabsetcyto", width = 12, status = "primary",
            tabsetPanel(id = "tabsetcyto",

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
                mod_bubble_plot_ui("bubbleplot_cyto",  c("CV", "Corrected_value"))
              ),
              
              #### Heatmap ####
              tabPanel(
                "Heatmap",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                       mod_heatmap_cyto_repo_ui("heatmap_cyto")),
                  mainPanel(
                    width = 9, 
                    htmlOutput("heatmap_output_cyto")
                    
                  )
                )
              )
            )
            
          ), #end of tabitem cytotab
          
          
          ####### D1 #######
          
          tabItem(
            tabName = "d1summtab",
            tabsetPanel(
              tabPanel(
                "Data",
                
                box(width = 12, status = "primary",
                    fluidRow(column(
                      width = 1, style="width: 7rem;",
                      dropdownButton(
                        conditionalPanel(condition = "output.check_data_D1 == false",
                                         h5(strong("Summarized data")),
                                         materialSwitch("summ_viewtable_D1", value = TRUE, status = "primary")
                        ),
                        circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                        tooltip = tooltipOptions(title = "Click to see options"))
                    )),
                    
                    shinycssloaders::withSpinner(DT::DTOutput("dtdata_D1"))),
                
                ### barra database D1
                fluidRow(
                  column(
                    1, style = "text-align:right;padding-right: 2rem; width: 25rem;",
                    actionButton("upD1_modalbutton", HTML("&nbsp;Add new data"), icon("file-arrow-up"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  column(1, style = "width: 3rem; padding: 0px;text-align:center;", strong(h3("or", style = "margin-top: 10px;"))),
                  column(
                    1, style = "padding-right: 30rem; padding-left: 2rem;",
                    actionButton("upload_updated_D1", "Upload database", icon("upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  
                  conditionalPanel(
                    condition = "output.check_ifsave_D1 == true",
                    column(
                      2, style = "text-align:center;",
                      actionButton("save_update_D1", HTML("&nbsp;Save database"), icon("floppy-disk"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:140%; font-weight: bold;')
                    )
                  ),
                  conditionalPanel(
                    condition = "output.checkupdated_D1_fordownload == true",
                    column(
                      3, style = "text-align:center;",
                      downloadButton("download_updated_D1", "Download database", style='padding:10px; font-size:140%; font-weight: bold;')
                    )
                  ),
                  column(
                    2, style = "text-align:center;",
                    actionButton("remove_update_D1", HTML("&nbsp;Restore database"),icon("arrow-rotate-left"), style='background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:140%; font-weight: bold;')
                  )
                  
                ),
                
                tags$head(tags$style("#upD1_tab .modal-dialog{ min-width:170rem}")),
                tags$head(tags$style("#upD1_tab .modal-body{ min-height:80rem}")),
                shinyBS::bsModal(
                  "upD1_tab", "Update database", trigger = "upD1_modalbutton", size = "large",
                  sidebarLayout(
                    sidebarPanel(width = 3,
                                 mod_load_cyto_ui("load_D1_mod"), ###############################################
                                 hr(),
                                 conditionalPanel(
                                   condition = "output.check_data_updated_D1 == false",
                                   div(actionButton("update_D1_bttn", "Merge!",icon("pen-to-square"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:110%; font-weight: bold;')), style = "text-align: center;")
                                 
                    ),
                    mainPanel(
                      width = 9,
                      box(width = NULL, status = "primary",
                          fluidRow(
                            column(width = 1, style="width: 7rem;",
                                   dropdownButton(
                                     conditionalPanel(condition = "output.check_data_updated_D1 == false",
                                                      h5(strong("Summarized data")),
                                                      materialSwitch("summ_viewtable_updated_D1", value = TRUE, status = "primary")
                                     ),
                                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                     tooltip = tooltipOptions(title = "Click to see options"))
                            )),
                          
                          shinycssloaders::withSpinner(DTOutput("newdata_D1_DT")))
                      
                    )
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
                
              )
            )
          ),
          
          
          
          #### tabitem D1 QUERY ####
          tabItem(
            tabName = "d1querytab",
            
            ###first query
            column(
              9,
              box(
              width = NULL, status = "primary",
              fluidRow(
                # column(3,
                #        selectInput("filtmfi_query_d1", "Filter Model_type", choices = "", multiple = TRUE),
                #        radioButtons("andor_query_d1", label = NULL, choices = c("AND", "OR"), selected = "OR",inline = TRUE)
                # ),
                column(4,
                       selectInput("selcol_query_d1", "MFI", choices = c( "CD40","MHC-II", "CD80"), multiple = TRUE),
                       radioButtons("andor_query_d1", label = NULL, choices = c("AND", "OR"), selected = "OR",inline = TRUE)
                ),
                column(3,
                       selectInput("selop_query_d1", "Operator", choices = c("greater than", "greater than or equal"))
                ),
                # conditionalPanel(
                #   condition = "input.selop_query_cyto != 'max' && input.selop_query_cyto != 'min'",
                  column(3, sliderInput("thresh_query_d1", "Times", value = 2.5,min = 1, max = 3, step = 0.1)),
                #),
                column(2, style = "text-align: center;", br(),shinyBS::bsButton("add2query_d1", label = HTML("&nbsp;Add"), style="success", icon("plus")))
              )
            ),
            
            
            ###second query
            conditionalPanel(
              condition = "output.checkadd2query_D1 == 'twoquery'",
              box(width = NULL, status = "primary",
                  fluidRow(
                    column(4, selectInput("selcol_query_d12", "Column", choices = "")),
                    column(
                      3,
                      selectInput("selop_query_d12", "Operator", choices = c("max", "min", "greater than", "greater than or equal", "equal", "less than or equal", "less than"))
                    ),
                    conditionalPanel(
                      condition = "input.selop_query_d12 != 'max' && input.selop_query_d12 != 'min'",
                      column(3, numericInput("thresh_query_d12", "Threshold", value = 1)))
                  )
              )
            )
            ),
            
            #button launch query
            box(width = 3, status = "primary",
                div(style = "text-align: center;", br(), br(), 
                  conditionalPanel("output.checkadd2query_D1 == 'twoquery'", br(), br(),br()),
                  actionButton("go_queryd1", "Search", icon("magnifying-glass"), style = "font-size:25px;"), 
                  br(), br(),
                  conditionalPanel("output.checkadd2query_D1 == 'twoquery'", br(), br(),br())
                  )),
            
            

            
            
            fluidRow(column(12,
                            box(
                              width = 4, status = "primary",
                              shinycssloaders::withSpinner(DTOutput("querydt_D1"))
                            ),
                            box(
                              width = 8, status = "primary",
                              shinycssloaders::withSpinner(DTOutput("query2dt_D1"))
                            )
            ))

          ),
          
          
          
          tabItem(
            tabName = "d1plottab",
            #tabBox(id = "tabsetd1", width = 12, status = "primary",
            tabsetPanel(id = "tabsetd1",

              
              ##### Barplot D1 #####
              tabPanel(
                "Barplot",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("typeeval_bar_D1", "Select a measure", choices = ""),
                    fluidRow(
                      column(6, selectInput("model_filt_bar_D1", "Filter Model type", choices = "")),
                      column(6, selectInput("family_filt_bar_D1", "Filter Product Family", choices = ""))),
                    selectInput("purif_filt_bar_D1", "Select a purification", choices = ""),
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
                      fluidRow(
                        column(6, selectInput("model_filt_bar2_D1", "Filter Model type", choices = "")),
                        column(6, selectInput("family_filt_bar2_D1", "Filter Product Family", choices = ""))),
                      selectInput("purif_filt_bar2_D1", "Select a purification", choices = ""),
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
                            column(7, selectInput("subdose_heatmap_D1", "Subtract:", choices = c("30-5"))),
                            column(5, style="padding-top: 5px;",br(), actionButton("revdose_heat_D1", icon("right-left"))))
                        )
                      )

                    ),
                    
                    hr(),
                    h4(strong("Scale options")),
                    fluidRow(column(5, br(), awesomeCheckbox("logheat_D1", "Log scale")),
                    column(7, selectInput("scale_type_heat_D1", "Scale type", choices = c("To max", "Adaptive", "Absolute", "Custom")))),
                    conditionalPanel(
                      condition = "input.scale_type_heat_D1 == 'Custom'",
                      uiOutput("uicustom_scale_heat_D1")
                    ),
                    
                    ### color pickers
                    hr(),
                    h4(strong("Colors")),
                    awesomeCheckbox("custom_color_heat_D1", "Custom colors"),
                    conditionalPanel(
                      condition = "input.custom_color_heat_D1 == true",
                      uiOutput("colormark_ui_heat_D1")
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
                      hr(),
                      sliderInput("sliderrowheat_D1", "Row cluster number:", min=2, max = 10, value=2, step = 1)
                      
                    )
                    
                    # conditionalPanel(condition = "input.rowdend == 0",
                    #                  h5(strong("Order data by annotation?")),
                    #                  awesomeCheckbox("heatsort", label = "Order", value = TRUE)
                    # ),
                    
       
                  ),
                  
                  mainPanel(
                    width = 9,
                    htmlOutput("heatmap_D1_output")
                  )
                )
              )
              
              
            )
          ), #end of tabitem D1
          
          
          ####### REPORTER #######
          tabItem(
            tabName = "reposummtab",
            tabsetPanel(
              tabPanel(
                "Data",
                
                box(width = 12, status = "primary",
                    fluidRow(column(
                      width = 1, style="width: 7rem;",
                      dropdownButton(
                        conditionalPanel(condition = "output.check_data_reporter == false",
                                         h5(strong("Summarized data")),
                                         materialSwitch("summ_viewtable_reporter", value = TRUE, status = "primary")
                        ),
                        circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                        tooltip = tooltipOptions(title = "Click to see options"))
                    )),
                    
                    shinycssloaders::withSpinner(DT::DTOutput("dtdata_reporter"))),
                
                
                ### barra database reporter
                fluidRow(
                  column(
                    1, style = "text-align:right;padding-right: 2rem; width: 25rem;",
                    actionButton("upreporter_modalbutton", HTML("&nbsp;Add new data"), icon("file-arrow-up"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  column(1, style = "width: 3rem; padding: 0px;text-align:center;", strong(h3("or", style = "margin-top: 10px;"))),
                  column(
                    1, style = "padding-right: 30rem; padding-left: 2rem;",
                    actionButton("upload_updated_reporter", "Upload database", icon("upload"), style='background:#2AAAE2; border-color:#2AAAE2;padding:10px; font-size:140%; font-weight: bold;')
                  ),
                  
                  conditionalPanel(
                    condition = "output.check_ifsave_reporter == true",
                    column(
                      2, style = "text-align:center;",
                      actionButton("save_update_reporter", HTML("&nbsp;Save database"), icon("floppy-disk"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:140%; font-weight: bold;')
                    )
                  ),
                  conditionalPanel(
                    condition = "output.checkupdated_reporter_fordownload == true",
                    column(
                      3, style = "text-align:center;",
                      downloadButton("download_updated_reporter", "Download database", style='padding:10px; font-size:140%; font-weight: bold;')
                    )
                  ),
                  column(
                    2, style = "text-align:center;",
                    actionButton("remove_update_reporter", HTML("&nbsp;Restore database"),icon("arrow-rotate-left"), style='background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:140%; font-weight: bold;')
                  )
                ),
                
                
                tags$head(tags$style("#upreporter_tab .modal-dialog{ min-width:170rem}")),
                tags$head(tags$style("#upreporter_tab .modal-body{ min-height:80rem}")),
                shinyBS::bsModal(
                  "upreporter_tab", "Update database", trigger = "upreporter_modalbutton", size = "large",
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      mod_load_cyto_ui("load_reporter_mod"),
                      hr(),
                      conditionalPanel(
                        condition = "output.check_data_updated_reporter == false",
                        div(actionButton("update_reporter_bttn", "Merge!",icon("pen-to-square"), style='background: #00a65a;border-color: #00a65a;padding:10px; font-size:110%; font-weight: bold;')), style = "text-align: center;")
                    ),
                    mainPanel(
                      width = 9,
                      box(width = NULL, status = "primary",
                          fluidRow(
                            column(width = 1, style="width: 7rem;",
                                   dropdownButton(
                                     conditionalPanel(condition = "output.check_data_updated_reporter == false",
                                                      h5(strong("Summarized data")),
                                                      materialSwitch("summ_viewtable_updated_reporter", value = TRUE, status = "primary")
                                     ),
                                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                     tooltip = tooltipOptions(title = "Click to see options"))
                            )),
                          
                          shinycssloaders::withSpinner(DTOutput("newdata_reporter_DT")))
                    )
                  )
                ) #end of modal
                
              ),
              
              tabPanel(
                "Data exploration",
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    awesomeRadio("seltype_infograph_reporter", "Plot type", choices = c("Data overview", "Product family")),
                    conditionalPanel(
                      condition = "input.seltype_infograph_reporter == 'Product family'",
                      awesomeCheckbox("first50_prodfam_reporter", "Shows first 50 product family", value = TRUE)
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph_reporter == 'Model types per fraction'",
                      #selectInput("query1_repo_prodfam", "Select a Product Family", choices = ""),
                      selectInput("query1_repo", "Select a Product", choices = "", multiple = T)
                    )
                  ),
                  mainPanel(
                    width = 10,
                    conditionalPanel(
                      condition = "input.seltype_infograph_reporter == 'Data overview'",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("countbarplot_reporter"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.seltype_infograph_reporter == 'Product family'",
                      shinycssloaders::withSpinner(uiOutput("prodfam_barplotUI_reporter"))
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph_reporter == 'Model types per fraction'",
                      shinycssloaders::withSpinner(DTOutput("modtype_perfrac_seap"))
                    ),
                    conditionalPanel(
                      condition = "input.seltype_infograph_reporter == 'Fractions frequence'",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("fractfreq_seap"))
                    )
                  )
                )
              )
            )
          ), #end of first tabitem (data)
          
          
          #### QUERY reporter ####
          tabItem(
            tabName = "repoquerytab",
            column(
              9,
              
              ### first query
              box(
                width = NULL, status = "primary",
                fluidRow(
                  column(3, selectInput("query_reporter", "Query type", choices = "")),
                  column(
                    4,
                    conditionalPanel(
                      condition = "input.query_reporter == 'Concentration greater than CTRL'",
                      sliderInput("query3_repo_thresh", "Threshold fraction greater than CTRL (%)", min = 100, max = 300, value = 200, step = 10)
                    ),
                    
                    conditionalPanel(
                      condition = "input.query_reporter == 'GFP fractions greater than CTRL+'",
                      sliderInput("query4_repo_thresh", "Threshold fraction greater than CTRL+ (%)", min = 10, max = 100, value = 50, step = 5)
                    )
                  ),
                  column(
                    3,
                    conditionalPanel(
                      condition = "input.query_reporter == 'Concentration greater than CTRL'",
                      selectInput("query3_repo_modtype", "Select a Model type", choices = "", multiple  = T)
                    )
                  ),
                  column(
                    2, style = "text-align:center;",br(),
                    conditionalPanel(
                      condition = "input.sel_reporter == 'SEAP'",
                      shinyBS::bsButton("add2query_seap", label = HTML("&nbsp;Add"), style="success", icon("plus"))
                    ),
                    conditionalPanel(
                      condition = "input.sel_reporter == 'TREM2'",
                      shinyBS::bsButton("add2query_trem", label = HTML("&nbsp;Add"), style="success", icon("plus"))
                    )
                  )
                )
              ),
              
              
              #### second queries
              #### Second query seap
              conditionalPanel(
                condition = "output.checkadd2query_seap == 'twoquery'",
                box(
                  width = NULL, status = "primary",
                  conditionalPanel(
                    condition = "input.query_reporter == 'Concentration greater than CTRL'",
                    fluidRow(
                      column(3,selectInput("query_reporter2", "Second query type", choices = "")),
                      column(
                        2, offset = 1, 
                        conditionalPanel(
                          condition = "output.check_second_seap_query == true",
                          br(),
                          actionButton("seap_query_plot", "Informative plots", icon("eye"))
                        )
                      )
                    ),
                    #modal
                    shinyBS::bsModal(
                      "modal_seap_query_plot", title = "Informative plots ",trigger = "seap_query_plot", size = "large",
                      fluidRow(
                        column(3, radioButtons("type_plot_seap_query", "Plot type:", choices = c("Model types per fraction", 
                                                                                                 "Fractions frequence"))),
                        column(9, conditionalPanel(
                          condition = "input.type_plot_seap_query == 'Model types per fraction'",
                          selectInput("prod_dt_seap_query", "Select one or more Products", choices = "", multiple = T)
                        ))
                      ),
                      fluidRow(
                        conditionalPanel(
                          condition = "input.type_plot_seap_query == 'Model types per fraction'",
                          column(12,shinycssloaders::withSpinner(DTOutput("plot_dt_seap_query")))
                        ),
                        conditionalPanel(
                          condition = "input.type_plot_seap_query == 'Fractions frequence'",
                          column(12,shinycssloaders::withSpinner(plotlyOutput("plotly_seap_query")))
                        )
                      )
                    ) #end of modal
                    
                  )
                )
              ),
              
              #### Second query trem2
              conditionalPanel(
                condition = "output.checkadd2query_trem == 'twoquery'",
                box(width = NULL, status = "primary",
                  fluidRow(
                    column(
                      3,
                      selectInput("query_reporter_trem2", "Second query type", 
                                  choices = c("Enriched fractions", "Vitality fractions greater than CTRL+"))),
                    column(
                      4,
                      conditionalPanel(
                        condition = "input.query_reporter_trem2 == 'Vitality fractions greater than CTRL+'",
                        sliderInput("query_vita_repo_thresh", "Threshold fraction greater than CTRL+ (%)", min = 10, max = 200, value = 100, step = 10)
                      )
                    ),
                    column(
                      2, offset = 3,style = "text-align:center;",br(),
                      conditionalPanel(
                        "input.query_reporter_trem2 == 'Vitality fractions greater than CTRL+'",
                        shinyBS::bsButton("add3query_trem", label = HTML("&nbsp;Add"), style="success", icon("plus"))
                      )
                    )
                  )
                )
              ), #end of 2 query trem
              
              
              #### Third query trem2
              conditionalPanel(
                condition = "output.checkadd3query_trem == 'three'",
                box(width = NULL, status = "primary",
                    fluidRow(column(3, selectInput("query_reporter_trem3", "Third query type", 
                                         choices = c("Enriched fractions")))
                    )
                )
              )

              
            ), #end of column 9
            
            #button launch query
            box(width = 3, status = "primary",
                div(style = "text-align: center;", br(), br(), 
                    conditionalPanel("output.checkadd2query_trem == 'twoquery' || output.checkadd2query_seap == 'twoquery'", br(), br(),br()),
                    actionButton("go_queryrepo", "Search", icon("magnifying-glass"), style = "font-size:25px;"), 
                    br(), br(),
                    conditionalPanel("output.checkadd2query_trem == 'twoquery' || output.checkadd2query_seap == 'twoquery'", br(), br(),br())
                )),
            
         
            box(width = 12, status = "primary", shinycssloaders::withSpinner(DTOutput("query_repo_dt")))
            
          ),
          
          
          tabItem(
            tabName = "repoplottab",
            tabsetPanel(
              #id = "tabsetreport",
              
              ##### Barplot reporter #####
              tabPanel(
                "Barplot",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    selectInput("typeeval_bar_reporter", "Select a measure", choices = ""),
                    fluidRow(
                      column(6, selectInput("model_filt_bar_reporter", "Filter Model type", choices = "")),
                      column(6, selectInput("family_filt_bar_reporter", "Filter Product Family", choices = ""))),
                    selectInput("purif_filt_bar_reporter", "Select a purification", choices = ""),
                    fluidRow(
                      column(6, awesomeCheckbox("addpoints_barplot_reporter", "Add points", value = TRUE)),
                      column(6, conditionalPanel(condition = "output.check_multID_bar1_reporter == true",
                                                 awesomeCheckbox("facet_bar_reporter", "Faceting expID", value = FALSE)))
                    ),
                    div(actionButton("add_barplot_reporter", "Add another barplot", icon("plus")), style = "text-align:center;"),
                    conditionalPanel(
                      condition = "output.show_barplot2_reporter == true",
                      br(),
                      selectInput("typeeval_bar2_reporter", "Select a measure", choices = ""),
                      fluidRow(
                        column(6, selectInput("model_filt_bar2_reporter", "Filter Model type", choices = "")),
                        column(6, selectInput("family_filt_bar2_reporter", "Filter Product Family", choices = ""))),
                      selectInput("purif_filt_bar2_reporter", "Select a purification", choices = ""),
                      fluidRow(
                        column(6, awesomeCheckbox("addpoints_barplot2_reporter", "Add points", value = TRUE)),
                        column(6, conditionalPanel(condition = "output.check_multID_bar2_reporter == true",
                                                   awesomeCheckbox("facet_bar2_reporter", "Faceting expID", value = FALSE)))
                      )
                    )
                  ),
                  
                  mainPanel(
                    width = 9,
                    shinycssloaders::withSpinner(plotly::plotlyOutput("barplot_reporter")),
                    conditionalPanel(
                      condition = "output.show_barplot2_reporter == true",
                      shinycssloaders::withSpinner(plotly::plotlyOutput("barplot2_reporter"))
                    )
                  )
                )
              ), #end of tabpanel barplot reporter
              
              
              ##### spiderplot ####
              tabPanel(
                "Spider Plot",
                mod_spiderplot_ui("spiderplot_reporter")
              ),
              
              ##### BubblePlot #####
              tabPanel(
                "Bubble Plot",
                mod_bubble_plot_ui("bubbleplot_reporter",  c("CV"))
              ),
              
              #### Heatmap ####
              tabPanel(
                "Heatmap",
                sidebarLayout(
                  sidebarPanel(width = 3,
                    mod_heatmap_cyto_repo_ui("heatmap_reporter")),
                  mainPanel(width = 9, htmlOutput("heatmap_output_reporter")
                  )
                )
              )
              
            )
          ), #end of tabitem explore
          
          
          #### tabitem integrazione  #####
          tabItem(
            tabName = "overinttab",
            fluidPage(
              fluidRow(
                column(6, box(width = NULL, status = "primary", shinycssloaders::withSpinner(DTOutput("checktable")))),
                column(
                  6,
                  fluidRow(
                    column(6, box(width = NULL, status = "primary", title = "D1", solidHeader = T,
                                  shinycssloaders::withSpinner(tableOutput("checktable_models_d1")))),
                    column(6, box(width = NULL, status = "primary", title = "TREM2", solidHeader = T,
                                  shinycssloaders::withSpinner(tableOutput("checktable_models_trem2"))))
                  ),
                  fluidRow(
                    column(6,box(width = NULL, status = "primary", title = "Cytotoxicity", solidHeader = T,
                      shinycssloaders::withSpinner(tableOutput("checktable_models_cyto")))),
                    column(6, box(width = NULL, status = "primary", title = "SEAP", solidHeader = T,
                                  shinycssloaders::withSpinner(tableOutput("checktable_models_seap"))))
                  )
                )
              )
            ),
            
            #info modal
            tags$head(tags$style("#modal_infoprod .modal-dialog{ min-width:170rem}")),
            tags$head(tags$style("#modal_infoprod .modal-body{ min-height:80rem}")),
            shinyBS::bsModal("modal_infoprod", title = "Information ",
                             trigger = "random_trigger", size = "large",
                             fluidRow(
                               box(width = 12, title = "Table", solidHeader = T, status = "primary", tableOutput("dt_infoprod")),
                               ),
                             fluidRow(
                               column(7, box(width = NULL, title = "Map", solidHeader = T, status = "primary", shinycssloaders::withSpinner(uiOutput("ui_map")))),
                               column(5, box(width = NULL, title = "Photo", solidHeader = T, status = "primary", shinycssloaders::withSpinner(uiOutput("phorganism", style = "text-align:center;"))))
                             )
            )

          ),
          
          
          #### QUERY INTEGRAZIONE ####
          tabItem(
            tabName = "intquerytab",
            
            ###first query
            column(
              9,
              box(
                width = NULL, status = "primary",
                fluidRow(
                  #select the database
                  column(2, awesomeRadio("seldata_query1_int", "Database", choices = c("TREM2", "SEAP"))),
                  
                  #### TREM2 and SEAP FIRST ####
                  conditionalPanel(
                    "input.seldata_query1_int == 'TREM2' || input.seldata_query1_int == 'SEAP'",
                    
                    #modeltype for seap
                    column(3, conditionalPanel("input.seldata_query1_int == 'SEAP'",
                                               selectInput("queryinteg_seap_modtype", "Filter Model type", choices = "", multiple  = T))),
                    
                    column(
                      3, 
                      fluidRow(
                        column(8,selectInput("queryint_active_var1", "Active fractions in:", choices = "")),
                        conditionalPanel(
                          "input.seldata_query1_int == 'TREM2'",
                          column(4, br(),shinyBS::bsButton("add_queryint_active_var", label = "", style="success", icon("plus")))
                        )
                      ),
                      
                      conditionalPanel(
                        "output.checkadd_queryint_active_var == 'twovar'",
                        fluidRow(column(8,br(),selectInput("queryint_active_var2", "Active fractions in:", choices = "")))
                      )
                  ),
                  
                  column(4,
                    sliderInput("thresh_active_integ1", "", min = 10, max = 100, value = 50),
                    conditionalPanel(
                      "output.checkadd_queryint_active_var == 'twovar'",
                      sliderInput("thresh_active_integ2", "Threshold fraction greater than CTRL+ (%)", min = 10, max = 200, value = 100, step =10)
                    )
                  )
                  

                  ) #end of trem2 and seap
                  
                )
              ), #end of box first query
              
              
              ####  second query ####

              box(width = NULL, status = "primary",
                  fluidRow(
                    column(2, awesomeRadio("seldata_query2_int", "Database", choices = c("Cytotoxicity", "TREM2", "SEAP"))),
                    
                    #### cytotoxicity ####
                    conditionalPanel(
                      "input.seldata_query2_int == 'Cytotoxicity'",
                      fluidRow(
                        column(3, selectInput("filtmod_query_cyto_integ", "Filter Model type", choices = "", multiple = TRUE)),
                        column(2, selectInput("selcol_query_cyto_integ", "Column", choices = "")),
                        column(2, selectInput("selop_query_cyto_integ", "Operator", 
                                              choices = c("max", "min", "greater than", 
                                                          "greater than or equal", "equal", "less than or equal", "less than"))
                        ),
                        column(2,
                          conditionalPanel(
                            condition = "input.selop_query_cyto_integ != 'max' && input.selop_query_cyto != 'min'",
                            numericInput("thresh_query_cyto_integ", "Threshold", value = 50))
                        )
                      )
                    ),
                    
                    #### TREM2 and SEAP FIRST ####
                    conditionalPanel(
                      "input.seldata_query2_int == 'TREM2' || input.seldata_query2_int == 'SEAP'",
                      column(
                        3, 
                        fluidRow(
                          column(8,selectInput("queryint2_active_var1", "Active fractions in:", choices = "")),
                          conditionalPanel(
                            "input.seldata_query2_int == 'TREM2'",
                            column(4, br(),shinyBS::bsButton("add_queryint2_active_var", label = "", style="success", icon("plus")))
                          )
                        ),
                        
                        conditionalPanel(
                          "output.checkadd_queryint2_active_var == 'twovar'",
                          fluidRow(column(8,br(),selectInput("queryint2_active_var2", "Active fractions in:", choices = "")))
                        )
                      ),
                      
                      column(4,
                             sliderInput("thresh_active2_integ1", "", min = 10, max = 100, value = 50),
                             conditionalPanel(
                               "output.checkadd_queryint2_active_var == 'twovar'",
                               sliderInput("thresh_active2_integ2", "Threshold fraction greater than CTRL+ (%)", min = 10, max = 200, value = 100, step =10)
                             )
                      ),
                      
                      #modeltype for seap
                      column(3, conditionalPanel("input.seldata_query2_int == 'SEAP'",
                                                 selectInput("queryinteg2_seap_modtype", "Select a Model type", choices = "", multiple  = T)))
                      
                    ) #end of trem2 and seap
                    
                    
                  )
              )
              
            ), #end of column 9
            
            #button launch query
            box(width = 3, status = "primary",
                div(style = "text-align: center;", br(), br(),
                    
                    conditionalPanel("output.checkadd_queryint2_active_var == 'twovar' || output.checkadd_queryint_active_var == 'twovar'", br()),
                    actionButton("go_queryint", "Search", icon("magnifying-glass"), style = "font-size:25px;"), 
                    conditionalPanel("output.checkadd_queryint2_active_var == 'twovar' || output.checkadd_queryint_active_var == 'twovar'", br(), br()),
                    br(), br(), hr(),
                    conditionalPanel("output.checkadd_queryint2_active_var == 'twovar' || output.checkadd_queryint_active_var == 'twovar'", br()),
                    awesomeRadio("query_intersection",
                      label = "Query results", choices = c("Intersection", "Union", "First", "Second"),
                      selected = "Intersection", inline = TRUE), 
                    br()
                )),
            
            

            column(12,
              tabsetPanel(
                tabPanel("Datatable",icon = icon("table"), shinycssloaders::withSpinner(DTOutput("query_integ_dt"))),
                tabPanel(
                  "Plots", icon = icon("chart-bar"),
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      selectInput("bubb_integ_X", "X axis variable", choices = ""),
                      selectInput("bubb_integ_Y", "Y axis variable", choices = ""),
                      selectInput("bubb_integ_fill", "Fill variable", choices = ""),
                      selectInput("bubb_integ_size", "Size variable", choices = "")
                    ),
                    mainPanel(
                      width = 9,
                      box(width = 12, status = "primary", shinycssloaders::withSpinner(plotlyOutput("bubble_query_integ")))
                    )
                  )
                )
              )
            )
            

            ) #end of tabitem query integrazione
          
          
        ) #end of tabItemS
        
      ) #end of dashboard body
    )
    
    
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
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ADViSEBioassay'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

