#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT DTOutput
#' @import shinyWidgets
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapOutput
#' @importFrom plotly plotlyOutput
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    tags$head(
      tags$style(
        HTML("#shiny-notification-panel {
               width: 400px;
              max-width: 500px;
             }"
        ))),
    
    
    # Your application UI logic 
    fluidPage(
      h1("ADViSEBioassay"),
      tabsetPanel(
        tabPanel(
          "Data",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              fluidRow(
                column(10, fileInput("exp_list_file","Select the Experiment list file (.xlsx)")),
                column(
                  2, br(), 
                  tags$head(tags$style("#edit_exp_list-upinternalmodal .modal-dialog{ width:1300px}")),
                  tags$head(tags$style("#edit_exp_list-upinternalmodal .modal-body{ min-height:1000px}")),
                  mod_edit_data_ui("edit_exp_list"))
              ),
              conditionalPanel(
                condition = "output.check_explist == false",
                fluidRow(
                  column(10, fileInput("target_file","Select the Target file (.xlsx)")),
                  column(
                    2, br(), 
                    tags$head(tags$style("#edit_target-upinternalmodal .modal-dialog{ width:1300px}")),
                    tags$head(tags$style("#edit_target-upinternalmodal .modal-body{ min-height:1000px}")),
                    mod_edit_data_ui("edit_target"))
                )
              ),
              conditionalPanel(
                condition = "output.check_target == false",
                div(actionButton("gocyto", "Run analysis", icon("cogs")), style = "text-align: center;")
                ),
              conditionalPanel(
                condition = "output.check_data == false",
                hr(),
                materialSwitch("summ_viewtable", label = "Summarize data", value = TRUE, status = "primary")
              )
              
              
            ),
            mainPanel(
              width = 9,
              DT::DTOutput("dtdata")
            )
          )
          
        ),
        
        ##### Barplot #####
        tabPanel(
          "Barplot",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectInput("typeeval_bar", "Select a measure", choices = c("Cytoxicity", "Vitality")),
              selectInput("model_filt_bar", "Filter Model type", choices = ""),
              selectInput("family_filt_bar", "Filter Product Family", choices = ""),
              fluidRow(
                column(6, awesomeCheckbox("addpoints_barplot", "Add points", value = TRUE)),
                column(6, conditionalPanel(condition = "output.check_multID_bar1 == true",
                                           awesomeCheckbox("facet_bar", "Faceting expID", value = FALSE)))
              ),
              div(actionButton("add_barplot", "Add another barplot", icon("plus")), style = "text-align:center;"),
              conditionalPanel(
                condition = "output.show_barplot2 == true",
                br(),
                selectInput("typeeval_bar2", "Select a measure", choices = c("Cytoxicity", "Vitality")),
                selectInput("model_filt_bar2", "Filter Model type", choices = ""),
                selectInput("family_filt_bar2", "Filter Product Family", choices = ""),
                fluidRow(
                  column(6, awesomeCheckbox("addpoints_barplot2", "Add points", value = TRUE)),
                  column(6, conditionalPanel(condition = "output.check_multID_bar2 == true",
                                             awesomeCheckbox("facet_bar2", "Faceting expID", value = FALSE)))
                )
              )
            ),
            
            mainPanel(
              width = 9,
              plotly::plotlyOutput("barplot"),
              conditionalPanel(
                condition = "output.show_barplot2 == true",
                plotly::plotlyOutput("barplot2")
              )
            )
            )
        ),
        
        
        ##### spiderplot ####
        tabPanel(
          "Spider Plot",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectInput("typeeval_spid", "Select a measure", choices = c("Cytoxicity", "Vitality")),
              selectInput("model_filt_spid", "Filter Model type", choices = ""),
              selectInput("family_filt_spid", "Filter Product Family", choices = ""),
              selectInput("dose_filt_spid", "Filter dose", choices = ""),
              awesomeCheckbox("add_2spider", "Add a second sample", value = FALSE),
              conditionalPanel(
                condition = "input.add_2spider == true",
                selectInput("model_filt_spid2", "Filter Model type", choices = ""),
                selectInput("family_filt_spid2", "Filter Product Family", choices = ""),
                selectInput("dose_filt_spid2", "Filter dose", choices = ""),
                awesomeCheckbox("add_3spider", "Add a third sample", value = FALSE),
                conditionalPanel(
                  condition = "input.add_3spider == true",
                  selectInput("model_filt_spid3", "Filter Model type", choices = ""),
                  selectInput("family_filt_spid3", "Filter Product Family", choices = ""),
                  selectInput("dose_filt_spid3", "Filter dose", choices = "")
                )
                               
              )
            ),
            
            mainPanel(
              width = 9,
              plotOutput("spidplot")
            )
          )
        ),
        
        #### Heatmap ####
        tabPanel("Heatmap",
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     div(actionButton("makeheatmap", label = "Make Heatmap", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:130%; font-weight: bold;'), align= "center"),
                     br(),
                     h4(strong("Data filtering")),
                     selectInput("typeeval_heat", "Select a measure", choices = c("Cytoxicity", "Vitality")),
                     fluidRow(
                       column(6, selectInput("prod_filt_heatmap", "Filter Product Family", choices = "")),
                       column(6, selectInput("mod_filt_heatmap", "Filter Model type", choices = "",multiple = TRUE))
                     ),
                     fluidRow(
                       column(6, selectInput("dose_op_heatmap", "Operation with doses", choices = c("filter", "mean", "subtract"))),
                       
                       conditionalPanel(
                         condition = "input.dose_op_heatmap == 'filter'",
                         column(6, radioButtons("filt_dose", "Filter dose", choices = "",inline = TRUE))
                       ),
                       
                       conditionalPanel(
                         condition = "input.dose_op_heatmap == 'subtract'",
                         column(4, selectInput("subdose_heatmap", "Subtract:", choices = c("30-5"))),
                         column(1, style="padding-top: 5px;",br(), actionButton("revdose_heat", icon("exchange-alt")))
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
                     fluidRow(column(5, style="padding-top: 5px;", awesomeCheckbox("show_valheat", "Show cell values")),
                              column(7, downloadButton("download_heat", "Download data heatmap"))),
                     h4(strong("Annotations")),
                     fluidRow(
                       column(6, selectInput("selectannot_row", "Row annotation:", choices = c("Model_Family","Experiment_id"))),
                       column(6, selectInput("selectannot_col", "Column annotation:", choices = "Product_Family"))
                     ),
                     
                     hr(),
                     h4(strong("Dendrogramm options")),
                     ###dendrogramm on column or row?
                     h5(strong("Where to show dendrogramm")),
                     fluidRow(
                       column(6, materialSwitch(inputId = "rowdend", label = "Row",  value = TRUE, status = "primary", width = "90%")),
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
      
    )
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

