#' heatmap_cyto_repo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_heatmap_cyto_repo_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    
    div(actionButton(ns("makeheatmap"), label = "Make Heatmap", class = "btn btn-primary btn-lg", width = "140px", style='padding:5px; font-size:130%; font-weight: bold;'), align= "center"),
    br(),
    h4(strong("Data filtering")),
    fluidRow(
      column(6,selectInput(ns("typeeval_heat"), "Select a measure", choices = c("Cytotoxicity", "Vitality"))),
      column(6, selectInput(ns("prod_filt_heatmap"), "Select a Product Family", choices = ""))
    ),
    selectInput(ns("purif_filt_heat"), "Select a purification", choices = "", multiple = FALSE),
    fluidRow(
      column(6, selectInput(ns("mod_filt_heatmap"), "Filter Model type (rows)", choices = "",multiple = TRUE)),
      column(6, selectInput(ns("column_filt_heatmap"), "Filter Product (columns)", choices = "",multiple = TRUE))
    ),
    
    
    fluidRow(
      column(6, selectInput(ns("dose_op_heatmap"), "Operation with doses", choices = c("filter", "mean", "subtract"))),
      
      column(
        6,
        conditionalPanel(
          condition = "input.dose_op_heatmap == 'filter'", ns = ns,
          radioButtons(ns("filt_dose"), "Filter dose", choices = "",inline = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.dose_op_heatmap == 'subtract'", ns = ns,
          fluidRow(
            column(7, selectInput(ns("subdose_heatmap"), "Subtract:", choices = c("30-5"))),
            column(5, style="padding-top: 5px;",br(), actionButton(ns("revdose_heat"), icon("exchange-alt"))))
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
      column(5, style="padding-top: 5px;", br(), awesomeCheckbox(ns("show_valheat"), "Show cell values")),
      column(7, 
             conditionalPanel(
               condition = "input.show_valheat == true", ns = ns,
               sliderInput(ns("range_showvalheat"), "Threshold", value = 0, min = 0, max = 100)))),
    fluidRow(column(5, actionButton(ns("view_dataheat"), "Check Data", icon("eye"))),
             column(7, downloadButton(ns("download_heat"), "Download data heatmap"), style = "text-align:right")),
    tags$head(tags$style(paste0("#", ns("viewdt_heatmap")," .modal-dialog{ width:1300px}"))),
    shinyBS::bsModal(
      ns("viewdt_heatmap"), trigger = ns("view_dataheat"), title = "Data Table Heatmap",
      fluidRow(
        conditionalPanel(condition = "input.dose_op_heatmap == 'filter'", ns = ns,
                         column(2, selectInput(ns("dose_dtheatmap"), "Select a dose", choices = ""))),
        column(10, div(DT::DTOutput(ns("dt_heatmap")), style = "overflow-x: scroll;")))
    ),
    hr(),
    h4(strong("Annotations")),
    fluidRow(
      column(6, selectInput(ns("selectannot_row"), "Row annotation:", choices = c("Model_Family","Experiment_id", "Corrected_value"), multiple = TRUE)),
      column(6, selectInput(ns("selectannot_col"), "Column annotation:", choices = "Product_Family"))
    ),
    
    hr(),
    h4(strong("Dendrogramm options")),
    ###dendrogramm on column or row?
    h5(strong("Where to show dendrogramm")),
    fluidRow(
      column(6, materialSwitch(ns("rowdend"), label = "Row",  value = FALSE, status = "primary", width = "90%")),
      column(6, materialSwitch(ns("columndend"), label = "Column",  value = FALSE, status = "primary", width = "90%"))
    ),
    
    conditionalPanel(condition = "input.rowdend == 1 || input.columndend == 1", ns = ns,
                     fluidRow(
                       column(6,
                              selectInput(ns("seldistheat"), "Distance function:", choices = c("euclidean", "maximum", "canberra"), selected = "euclidean") #, "minkowski","manhattan",
                       ),
                       column(6,
                              selectInput(ns("selhclustheat"), "Clustering method:", choices = c("ward.D2", "complete", "average" , "median"), selected = "complete") #, "centroid","mcquitty","ward.D2", "single", 
                       )
                     )
    ),
    
    conditionalPanel(condition = "input.rowdend == 0", ns = ns,
                     h5(strong("Order data by annotation?")),
                     awesomeCheckbox(ns("heatsort"), label = "Order", value = TRUE)
    ),
    
    conditionalPanel(condition = "input.rowdend == 1", ns = ns,
                     hr(),
                     sliderInput(ns("sliderrowheat"), "Column cluster number:", min=2, max = 10, value=2, step = 1)
    ),
    
    conditionalPanel(condition = "input.columndend == 1", ns = ns,
                     hr(),
                     sliderInput(ns("slidercolheat"), "Column cluster number:", min=2, max = 10, value=2, step = 1)
    )
    
  )
}
    
#' heatmap_cyto_repo Server Functions
#'
#' @noRd 
mod_heatmap_cyto_repo_server <- function(id, data, data_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(data(),{
      if(data_type() == "cyto"){
        updateSelectInput(session, "typeeval_heat", choices = c("Cytotoxicity", "Vitality"))
      }
      if(data_type() == "SEAP"){
        updateSelectInput(session, "typeeval_heat", choices = c("Concentration"))
      }
      if(data_type() == "TREM2"){
        updateSelectInput(session, "typeeval_heat", choices = c("Cytotoxicity", "Vitality", "GFP"))
      }
    })
    
    observe({
      if(is.null(input$selectannot_row) || length(input$selectannot_row) >1){
        updateAwesomeCheckbox(session, "heatsort", value = FALSE)
        shinyjs::disable("heatsort")
      }else{
        shinyjs::enable("heatsort")
      }
    })
    
    prod_total = reactive({
      req(data())
      data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
    })
    
    # DATA STORAGE
    values_comb_heat <- reactiveValues(
      comb = NULL # original data
    )
    
    observeEvent(prod_total(),{
      updateSelectInput(session, "prod_filt_heatmap", choices = unique(prod_total()$Product_Family))
    })
    
    #purification filtering
    observeEvent(input$prod_filt_heatmap,{
      doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap)
      updateSelectInput(session, "purif_filt_heat", choices = unique(doses$Purification), selected = unique(doses$Purification)[1])
    })
    
    
    observeEvent(c(input$prod_filt_heatmap, input$purif_filt_heat),{
      req(input$prod_filt_heatmap)
      req(input$purif_filt_heat)
      
      doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap) %>% 
        dplyr::filter(Purification %in% input$purif_filt_heat)
      
      #row filtering
      updateSelectInput(session, "mod_filt_heatmap", choices = c("All", unique(doses$Model_type)), selected = "All")
      
      #column filtering
      updateSelectInput(session, "column_filt_heatmap", choices = c("All", "CTRL", "CTRL+", input$prod_filt_heatmap), selected = "All")
      
      updateRadioButtons(session, "filt_dose", choices = c("All",unique(doses$Dose)),inline = TRUE)
      updateSelectInput(session, "dose_dtheatmap", choices = unique(doses$Dose))
      
      if(data_type() == "cyto"){
        if(length(unique(doses$Experiment_id)) > length(unique(doses$Model_type))){
          updateSelectInput(session, "selectannot_row", choices = c("Model_Family", "Corrected_value"), selected = "Model_Family")
        }else{
          updateSelectInput(session, "selectannot_row", choices = c("Model_Family","Experiment_id","Corrected_value"), selected = "Model_Family")
        }
      }
      if(data_type() == "TREM2"){
        if(length(unique(doses$Experiment_id)) > length(unique(doses$Model_type))){
          updateSelectInput(session, "selectannot_row", choices = "")
        }else{
          updateSelectInput(session, "selectannot_row", choices = c("Experiment_id"))
        }
      }
      
      if(data_type() == "SEAP"){
        if(length(unique(doses$Experiment_id)) > length(unique(doses$Model_type))){
          updateSelectInput(session, "selectannot_row", choices = c("Model_Family", "Calibration_ID"), selected = "Model_Family")
        }else{
          updateSelectInput(session, "selectannot_row", choices = c("Model_Family","Experiment_id","Calibration_ID"), selected = "Model_Family")
        }
      }
      
    })
    
    
    
    #### for doses
    observeEvent(input$column_filt_bubb,{
      if(input$column_filt_heatmap %in% input$prod_filt_heatmap || "All" %in% input$column_filt_heatmap){
        updateSelectInput(session, "dose_op_heatmap", choices = c("filter", "mean", "subtract"))
      }else{
        updateSelectInput(session, "dose_op_heatmap", choices = c("filter", "mean"))
      }
    })
    
    
    observe({
      doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap) %>% 
        dplyr::filter(Purification %in% input$purif_filt_heat)
      
      if(!("All" %in% input$mod_filt_heatmap)){
        doses = dplyr::filter(doses, Model_type %in% input$mod_filt_heatmap)
      }
      
      values_comb_heat$comb <- rev(sort(unique(doses$Dose)))
      updateRadioButtons(session, "filt_dose_bubb", choices = c("All",sort(unique(doses$Dose))),inline = TRUE)
      
    })
    
    
    observeEvent(input$revdose_bubb,{
      values_comb_heat$comb <- rev(values_comb_heat$comb)
    })
    
    observeEvent(values_comb_heat$comb,{
      req(values_comb_heat$comb)
      if(!is.null(values_comb_heat$comb)){
        combin = utils::combn(values_comb_heat$comb, 2, paste, collapse = '-')
        updateSelectInput(session, "subdose_heatmap", choices = combin)
      }
    })
    
    
    
    # observeEvent(input$prod_filt_heatmap,{
    #   doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap)
    #   updateRadioButtons(session, "filt_dose", choices = c("All",unique(doses$Dose)),inline = TRUE)
    #   updateSelectInput(session, "dose_dtheatmap", choices = unique(doses$Dose))
    #   
    #   CBC150 = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap)
    #   if(length(unique(CBC150$Experiment_id)) > length(unique(CBC150$Model_type))){
    #     updateSelectInput(session, "selectannot_row", choices = c("Model_Family", "Corrected_value"), selected = "Model_Family")
    #   }else{
    #     updateSelectInput(session, "selectannot_row", choices = c("Model_Family","Experiment_id","Corrected_value"), selected = "Model_Family")
    #   }
    #   
    #   
    # })
    
    
    
    
    # #slider for columns
    # output$sliderheatcol <- renderUI({
    #   req(dataforheatmap())
    #   len = SummarizedExperiment::rowData(dataforheatmap())$Class %>% unique() %>% length() #numero di lipidi (poi si ruota)
    #   sliderInput("slidercolheat", "Column cluster number:", min=2, max = len, value=2, step = 1)
    # })
    # 
    # #slider for rows
    # output$sliderheatrow <- renderUI({
    #   req(dataforheatmap())
    #   len = SummarizedExperiment::colData(dataforheatmap())$SampleID %>% unique() %>% length() #numero di sample (poi si ruota)
    #   sliderInput("sliderrowheat", "Row cluster number:", min = 2, max = len, value = 2, step = 1)
    # })
    # 
    
    
    
    data_heatmap = reactive({
      req(prod_total())
      
      CBC150 = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap) %>% 
        dplyr::filter(Purification %in% input$purif_filt_heat)
      
      ### model type filtering
      if(is.null(input$mod_filt_heatmap)){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select something in the model type filtering.")), type = "error")
        validate(need(input$mod_filt_heatmap, "Select something in the model type filtering."))
      }
      
      if(!("All" %in% input$mod_filt_heatmap)){
        if(length(input$mod_filt_heatmap) < 2 && input$rowdend == TRUE){
          showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select at least two model types or disable row clustering.")), type = "error")
          validate(need(length(input$mod_filt_heatmap) > 2, "Select at least two model types or disable row clustering."))
        }
        CBC150 = dplyr::filter(CBC150, Model_type %in% input$mod_filt_heatmap)
      }
      
      #control
      filt_cnt = data() %>% dplyr::filter(if_any("Product_Family", ~grepl("CTRL",.))) %>%
        dplyr::filter(Experiment_id %in% unique(CBC150$Experiment_id)) %>%
        tidyr::unite("Product", Product, Dose, sep = " ")
      
      #measure type
      #type_meas = ifelse(input$typeeval_heat == "Cytotoxicity", "Cytotoxicity.average", "Vitality.average")
      #type_meas = ifelse(input$typeeval_heat %in% c("Cytotoxicity", "Vitality", "Concentration", "GFP"), paste0(input$typeeval_heat,".average"),
      #                   input$typeeval_heat)
      
      ###filtering option
      if(input$dose_op_heatmap == "filter"){
        cbc_filtered = split(CBC150, f = ~Dose) %>% lapply( function(x) dplyr::bind_rows(x, filt_cnt))
        
        
        cbc_filtered = lapply(cbc_filtered, function(x){
          if(length(unique(x$Experiment_id)) > length(unique(x$Model_type))){
            showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                           Duplicated will be averaged.")), type = "default")
            x %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>% 
              dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
          }else{x}
        })
        
        #####mean option
      }else if(input$dose_op_heatmap == "mean"){
        
        cbc_filtered = CBC150 %>% dplyr::group_by(across(-c(where(is.numeric))))%>% 
          dplyr::summarise(across(where(is.double) & !Dose, mean, na.rm = T)) %>% dplyr::ungroup() %>% 
          dplyr::bind_rows(filt_cnt)
        
        if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
          cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>% 
            dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
        }
        
        ##### subtract option
      }else{
        combination = strsplit(input$subdose_heatmap, "-")
        cbc_filtered = CBC150 %>% dplyr::group_by(across(-c(where(is.numeric)))) %>% 
          dplyr::summarise(across(where(is.double), ~ .x[Dose == combination[[1]][1]] - .x[Dose == combination[[1]][2]], na.rm = T)) %>% 
          dplyr::ungroup() %>% dplyr::bind_rows(filt_cnt)
        
        if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
          cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>% 
            dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
        }
        
      }
      
      #cbc_filtered
      ##column (product) filtering
      if(is.null(input$column_filt_heatmap)){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select something in the product (columns) filtering.")), type = "error")
        validate(need(input$column_filt_heatmap, "Select something in the product (columns) filtering."))
      }
      
      if(!("All" %in% input$column_filt_heatmap)){
        if(input$column_filt_heatmap == "CTRL" && input$columndend == TRUE){
          showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select at least two products (columns) or disable column clustering.")), type = "error")
          validate(need(input$column_filt_heatmap == "CTRL", "Select at least two products (columns) or disable column clustering."))
        }
        if(class(cbc_filtered)[1] == "list"){
          lapply(cbc_filtered, function(x) x %>% dplyr::filter(Product_Family %in% input$column_filt_heatmap))
        }else{
          dplyr::filter(cbc_filtered, Product_Family %in% input$column_filt_heatmap)
        }
      }else{
        cbc_filtered
      }
      
    })
    
    
    heatmap = eventReactive(input$makeheatmap,{
      req(data_heatmap())
      
      if(!("All" %in% input$column_filt_heatmap)){
        if(input$column_filt_heatmap == "CTRL" && input$columndend == TRUE){
          showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select at least two products (columns) or disable column clustering.")), type = "error")
          validate(need(input$column_filt_heatmap == "CTRL", "Select at least two products (columns) or disable column clustering."))
        }
      }
      
      
      ht_list = NULL
      #measure type
      #type_meas = ifelse(input$typeeval_heat == "Cytotoxicity", "Cytotoxicity.average", "Vitality.average")
      #type_meas = ifelse(input$typeeval_heat %in% c("Cytotoxicity", "Vitality", "Concentration", "GFP"), paste0(input$typeeval_heat,".average"),
      #                   input$typeeval_heat)
      type_meas = paste0(input$typeeval_heat,".average")
      
      
      if(input$typeeval_heat == "Concentration"){
        color_scale = c("white", "blue")
      }else{
        color_scale = circlize::colorRamp2(c(0, 100), c("white", "blue"))
      }

      if(input$dose_op_heatmap == "filter"){
        
        if(input$filt_dose == "All"){
          doses = as.character(names(data_heatmap()))
        }else{
          doses = as.character(input$filt_dose)
        }
        for (i in doses){
          if(input$typeeval_heat %in% c("Cytotoxicity", "Vitality", "GFP")){
            unit_legend = paste("%",input$typeeval_heat,"at",i,"ug/mL")
          }else if(input$typeeval_heat == "Concentration"){
            unit_legend = paste("ng/mL",input$typeeval_heat,"at",i,"ug/mL")
          }
          ht_list = ht_list + make_heatmap(
            data = as.data.frame(data_heatmap()[[i]]),
            add_rowannot = input$selectannot_row,
            add_colannot = input$selectannot_col,
            title = paste(input$prod_filt_heatmap,i,"ug/mL"),
            order_data = input$heatsort,
            row_dend = input$rowdend, 
            row_nclust = input$sliderrowheat, 
            col_dend = input$columndend, 
            col_nclust = input$slidercolheat, 
            dist_method = input$seldistheat, 
            clust_method = input$selhclustheat, 
            unit_legend = unit_legend,
            add_values = input$show_valheat,
            thresh_values = input$range_showvalheat,
            typeeval_heat = type_meas,
            color_scale = color_scale
          )
        }
      }else if(input$dose_op_heatmap == "mean"){
        if(input$typeeval_heat %in% c("Cytotoxicity", "Vitality", "GFP")){
          unit_legend = paste("%",input$typeeval_heat)
        }else if(input$typeeval_heat == "Concentration"){
          unit_legend = paste("ng/mL",input$typeeval_heat)
        }
        ht_list = make_heatmap(
          data = data_heatmap(),
          add_rowannot = input$selectannot_row,
          add_colannot = input$selectannot_col,
          title = paste(input$prod_filt_heatmap),
          order_data = input$heatsort,
          row_dend = input$rowdend, 
          row_nclust = input$sliderrowheat, 
          col_dend = input$columndend, 
          col_nclust = input$slidercolheat, 
          dist_method = input$seldistheat, 
          clust_method = input$selhclustheat, 
          unit_legend = unit_legend,
          add_values = input$show_valheat,
          thresh_values = input$range_showvalheat,
          typeeval_heat = type_meas,
          color_scale = color_scale
        )
      }else{
        if(input$typeeval_heat %in% c("Cytotoxicity", "Vitality", "GFP")){
          unit_legend = paste("%",input$typeeval_heat)
        }else if(input$typeeval_heat == "Concentration"){
          unit_legend = paste("ng/mL",input$typeeval_heat)
        }
        ht_list = make_heatmap(
          data = data_heatmap(),
          add_rowannot = input$selectannot_row,
          add_colannot = input$selectannot_col,
          title = paste(input$prod_filt_heatmap,input$subdose_heatmap,"ug/mL"),
          order_data = input$heatsort,
          row_dend = input$rowdend, 
          row_nclust = input$sliderrowheat, 
          col_dend = input$columndend, 
          col_nclust = input$slidercolheat, 
          dist_method = input$seldistheat, 
          clust_method = input$selhclustheat, 
          unit_legend = paste("%",input$typeeval_heat),
          color_scale = circlize::colorRamp2(c(-100, 0, 100), c("green","white", "red")),
          add_values = input$show_valheat,
          thresh_values = input$range_showvalheat,
          typeeval_heat = type_meas
        )
      }
      ComplexHeatmap::draw(ht_list, merge_legend = TRUE, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(3, "cm"))
    })
    
    
    
    # observeEvent(heatmap(),{
    #   InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, heatmap(), heatmap_id  = "heatmap_output")
    # })
    
    
    output$dt_heatmap = renderDT({
      req(data_heatmap())
      if(input$dose_op_heatmap == "filter"){
        data_heatmap()[[input$dose_dtheatmap]] %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
      }else{
        data_heatmap() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
      }
    })
    
    
    #### Download handler for the download button
    output$download_heat <- downloadHandler(
      #put the file name with also the file extension
      filename = function() {
        paste0("Data_heatmap", Sys.Date(), ".xlsx")
      },
      
      # This function should write data to a file given to it by the argument 'file'.
      content = function(file) {
        openxlsx::write.xlsx(data_heatmap(), file)
      }
    )
    
    
    return(reactive({
      req(heatmap())
      return(heatmap())
    }))
    
 
  })
}
    
## To be copied in the UI
# mod_heatmap_cyto_repo_ui("heatmap_cyto_repo_1")
    
## To be copied in the server
# mod_heatmap_cyto_repo_server("heatmap_cyto_repo_1")
