#' bubble_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
#' 
mod_bubble_plot_ui <- function(id, size_choices = c("CV", "Corrected_value")){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4(strong("Data filtering")),
        fluidRow(
          column(6,selectInput(ns("typeeval_bubb"), "Select a measure", choices = c("Cytotoxicity", "Vitality"))),
          column(6, selectInput(ns("prod_filt_bubb"), "Select a Product Family", choices = ""))
        ),
        selectInput(ns("purif_filt_bubb"), "Select a purification", choices = "", multiple = FALSE),
        fluidRow(
          column(6, selectInput(ns("mod_filt_bubb"), "Filter Model type (rows)", choices = "",multiple = TRUE)),
          column(6, selectInput(ns("column_filt_bubb"), "Filter Product (columns)", choices = "",multiple = TRUE))
        ),
        fluidRow(
          column(6, selectInput(ns("dose_op_bubb"), "Operation with doses", choices = c("filter", "mean", "subtract"))),
          

          column(
            6,
            conditionalPanel(
              condition = "input.dose_op_bubb == 'filter'", ns= ns,
              radioButtons(ns("filt_dose_bubb"), "Filter dose", choices = "",inline = TRUE)),
            
            conditionalPanel(
              condition = "input.dose_op_bubb == 'subtract'", ns = ns,
              fluidRow(
                column(6,selectInput(ns("subdose_bubb"), "Subtract:", choices = c("30-5"))),
                column(6,style="padding-top: 5px;",br(), actionButton(ns("revdose_bubb"), icon("exchange-alt"))))
            )
            
          )
          

        ),
        
        h4(strong("Plot options")),
        fluidRow(
          column(7, selectInput(ns("varsize_bubb"), "Variable for size argument", choices = size_choices)),
          conditionalPanel(
            condition = "input.varsize_bubb == 'CV'", ns = ns,
            column(5, h5(strong("Filter per CV")), materialSwitch(ns("CV_filtering"), value = FALSE, status = "primary"))
          )
        ),
        conditionalPanel(
          condition = "input.CV_filtering == true && input.varsize_bubb == 'CV'", ns = ns,
          sliderInput(ns("CV_threshold"), "High CV threshold", min = 0, max = 1, value = 1, step = 0.05)
        ),
        hr(),
        h4(strong("Data bubbleplot")),
        fluidRow(column(5, actionButton(ns("view_databubb"), "Check Data", icon("eye"))),
                 column(7, downloadButton(ns("download_bubb"), "Download data"), style = "text-align:right")),
        
        tags$head(tags$style(paste0("#", ns("viewdt_bubble")," .modal-dialog{ width:1300px}"))),
        shinyBS::bsModal(
          ns("viewdt_bubble"), trigger = ns("view_databubb"), title = "Data Table Heatmap",
          div(DT::DTOutput(ns("dt_bubble")), style = "overflow-x: scroll;")
        )
        
      ),
      
      mainPanel(
        width = 9,
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("bubbleplot"),height = "650px"))
      )
    )
 
  )
}
    
#' bubble_plot Server Functions
#' 
#' @importFrom stats na.omit
#'
#' @noRd 
mod_bubble_plot_server <- function(id, data, type_data = "cyto"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    observeEvent(data(),{
      if(type_data == "D1"){
        marker = colnames(dplyr::select(data(), where(is.double),-dplyr::starts_with(c("Cyto", "Vita")),-Dose, -dplyr::ends_with(".CV")))
        updateSelectInput(session, "typeeval_bubb", choices = c("Cytotoxicity", "Vitality", marker))
      }
    })
    
    
    prod_total = reactive({
      req(data())
      data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
    })
    
    
    # DATA STORAGE
    values_comb_bubb <- reactiveValues(
      comb = NULL # original data
    )
    
    observeEvent(prod_total(),{
      updateSelectInput(session, "prod_filt_bubb", choices = unique(prod_total()$Product_Family))
    })
    
    #purification filtering
    observeEvent(input$prod_filt_bubb,{
      doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_bubb)
      updateSelectInput(session, "purif_filt_bubb", choices = unique(doses$Purification), selected = unique(doses$Purification)[1])
    })
    
    
    observeEvent(c(input$prod_filt_bubb, input$purif_filt_bubb),{
      doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_bubb) %>% 
        dplyr::filter(Purification == input$purif_filt_bubb)
      
      #row filtering
      updateSelectInput(session, "mod_filt_bubb", choices = c("All", unique(doses$Model_type)), selected = "All")
      
      #column filtering
      updateSelectInput(session, "column_filt_bubb", choices = c("All", "CTRL", "CTRL+", input$prod_filt_bubb), selected = "All")
      
    })
    
    
    observe({
      doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_bubb) %>% 
        dplyr::filter(Purification == input$purif_filt_bubb)
      
      if(!("All" %in% input$mod_filt_bubb)){
        doses = dplyr::filter(doses, Model_type %in% input$mod_filt_bubb)
      }
      
      values_comb_bubb$comb <- rev(sort(unique(doses$Dose)))
      updateRadioButtons(session, "filt_dose_bubb", choices = c("All",sort(unique(doses$Dose))),inline = TRUE)
    })
    

    observeEvent(input$revdose_bubb,{
      values_comb_bubb$comb <- rev(values_comb_bubb$comb)
    })
    
    observeEvent(values_comb_bubb$comb,{
      req(values_comb_bubb$comb)
      if(!is.null(values_comb_bubb$comb)){
        combin = utils::combn(values_comb_bubb$comb, 2, paste, collapse = '-')
        updateSelectInput(session, "subdose_bubb", choices = combin)
      }
    })
    
    

    observeEvent(input$column_filt_bubb,{
      if(input$column_filt_bubb %in% input$prod_filt_bubb || "All" %in% input$column_filt_bubb){
        updateSelectInput(session, "dose_op_bubb", choices = c("filter", "mean", "subtract"))
      }else{
        updateSelectInput(session, "dose_op_bubb", choices = c("filter", "mean"))
      }
    })
    
    
    
    data_bubble = reactive({
      req(prod_total())
      
      
      type_cv = ifelse(type_data == "cyto", input$varsize_bubb, paste0(input$typeeval_bubb, ".CV"))
      
      #measure type
      type_meas = ifelse(input$typeeval_bubb == "Cytotoxicity", "Cytotoxicity.average", 
                         ifelse(input$typeeval_bubb == "Vitality", "Vitality.average", input$typeeval_bubb))
      

      CBC150 = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_bubb) %>% 
        dplyr::filter(Purification == input$purif_filt_bubb)
      
      ### model type filtering
      if(is.null(input$mod_filt_bubb)){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select something in the model type filtering.")), type = "error")
        validate(need(input$mod_filt_bubb, "Select something in the model type filtering."))
      }
      
      if(!("All" %in% input$mod_filt_bubb)){
        CBC150 = dplyr::filter(CBC150, Model_type %in% input$mod_filt_bubb)
      }
      
      #control
      filt_cnt = data() %>% dplyr::filter(if_any("Product_Family", ~grepl("CTRL",.))) %>%
        dplyr::filter(Experiment_id %in% unique(CBC150$Experiment_id)) %>%
        tidyr::unite("Product", Product, Dose, sep = " ")
      

      
      ###filtering option
      if(input$dose_op_bubb == "filter"){
        
        
        if(input$filt_dose_bubb != "All"){
          cbc_filtered = dplyr::filter(CBC150, Dose %in% input$filt_dose_bubb)
          filt_cnt = filt_cnt %>% dplyr::filter(Experiment_id %in% unique(cbc_filtered$Experiment_id))
        }else{
          cbc_filtered = CBC150
        }
        
        cbc_filtered = split(cbc_filtered, f = ~Dose) %>% lapply(function(x) {
          x = dplyr::bind_rows(x, filt_cnt)
          x$Dose[is.na(x$Dose)] <-  unique(na.omit(x$Dose))
          x
        })
        

        cbc_filtered = lapply(cbc_filtered, function(x){
          if(length(unique(x$Experiment_id)) > length(unique(x$Model_type))){
            showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                           Duplicated will be averaged.")), type = "default")
            message("There are multiple Experiment ID for the same Model_type. Duplicated will be averaged.")
            x %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric))), Dose) %>% 
              dplyr::summarise(across(c(type_meas, type_cv), mean, na.rm = T)) %>% dplyr::ungroup()
          }else{x}
        })
        
        cbc_filtered = data.table::rbindlist(cbc_filtered) %>% as.data.frame() 
   

        #####mean option
      }else if(input$dose_op_bubb == "mean"){
        
        cbc_filtered = CBC150 %>% dplyr::group_by(across(-c(where(is.numeric))))%>% 
          dplyr::summarise(across(where(is.double) & !Dose, mean, na.rm = T)) %>% dplyr::ungroup() %>% 
          dplyr::bind_rows(filt_cnt)
        
        if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
          cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>% 
            dplyr::summarise(across(c(type_meas, type_cv), mean, na.rm = T)) %>% dplyr::ungroup()
        }
        
        ##### subtract option
      }else{
        combination = strsplit(input$subdose_bubb, "-")
        cbc_filtered = CBC150 %>% dplyr::group_by(across(-c(where(is.numeric), Dose))) %>% 
          dplyr::summarise(across(c(type_meas, type_cv), ~ .x[Dose == combination[[1]][1]] - .x[Dose == combination[[1]][2]], na.rm = T)) %>% 
          dplyr::ungroup() %>% dplyr::bind_rows(filt_cnt)
        
        if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
          cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>% 
            dplyr::summarise(across(c(type_meas, type_cv), mean, na.rm = T)) %>% dplyr::ungroup()
        }
        
      }
      
      
      ##column (product) filtering
      if(is.null(input$column_filt_bubb)){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select something in the product (columns) filtering.")), type = "error")
        validate(need(input$column_filt_bubb, "Select something in the product (columns) filtering."))
      }
      
      if(!("All" %in% input$column_filt_bubb)){
        if(class(cbc_filtered)[1] == "list"){
          lapply(cbc_filtered, function(x) x %>% dplyr::filter(Product_Family %in% input$column_filt_bubb))
        }else{
          dplyr::filter(cbc_filtered, Product_Family %in% input$column_filt_bubb)
        }
      }else{
        cbc_filtered
      }
      
    })
    
    
    output$bubbleplot = renderPlotly({
      req(data_bubble())
      type_meas = ifelse(input$typeeval_bubb == "Cytotoxicity", "Cytotoxicity.average", 
                         ifelse(input$typeeval_bubb == "Vitality", "Vitality.average", input$typeeval_bubb))
      
      type_cv = ifelse(type_data == "cyto", input$varsize_bubb, paste0(input$typeeval_bubb, ".CV"))
      ord = order_data(data_bubble(),as_factor = TRUE)

      if(input$CV_filtering == TRUE & input$varsize_bubb == 'CV'){
        ord = ord %>% dplyr::mutate(Shape = dplyr::case_when(is.na(dplyr::across(type_cv)) ~ "NA", 
                                                      dplyr::across(type_cv) > input$CV_threshold ~ "Large CV", 
                                                      TRUE ~ "Low CV"))
        
        temp = ggplot(data = dplyr::filter(ord, !is.na(dplyr::across(type_cv))), 
                      aes(x = Product, y = Model_type, size = !!sym(type_cv), color = !!sym(type_meas), shape = Shape)) +
          geom_point(alpha = 0.75, na.rm=FALSE) + scale_size_continuous(range = c(1,10)) + 
          scale_shape_manual(values=c("Large CV"=10, "Low CV"=19, "NA"= 17)) +
          theme(panel.background = element_rect(fill = "#C8C8C8"), axis.text.x = element_text(angle = 90,vjust = 0.4,hjust = 1))
        
        #se ci sono NA in type_cv aggiungo i punti
        if(TRUE %in% is.na(ord[,type_cv])){
          temp = temp + geom_point(data = dplyr::filter(ord, is.na(dplyr::across(type_cv))), aes(shape='NA'), size=4)
        }
        
      }else{
        
        temp = ggplot(ord, aes(x = Product, y = Model_type, size = !!sym(type_cv), color = !!sym(type_meas))) +
          geom_point(alpha = 0.75, shape = 16) + 
          scale_size_continuous(range = c(1,10)) +
          theme(panel.background = element_rect(fill = "#C8C8C8"), axis.text.x = element_text(angle = 90,vjust = 0.4,hjust = 1))
        
        #se ci sono NA in type_cv aggiungo i punti
        if(TRUE %in% is.na(ord[,type_cv])){
          temp = temp + geom_point(data = dplyr::filter(ord, is.na(dplyr::across(type_cv))), aes(shape='NA'), size=4) +
            scale_shape_manual(values=c('NA'=17, 'Not NA'=19))
        }
        
      }
      

      
      if(input$dose_op_bubb == "filter"){
        temp = temp + facet_wrap(~Dose)
      }
      
      if(input$typeeval_bubb == "Cytotoxicity" || input$typeeval_bubb == "Vitality"){
        if(input$dose_op_bubb == "subtract"){
          temp = temp + scale_color_gradient2(low = "green", mid = "white", high = "red", limits = c(-100,100))
        }else{
          temp = temp + scale_color_gradient(low = "white", high = "blue", limits = c(0,100))
        }
      }else{
        temp = temp + scale_color_gradient(low = "white", high = "blue")
      }
      
      
      
      plotly::ggplotly(temp)
      
    })
    
    
    
    output$dt_bubble = renderDT({
      req(data_bubble())
      if(input$dose_op_bubb == "filter"){
        if(input$filt_dose_bubb == "All"){
          data_bubble() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
        }else{
          data_bubble() %>% dplyr::filter(Dose == input$filt_dose_bubb) %>% 
            dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
        }
        
      }else{
        data_bubble() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
      }
    })
    
    
    #### Download handler for the download button
    output$download_bubb <- downloadHandler(
      #put the file name with also the file extension
      filename = function() {
        paste0("Data_bubbleplot", Sys.Date(), ".xlsx")
      },
      
      # This function should write data to a file given to it by the argument 'file'.
      content = function(file) {
        openxlsx::write.xlsx(data_bubble(), file)
      }
    )
    
    
    
 
  })
}
    
## To be copied in the UI
# mod_bubble_plot_ui("bubble_plot_ui_1")
    
## To be copied in the server
# mod_bubble_plot_server("bubble_plot_ui_1")
