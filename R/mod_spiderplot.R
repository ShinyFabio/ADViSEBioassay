#' spiderplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spiderplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(ns("typeeval_spid"), "Select a measure", choices = c("Cytotoxicity", "Vitality")),
        fluidRow(
          column(6, selectInput(ns("model_filt_spid"), "Filter Model type", choices = "")),
          column(6, selectInput(ns("family_filt_spid"), "Filter Product Family", choices = ""))),
        fluidRow(
          column(6, selectInput(ns("dose_filt_spid"), "Filter dose", choices = "")),
          column(6, radioButtons(ns("purif_filt_spid"), "Filter purification", choices = "",inline = TRUE))),
        awesomeCheckbox(ns("add_2spider"), "Add a second sample", value = FALSE),
        conditionalPanel(
          condition = "input.add_2spider == true", ns = ns,
          fluidRow(
            column(6, selectInput(ns("model_filt_spid2"), "Filter Model type", choices = "")),
            column(6, selectInput(ns("family_filt_spid2"), "Filter Product Family", choices = ""))),
          fluidRow(
            column(6, selectInput(ns("dose_filt_spid2"), "Filter dose", choices = "")),
            column(6, radioButtons(ns("purif_filt_spid2"), "Filter purification", choices = "",inline = TRUE))),
          awesomeCheckbox(ns("add_3spider"), "Add a third sample", value = FALSE),
          conditionalPanel(
            condition = "input.add_3spider == true", ns = ns,
            fluidRow(
              column(6, selectInput(ns("model_filt_spid3"), "Filter Model type", choices = "")),
              column(6, selectInput(ns("family_filt_spid3"), "Filter Product Family", choices = ""))),
            fluidRow(
              column(6, selectInput(ns("dose_filt_spid3"), "Filter dose", choices = "")),
              column(6, radioButtons(ns("purif_filt_spid3"), "Filter purification", choices = "",inline = TRUE)))
          )
          
        )
      ),
      
      mainPanel(
        width = 9,
        shinycssloaders::withSpinner(plotOutput(ns("spidplot"),  height = 600))
      )
    )
 
  )
}
    
#' spiderplot Server Functions
#'
#' @noRd 
mod_spiderplot_server <- function(id, data, type_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    

    
    observeEvent(data(),{
      updateSelectInput(session, "model_filt_spid", choices = unique(data()$Model_type))
      updateSelectInput(session, "model_filt_spid2", choices = unique(data()$Model_type))
      updateSelectInput(session, "model_filt_spid3", choices = unique(data()$Model_type))
      
      
      if(type_data() == "D1"){
        marker = colnames(dplyr::select(data(), where(is.double),-dplyr::starts_with(c("Cyto", "Vita")),-Dose, -dplyr::ends_with(".CV")))
        updateSelectInput(session, "typeeval_spid", choices = c("Cytotoxicity", "Vitality", marker))
      }
      if(type_data() == "SEAP"){
        updateSelectInput(session, "typeeval_spid", choices = c("Concentration" = "Concentration.average"))
      }
      if(type_data() == "TREM2"){
        updateSelectInput(session, "typeeval_spid", choices = c("Vitality" = "Vitality.average",
                                                                "GFP" = "GFP.average"))
      }
    })
    
    
    ### Product Family
    observeEvent(input$model_filt_spid,{
      validate(need(input$model_filt_spid, "Select something in the Model Type filtering."))
      family1 = data() %>% dplyr::filter(Model_type == input$model_filt_spid) %>%
        dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
      updateSelectInput(session, "family_filt_spid", choices = unique(family1))
    })
    
    observeEvent(input$model_filt_spid2,{
      validate(need(input$model_filt_spid2, "Select something in the Model Type filtering."))
      family2 = data() %>% dplyr::filter(Model_type == input$model_filt_spid2) %>%
        dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
      updateSelectInput(session, "family_filt_spid2", choices = unique(family2))
    })
    
    observeEvent(input$model_filt_spid3,{
      validate(need(input$model_filt_spid3, "Select something in the Model Type filtering."))
      family3 = data() %>% dplyr::filter(Model_type == input$model_filt_spid3) %>%
        dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
      updateSelectInput(session, "family_filt_spid3", choices = unique(family3))
    })
    
    
    #### Dose
    observeEvent(input$family_filt_spid,{
      validate(need(input$family_filt_spid, "Select something in the Product Family type filtering."))
      doses = data() %>% dplyr::filter(Product_Family == input$family_filt_spid & Model_type == input$model_filt_spid)
      updateSelectInput(session, "dose_filt_spid", choices = unique(doses$Dose))
    })
    
    observeEvent(input$family_filt_spid2,{
      validate(need(input$family_filt_spid2, "Select something in the Product Family type filtering."))
      doses2 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid2 & Model_type == input$model_filt_spid2)
      updateSelectInput(session, "dose_filt_spid2", choices = unique(doses2$Dose))
    })
    
    
    observeEvent(input$family_filt_spid3,{
      validate(need(input$family_filt_spid3, "Select something in the Product Family type filtering."))
      doses3 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid3 & Model_type == input$model_filt_spid3)
      updateSelectInput(session, "dose_filt_spid3", choices = unique(doses3$Dose))
    })
    
    
    
    #### Purification
    observeEvent({
      input$dose_filt_spid
      input$family_filt_spid
      },{
      validate(need(input$dose_filt_spid, "Select something in the Dose filtering."))
      validate(need(input$family_filt_spid, "Select something in the Family filtering."))
      doses = data() %>% dplyr::filter(Product_Family == input$family_filt_spid & Model_type == input$model_filt_spid & Dose == input$dose_filt_spid)
      updateRadioButtons(session, "purif_filt_spid", choices = unique(doses$Purification))
    })
    
    observeEvent({
      input$dose_filt_spid2
      input$family_filt_spid2
    },{
      validate(need(input$dose_filt_spid2, "Select something in the Dose filtering."))
      validate(need(input$family_filt_spid2, "Select something in the Family filtering."))
      doses2 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid2 & Model_type == input$model_filt_spid2 & Dose == input$dose_filt_spid2)
      updateRadioButtons(session, "purif_filt_spid2", choices = unique(doses2$Purification))
    })
    
    
    observeEvent({
      input$dose_filt_spid3
      input$family_filt_spid3
    },{
      validate(need(input$dose_filt_spid3, "Select something in the Dose filtering."))
      validate(need(input$family_filt_spid3, "Select something in the Family filtering."))
      doses3 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid3 & Model_type == input$model_filt_spid3 & Dose == input$dose_filt_spid3)
      updateRadioButtons(session, "purif_filt_spid3", choices = unique(doses3$Purification))
    })
    
    
    
    
    
    output$spidplot = renderPlot({
      req(data())
      # validate(need(input$model_filt_spid, "Select something in the Model Type filtering."))
      # validate(need(input$family_filt_spid, "Select something in the Product Family type filtering."))
      # validate(need(input$dose_filt_spid, "Select something in the Dose filtering."))
      # validate(need(input$purif_filt_spid, "Select something in the Purification filtering."))
      
      #measure type
      type_meas = ifelse(input$typeeval_spid == "Cytotoxicity", "Cytotoxicity.average", 
                         ifelse(input$typeeval_spid == "Vitality", "Vitality.average", input$typeeval_spid))
      
      data_plot1 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid & Model_type == input$model_filt_spid) %>% 
        dplyr::filter(Dose == input$dose_filt_spid) %>% dplyr::filter(Purification == input$purif_filt_spid) %>%
        dplyr::mutate(Product = stringr::str_split(Product, "_", simplify = T)[,ncol(stringr::str_split(Product, "_",simplify = T))]) %>% 
        dplyr::mutate(Product = case_when(Product == "" ~ "Ext", TRUE ~ Product))
      cnts1 =  dplyr::filter(data(), stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot1$Experiment_id))
      
      if(input$add_2spider == FALSE){
        
        if(length(unique(data_plot1$Experiment_id))>1){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid,", and 
                                                    will be averaged.")), type = "default")
          radardata = rbind(data_plot1, cnts1) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(type_meas, mean, na.rm = T)) %>% 
            tibble::column_to_rownames("Product") %>% dplyr::select(type_meas) %>% t()
        }else{
          radardata = rbind(data_plot1, cnts1) %>% tibble::column_to_rownames("Product") %>% 
            dplyr::select(dplyr::all_of(type_meas)) %>% t()
        }
        
        title_spid = paste(gsub(".average","",input$typeeval_spid),"of",input$family_filt_spid, "at", input$dose_filt_spid, "ug/mL")
        
        
      }else{
        req(input$family_filt_spid2, input$model_filt_spid2, input$dose_filt_spid2, input$purif_filt_spid2)
        validate(need(paste(input$model_filt_spid,input$family_filt_spid,input$dose_filt_spid,input$purif_filt_spid) != paste(input$model_filt_spid2,input$family_filt_spid2,input$dose_filt_spid2,input$purif_filt_spid2), 
                      "Please select a different product, dose or purification"))
        #first sample
        if(length(unique(data_plot1$Experiment_id))>1){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid,", and 
                                                    will be averaged.")), type = "default")
          first = rbind(data_plot1, cnts1) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(type_meas, mean, na.rm = T)) %>% 
            dplyr::mutate(Sample = paste(input$model_filt_spid, input$family_filt_spid, input$dose_filt_spid, input$purif_filt_spid, sep = "."))
        }else{
          first = rbind(data_plot1, cnts1) %>% dplyr::mutate(Sample = paste(input$model_filt_spid, input$family_filt_spid, input$dose_filt_spid, input$purif_filt_spid, sep = ".")) %>% 
            dplyr::select(Product, type_meas, Sample)
        }
        
        #second sample
        data_plot2 = data() %>% dplyr::filter(Model_type == input$model_filt_spid2 & Product_Family == input$family_filt_spid2) %>% 
          dplyr::filter(Dose == input$dose_filt_spid2) %>% dplyr::filter(Purification == input$purif_filt_spid2) %>% 
          dplyr::mutate(Product = stringr::str_split(Product, "_", simplify = T)[,ncol(stringr::str_split(Product, "_",simplify = T))]) %>% 
          dplyr::mutate(Product = case_when(Product == "" ~ "Ext", TRUE ~ Product))
        cnts2 =  dplyr::filter(data(), stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot2$Experiment_id)) 
        
        if(length(unique(data_plot2$Experiment_id))>1){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid2,", and 
                                                    will be averaged.")), type = "default")
          second = rbind(data_plot2, cnts2) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(type_meas, mean, na.rm = T)) %>%
            dplyr::mutate(Sample = paste(input$model_filt_spid2, input$family_filt_spid2, input$dose_filt_spid2, input$purif_filt_spid2, sep = "."))
        }else{
          second = rbind(data_plot2, cnts2) %>% dplyr::mutate(Sample = paste(input$model_filt_spid2, input$family_filt_spid2, input$dose_filt_spid2, input$purif_filt_spid2, sep = ".")) %>% 
            dplyr::select(Product, type_meas, Sample)
        }
        
        
        if(input$add_3spider == FALSE){
          radardata = rbind(first,second) %>% dplyr::select(Product, dplyr::all_of(type_meas),Sample) %>% 
            tidyr::pivot_wider(names_from = Product, values_from = type_meas) %>% 
            tibble::column_to_rownames("Sample") %>% replace(is.na(.), -20)
          
          if(!all(unique(data_plot1$Product) %in% unique(data_plot2$Product))){
            message("Some Products are different in one of the conditions. It could be hard to analyze it.
                                     Missing Products are displayed with a point in the centre.")
            shinyWidgets::show_alert("Warning!", "Some Products are different in one of the conditions. It could be hard to analyze it.
                                     Missing Products are displayed with a point in the centre.", type = "warning")
          }
          
          #reorder column data
          order = unique(rbind(first,second)$Product)
          radardata = radardata[,order]
          
          doses2 = unique(c(input$dose_filt_spid, input$dose_filt_spid2))
          doses2 = ifelse(length(doses2) > 1, paste0(doses2[1], " and ", doses2[2] ), doses2)
          if(input$family_filt_spid == input$family_filt_spid2){
            title_spid = paste(gsub(".average","",input$typeeval_spid), "of",input$family_filt_spid, "at", doses2, "ug/mL")
          }else{
            title_spid = paste(gsub(".average","",input$typeeval_spid), "of",input$family_filt_spid,"and", input$family_filt_spid2, "at", doses2, "ug/mL")
          }
          
          
        }else{
          req(input$family_filt_spid3, input$model_filt_spid3, input$dose_filt_spid3, input$purif_filt_spid3)
          #third sample
          validate(need(paste(input$model_filt_spid2,input$family_filt_spid2,input$dose_filt_spid2,input$purif_filt_spid2) != paste(input$model_filt_spid3,input$family_filt_spid3,input$dose_filt_spid3,input$purif_filt_spid3), "Please select a different product, dose or purification."))
          validate(need(paste(input$model_filt_spid,input$family_filt_spid,input$dose_filt_spid,input$purif_filt_spid) != paste(input$model_filt_spid3,input$family_filt_spid3,input$dose_filt_spid3,input$purif_filt_spid3), "Please select a different product, dose or purification."))
          
          data_plot3 = data() %>% dplyr::filter(Model_type == input$model_filt_spid3) %>% dplyr::filter(Product_Family == input$family_filt_spid3) %>% 
            dplyr::filter(Dose == input$dose_filt_spid3) %>% dplyr::filter(Purification == input$purif_filt_spid3) %>% 
            dplyr::mutate(Product = stringr::str_split(Product, "_", simplify = T)[,ncol(stringr::str_split(Product, "_",simplify = T))]) %>% 
            dplyr::mutate(Product = case_when(Product == "" ~ "Ext", TRUE ~ Product))
          cnts3 =  dplyr::filter(data(), stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot3$Experiment_id)) 
          
          if(length(unique(data_plot3$Experiment_id))>1){
            showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid3,", and 
                                                    will be averaged.")), type = "default")
            third = rbind(data_plot3, cnts3) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(type_meas, mean, na.rm = T)) %>%
              dplyr::mutate(Sample = paste(input$model_filt_spid3, input$family_filt_spid3, input$dose_filt_spid3, input$purif_filt_spid3, sep = "."))
          }else{
            third = rbind(data_plot3, cnts3) %>% dplyr::mutate(Sample = paste(input$model_filt_spid3, input$family_filt_spid3, input$dose_filt_spid3, input$purif_filt_spid3, sep = ".")) %>% 
              dplyr::select(Product, type_meas, Sample)
          }
          
          
          radardata = rbind(first,second,third) %>% dplyr::select(Product,dplyr::all_of(type_meas),Sample) %>% 
            tidyr::pivot_wider(names_from = Product, values_from = type_meas) %>% 
            tibble::column_to_rownames("Sample") %>% replace(is.na(.), -20)
          

          if(!all(
            all(unique(data_plot1$Product) %in% unique(data_plot2$Product)),
            all(unique(data_plot1$Product) %in% unique(data_plot3$Product)),
            all(unique(data_plot2$Product) %in% unique(data_plot3$Product)))){
            
            message("Some Products are missing in one of the conditions. It could be hard to analyze it.
                                     Missing Products are displayed with a point in the centre.")
            shinyWidgets::show_alert("Warning!", "Some Products are different in one of the conditions. It could be hard to analyze it.
                                     Missing Products are displayed with a point in the centre.", type = "warning")
          }
          
          
          #reorder column data
          order = unique(rbind(first,second,third)$Product)
          radardata = radardata[,order]
          
          doses3 = unique(c(input$dose_filt_spid, input$dose_filt_spid2, input$dose_filt_spid3))
          doses3 = ifelse(length(doses3) == 2, paste0(doses3[1], " and ", doses3[2]), 
                          ifelse(length(doses3) == 3, paste0(doses3[1], ", ", doses3[2], " and ",doses3[3] ), doses3))
          if(length(unique(c(input$family_filt_spid, input$family_filt_spid2, input$family_filt_spid3))) == 3){
            title_spid = paste(gsub(".average","",input$typeeval_spid), "of",input$family_filt_spid,",", input$family_filt_spid2, "and", input$family_filt_spid3,
                               "at", doses3, "ug/mL")
          }else{
            title_spid = paste(gsub(".average","",input$typeeval_spid), "of", paste0(unique(c(input$family_filt_spid, input$family_filt_spid2, input$family_filt_spid3)), collapse = " and "),
                               "at", doses3, "ug/mL")
          }
          
          
        }
        
        
      }
      
      
      if(input$typeeval_spid == "Cytotoxicity" || input$typeeval_spid == "Vitality"){
        lab =  c(0, 20, 40, 60, 80, 100)
        g2 = rbind("Max" = 100, "Min" = 0, radardata)
      }else{
        
        max = round(max(radardata))
        
        while(max%%5 != 0){max = max+1}

        max = round(max/10^(nchar(max)-1)) * 10^(nchar(max)-1)
        
        lab = c(0, max*2/10, max*4/10, max*6/10, max*8/10, max)
        g2 = rbind("Max" = max, "Min" = 0, radardata)
        
      }

      
      create_beautiful_radarchart(g2, caxislabels = lab, color = grDevices::hcl.colors(3, palette = "Dynamic"),
                                  title = title_spid, x_legend = 1, y_legend = 1.3)
      
      
    })
 
  })
}
    
## To be copied in the UI
# mod_spiderplot_ui("spiderplot_ui_1")
    
## To be copied in the server
# mod_spiderplot_server("spiderplot_ui_1")
