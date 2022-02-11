#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import ggplot2
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom data.table rbindlist
#' @importFrom DT renderDT
#' @importFrom shinyWidgets show_alert
#' @importFrom readxl read_xlsx
#' @importFrom tools file_ext
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom janitor remove_empty
#' @importFrom tidyr unite pivot_wider
#' @importFrom stringr str_detect str_split_fixed
#' @importFrom InteractiveComplexHeatmap makeInteractiveComplexHeatmap
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  
  ##### EXP list #####
  exp_list_to_edit = reactive({
    req(input$exp_list_file)
    ext <- tools::file_ext(input$exp_list_file$name)
    if(ext != "xlsx"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
    }
    validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
    showNotification(tagList(icon("check"), HTML("&nbsp;Experiment list file loaded.")), type = "message")
    readxl::read_xlsx(input$exp_list_file$datapath) %>% janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
  })
  

  exp_list = mod_edit_data_server("edit_exp_list", data_input = exp_list_to_edit, maxrows = 200)
  
  #check data correctly loaded
  output$check_explist = reactive(
    return(is.null(exp_list()))
  )
  outputOptions(output, "check_explist", suspendWhenHidden = FALSE)
  
  
  #### Target file ####
  
  target_to_edit = reactive({
    req(input$target_file, exp_list())
    ext <- tools::file_ext(input$target_file$name)
    if(ext != "xlsx"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
    }
    validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
    Target_file = readxl::read_xlsx(input$target_file$datapath, na = "NA") %>% janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
    
    #filter target based on exp_list
    Target_file = Target_file %>% dplyr::filter(Experiment_id %in% exp_list()$Experiment_id)
    
    to_rem = NULL
    for(i in unique(Target_file$Experiment_id)){
      expid = Target_file %>% dplyr::filter(Experiment_id == i)
      #check id and support type
      num_supp = unique(expid$Support_type)
      if(length(num_supp) > 1){
        print(paste0("There are more than one Support type (",num_supp, ") for the ", i, " and will be removed. Check the target file."))
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                              HTML("There are more than one Support type (",num_supp, ") for the", i, "and will be removed. Check the target file.")), type = "warning")
        to_rem = c(to_rem, i)
      }
      
      #check well numbers in the plate
      if(length(expid$Well) != 96){
        print(paste0("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file."))
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                               HTML("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file.")), type = "warning")
        to_rem = c(to_rem, i)
      }
      #check well replicates
      if(length(unique(expid$Well)) != 96){
        dup_wells = expid[duplicated(expid$Well),]$Well
        print(paste0("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file."))
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                               HTML("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file.")), type = "warning")
        to_rem = c(to_rem, i)
      }
    }
    
    if(is.null(to_rem)){
      showNotification(tagList(icon("check"), HTML("&nbsp;Target file loaded.")), type = "message")
      Target_file
    }else{
      showNotification(tagList(icon("check"), HTML("&nbsp;Target file loaded. Some experiments are removed.")), type = "message")
      Target_file %>% dplyr::filter(!(Experiment_id %in% unique(to_rem)))
    }
    
  })
  
  
  target = mod_edit_data_server("edit_target", data_input = target_to_edit, maxrows = 150)
  
  #check data correctly loaded
  output$check_target = reactive(
    return(is.null(target()))
  )
  outputOptions(output, "check_target", suspendWhenHidden = FALSE)
  
  
  
  #### eval cytotox ####
  data_notsumm = eventReactive(input$gocyto,{
    req(target(), exp_list())
    
    check_files = paste0(exp_list()$Path, exp_list()$File)

    file_list <- unlist(strsplit(exp_list()$File, split = ","))
    
    showNotification(tagList(icon("info"), HTML("&nbsp;Number of files to be imported: ", length(file_list))), type = "default")
    message(paste0("Number of files to be imported: ", length(file_list)))
    
    message(paste0("Name of files to be imported: ", "\n"))
    for (k in 1:length(file_list)){
      message(paste0(file_list[k]))
    }
    
    if (!all(file_list %in% list.files(unique(exp_list()$Path)))){
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;At least one file is missing or reported with the wrong name!")), type = "error")
      message("At least one file is missing or reported with the wrong name!")
      return(NULL)
    } else {
    
      
      processed.experiment = list()
      expid = exp_list()$Experiment_id
      
      
      percentage <- 0
      withProgress(message = "Reading data...", value=0, {
        processed.experiment = lapply(expid, function(x){
          exp_list = dplyr::filter(exp_list(), Experiment_id == x)
          percentage <<- percentage + 1/length(expid)*100
          incProgress(1/length(expid), detail = paste0("Progress: ",round(percentage,0), " %"))
          print(paste("Loading experiment",x,sep =" "))  
          file_explist = dplyr::filter(exp_list, Experiment_id == x)
          read_cytoxicity(ifile = file_explist$File, 
                          path = file_explist$Path,
                          instrument = file_explist$Instrument,
                          scan = file_explist$Scan, 
                          sample.anno=dplyr::filter(target(), Experiment_id == x)) %>%
            eval_cytoxicity()
          
        })
      })
      
      names(processed.experiment) = expid

      showNotification(tagList(icon("check"), HTML("&nbsp;Analysis completed!")), type = "message")
      tibble::as_tibble(data.table::rbindlist(processed.experiment,use.names=TRUE))
    }
  })
  
  data = reactive({
    req(data_notsumm())
    summarise_cytoxicity(data_notsumm(), group = c("Experiment_id","Model_type", "Model_Family", "Product","Product_Family", "Dose"))
  })
  
  #check data correctly loaded
  output$check_data = reactive(
    return(is.null(data()))
  )
  outputOptions(output, "check_data", suspendWhenHidden = FALSE)
  
  
  output$dtdata = renderDT({
    req(data())
    if(input$summ_viewtable == TRUE){
      data()
    }else{
      data_notsumm()
    }
  })
  
  
  ##### heatmap ####
  
  prod_total = reactive({
    req(data())
    data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) #%>% 
      #dplyr::mutate(First = stringr::str_split_fixed(Product, "_",n = 3)[,1])
  })
  
  observeEvent(prod_total(),{
    updateSelectInput(session, "prod_filt_heatmap", choices = unique(prod_total()$Product_Family))
  })
  

  observeEvent(input$prod_filt_heatmap,{
    doses = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap)
    updateSelectInput(session, "mod_filt_heatmap", choices = c("All", unique(prod_total()$Model_type)), selected = "All")
    updateRadioButtons(session, "filt_dose", choices = c("All",unique(doses$Dose)),inline = TRUE)
  })
  
  

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
  
  observeEvent(input$prod_filt_heatmap,{
    CBC150 = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap)
    if(length(unique(CBC150$Experiment_id)) > length(unique(CBC150$Model_type))){
      updateSelectInput(session, "selectannot_row", choices = "Model_Family")
    }else{
      updateSelectInput(session, "selectannot_row", choices = c("Model_Family","Experiment_id"))
    }
  })
  
  dataheat = eventReactive(input$makeheatmap,{
    req(prod_total())
    
    CBC150 = prod_total() %>% dplyr::filter(Product_Family == input$prod_filt_heatmap)

    if(is.null(input$mod_filt_heatmap)){
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select something in the model type filtering.")), type = "error")
      validate(need(input$mod_filt_heatmap, "Select something in the model type filtering."))
    }

    if(!("All" %in% input$mod_filt_heatmap)){
      if(length(input$mod_filt_heatmap) < 2 && input$rowdend == TRUE){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;Select at least two model type or disable the row clustering.")), type = "error")
        validate(need(length(input$mod_filt_heatmap) > 2, "Select at least two model types or disable the row clustering."))
      }
      CBC150 = dplyr::filter(CBC150, Model_type %in% input$mod_filt_heatmap)
    }


    filt_cnt = data() %>% dplyr::filter(if_any("Product_Family", ~grepl("CTRL",.))) %>%
      dplyr::filter(Experiment_id %in% unique(CBC150$Experiment_id)) %>%
      tidyr::unite("Product", Product, Dose, sep = " ")

    ht_list = NULL
    if(input$filt_dose == "All"){
      doses = as.character(unique(CBC150$Dose))
    }else{
      doses = as.character(input$filt_dose)
    }
    for(i in doses){

      cbc_filtered = CBC150 %>% dplyr::filter(Dose == i) %>% 
        dplyr::bind_rows(filt_cnt)
      
      if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
        
       cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>% 
        dplyr::summarise(across(Cytoxicity.average, mean, na.rm = T)) %>% dplyr::ungroup()
      }
      
      ht_list = ht_list + make_heatmap(
        data = cbc_filtered,
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
        unit_legend = paste("% cytotoxicity",i,"ug/mL"),
        col_label_size = 8 
      )
    }
    ComplexHeatmap::draw(ht_list, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(3, "cm"))

  })

  observeEvent(dataheat(),{
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, dataheat(), heatmap_id  = "heatmap_output")
  })

  
  
  ####barplot ####
  
  output$show_barplot2 = reactive({
    ifelse(input$add_barplot %%2 == 1, TRUE, FALSE)
  })
  outputOptions(output, "show_barplot2", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$add_barplot,{
    if(input$add_barplot %%2 == 1){
      updateActionButton (session, "add_barplot",label = HTML("&nbsp;Remove second barplot"),icon("minus")) 
    }else{
      updateActionButton(session, "add_barplot", label = HTML("&nbsp;Add another barplot"), icon("plus"))
    }
  })
  
  
  observeEvent(data(),{
    updateSelectInput(session, "model_filt_bar", choices = unique(data()$Model_type))
    updateSelectInput(session, "model_filt_bar2", choices = unique(data()$Model_type))
    
  })
  
  observeEvent(input$model_filt_bar,{
    family = data() %>% dplyr::filter(Model_type == input$model_filt_bar) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar", choices = unique(family))
  })
  
  
  output$check_multID_bar1 = reactive({
    req(data())
    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar & Model_type == input$model_filt_bar)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar1", suspendWhenHidden = FALSE)
  
  
  output$barplot = plotly::renderPlotly({
    req(data())

    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar & Model_type == input$model_filt_bar)
    cnts = data_notsumm() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    level_order = c("CTRL", "MEKinhibitor", "DOXORUBICIN", "CISPLATIN", unique(data_plot_not1$Product))
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = Cytoxicity, fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar( position = position_dodge(), stat = "summary",fun = "mean") +geom_point(position = position_dodge(width = 1))+
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }

    if(input$addpoints_barplot == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
      
    plotly::ggplotly(plot)

  })
  
  
  
  ### second barplot
  observeEvent(input$model_filt_bar2,{
    family = data() %>% dplyr::filter(Model_type == input$model_filt_bar2) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar2", choices = unique(family))
  })
  
  
  
  output$check_multID_bar2 = reactive({
    req(data())
    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar2 & Model_type == input$model_filt_bar2)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar2", suspendWhenHidden = FALSE)
  
  
  output$barplot2 = plotly::renderPlotly({
    req(data())
    
    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar2 & Model_type == input$model_filt_bar2)
    cnts = data_notsumm() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    level_order = c("CTRL", "MEKinhibitor", "DOXORUBICIN", "CISPLATIN", unique(data_plot_not1$Product))
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = Cytoxicity, fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar( position = position_dodge(), stat = "summary",fun = "mean") +geom_point(position = position_dodge(width = 1))+
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar2) +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar2,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar2 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  

  
  
  #### Spider plot ####
  
  observeEvent(data(),{
    updateSelectInput(session, "model_filt_spid", choices = unique(data()$Model_type))
    updateSelectInput(session, "model_filt_spid2", choices = unique(data()$Model_type))
    updateSelectInput(session, "model_filt_spid3", choices = unique(data()$Model_type))
  })
  
  
  observeEvent({
    input$model_filt_spid
    input$model_filt_spid2
    input$model_filt_spid3
    },{
    family1 = data() %>% dplyr::filter(Model_type == input$model_filt_spid) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_spid", choices = unique(family1))
    
    family2 = data() %>% dplyr::filter(Model_type == input$model_filt_spid2) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_spid2", choices = unique(family2))
    
    family3 = data() %>% dplyr::filter(Model_type == input$model_filt_spid3) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_spid3", choices = unique(family3))
    
  })
  
  
  observeEvent(input$family_filt_spid,{
    doses = data() %>% dplyr::filter(Product_Family == input$family_filt_spid & Model_type == input$model_filt_spid)
    updateSelectInput(session, "dose_filt_spid", choices = unique(doses$Dose))
  })
  
  observeEvent(input$family_filt_spid2,{
    doses2 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid2 & Model_type == input$model_filt_spid2)
    updateSelectInput(session, "dose_filt_spid2", choices = unique(doses2$Dose))
  })
  
  
  observeEvent(input$family_filt_spid3,{
    doses3 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid3 & Model_type == input$model_filt_spid3)
    updateSelectInput(session, "dose_filt_spid3", choices = unique(doses3$Dose))
  })
  
  
  output$spidplot = renderPlot({
    req(data())
    

    data_plot1 = data() %>% dplyr::filter(Product_Family == input$family_filt_spid & Model_type == input$model_filt_spid) %>% 
      dplyr::filter(Dose == input$dose_filt_spid) %>% dplyr::mutate(Product = stringr::str_split_fixed(Product, "_",n = 3)[,2]) %>% 
      dplyr::mutate(Product = case_when(Product == "" ~ "Ext", TRUE ~ Product))
    cnts1 =  dplyr::filter(data(), stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot1$Experiment_id))
    
    if(input$add_2spider == FALSE){

      if(length(unique(data_plot1$Experiment_id))>1){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid,", and 
                                                    will be averaged.")), type = "default")
        radardata = rbind(data_plot1, cnts1) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(Cytoxicity.average, mean, na.rm = T)) %>% 
          tibble::column_to_rownames("Product") %>% dplyr::select(Cytoxicity.average) %>% t()
      }else{
        radardata = rbind(data_plot1, cnts1) %>% tibble::column_to_rownames("Product") %>% dplyr::select(Cytoxicity.average) %>% t()
      }

      title_spid = paste("Cytoxicity of",input$family_filt_spid, "at", input$dose_filt_spid, "ug/mL")
      

    }else{
      validate(need(paste(input$family_filt_spid,input$dose_filt_spid) != paste(input$family_filt_spid2,input$dose_filt_spid2), "Please select a different product or dose"))
      #first sample
      if(length(unique(data_plot1$Experiment_id))>1){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid,", and 
                                                    will be averaged.")), type = "default")
        first = rbind(data_plot1, cnts1) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(Cytoxicity.average, mean, na.rm = T)) %>% 
          dplyr::mutate(Sample = paste(input$model_filt_spid, input$family_filt_spid, input$dose_filt_spid, sep = "."))
      }else{
        first = rbind(data_plot1, cnts1) %>% dplyr::mutate(Sample = paste(input$model_filt_spid, input$family_filt_spid, input$dose_filt_spid, sep = ".")) %>% 
          dplyr::select(Product, Cytoxicity.average, Sample)
      }

      #second sample
      data_plot2 = data() %>% dplyr::filter(Model_type == input$model_filt_spid2 & Product_Family == input$family_filt_spid2) %>% 
        dplyr::filter(Dose == input$dose_filt_spid2) %>% dplyr::mutate(Product = stringr::str_split_fixed(Product, "_",n = 3)[,2]) %>% 
        dplyr::mutate(Product = case_when(Product == "" ~ "Ext", TRUE ~ Product))
      cnts2 =  dplyr::filter(data(), stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot2$Experiment_id)) 
      
      if(length(unique(data_plot2$Experiment_id))>1){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid2,", and 
                                                    will be averaged.")), type = "default")
        second = rbind(data_plot2, cnts2) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(Cytoxicity.average, mean, na.rm = T)) %>%
          dplyr::mutate(Sample = paste(input$model_filt_spid2, input$family_filt_spid2, input$dose_filt_spid2, sep = "."))
      }else{
        second = rbind(data_plot2, cnts2) %>% dplyr::mutate(Sample = paste(input$model_filt_spid2, input$family_filt_spid2, input$dose_filt_spid2, sep = ".")) %>% 
          dplyr::select(Product, Cytoxicity.average, Sample)
      }
      

      if(input$add_3spider == FALSE){
        radardata = rbind(first,second) %>% dplyr::select(Product,Cytoxicity.average,Sample) %>% 
          tidyr::pivot_wider(names_from = Product, values_from = Cytoxicity.average) %>% 
          tibble::column_to_rownames("Sample")
        #reorder column data
        order = unique(rbind(data_plot2, cnts2)$Product)
        radardata = radardata[,order]
        
        doses2 = unique(c(input$dose_filt_spid, input$dose_filt_spid2))
        doses2 = ifelse(length(doses2) > 1, paste0(doses2[1], " and ", doses2[2] ), doses2)
        title_spid = paste("Cytoxicity of",input$family_filt_spid,"and", input$family_filt_spid2, "at", doses2, "ug/mL")

      }else{
        #third sample
        validate(need(paste(input$family_filt_spid2,input$dose_filt_spid2) != paste(input$family_filt_spid3,input$dose_filt_spid3), "Please select a different product or dose"))
        validate(need(paste(input$family_filt_spid,input$dose_filt_spid) != paste(input$family_filt_spid3,input$dose_filt_spid3), "Please select a different product or dose"))
        
        data_plot3 = data() %>% dplyr::filter(Model_type == input$model_filt_spid3) %>% dplyr::filter(Product_Family == input$family_filt_spid3) %>% 
          dplyr::filter(Dose == input$dose_filt_spid3) %>% dplyr::mutate(Product = stringr::str_split_fixed(Product, "_",n = 3)[,2]) %>% 
          dplyr::mutate(Product = case_when(Product == "" ~ "Ext", TRUE ~ Product))
        cnts3 =  dplyr::filter(data(), stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot3$Experiment_id)) 
        
        if(length(unique(data_plot3$Experiment_id))>1){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_spid3,", and 
                                                    will be averaged.")), type = "default")
          third = rbind(data_plot3, cnts3) %>% dplyr::group_by(Product) %>% dplyr::summarise(across(Cytoxicity.average, mean, na.rm = T)) %>%
            dplyr::mutate(Sample = paste(input$model_filt_spid3, input$family_filt_spid3, input$dose_filt_spid3, sep = "."))
        }else{
          third = rbind(data_plot3, cnts3) %>% dplyr::mutate(Sample = paste(input$model_filt_spid3, input$family_filt_spid3, input$dose_filt_spid3, sep = ".")) %>% 
            dplyr::select(Product, Cytoxicity.average, Sample)
        }
        

        radardata = rbind(first,second,third) %>% dplyr::select(Product,Cytoxicity.average,Sample) %>% 
          tidyr::pivot_wider(names_from = Product, values_from = Cytoxicity.average) %>% 
          tibble::column_to_rownames("Sample")

        #reorder column data
        order = unique(rbind(data_plot3, cnts3)$Product)
        radardata = radardata[,order]
        
        doses3 = unique(c(input$dose_filt_spid, input$dose_filt_spid2, input$dose_filt_spid3))
        doses3 = ifelse(length(doses3) == 2, paste0(doses3[1], " and ", doses3[2] ), 
                        ifelse(length(doses3) == 3, paste0(doses3[1], ", ", doses3[2], " and ",doses3[3] ), doses3))
        title_spid = paste("Cytoxicity of",input$family_filt_spid,",", input$family_filt_spid2, "and", input$family_filt_spid3,
                           "at", doses3, "ug/mL")

      }
      

    }

    lab =  c(0, 20, 40, 60, 80, 100)
    g2 = rbind("Max" = 100, "Min" = 0, radardata)
    
    create_beautiful_radarchart(g2, caxislabels = lab, color = grDevices::hcl.colors(3, palette = "Dynamic"),
                                title = title_spid, x_legend = 1, y_legend = 1.3)
    
  
    }, width = 800, height = 600)
  
}
