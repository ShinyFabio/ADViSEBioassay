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
#' @importFrom shinyWidgets show_alert ask_confirmation
#' @importFrom readxl read_xlsx
#' @importFrom openxlsx write.xlsx
#' @importFrom tools file_ext
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom janitor remove_empty
#' @importFrom tidyr unite pivot_wider
#' @importFrom stringr str_detect str_split_fixed
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap
#' @importFrom grid unit gpar
#' @importFrom InteractiveComplexHeatmap makeInteractiveComplexHeatmap
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  options(scipen = 999)
  

  observeEvent(input$jumptohome, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "home")
  })
  
  
  # the modal dialog where the user can enter the query details.
  query_modal <- modalDialog(
    h3(strong("Welcome to ADViSELipidomics"), style = "text-align: center"),
    br(),
    h4("Before start, please enter your name and your company."),
    textInput("indata_analyst", list(HTML("&nbsp;"), icon("user"), HTML("&nbsp;Enter your name")), value = "Name"),
    textInput("inlab_analyst", list(HTML("&nbsp;"), icon("building"), HTML("&nbsp;Enter your company")), value = "Company"),
    easyClose = F,
    footer = tagList(
      actionButton("run", "Run")
    )
  )
  
  
  
  # Show the model on start up ...
  observeEvent(input$sidebarmenu,{
    req(input$sidebarmenu == "rawsub")
    showModal(query_modal)
  },ignoreInit = TRUE, once = TRUE)
  
  
  # ... or when user wants to change query
  observeEvent(input$change,{
    showModal(query_modal)
  })
  
  observeEvent(input$run, {
    removeModal()
  })
  
  output$nome = renderText({
    if(is.null(input$indata_analyst)){
      "Name"
    }else{
      input$indata_analyst
    }
  })
  
  
  
  ##### cytotoxicity ######
  
  output$valbox_cyto = renderUI({
    if(!exists("database_cyto")){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Cytotoxicity data: "),style = "color: white"),
                     h5("No Cytotoxicity data present in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }else{
      n_az = database_cyto$mydataset$Experiment_id %>% unique() %>% length()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Cytotoxicity data: "),style = "color: white"),
                     h5(strong(n_az), " experiments.", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }
  })
  
  
  #carica il file
  loaded_database_cyto1 = eventReactive(input$loaddatabase,{
    if(exists("database_cyto")){
      showNotification(tagList(icon("check"), HTML("&nbsp;Cytotoxicity data loading...")), type = "message")
      return(database_cyto)
    }else{
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;Cytotoxicity data not loaded")), type = "error")
      return(NULL)
    }
  })
  
  observeEvent(loaded_database_cyto1(),{
    if(!is.null(loaded_database_cyto1())){
      showNotification(tagList(icon("check"), HTML("&nbsp;Cytotoxicity data loaded!")), type = "message")
    }
  })
  
  
  #### update data if present
  cyto_from_mod = mod_load_cyto_server("load_cyto_mod")
  
  
  #check data correctly loaded
  output$check_data_updated = reactive(
    return(is.null(cyto_from_mod()))
  )
  outputOptions(output, "check_data_updated", suspendWhenHidden = FALSE)
  
  
  output$newdata_cyto_DT = renderDT({
    req(cyto_from_mod())
    if(input$summ_viewtable_updated == TRUE){
      cyto_from_mod()$mydataset
    }else{
      cyto_from_mod()$myprocesseddata
    }
  },options = list(scrollX = TRUE))
  
  loaded_database_cyto = reactiveVal()
  
  observeEvent(loaded_database_cyto1(),{
    loaded_database_cyto(loaded_database_cyto1())
  })

  observeEvent(input$update_cyto_bttn,{
    if(!is.null(cyto_from_mod())){
      loaded_database_cyto(update_database(old_data = loaded_database_cyto1(), new_data = cyto_from_mod()))
    }
  })
  
  

  observeEvent(input$save_update,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmsave_cyto",
      type = "warning",
      title = "Save and update database?",
      text = "Do you want to save and update the internal database? Be sure that everything works before update.
      If you need to restore the original database, you have to download again ADViSEBioassay."
    )
  })

  observeEvent(input$confirmsave_cyto,{
    checkdatabase = tryCatch({loaded_database_cyto()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    if(input$confirmsave_cyto == TRUE && checkdatabase == FALSE){
      filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/database_cyto.rda")
      database_cyto = loaded_database_cyto()
      usethis::use_data(database_cyto, overwrite = TRUE)
      #save(database_cyto, file = filepath)
    }
  })
  

  data = reactive({
    req(loaded_database_cyto())
    loaded_database_cyto()$mydataset
  })
  
  data_notsumm = reactive({
    req(loaded_database_cyto())
    loaded_database_cyto()$myprocesseddata
  })
  
  exp_list = reactive({
    req(loaded_database_cyto())
    loaded_database_cyto()$exp_list
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
  },options = list(scrollX = TRUE))
  
  
 #### cytotoxicity mod ####
  

  
  
  # data = reactive({
  #   req(cyto_from_mod(), data2())
  #   cyto_from_mod()$data
  # })
  # 
  # data_notsumm = reactive({
  #   req(cyto_from_mod())
  #   cyto_from_mod()$data_notsumm
  # })
  # 
  # exp_list = reactive({
  #   req(cyto_from_mod())
  #   cyto_from_mod()$exp_list
  # })
  # 
  

  
  
  
  ##### informative plots #####
  
  output$countbarplot = plotly::renderPlotly({
    req(data(), exp_list())
    
    count = data() %>% dplyr::select(where(is.character)) %>% dplyr::mutate(across(where(is.character), ~length(unique(.x))))
    count = t(count[1,])
    colnames(count) = "Count"
    count = count %>% as.data.frame() %>% tibble::rownames_to_column("Measure")
    count$Var = count$Measure
    
    inst = as.data.frame(table(exp_list()$Instrument))
    inst$Measure = "Instrument"
    scans = as.data.frame(table(exp_list()$Scan))
    scans$Measure = "Scan"
    

    scanint = rbind(inst,scans)
    colnames(scanint) = c("Var", "Count","Measure")
    
    count = rbind(count,scanint)
    count$Measure = factor(count$Measure, levels = unique(count$Measure))
    count$Var = factor(count$Var, levels = unique(count$Var))
    temp = ggplot(count, aes(x = Measure, y = Count, group = Var)) + geom_col(aes(fill = Var)) + 
      geom_text(aes(label=Count),position = position_stack(vjust = 0.5)) + ggtitle("Data overview") + labs(x = "Measure", fill = "Measure", y = "Total numbers") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
    
    plotly::ggplotly(temp)
    
  })
  
  
  
  
  output$modtype_barplto = plotly::renderPlotly({
    req(data())
    temp = ggplot(data(), aes(x= Model_type)) + geom_bar(aes(fill = Model_Family)) + ggtitle("Sample for each Model type")+ 
      ylab("Number of samples") + theme(axis.text.x = element_text(angle = 315, hjust = 0))
    plotly::ggplotly(temp)
  })
  

  output$prodfam_barplot = plotly::renderPlotly({
    req(data())
    data_filt = data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) 
    prod_table =  as.data.frame(table(data_filt$Product_Family))
    colnames(prod_table)[1] = "Product_Family"
    
    prod_table = prod_table[order(-prod_table$Freq),]
    prod_table$Product_Family = factor(prod_table$Product_Family, levels = rev(prod_table$Product_Family))
    
    
    if(input$first50_prodfam == TRUE){
      prod_table = prod_table[1:50,]
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(prod_table$Product_Family))/(50 + length(prod_table$Product_Family)/10) #640 found with html inspect when 50 prod are shown.
    }
    temp = ggplot(prod_table) + geom_col(aes(y = Product_Family, x = Freq, fill = Product_Family))+ 
      xlab("Number of samples")+ ylab("Product Family") + ggtitle("Sample for each Product Family") + 
      theme(axis.text.y = element_text(size = 7.4))

    
    plotly::ggplotly(temp, tooltip = c("x", "fill"))
  })
  
  output$prodfam_barplotUI = renderUI({
    
    if(input$first50_prodfam == TRUE){
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(unique(data()$Product_Family)))/(50 + length(unique(data()$Product_Family))/10) #640 found with html inspect when 50 prod are shown.
    }
    
    plotly::plotlyOutput("prodfam_barplot", height = paste0(size_plot,"px"))
  })
  
  ####pie plot 
  
  
  # These reactive values keep track of the drilldown state
  # (NULL means inactive)
  drills <- reactiveValues(category = NULL,
                           sub_category = NULL)
  
  # report sales by category, unless a category is chosen
  filtdata_pie <- reactive({
    if (is.null(drills$category)) {
      return(dplyr::count(data(), Model_Family))
    }
    temp = data() %>%
      dplyr::filter(Model_Family %in% drills$category) %>%
      dplyr::count(Model_type)
    
    if (is.null(drills$sub_category)) {
      return(temp)
    }
    
    data() %>%
      dplyr::filter(Model_Family %in% drills$category) %>%
      dplyr::filter(Model_type %in% drills$sub_category) %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) %>% 
      dplyr::count(Product_Family)
    
  })
  
  # Note that pie charts don't currently attach the label/value 
  # with the click data, but we can include as `customdata`
  output$piemodfamilyplot <- renderPlotly({
    req(filtdata_pie())
    d <- setNames(filtdata_pie(), c("labels", "values"))
    if(is.null(drills$sub_category)){
      plotly::plot_ly(d, source = "drillpie") %>%
        plotly::add_pie(labels = ~labels, values = ~values, customdata = ~labels, textinfo = 'label+value') %>%
        plotly::layout(title = if(is.null(drills$category)){"Model_Family distribution"}else{ paste("Distribution for", drills$category)})
    }else{
      
      d = d[order(-d$values),]
      d$labels = factor(d$labels, levels = rev(d$labels))
      
      plot = ggplot(d) + geom_col(aes(y = labels, x = values,fill = labels))+ 
        xlab("Number of samples")+ ylab("Product Family") + ggtitle(paste0("Sample for each Product Family in ",drills$sub_category)) + 
        theme(axis.text.y = element_text(size = 7.4))
      
      size_plot = round(84 + (640*length(d$labels))/(50 + length(d$labels)/10)) #640 found with html inspect when 50 prod are shown.
      plotly::ggplotly(plot, source = "drillpie")
    }
    
  })
  
  output$piemodfamilyplotUI = renderUI({
    req(filtdata_pie())
    d <- setNames(filtdata_pie(), c("labels", "values"))
    size_plot = round(84 + (640*length(d$labels))/(50 + length(d$labels)/10)) #640 found with html inspect when 50 prod are shown.
    if(is.null(drills$sub_category)){
      plotly::plotlyOutput("piemodfamilyplot")
    }else{
      plotly::plotlyOutput("piemodfamilyplot", height = paste0(size_plot,"px"))
    }
    
  })
  

  # update the current category when appropriate
  observeEvent(plotly::event_data("plotly_click", source = "drillpie"),{
    req(plotly::event_data("plotly_click", source = "drillpie"))
    cd <- plotly::event_data("plotly_click", source = "drillpie")$customdata[[1]]
    
    if (isTRUE(cd %in% unique(data()$Model_Family))){
      drills$category <- cd
    }
    
    if (isTRUE(cd %in% unique(data()$Model_type))){
      drills$sub_category <- cd
    }
  })
  
  output$back <- renderUI({
    if (!is.null(drills$category) && is.null(drills$sub_category)) {
      actionButton("clear", "Back", icon("chevron-left"))
    }
  })
  
  output$back1 <- renderUI({
    if (!is.null(drills$sub_category)) {
      actionButton("clear1", "Back", icon("chevron-left"))
    }
  })
  
  observeEvent(input$clear,
               drills$category <- NULL)
  observeEvent(input$clear1,
               drills$sub_category <- NULL)
  

 
  ## heatmap product family vs model type
  
  heat_informative = reactive({
    req(data())
    data_wout_cnt = data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) 

    prod_table2 =  table(data_wout_cnt[,c("Product_Family","Model_type")]) %>% as.matrix() 
    
    
    unit_legend = "N of samples"

    #col annotation
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
    
    annotdata_col = data_wout_cnt %>% dplyr::select(Model_type, Model_Family) %>% dplyr::distinct() %>% 
      dplyr::arrange(Model_type, colnames(prod_table2)) %>% tibble::column_to_rownames("Model_type")
    leng_col = annotdata_col %>% table() %>% length()
    colorannot_col = stats::setNames(getPalette(leng_col), c(row.names(table(annotdata_col))))
    colorannot_col = stats::setNames(list(colorannot_col), paste("Model_Family"))
    col_ha = ComplexHeatmap::HeatmapAnnotation(df = annotdata_col, which = "column", col = colorannot_col, border = TRUE)

    prod_table2 = prod_table2 %>% as.data.frame() %>% tidyr::pivot_wider(names_from = "Model_type",values_from = "Freq") %>%
      dplyr::mutate(dplyr::across(.cols = -1, ~ cut(.x, breaks=c(-Inf,0, 10, 50, 100, Inf), labels=c('0', '0-10', '10-50', '50-100', '>100')))) %>% 
      tibble::column_to_rownames("Product_Family") %>% as.matrix()
    
    ComplexHeatmap::Heatmap(prod_table2, name = unit_legend, rect_gp = grid::gpar(col = "white", lwd = 1), 
                            row_title = "Product Family", column_title = "Model type", 
                            row_names_gp = grid::gpar(fontsize = 6), column_names_gp = grid::gpar(fontsize = 8), #size testo
                            cluster_rows = FALSE, cluster_columns = FALSE, 
                            bottom_annotation = col_ha,
                            row_gap = grid::unit(2, "mm"), column_gap = grid::unit(2, "mm"), #spazio tra le divisioni
                            col = structure(c("red","orange", "blue","lightblue","white"), names = c('>100','50-100', '10-50', '0-10', '0'))
    ) %>% ComplexHeatmap::draw(merge_legend = TRUE, padding = unit(c(2, 2, 2, 5), "mm"))
  })
  
  
  observeEvent(heat_informative(),{
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, heat_informative(), heatmap_id  = "heatmap_inform_output")
  })
  
  
  ##### bubbleplot ####
  
  mod_bubble_plot_server("bubbleplot_cyto", data = data)
  
 
  
  
  ##### heatmap ####
  
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
    
    if(length(unique(doses$Experiment_id)) > length(unique(doses$Model_type))){
      updateSelectInput(session, "selectannot_row", choices = c("Model_Family", "Corrected_value"), selected = "Model_Family")
    }else{
      updateSelectInput(session, "selectannot_row", choices = c("Model_Family","Experiment_id","Corrected_value"), selected = "Model_Family")
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
      combin = combn(values_comb_heat$comb, 2, paste, collapse = '-')
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
    type_meas = ifelse(input$typeeval_heat == "Cytotoxicity", "Cytotoxicity.average", "Vitality.average")
    
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
    type_meas = ifelse(input$typeeval_heat == "Cytotoxicity", "Cytotoxicity.average", "Vitality.average")
    
    if(input$dose_op_heatmap == "filter"){
      
      if(input$filt_dose == "All"){
        doses = as.character(names(data_heatmap()))
      }else{
        doses = as.character(input$filt_dose)
      }
      for (i in doses){
        ht_list = ht_list + make_heatmap(
          data = as.data.frame(data_heatmap()[[i]]),
          filt_data_col = input$column_filt_heatmap,
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
          unit_legend = paste("%",input$typeeval_heat,i,"ug/mL"),
          add_values = input$show_valheat,
          thresh_values = input$range_showvalheat,
          typeeval_heat = type_meas
        )
      }
    }else if(input$dose_op_heatmap == "mean"){
      ht_list = make_heatmap(
        data = data_heatmap(),
        filt_data_col = input$column_filt_heatmap,
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
        unit_legend = paste("%",input$typeeval_heat),
        add_values = input$show_valheat,
        thresh_values = input$range_showvalheat,
        typeeval_heat = type_meas
      )
    }else{
      ht_list = make_heatmap(
        data = data_heatmap(),
        filt_data_col = input$column_filt_heatmap,
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
  
 

  observeEvent(heatmap(),{
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, heatmap(), heatmap_id  = "heatmap_output")
  })

  
  output$dt_heatmap = renderDT({
    req(data_heatmap())
    if(input$dose_op_heatmap == "filter"){
      data_heatmap()[[input$dose_dtheatmap]]
    }else{
      data_heatmap()
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
    req(data_notsumm())
    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar & Model_type == input$model_filt_bar)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar1", suspendWhenHidden = FALSE)
  
  
  output$barplot = plotly::renderPlotly({
    req(data_notsumm())

    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar & Model_type == input$model_filt_bar)
    cnts = data_notsumm() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    
    if(all(c("MEKinhibitor", "DOXORUBICIN", "CISPLATIN") %in% cnts$Product)){
      level_order = c("CTRL", "MEKinhibitor", "DOXORUBICIN", "CISPLATIN", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar( position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar) + labs(fill = "Dose") +
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
    req(data_notsumm())
    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar2 & Model_type == input$model_filt_bar2)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar2", suspendWhenHidden = FALSE)
  
  
  output$barplot2 = plotly::renderPlotly({
    req(data_notsumm())
    
    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar2 & Model_type == input$model_filt_bar2)
    cnts = data_notsumm() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)

    if(all(c("MEKinhibitor", "DOXORUBICIN", "CISPLATIN") %in% cnts$Product_Family)){
      level_order = c("CTRL", "MEKinhibitor", "DOXORUBICIN", "CISPLATIN", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar2), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar( position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar2) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar2,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar2 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot2 == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  

  
  
  #### Spider plot ####
  
  mod_spiderplot_server("spiderplot_cyto", data = data)

  
######## D1 ########
  
  
  output$valbox_D1 = renderUI({
    if(exists("database_D1") == FALSE){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("D1 data: "),style = "color: white"),
                     h5("No D1 data present in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }else{
      n_az = database_D1$mydataset$Experiment_id %>% unique() %>% length()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("D1 data: "),style = "color: white"),
                     h5(strong(n_az), " experiments.", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }
  })
  
  
  ##### EXP list #####
  exp_list_to_edit_D1 = reactive({
    req(input$exp_list_file_D1)
    ext <- tools::file_ext(input$exp_list_file_D1$name)
    if(ext != "xlsx"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
    }
    validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
    showNotification(tagList(icon("check"), HTML("&nbsp;Experiment list file loaded.")), type = "message")
    readxl::read_xlsx(input$exp_list_file_D1$datapath) %>% janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
  })
  
  
  exp_list_D1 = mod_edit_data_server("edit_exp_list_D1", data_input = exp_list_to_edit_D1, maxrows = 200)
  
  #check data correctly loaded
  output$check_explist_D1 = reactive(
    return(is.null(exp_list_D1()))
  )
  outputOptions(output, "check_explist_D1", suspendWhenHidden = FALSE)
  
  
  #### Target file ####
  
  target_to_edit_D1 = reactive({
    req(input$target_file_D1, exp_list_D1())
    ext <- tools::file_ext(input$target_file_D1$name)
    if(ext != "xlsx"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
    }
    validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
    Target_file = readxl::read_xlsx(input$target_file_D1$datapath, na = "NA") %>% 
      janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
    
    #filter target based on exp_list
    Target_file = Target_file %>% dplyr::filter(Experiment_id %in% exp_list_D1()$Experiment_id)

    to_rem = NULL
    for(i in unique(Target_file$Experiment_id)){
      expid = Target_file %>% dplyr::filter(Experiment_id == i)
      
      #in realtà support_type non è presente
      if("Support_type" %in% colnames(expid)){
        #check id with support type
        num_supp = unique(expid$Support_type)
        if(length(num_supp) > 1){
          print(paste0("There are more than one Support type (",num_supp, ") for the ", i, " and will be removed. Check the target file."))
          showNotification(duration = 8, tagList(icon("exclamation-circle"),
                                                 HTML("There are more than one Support type (",num_supp, ") for the", i, "and will be removed. Check the target file.")), type = "warning")
          to_rem = c(to_rem, i)
        }
      }

      
      #check well numbers in the plate
      if(length(expid$Well) != 96){
        print(paste0("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file."))
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                               HTML("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file.")), type = "warning")
        to_rem = c(to_rem, i)
      }
      
      #check well replicates
      if(length(unique(expid$Well)) != length(expid$Well)){
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
  
  
  target_D1 = mod_edit_data_server("edit_target_D1", data_input = target_to_edit_D1, maxrows = 150)
  
  #check data correctly loaded
  output$check_target_D1 = reactive(
    return(is.null(target_D1()))
  )
  outputOptions(output, "check_target_D1", suspendWhenHidden = FALSE)
  
  
  
  #### eval cytotox ####
  data_notsumm_D1 = eventReactive(input$gocyto_D1,{
    req(target_D1(), exp_list_D1())
    
    check_files = paste0(exp_list_D1()$Path, exp_list_D1()$File)
    
    file_list <- unlist(strsplit(exp_list_D1()$File, split = ","))

    showNotification(tagList(icon("info"), HTML("&nbsp;Number of files to be imported: ", length(file_list))), type = "default")
    message(paste0("Number of files to be imported: ", length(file_list)))
    
    message(paste0("Name of files to be imported: ", "\n"))
    for (k in 1:length(file_list)){
      message(paste0(file_list[k]))
    }
    
    if (!all(file_list %in% list.files(unique(exp_list_D1()$Path)))){
      file_wrong = file_list[!file_list %in% list.files(unique(exp_list_D1()$Path))]
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;At least one file is missing or reported with the wrong name! Check",file_wrong)), type = "error")
      message("At least one file is missing or reported with the wrong name! Check",file_wrong)
      return(NULL)
    } else {
      

      
      processed.experiment = list()
      expid = exp_list_D1()$Experiment_id
      
      percentage <- 0
      
      withProgress(message = "Reading data...", value=0, {
        processed.experiment = lapply(expid, function(x){
          percentage <<- percentage + 1/length(expid)*100
          incProgress(1/length(expid), detail = paste0("Progress: ",round(percentage,0), " %"))
          print(paste("Loading experiment",x,sep =" "))  
          
          file_explist = dplyr::filter(exp_list_D1(), Experiment_id == x)
          file_target = dplyr::filter(target_D1(), Experiment_id == x)
          read_D1(file_explist, file_target, filter.na = "Product")
        })
      })
      
      names(processed.experiment) = expid
      myprocesseddata_D1 = tibble::as_tibble(data.table::rbindlist(processed.experiment,use.names=TRUE))
      

      col_to_check = c("Model_type","Product_Family")
      
      err = 0
      for(i in col_to_check){
        if(TRUE %in% is.na(myprocesseddata_D1[,i])){
          message(paste0("There are some NA values inside",i,". Check the target file"))
          showNotification(tagList(icon("times-circle"), HTML("&nbsp;There are some NA values inside",i,". Check the target file")), type = "error")
          err = err+1
        }
      }
      if(err == 0){
        showNotification(tagList(icon("check"), HTML("&nbsp;Analysis completed!")), type = "message")
        return(myprocesseddata_D1)
      }else{return(NULL)}
      
      
    }
  })
  
  data_D1 = reactive({
    req(data_notsumm_D1())
    summarise_cytoxicity(data_notsumm_D1(), group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"), method = "d1")
  })
  
  #check data correctly loaded
  output$check_data_D1 = reactive(
    return(is.null(data_D1()))
  )
  outputOptions(output, "check_data_D1", suspendWhenHidden = FALSE)
  
  
  output$dtdata_D1 = renderDT({
    req(data_D1())
    if(input$summ_viewtable_D1 == TRUE){
      data_D1()
    }else{
      data_notsumm_D1()
    }
  })
  
  
  
  ##### informative plots D1######
  
  output$countbarplot_D1 = plotly::renderPlotly({
    req(data_D1(), exp_list_D1())
    
    count = data_D1() %>% dplyr::select(where(is.character)) %>% dplyr::mutate(across(where(is.character), ~length(unique(.x))))
    count = t(count[1,])
    colnames(count) = "Count"
    count = count %>% as.data.frame() %>% tibble::rownames_to_column("Measure")
    count$Var = count$Measure
    
    inst = as.data.frame(table(exp_list_D1()$Instrument))
    inst$Measure = "Instrument"
    scanint = rbind(inst)
    colnames(scanint) = c("Var", "Count","Measure")
    
    count = rbind(count,scanint)
    count$Measure = factor(count$Measure, levels = unique(count$Measure))
    count$Var = factor(count$Var, levels = unique(count$Var))
    temp = ggplot(count, aes(x = Measure, y = Count, group = Var)) + geom_col(aes(fill = Var)) + 
      geom_text(aes(label=Count),position = position_stack(vjust = 0.5)) + ggtitle("Data overview") + labs(x = "Measure", fill = "Measure", y = "Total numbers") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
    
    plotly::ggplotly(temp)
    
  })
  
  
  output$prodfam_barplot_D1 = plotly::renderPlotly({
    req(data_D1())
    
    prod_table =  as.data.frame(table(data_D1()$Product_Family))
    colnames(prod_table)[1] = "Product_Family"
    
    prod_table = prod_table[order(-prod_table$Freq),]
    prod_table$Product_Family = factor(prod_table$Product_Family, levels = rev(prod_table$Product_Family))
    
    
    if(input$first50_prodfam_D1 == TRUE && length(unique(data_D1()$Product_Family)) > 50){
      prod_table = prod_table[1:50,]
    }
    
    temp = ggplot(prod_table) + geom_col(aes(y = Product_Family, x = Freq, fill = Product_Family))+ 
      xlab("Number of samples")+ ylab("Product Family") + ggtitle("Sample for each Product Family") + 
      theme(axis.text.y = element_text(size = 7.4))
    
    plotly::ggplotly(temp, tooltip = c("x", "fill"))
  })
  
  output$prodfam_barplotUI_D1 = renderUI({
    
    if(input$first50_prodfam_D1 == TRUE || length(unique(data_D1()$Product_Family)) < 50){
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(unique(data_D1()$Product_Family)))/(50 + length(unique(data_D1()$Product_Family))/10) #640 found with html inspect when 50 prod are shown.
    }
    
    plotly::plotlyOutput("prodfam_barplot_D1", height = paste0(size_plot,"px"))
  })
  
  
  ####barplot D1 ####
  
  output$show_barplot2_D1 = reactive({
    ifelse(input$add_barplot_D1 %%2 == 1, TRUE, FALSE)
  })
  outputOptions(output, "show_barplot2_D1", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$add_barplot_D1,{
    if(input$add_barplot_D1 %%2 == 1){
      updateActionButton(session, "add_barplot_D1",label = HTML("&nbsp;Remove second barplot"),icon("minus")) 
    }else{
      updateActionButton(session, "add_barplot_D1", label = HTML("&nbsp;Add another barplot"), icon("plus"))
    }
  })
  
  
  observeEvent(data_D1(),{
    updateSelectInput(session, "model_filt_bar_D1", choices = unique(data_D1()$Model_type))
    updateSelectInput(session, "model_filt_bar2_D1", choices = unique(data_D1()$Model_type))
    
  })
  
  observeEvent(input$model_filt_bar_D1,{
    family = data_D1() %>% dplyr::filter(Model_type == input$model_filt_bar_D1) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar_D1", choices = unique(family))
  })
  
  
  output$check_multID_bar1_D1 = reactive({
    req(data_notsumm_D1())
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), Product_Family == input$family_filt_bar_D1 & Model_type == input$model_filt_bar_D1)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar1_D1", suspendWhenHidden = FALSE)
  
  
  output$barplot_D1 = plotly::renderPlotly({
    req(data_notsumm_D1())
    
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), Product_Family == input$family_filt_bar_D1 & Model_type == input$model_filt_bar_D1)
    cnts = data_notsumm_D1() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    
    if(all(c("LPS") %in% cnts$Product)){
      level_order = c("CTRL", "LPS", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    

    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar_D1), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar(position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar_D1) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar_D1,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar_D1 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot_D1 == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  
  
  
  ### second barplot
  observeEvent(input$model_filt_bar2_D1,{
    family = data_D1() %>% dplyr::filter(Model_type == input$model_filt_bar2_D1) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar2_D1", choices = unique(family))
  })
  
  
  
  output$check_multID_bar2_D1 = reactive({
    req(data_notsumm_D1())
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), Product_Family == input$family_filt_bar2_D1 & Model_type == input$model_filt_bar2_D1)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar2_D1", suspendWhenHidden = FALSE)
  
  
  output$barplot2_D1 = plotly::renderPlotly({
    req(data_notsumm_D1())
    
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), Product_Family == input$family_filt_bar2_D1 & Model_type == input$model_filt_bar2_D1)
    cnts = data_notsumm_D1() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)

    if(all(c("LPS") %in% cnts$Product_Family)){
      level_order = c("CTRL", "LPS", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar2_D1), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar(position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar2_D1) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar2_D1,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar2_D1 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot2_D1 == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  
  ##### spiderplot d1 ####
  
  mod_spiderplot_server("spiderplot_D1", data = data_D1)
  
  ##### bubbleplot d1 #####
  mod_bubble_plot_server("bubbleplot_D1", data = data_D1, type_data = "D1")
  
  
  ##### heatmap D1 ####
  
  
  prod_total_D1 = reactive({
    req(data_notsumm_D1())
    data_notsumm_D1() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  })
  
  
  observeEvent(prod_total_D1(),{
    updateSelectInput(session, "prodfam_heat_D1", choices = unique(prod_total_D1()$Product_Family))
  })

  
  # DATA STORAGE
  values_comb_heat_D1 <- reactiveValues(
    comb = NULL # original data
  )



  #purification filtering
  observeEvent(input$prodfam_heat_D1,{
    doses = prod_total_D1() %>% dplyr::filter(Product_Family == input$prodfam_heat_D1)
    updateSelectInput(session, "purif_filt_heat_D1", choices = unique(doses$Purification), selected = unique(doses$Purification)[1])
  })



  observeEvent(c(input$prodfam_heat_D1, input$purif_filt_heat_D1),{
    req(input$prodfam_heat_D1)
    req(input$purif_filt_heat_D1)
    filt = dplyr::filter(prod_total_D1(), Product_Family == input$prodfam_heat_D1) %>%
      dplyr::filter(Purification == input$purif_filt_heat_D1)
    values_comb_heat_D1$comb <- rev(unique(filt$Dose))
    updateRadioButtons(session, "filt_dose_D1", choices = unique(filt$Dose),inline = TRUE) #c("All",
  })

  observeEvent(input$revdose_heat_D1,{
    values_comb_heat_D1$comb <- rev(values_comb_heat_D1$comb)
  })

  observeEvent(values_comb_heat_D1$comb,{
    req(values_comb_heat_D1$comb)
    if(!is.null(values_comb_heat_D1$comb)){
      combin = combn(values_comb_heat_D1$comb, 2, paste, collapse = '-')
      updateSelectInput(session, "subdose_heatmap_D1", choices = combin)
    }
  })





  dataheat_D1 = reactive({
    req(data_notsumm_D1())

    vitaorcyto = ifelse(input$typeeval_heat_D1 == "Vitality", "Cytotoxicity", "Vitality")


    CBC150_D1 = data_notsumm_D1() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) %>%
      dplyr::filter(Product_Family == input$prodfam_heat_D1) %>%
      dplyr::filter(Purification == input$purif_filt_heat_D1)


    filt_cnt = data_notsumm_D1() %>% dplyr::filter(if_any("Product_Family", ~grepl("CTRL",.))) %>%
      dplyr::filter(Experiment_id %in% unique(CBC150_D1$Experiment_id)) %>%
      tidyr::unite("Product", Product, Dose, sep = " ")

    #if filter
    if(input$dose_op_heatmap_D1 == "filter"){


      cbc_filtered = split(CBC150_D1, f = ~Dose) %>%
        lapply( function(x) dplyr::bind_rows(x, filt_cnt) %>%
                  dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate))) %>%
                  dplyr::summarise(across(where(is.double) & !Dose, mean, na.rm = T)) %>%
                  dplyr::ungroup())

      cbc_filtered = lapply(cbc_filtered, function(x){
        if(length(unique(x$Experiment_id)) > length(unique(x$Model_type))){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                                 Duplicated will be averaged.")), type = "default")
          x %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>%
            dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup() %>% dplyr::select(Product, where(is.numeric)) %>% tibble::column_to_rownames("Product") %>%
            dplyr::select(-dplyr::all_of(vitaorcyto))
        }else{x %>% dplyr::select(Product, where(is.numeric)) %>% tibble::column_to_rownames("Product") %>%
            dplyr::select(-dplyr::all_of(vitaorcyto))}
      })

    }else if(input$dose_op_heatmap_D1 == "mean"){
      #if mean
      cbc_filtered = CBC150_D1 %>% dplyr::bind_rows(filt_cnt) %>%
        dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate))) %>%
        dplyr::summarise(across(where(is.double) & !Dose, mean, na.rm = T)) %>% dplyr::ungroup()


      if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                                 Duplicated will be averaged.")), type = "default")
        cbc_filtered = cbc_filtered %>% dplyr::group_by(across(Product)) %>%
          dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
      }

      cbc_filtered = cbc_filtered %>% dplyr::select(Product, where(is.numeric)) %>% tibble::column_to_rownames("Product") %>%
        dplyr::select(-dplyr::all_of(vitaorcyto))

    }else{
      #if subtract
      combination = strsplit(input$subdose_heatmap_D1, "-")

      cbc_filtered = CBC150_D1 %>% dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate)), Dose) %>%
        dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>%
        dplyr::ungroup()


      if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
        cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric))), Dose) %>%
          dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
      }


      cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(where(is.numeric)))) %>%
        dplyr::summarise(across(where(is.double), ~ .x[Dose == as.numeric(combination[[1]][1])] - .x[Dose == as.numeric(combination[[1]][2])], na.rm = T)) %>%
        dplyr::ungroup()

      filt_cnt_summ =  filt_cnt %>% dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate))) %>%
        dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>%
        dplyr::ungroup()

      if(length(unique(filt_cnt_summ$Experiment_id)) > length(unique(filt_cnt_summ$Model_type))){
        filt_cnt_summ = filt_cnt_summ %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>%
          dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
      }

      cbc_filtered = cbc_filtered %>% dplyr::bind_rows(filt_cnt_summ) %>% dplyr::select(Product, where(is.numeric),-Dose) %>% tibble::column_to_rownames("Product") %>%
        dplyr::select(-dplyr::all_of(vitaorcyto))

    }

    cbc_filtered


  })


  heatmap_D1 = eventReactive(input$makeheatmap_D1,{
    req(dataheat_D1())
    cbc_filtered = dataheat_D1()
    if(input$dose_op_heatmap_D1 == "filter"){


      doses = as.character(input$filt_dose_D1)
      ht = make_heatmap_D1(data = cbc_filtered[[doses]],
                           type = input$dose_op_heatmap_D1,
                           row_dend = input$rowdend_D1,
                           row_nclust = input$sliderrowheat_D1,
                           dist_method = input$seldistheat_D1,
                           clust_method = input$selhclustheat_D1,
                           add_values= input$show_valheat_D1)

      ComplexHeatmap::draw(ht, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(1, "cm"),
        column_title  = paste(doses, "ug/ml"),column_title_gp = grid::gpar(fontsize = 18))

    }else if(input$dose_op_heatmap == "mean"){
      ht = make_heatmap_D1(data = cbc_filtered,
                           type = input$dose_op_heatmap_D1,
                           row_dend = input$rowdend_D1,
                           row_nclust = input$sliderrowheat_D1,
                           dist_method = input$seldistheat_D1,
                           clust_method = input$selhclustheat_D1,
                           add_values= input$show_valheat_D1)

      ComplexHeatmap::draw(ht, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(1, "cm"))
    }else{
      ht = make_heatmap_D1(data = cbc_filtered,
                           type = input$dose_op_heatmap_D1,
                           row_dend = input$rowdend_D1,
                           row_nclust = input$sliderrowheat_D1,
                           dist_method = input$seldistheat_D1,
                           clust_method = input$selhclustheat_D1,
                           add_values= input$show_valheat_D1)
      ComplexHeatmap::draw(ht, merge_legend = TRUE, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(1, "cm"))
    }
  })


  observeEvent(heatmap_D1(),{
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, heatmap_D1(), heatmap_id  = "heatmap_D1_output")
  })


}
