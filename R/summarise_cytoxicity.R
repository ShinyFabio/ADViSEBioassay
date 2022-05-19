#' summarise_cytoxicity
#'
#' @description \code{summarise_cytoxicity} draw multiple break line (br())
#'
#' @param X data to summarise.
#' @param group Column used for the grouping. The columns in group will not be removed.
#' @param method Charcter. Can be "cyto" or "d1".
#' 
#'
#' @importFrom dplyr group_by summarise ungroup across
#' @importFrom tidyr replace_na
#' @importFrom stats sd
#'


summarise_cytoxicity = function(X, group , method = "cyto", markers_name = c("CD80", "CD40", "MHC-II")){
  
  if (method == "cyto"){
    
    X<- X %>% dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>%
      dplyr::summarise(
        Cytotoxicity.average = mean(Cytotoxicity, na.rm=TRUE),
        Cytotoxicity.sd = sd(Cytotoxicity,na.rm = TRUE),
        Cytotoxicity.nreps =n(),
        Vitality.average = mean(Vitality, na.rm=TRUE),
        CV = sd(Corrected_value,na.rm = TRUE)/mean(Corrected_value, na.rm = TRUE),
        Corrected_value = mean(Corrected_value, na.rm = TRUE)
      ) %>% dplyr::ungroup()
    #X$CV = tidyr::replace_na(X$CV, 0)
  }else if(method == "d1"){
    
    fun_CV = function(x){sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)}
    
      X <- X %>% dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>%
      dplyr::summarise(
        Cytotoxicity.average = mean(Cytotoxicity, na.rm=TRUE),
        Cytotoxicity.sd = sd(Cytotoxicity,na.rm = TRUE),
        Cytotoxicity.nreps =n(),
        Vitality.average = mean(Vitality, na.rm=TRUE),
        Cytotoxicity.CV = fun_CV(Cytotoxicity),
        Vitality.CV = fun_CV(Vitality),
        mean = across(dplyr::all_of(markers_name), ~mean(.x, na.rm = TRUE)),
        across(dplyr::all_of(markers_name), ~fun_CV(.x))
      ) %>% dplyr::ungroup() %>% dplyr::rename_with(.cols = markers_name, .fn = ~paste0(.x,".CV")) %>% 
        tidyr::unnest_wider(mean)

  }else{
    return(NULL)
    message("wrong method in summarise_cytotoxicity")
  }

  return(X)
}
