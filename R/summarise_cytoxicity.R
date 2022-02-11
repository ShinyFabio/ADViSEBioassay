#' eval_cytoxicity
#'
#' @description \code{read_cytoxicity} draw multiple break line (br())
#'
#' @param X X
#'
#' @importFrom dplyr group_by summarise ungroup
#'


summarise_cytoxicity = function(X, group , method="mean"){
  
  if (method=="mean"){
    
    #group_ <- rlang::syms(...) ## with syms ... should be passed as charachter vector i.e, "name vars"
  
    X<- X %>% dplyr::group_by(dplyr::across(group)) %>%
      dplyr::summarise(
        Cytoxicity.average = mean(Cytoxicity, na.rm=TRUE),
        Cytoxicity.sd = sd(Cytoxicity,na.rm = TRUE),
        Cytoxicity.nreps =n()
      ) %>% dplyr::ungroup()
  }

  return(X)
}
