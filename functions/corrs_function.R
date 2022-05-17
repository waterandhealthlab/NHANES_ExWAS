corrs_function <- function(pairs_matrix_df, survey_design, vars_cat) {
  
  db_corrs_all <- data.frame()
  
  
  #in the loop each pair is used to make the correlations and the result is appeneded in the empty df
  
  for(i in 1:nrow(pairs_matrix_df)) {
    
    corr_pair <- pairs_matrix_df[i,]
    
    corr_formula_tr <- paste0("~ ", corr_pair$V1, " + ", corr_pair$V2)
    
    corr_res_tr <- jtools::svycor(formula=as.formula(corr_formula_tr), design = survey_design, na.rm=T)
    
    db_corrs <- as.data.frame(corr_res_tr$cors)%>% 
      rownames_to_column(var = "variables_from") %>% 
      pivot_longer(cols=-variables_from, names_to="variables_to", values_to = "corr") %>% 
      filter(variables_from!=variables_to) 
    
    db_corrs_all <- bind_rows(db_corrs_all, db_corrs)
    
    rm(db_corrs, corr_pair, corr_res_tr, corr_formula_tr)
    
    
  }
  
  assign(x = vars_cat, value = db_corrs_all, envir = .GlobalEnv)
  
}
