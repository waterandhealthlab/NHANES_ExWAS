cat_small_tables_bysex <- function(x, survey_object){
  
  var_name=paste0(as_label(enquo(x)))
  print(var_name)
  
  overall <- as_survey_design(survey_object)%>%
    filter(!is.na({{x}})) %>% 
    group_by({{x}}) %>%
    summarise(perc = survey_mean(level = .95, vartype = "ci", na.rm = T),
              .groups = "drop") %>% 
    mutate_at(.vars = vars(starts_with("perc")), .funs = list(~round(.*100, digits=1))) %>% 
    mutate(values={{x}},
           var_name=var_name)  %>%
    mutate(perc_ci_95=paste0(perc, " [",perc_low,"%, ",perc_upp,"%]")) %>%
    select(var_name, values, perc_ci_95)
  
  
  bysex <- as_survey_design(survey_object)%>%
    filter(!is.na({{x}})) %>% 
    group_by(sex, {{x}}) %>%
    summarise(perc = survey_mean(level = .95, vartype = "ci", na.rm = T),
              .groups = "drop") %>% 
    mutate_at(.vars = vars(starts_with("perc")), .funs = list(~round(.*100, digits=1))) %>% 
    mutate(values={{x}},
           var_name=var_name)  %>%
    mutate(perc_ci_95=paste0(perc, " [",perc_low,"%, ",perc_upp,"%]")) %>%
    select(sex, var_name, values, perc_ci_95) %>% 
    pivot_wider(id_cols = c("var_name", "values"), names_from = sex, values_from = perc_ci_95)
  
  
  test_form <- paste0("~", deparse(substitute(x)), "+sex")
  test_res <- svychisq(as.formula(test_form) , design = survey_object)
  
  test_res_col <- data.frame(p_val=c(round(test_res$p.value, digits=3), rep(NA_real_, times=nrow(bysex)-1)))
  
  k <- full_join(overall, bysex, by=c("var_name", "values")) %>% 
    rename(Overall="perc_ci_95") %>% 
    bind_cols(., test_res_col)
  return(k)
  rm(var_name, overall, bysex, test_form, test_res)
}

cat_small_tables_forsex <- function(x, survey_object){
  
  var_name=paste0(as_label(enquo(x)))
  print(var_name)
  
  overall <- as_survey_design(survey_object)%>%
    filter(!is.na({{x}})) %>% 
    group_by({{x}}) %>%
    summarise(perc = survey_mean(level = .95, vartype = "ci", na.rm = T),
              .groups = "drop") %>% 
    mutate_at(.vars = vars(starts_with("perc")), .funs = list(~round(.*100, digits=1))) %>% 
    mutate(values={{x}},
           var_name=var_name)  %>%
    mutate(perc_ci_95=paste0(perc, " [",perc_low,"%, ",perc_upp,"%]")) %>%
    select(var_name, values, perc_ci_95)
  return(overall)
  rm(var_name, overall, test_form, test_res)
}

cont_small_tables_bysex <- function(x, survey_object){
  
  var_name=paste0(as_label(enquo(x)))
  #print(var_name)
  
  overall <- as_survey_design(survey_object)%>%
    filter(!is.na({{x}})) %>% 
    summarise(average = survey_mean({{x}}, vartype = "se", na.rm = T),
              med = survey_median({{x}}, na.rm = T),
              iqr = survey_quantile({{x}}, quantiles = c(0.25, 0.75), na.rm = T),
              .groups = "drop") %>%
    rename(se="average_se") %>% 
    select(-ends_with("_se")) %>% 
    mutate(var_name=var_name) %>%
    mutate(values=as.character("")) %>% 
    mutate_if(.predicate=is.numeric, .funs = ~round(., digits=1)) %>% 
    mutate(estimates=paste0(average, " (", se, ") | ", med, " [", iqr_q25, ", ", iqr_q75, "]" )) %>%
    select(var_name, values, estimates)
  
  bysex <- as_survey_design(survey_object)%>%
    filter(!is.na({{x}})) %>% 
    group_by(sex) %>%
    summarise(average = survey_mean({{x}}, vartype = "se", na.rm = T),
              med = survey_median({{x}}, na.rm = T),
              iqr = survey_quantile({{x}}, quantiles = c(0.25, 0.75), na.rm = T),
              .groups = "drop") %>% 
    rename(se="average_se") %>% 
    select(-ends_with("_se")) %>% 
    mutate(var_name=var_name) %>%
    mutate_if(.predicate=is.numeric, .funs = ~round(., digits=1)) %>% 
    mutate(estimates=paste0(average, " (", se, ") | ", med, " [", iqr_q25, ", ", iqr_q75, "]" )) %>%
    select(sex, var_name, estimates) %>% 
    pivot_wider(id_cols = c("var_name"), names_from = sex, values_from = estimates)
  
  test_form <- paste0(deparse(substitute(x)), "~sex")
  test_res <- svyranktest(as.formula(test_form) , design = survey_object, test = "wilcoxon")
  
  k <- full_join(overall, bysex, by=c("var_name")) %>% 
    rename(Overall="estimates") %>% 
    mutate(p_val=round(test_res$p.value, digits=3))
  
  assign("new", k, envir = .GlobalEnv)  
  
  return(k)
  rm(var_name, overall, bysex, test_form, test_res)
  
}
