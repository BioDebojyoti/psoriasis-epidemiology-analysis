match_and_balance <- function(data2reg,
                              exposure,
                              covariates2match,
                              matching_method = "nearest") {
  
  data2reg <- data2reg[complete.cases(data2reg[, c("ABISnr", "Diagnos", exposure, covariates2match)]), ]
  
  data2reg <- data2reg %>% dplyr::select(all_of(c(
    "ABISnr", "Diagnos", exposure, covariates2match
  )))
  
  match_formula <- as.formula(paste0(exposure, "~", paste0(unlist(covariates2match), collapse = " + ")))
  
  
  m.out.original <- MatchIt::matchit(
    match_formula,
    data = data2reg,
    method = NULL,
    distance = "glm"
  )
  
  # Function to perform matching and check for warning
  run_matching <- function() {
    m.out.balanced <- tryCatch({
      # Run matchit and capture warnings
      m.out <- MatchIt::matchit(
        match_formula,
        data = data2reg,
        method = matching_method,
        distance = "glm"
      )
      # Return matchit output
      m.out
    }, warning = function(w) {
      # Check if the warning contains the specific message
      if (grepl("Fewer control units than treated units", w$message)) {
        message("Warning encountered: Fewer control units than treated units. Reversing exposure levels and retrying.")
        
        # Reverse the levels of the exposure factor variable
        data2reg[[exposure]] <<- relevel(factor(data2reg[[exposure]]), ref = tail(levels(factor(data2reg[[exposure]])), 1))
        
        # Rerun matchit with reversed exposure levels
        m.out <- MatchIt::matchit(
          match_formula,
          data = data2reg,
          method = matching_method,
          distance = "glm"
        )
        return(m.out)
      }
      # Return the original warning
      warning(w)
    })
    
    return(m.out.balanced)
  }
  
  print(levels(data2reg[[exposure]]))
  
  # Call the function
  m.out.balanced <- run_matching()

    print(levels(data2reg[[exposure]]))
  
  # m.out.balanced <- MatchIt::matchit(
  #   match_formula,
  #   data = data2reg,
  #   method = matching_method,
  #   distance = "glm"
  # )
  
  matched_data <- MatchIt::match.data(m.out.balanced)
  
  return(list(m.out.original, m.out.balanced, matched_data))
}

conditional_regression <- function(matched_data_4_cond_reg, exposure, method2use = "exact", matchMethod = NA_character_){
  
  matched_data_4_cond_reg <- as.data.frame(matched_data_4_cond_reg) %>%
    dplyr::select(all_of(c("Diagnos", exposure, "subclass")))
    
  formula_cond_reg <- as.formula(paste0(
    'I(Diagnos == "1") ~ ',
    exposure,
    ' + strata(subclass)',
    collapse = ''
  ))
  
  print(formula_cond_reg)
  
  clogit_model <- survival::clogit(
    formula = formula_cond_reg, 
    data = matched_data_4_cond_reg,
    method = method2use
  )
  
  first_row <- data.frame(
    Term = c(gsub("_", " ", gsub(
      "_cat", "", gsub("_dichotomous", "", gsub("_12_36_month", "", exposure))
    ))),
    OR = c(NA_real_),
    P.value = c(NA_real_),
    Conf.low = c(NA_real_),
    Conf.high = c(NA_real_),
    Match.Method = c(NA_character_)
  )
  
  clogit_model_out <- as.data.frame(clogit_model %>%
                                      broom::tidy(
                                        exponentiate = TRUE, 
                                        conf.int = TRUE) %>%
                                      dplyr::mutate_if(
                                        is.numeric, round,3)
                                    ) %>%
    dplyr::select(-all_of(c("std.error", "statistic")))
  
  clogit_model_out$match.method <- c(matchMethod)
  names(clogit_model_out) <- names(first_row)
  clogit_model_out$Term <- comprehenr::to_vec(
    for(t in clogit_model_out$Term) gsub(exposure,"\t",t)
  )
  clogit_model_out <- rbind(
    first_row,
    clogit_model_out
  )
  
  clogit_model_out[["OR(95% CI)"]] <- comprehenr::to_vec(
    for (i in seq_along(clogit_model_out$OR))
      if (is.na(clogit_model_out$OR[i]))
        NA_character_
    else
      paste0(
        as.character(clogit_model_out$OR[i]),
        "(",
        as.character(clogit_model_out$Conf.low[i]),
        "-",
        as.character(clogit_model_out$Conf.high[i]),
        ")",
        collpase = ""
      )
  )
  
  clogit_model_out <- clogit_model_out %>%
    dplyr::select(all_of(c(
      "Term", "OR(95% CI)", "P.value", "Match.Method"
    )))
      
  return(list(clogit_model, clogit_model_out))
  
}

clean_labs <- function(x) {
  x <- gsub("_", " ", 
            gsub("_cat", "", 
                 gsub("_dichotomous", "", 
                      gsub("_12_36_month", "", 
                           gsub("\u2013", "-", 
                                gsub("\u2014", "-", 
                                     gsub("\u2264", "<=", x)
                                     )
                                )
                           )
                      )
                 )
            )
  return(x)
}