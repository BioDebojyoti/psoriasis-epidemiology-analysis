################################################################################
load_all_parameter <- function(){
  all_parameters <- data.frame(
    matrix(ncol = 4, nrow = 0)
  )
  colnames(all_parameters) <- c("variable2categorise",
                                "category_variable",
                                "breaks",
                                "labels")
  
  
  
  # f_7_1
  variable2categorise = list("f_7_1")
  category_variable = list("Cold")
  breaks = list(c(0,1,2,3,4))
  labels = list(c("Never","1\u20132 times","3\u20135 times","6 times or more"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

  
  # f_7_2
  variable2categorise = list("f_7_2")
  category_variable = list("Tonsilitis_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # f_7_3
  variable2categorise = list("f_7_3")
  category_variable = list("Otitis_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

  
  # f_7_6
  variable2categorise = list("f_7_6")
  category_variable = list("Infection_requiring_antibiotic_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # f_7_7
  variable2categorise = list("f_7_7")
  category_variable = list("Gastroenteritis_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  


  # f_7_8
  variable2categorise = list("f_7_8")
  category_variable = list("Influenza_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # infection_frequency
  variable2categorise = list("infection_frequency")
  category_variable = list("Infection_frequency_cat")
  breaks = list(c(0,1,2,3,4))
  labels = list(c("Never", "1\u20132 times", "3\u20135 times", "6 times or more"))
  
  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  ##################### yes-no questionnaire ###################################

  # f_8c_1
  variable2categorise = list("f_8c_1")
  category_variable = list("Chicken_pox")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # f_8d_1
  # variable2categorise = list("f_8d_1")
  # category_variable = list("whooping_cough")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  
  # f_8e_1
  variable2categorise = list("f_8e_1")
  category_variable = list("Worm_infestation")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # f_8f_1
  variable2categorise = list("f_8f_1")
  category_variable = list("Lyme_disease")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # f_8g_1
  variable2categorise = list("f_8g_1")
  category_variable = list("Other_specific_infection")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  ##############################################################################

  
  # f_26_4
  variable2categorise = list("f_26_4")
  category_variable = list("Penicillin_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  
  # f_26_5
  variable2categorise = list("f_26_5")
  category_variable = list("Other_antibiotic_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  
  # any_antibiotic
  variable2categorise = list("any_antibiotic")
  category_variable = list("Any_antibiotic_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  
  # # f_26_7
  # variable2categorise = list("f_26_7")
  # category_variable = list("cortisone_dichotomous")
  # # breaks = list(c(0,1,2,Inf))
  # # labels = list(c("Never", "1-2 times","3 times or more"))
  # breaks = list(c(0,1,Inf))
  # labels = list(c("Never", "At least once"))
  # 
  # all_parameters <- rbind(
  #   all_parameters,
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # 
  # # f_26_8
  # variable2categorise = list("f_26_8")
  # category_variable = list("cortisone_tablets_dichotomous")
  # breaks = list(c(0,1,Inf))
  # labels = list(c("Never","At least once"))
  # 
  # all_parameters <- rbind(
  #   all_parameters,
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  ######################## confounders #########################################  
  # b_5
  variable2categorise = list("b_5")
  category_variable = list("Exclusive_breastfeeding_dichotomous_4")
  breaks = list(c(0,4,Inf))
  labels = list(c("\u2264 4","> 4"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # fr_27
  variable2categorise = list("fr_27")
  category_variable = list("Education_mother")
  breaks = list(c(0,1,2,3,4,5,6))
  labels = list(c("Compulsory_school_9_year",
                  "Upper_secondary_school_practical",
                  "Upper_secondary_school_theoretical",
                  "Community_college",
                  "University_1\u20133_years",
                  "University_3\u20135_years_or_more"))

  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )


  # fr_28
  variable2categorise = list("fr_28")
  category_variable = list("Education_father")
  breaks = list(c(0,1,2,3,4,5,6))
  labels = list(c("Compulsory_school_9_year",
                  "Upper_secondary_school_practical",
                  "Upper_secondary_school_theoretical",
                  "Community_college",
                  "University_1\u20133_years",
                  "University_3\u20135_years_or_more"))

  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # first degree heredity
  variable2categorise = list("first_degree_psoriasis_heredity")
  category_variable = list("First_degree_psoriasis_heredity_cat")
  breaks = list(c(-1,0,1))
  labels = list(c("No", "Yes"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # Gender
  variable2categorise = list("Gender")
  category_variable = list("Sex_cat")
  breaks = list(c(-1,0,1))
  labels = list(c("Boy", "Girl"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  return(as.data.frame(all_parameters))
}

all_parameters <- load_all_parameter()