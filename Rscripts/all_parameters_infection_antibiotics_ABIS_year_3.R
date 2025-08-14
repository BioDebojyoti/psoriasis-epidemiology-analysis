################################################################################
load_all_parameter <- function(){
  all_parameters <- data.frame(
    matrix(ncol = 4, nrow = 0)
  )
  colnames(all_parameters) <- c("variable2categorise",
                                "category_variable",
                                "breaks",
                                "labels")
  
  
  
  # c_10a
  variable2categorise = list("c_10a")
  category_variable = list("Cold_12_36_month")
  breaks = list(c(0,1,2,3,4))
  labels = list(c("Never","1\u20132 times","3\u20135 times","6 times or more"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  # # c_10b
  # variable2categorise = list("c_10b")
  # category_variable = list("tonsilitis_12_36_month")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )

  # c_10b
  variable2categorise = list("c_10b")
  category_variable = list("Tonsilitis_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))

  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # c_10c
  # variable2categorise = list("c_10c")
  # category_variable = list("otitis_12_36_month")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  
  # c_10c
  variable2categorise = list("c_10c")
  category_variable = list("Otitis_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # c_10d
  # variable2categorise = list("c_10d")
  # category_variable = list("pneumonia_12_36_month")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # c_10d
  variable2categorise = list("c_10d")
  category_variable = list("Pneumonia_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # c_10e
  # variable2categorise = list("c_10e")
  # category_variable = list("meningitis_12_36_month")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # c_10e
  # variable2categorise = list("c_10e")
  # category_variable = list("meningitis_12_36_month_dichotomous")
  # breaks = list(c(0,1,Inf))
  # labels = list(c("Never","At least once"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # # c_10f
  # variable2categorise = list("c_10f")
  # category_variable = list("infection_requiring_antibiotic_12_36_month")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )

  # c_10f
  variable2categorise = list("c_10f")
  category_variable = list("Infection_requiring_antibiotic_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
 
  # c_10g
  variable2categorise = list("c_10g")
  category_variable = list("Gastroenteritis_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  
  # c_10h
  variable2categorise = list("c_10h")
  category_variable = list("Influenza_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # c_10i
  variable2categorise = list("c_10i")
  category_variable = list("Three_day_fever_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  

  # c_11d
  variable2categorise = list("c_11d")
  category_variable = list("Chicken_pox")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

  
  # c_11e
  variable2categorise = list("c_11e")
  category_variable = list("Whooping_cough_12_36_month")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  
  # c_11f
  variable2categorise = list("c_11f")
  category_variable = list("Other_infection_12_36_month")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
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
  
  # c_26d
  variable2categorise = list("c_26d")
  category_variable = list("Penicillin_12_36_month_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # c_26e
  variable2categorise = list("c_26e")
  category_variable = list("Other_antibiotic_12_36_month_dichotomous")
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
  category_variable = list("Any_antibiotic_cat_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # c_26g
  variable2categorise = list("c_26g")
  category_variable = list("Cortisone_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
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