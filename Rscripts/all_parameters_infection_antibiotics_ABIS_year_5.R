################################################################################
load_all_parameter <- function(){
  all_parameters <- data.frame(
    matrix(ncol = 4, nrow = 0)
  )
  colnames(all_parameters) <- c("variable2categorise",
                                "category_variable",
                                "breaks",
                                "labels")
  
  
  
  # e_8a
  variable2categorise = list("e_8a")
  category_variable = list("Cold")
  breaks = list(c(0,1,2,3,4))
  labels = list(c("Never","1\u20132 times","3\u20135 times","6 times or more"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # # e_8a
  # variable2categorise = list("e_8a")
  # category_variable = list("cold_dichotomous")
  # breaks = list(c(0,1,Inf))
  # labels = list(c("Never","At least once"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # # e_8b
  # variable2categorise = list("e_8b")
  # category_variable = list("tonsilitis")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8b
  variable2categorise = list("e_8b")
  category_variable = list("Tonsilitis_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # # e_8c
  # variable2categorise = list("e_8c")
  # category_variable = list("otitis")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8c
  variable2categorise = list("e_8c")
  category_variable = list("Otitis_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # e_8d
  # variable2categorise = list("e_8d")
  # category_variable = list("pneumonia")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8d
  # variable2categorise = list("e_8d")
  # category_variable = list("pneumonia_dichotomous")
  # breaks = list(c(0,1,Inf))
  # labels = list(c("Never","At least once"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # e_8e
  # variable2categorise = list("e_8e")
  # category_variable = list("meningitis")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # # e_8e
  # variable2categorise = list("e_8e")
  # category_variable = list("meningitis_dichotomous")
  # breaks = list(c(0,1,Inf))
  # labels = list(c("Never","At least once"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # # e_8f
  # variable2categorise = list("e_8f")
  # category_variable = list("infection_requiring_antibiotic")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8f
  variable2categorise = list("e_8f")
  category_variable = list("Infection_requiring_antibiotic_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # e_8g
  # variable2categorise = list("e_8g")
  # category_variable = list("gastroenteritis")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8g
  variable2categorise = list("e_8g")
  category_variable = list("Gastroenteritis_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # e_8h
  # variable2categorise = list("e_8h")
  # category_variable = list("gastroenteritis_bacterial")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8h
  variable2categorise = list("e_8h")
  category_variable = list("Gastroenteritis_bacterial_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # e_8i
  # variable2categorise = list("e_8i")
  # category_variable = list("influenza")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8i
  variable2categorise = list("e_8i")
  category_variable = list("Influenza_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # # e_8j
  # variable2categorise = list("e_8j")
  # category_variable = list("three_day_fever")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_8j
  variable2categorise = list("e_8j")
  category_variable = list("Three_day_fever_dichotomous")
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
  # e_9a
  # variable2categorise = list("e_9a")
  # category_variable = list("measles")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # e_9b
  # variable2categorise = list("e_9b")
  # category_variable = list("rubella")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # e_9c
  # variable2categorise = list("e_9c")
  # category_variable = list("mumps")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  
  # e_9d
  variable2categorise = list("e_9d")
  category_variable = list("Chicken_pox")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # e_9e
  variable2categorise = list("e_9e")
  category_variable = list("Whooping_cough")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  
  # e_9f
  variable2categorise = list("e_9f")
  category_variable = list("Worm_infestation")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # e_9g
  variable2categorise = list("e_9g")
  category_variable = list("Lyme_disease")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # e_9h
  variable2categorise = list("e_9h")
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
  
  # # e_28d
  # variable2categorise = list("e_28d")
  # category_variable = list("penicillin")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_28d
  variable2categorise = list("e_28d")
  category_variable = list("Penicillin_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # e_28e
  # variable2categorise = list("e_28e")
  # category_variable = list("other_antibiotic")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_28e
  variable2categorise = list("e_28e")
  category_variable = list("Other_antibiotic_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # any_antibiotic
  # variable2categorise = list("any_antibiotic")
  # category_variable = list("any_antibiotic_cat")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
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
  
  # # e_28g
  # variable2categorise = list("e_28g")
  # category_variable = list("cortisone")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_28g
  # variable2categorise = list("e_28g")
  # category_variable = list("cortisone_tertile")
  # breaks = list(c(0,1,2,Inf))
  # labels = list(c("Never", "1-2 times","3 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_28g
  variable2categorise = list("e_28g")
  category_variable = list("Cortisone_dichotomous")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))

  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  # # e_28h
  # variable2categorise = list("e_28h")
  # category_variable = list("cortisone_tablets")
  # breaks = list(c(0,1,2,3,4))
  # labels = list(c("Never","1-2 times","3-5 times","6 times or more"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # e_28h
  variable2categorise = list("e_28h")
  category_variable = list("Cortisone_tablets_dichotomous")
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