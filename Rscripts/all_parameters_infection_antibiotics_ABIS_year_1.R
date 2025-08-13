################################################################################
load_all_parameter <- function(){
  all_parameters <- data.frame(
    matrix(ncol = 4, nrow = 0)
  )
  colnames(all_parameters) <- c("variable2categorise",
                                "category_variable",
                                "breaks",
                                "labels")

  
  # b_3a
  variable2categorise = list("b_3a")
  category_variable = list("Infection_first_month")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_3b
  variable2categorise = list("b_3b")
  category_variable = list("Respiratory_problem_first_month")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_4a
  variable2categorise = list("b_4a")
  category_variable = list("Cold_upper_airway_infection")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_4b
  variable2categorise = list("b_4b")
  category_variable = list("Otitis")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_4c
  variable2categorise = list("b_4c")
  category_variable = list("Pneumonia")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_4d
  variable2categorise = list("b_4d")
  category_variable = list("Gastroenteritis")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_4e
  variable2categorise = list("b_4e")
  category_variable = list("Other_infection")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  
  
  # b_12a
  variable2categorise = list("b_12a")
  category_variable = list("Measles")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

  # b_12b
  variable2categorise = list("b_12b")
  category_variable = list("Rubella")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

    
  # b_12c
  variable2categorise = list("b_12c")
  category_variable = list("Mumps")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_12d
  variable2categorise = list("b_12d")
  category_variable = list("Chicken_pox")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  # b_12e
  variable2categorise = list("b_12e")
  category_variable = list("Whooping_cough")
  breaks = list(c(0,1,2,3))
  labels = list(c("Yes", "No", "do not know"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )
  
  ######################### Vaccinations    ####################################
  
  
  # b_11a
  # variable2categorise = list("b_11a")
  # category_variable = list("tuberculosis_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  
  # # b_11b
  # variable2categorise = list("b_11b")
  # category_variable = list("polio_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11c
  # variable2categorise = list("b_11c")
  # category_variable = list("tetanus_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11d
  # variable2categorise = list("b_11d")
  # category_variable = list("diphtheria_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11e
  # variable2categorise = list("b_11e")
  # category_variable = list("measles_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11f
  # variable2categorise = list("b_11f")
  # category_variable = list("mumps_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11g
  # variable2categorise = list("b_11g")
  # category_variable = list("rubella_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11h
  # variable2categorise = list("b_11h")
  # category_variable = list("hepatitis_B_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11i
  # variable2categorise = list("b_11i")
  # category_variable = list("pertusis_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  # 
  # # b_11j
  # variable2categorise = list("b_11j")
  # category_variable = list("hemophilus_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  # 
  
  # b_11k
  # variable2categorise = list("b_11k")
  # category_variable = list("other_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # DTaP_Hib_IPV_HepB
  # variable2categorise = list("DTaP_Hib_IPV_HepB")
  # category_variable = list("DTaP_Hib_IPV_HepB_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  # MMR
  # variable2categorise = list("MMR")
  # category_variable = list("MMR_vaccine")
  # breaks = list(c(0,1,2,3))
  # labels = list(c("Yes", "No", "do not know"))
  # 
  # all_parameters <- rbind(
  #   all_parameters, 
  #   data.frame(
  #     I(variable2categorise), I(category_variable), I(breaks), I(labels)
  #   )
  # )
  
  ######### frequency questionnaire  ###########################################
  

  # b_42a
  variable2categorise = list("b_42a")
  category_variable = list("Common_cold_dichotomous_cat")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

  
  # b_42b
  variable2categorise = list("b_42b")
  category_variable = list("Gastroenteritis_dichotomous_cat")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))
  
  all_parameters <- rbind(
    all_parameters, 
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )

  
  # b_42c
  variable2categorise = list("b_42c")
  category_variable = list("Infection_requiring_penicillin_dichotomous_cat")
  breaks = list(c(0,1,Inf))
  labels = list(c("Never","At least once"))

  all_parameters <- rbind(
    all_parameters,
    data.frame(
      I(variable2categorise), I(category_variable), I(breaks), I(labels)
    )
  )


  # b_42d
  variable2categorise = list("b_42d")
  category_variable = list("Influenza_dichotomous_cat")
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