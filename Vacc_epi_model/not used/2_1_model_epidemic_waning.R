# This contains the functions specific to the epidemic model with vaccine waning

gen_seeiir_ag_vacc_waning <- odin::odin({
  # Number of groups
  no_groups <- user()
  
  # INITIAL CONDITIONS
  # Population size by age/risk group
  pop[] <- user()
  # Start vaccinated by age/risk group
  V0[] <- user()
  R0[] <- user()
  RV0[] <- user()
  # Initial infection by age/risk group
  I0[] <- user()
  
  # MODEL PARAMETERS
  # Susceptibility
  susc[] <- user()
  
  # Transmissibility
  trans <- user()
  
  # Latent periods
  gamma1 <- user()
  gamma2 <- user()
  
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  
  # efficacy
  alpha[] <- user()
  
  # Contact matrix
  cij[,] <- user()
  
  # Force of infection
  lambda[] <- trans * susc[i] * (sum(sij[i,]))
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <- if (sumN[i]>0) vI[i]*pop[i]/sumN[i] else 0
  
  # Transmission matrix
  sij[,] <- cij[i,j] * (I1[j] + I2[j] + I1v[j] + I2v[j])
  
  # Newly infected
  newInf[] <- lambda[i] * S[i]
  newInfv[] <- lambda[i] * Sv[i]
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  # Derivatives of the not vaccinated group
  deriv(S[])  <- + omega*Sv[i] - newInf[i] - v[i] * S[i] 
  deriv(E1[]) <- + omega*E1v[i] + newInf[i] - gamma1 * E1[i] - v[i] * E1[i] 
  deriv(E2[]) <- + omega*E2v[i] + gamma1 * (E1[i] - E2[i]) - v[i] * E2[i]
  deriv(I1[]) <- + omega*I1v[i] + gamma1 * E2[i]  - gamma2 * I1[i] - v[i] * I1[i] 
  deriv(I2[]) <- + omega*I2v[i] + gamma2 * (I1[i] - I2[i]) - v[i] * I2[i] 
  deriv(R[])  <- + omega*Rv[i] + gamma2 * I2[i] - v[i] * R[i] 
  
  # Derivatives vaccination group
  deriv(Sv[])  <- - omega*Sv[i]  - newInfv[i] + v[i] * (1-alpha[i]) * S[i] 
  deriv(E1v[]) <- - omega*E1v[i] + newInfv[i] - gamma1 * E1v[i] + v[i] * E1[i]
  deriv(E2v[]) <- - omega*E2v[i] + gamma1 * (E1v[i] - E2v[i]) + v[i] * E2[i]
  deriv(I1v[]) <- - omega*I1v[i] + gamma1 * E2v[i]  - gamma2 * I1v[i] + v[i] * I1[i]
  deriv(I2v[]) <- - omega*I2v[i] + gamma2 * (I1v[i] - I2v[i]) + v[i] * I2[i]
  deriv(Rv[])  <- - omega*Rv[i]  + gamma2 * I2v[i] + v[i] * (R[i] + alpha[i] * S[i])
  
  # Tracking the cumulative amount of infections over time for output of incidence
  deriv(cumI[]) <- newInf[i] + newInfv[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- pop[i]*(1-V0[i])*(1-R0[i]) - I0[i]
  initial(E1[1:no_groups]) <- 0
  initial(E2[1:no_groups]) <- 0
  initial(I1[1:no_groups]) <- I0[i]
  initial(I2[1:no_groups]) <- 0
  initial(R[1:no_groups]) <- pop[i]*(1-V0[i])*(R0[i])
  initial(cumI[1:no_groups]) <- 0
  
  initial(Sv[1:no_groups]) <- (pop[i]*V0[i])*(1-RV0[i])
  initial(E1v[1:no_groups]) <- 0
  initial(E2v[1:no_groups]) <- 0
  initial(I1v[1:no_groups]) <- 0
  initial(I2v[1:no_groups]) <- 0
  initial(Rv[1:no_groups]) <- (pop[i]*V0[i])*(RV0[i])
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  
  dim(pop) <- no_groups
  dim(I0) <- no_groups
  dim(V0) <- no_groups
  dim(R0) <- no_groups
  dim(RV0) <- no_groups
  dim(susc) <- no_groups
  dim(lambda) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  dim(sumN) <- no_groups  
  dim(alpha) <- no_groups
  dim(cij) <- c(no_groups, no_groups)
  dim(sij) <- c(no_groups, no_groups)
  
  dim(S) <- no_groups
  dim(E1) <- no_groups
  dim(E2) <- no_groups
  dim(I1) <- no_groups
  dim(I2) <- no_groups
  dim(R) <- no_groups
  dim(Sv) <- no_groups
  dim(E1v) <- no_groups
  dim(E2v) <- no_groups
  dim(I1v) <- no_groups
  dim(I2v) <- no_groups
  dim(Rv) <- no_groups
  dim(cumI) <- no_groups
  dim(newInf) <- no_groups
  dim(newInfv) <- no_groups
})


infectionODEs_epidemic <- function(population, initial_infected, vaccine_calendar, contact_matrix,
                          susceptibility, transmissibility, infection_delays, interval 
                          ,waning_rate, initial_vaccinated_prop, initial_Rv_prop, initial_R_prop,
                          begin_date, end_date, year_to_run, efficacy_NH, efficacy_SH, ...
) {


  # anything in this function has already been checked for crossing the year
  # so check months compared to the NH vs SH runs
  end_date2 <- end_date
  if (month(begin_date) >= 3 & month(begin_date) <9){
    end_date <- as.Date(paste0(year_to_run, "-08-30"))
    start_h <- "SH"
    efficacy_now <- efficacy_SH
  } else {
    end_date <- as.Date(paste0(as.character(year_to_run), "-03-01"))-1
    start_h <- "NH"
    efficacy_now <- efficacy_NH
  }
  
  t <- as.numeric(seq(begin_date, as.Date(end_date), interval))
  
  no_groups <- length(population)
  no_risk_groups <- no_groups/nrow(contact_matrix)
  no_age_groups <- no_groups/no_risk_groups
  
  # Contacts matrix only covers one set of age groups, here we "repeat" it to also cover 
  # risk groups
  
  new_cij <- matrix(rep(0,no_groups*no_groups), nrow = no_groups)
  for (k in 1:no_risk_groups) {
    for (l in 1:no_risk_groups) {
      lk <- (k - 1)*no_age_groups + 1
      ll <- (l - 1)*no_age_groups + 1
      new_cij[lk:(lk + no_age_groups - 1), ll:(ll + no_age_groups - 1)] <- contact_matrix
    }
  }
  
  calendar <- vaccine_calendar$calendar[c(nrow(vaccine_calendar$calendar),1:nrow(vaccine_calendar$calendar)),]
  dates <- as.numeric(c(t[1], vaccine_calendar$dates))
  
  # age the vaccinated population by 1 year. 
  initial_vaccinated_prop <- as.numeric(initial_vaccinated_prop)
  initial_Rv_prop <- as.numeric(initial_Rv_prop)
  #Assume that all R become susceptible again at the end of the year.
  initial_R_prop <- rep(0,18)

  # Set the parameter values
  mod <- gen_seeiir_ag_vacc_waning$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                       pop = population,
                                       I0 = initial_infected,
                                       V0 = initial_vaccinated_prop,
                                       R0 = initial_R_prop,
                                       RV0 = initial_Rv_prop,
                                       susc = rep(susceptibility,no_risk_groups),
                                       alpha = efficacy_now,
                                       omega = waning_rate,
                                       dates = dates,
                                       calendar = calendar[,1:no_groups],
                                       gamma1 = 2/infection_delays[1], gamma2 = 2/infection_delays[2]
                                       #    ageing =ageing
  )

  # run the model 
  y <- mod$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  y_tail <- tail(y,1)
  
  # Change the dates and efficacy to other hemisphere
  if(start_h == "NH"){
    begin_date <- as.Date(paste0(year_to_run, "-03-01"))
    efficacy_now <- efficacy_SH
    } else if(start_h == "SH"){
      begin_date <- as.Date(paste0(year_to_run, "-09-01"))
      efficacy_now <- efficacy_NH
      }
  
  t <- as.numeric(seq(begin_date, end_date2, 7))

  if(length(t)>27){stop("CAREFUL! SECOND RUN IS LONGER THAN 6 MONTHS")}
  #Update the vaccination to NH
  keepers <- which(vaccine_calendar$dates>= begin_date &vaccine_calendar$dates < end_date2)
  vaccine_calendar$dates <- vaccine_calendar$dates[keepers] 
  browser()
  input_inner <- if(!is.na(vaccine_calendar$dates[1])){vaccine_calendar$dates}
  
  input_dates <- c(t[1], tail(t,1))
  # choose the correct efficacy
  vaccine_calendar$efficacy <- efficacy_now
  if(length(input_dates)>2){
    new_coverage = change_coverage(matrix(rep(0,18*length(input_dates)), ncol = 18),
                                   vaccine_scenarios[[scenario]][["coverage"]])} else{
                                     new_coverage =matrix(rep(0,18*length(input_dates)), ncol = 18)
                                     vaccine_calendar$efficacy <- rep(0,18)
                                   }

  calender = as_vaccination_calendar(efficacy =
                                       vaccine_calendar$efficacy,
                                     dates = input_dates,
                                     coverage = as.data.frame(new_coverage),
                                     no_age_groups = 6,
                                     no_risk_groups = 2)
  # recreate the inputs
  calendar <- calender$calendar
  
  # carry on the run
  mod2 <- gen_seeiir_ag_vacc_waning_NH$new(no_groups = no_groups, cij = new_cij, trans = transmissibility,
                                           pop = population,
                                           allS = y_tail[2:19],
                                           allE1 = y_tail[20:37],
                                           allE2 = y_tail[38:55],
                                           allI1 = y_tail[56:73],
                                           allI2 = y_tail[74:91],
                                           allR = y_tail[92:109],
                                           allCI = y_tail[110:127],
                                           allSv = y_tail[128:145],
                                           allEv1 = y_tail[146:163],
                                           allEv2 = y_tail[164:181],
                                           allIv1 = y_tail[182:199],
                                           allIv2 = y_tail[200:217],
                                           allRv = y_tail[218:235],
                                           susc = rep(susceptibility,no_risk_groups),
                                           alpha = calender$efficacy[1:no_groups],
                                           omega = waning_rate,
                                           dates = input_dates,
                                           calendar = calendar[,1:no_groups],
                                           gamma1 = 2/infection_delays[1], gamma2 = 2/infection_delays[2])
  
  y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  y <- rbind(y, y2[-1,])
  
  # update from here - i'm not sure the below will work
  y <- mod$transform_variables(y)$cumI
  
  # Returning the differences in cumulative infections from one week to the other
  y <- data.frame(y[2:(nrow(y)), ] - y[1:(nrow(y) - 1), ])

  #Cleanup 
  colnames(y) <- names(population)
  #sum of infections
  return(colSums(mutate(y)))

}


# This function defines the incidence function. 
# Loops round and checks again, if there wasn't an incidence function first time

epidemic_scenarios <- function (vaccine_calendar, parameters, contact_ids, incidence_function, 
                               time_column,
                               waning_rate,
                               vaccination_ratio_input,
                               begin_date, 
                               end_date,
                               year_to_run,
                               efficacy_NH, efficacy_SH,
                               ..., verbose = T) 
{
  if (missing(incidence_function)) {
    var_names <- names(sys.call())
    #print(paste("This is var_names", var_names, " "))
    if (!"polymod_data" %in% var_names) {
      stop("No polymod_data set provided")
    }
    else {
      polymod_data <- eval(match.call()[["polymod_data"]])
    }
    if (missing(contact_ids)) {
      stop("No contact_ids set provided")
    }
    if (!"demography" %in% var_names) {
      stop("No demography provided, i.e. a vector with population size by age (starting at age is zero)")
    }
    else {
      demography <- eval(match.call()[["demography"]])
    }
    time_column = "Time"
    incidence_function <- function(vaccine_calendar, parameters, 
                                   contact_ids,vaccination_ratio_output, ...) {
      if (!"age_group_limits" %in% var_names) {
        if (verbose) 
          warning("Missing age_group_limits, using default: c(1,5,15,25,45,65)")
        age_group_limits <- c(1, 5, 15, 25, 45, 65)
      }
      else {
        age_group_limits <- eval(match.call()[["age_group_limits"]])
      }
      contacts <- contact_matrix(as.matrix(polymod_data[contact_ids, 
      ]), demography, age_group_limits)
      age.groups <- stratify_by_age(demography, age_group_limits)
      if (!"risk_ratios" %in% var_names) {
        if (verbose) 
          warning("Missing risk_ratios, using default UK based values")
        risk_ratios <- matrix(c(0.021, 0.055, 0.098, 
                                0.087, 0.092, 0.183, 0.45, 0, 0, 0, 0, 0, 0, 
                                0), ncol = 7, byrow = T)
      }
      else {
        risk_ratios <- eval(match.call()[["risk_ratios"]])
      }
      verbose <<- F
      popv <- stratify_by_risk(age.groups, risk_ratios)
      initial.infected <- rep(10^parameters[9], 6)
      initial.infected <- stratify_by_risk(initial.infected, 
                                           risk_ratios)
      infectionODEs_epidemic(popv, initial.infected, vaccine_calendar, contacts,
                    susceptibility = c(parameters[6], parameters[6], parameters[6], parameters[7], parameters[7], parameters[8]),
                    transmissibility = parameters[5],
                    infection_delays = c(0.8,1.8), interval = 7,
                    waning_rate = waning_rate,
                    initial_vaccinated_prop = unlist(vaccination_ratio_input[[1]]),
                    initial_Rv_prop = unlist(vaccination_ratio_input[[2]]),
                    initial_R_prop = unlist(vaccination_ratio_input[[3]]), 
                    begin_date = begin_date, 
                    end_date = end_date, 
                    year_to_run = year_to_run, 
                    efficacy_NH = efficacy_NH, 
                    efficacy_SH = efficacy_SH
                    
      )
      
      
    }
  }
  if (is.null(nrow(parameters))) {
    
    if (missing(contact_ids)) {
      
      result <- incidence_function(vaccine_calendar, parameters, 
                                   ...)
    }
    else {
      
      result <- incidence_function(vaccine_calendar, parameters, 
                                   contact_ids, ...)
      
    }
    # if (!missing(time_column) && !is.null(time_column)) 
    #   result[[time_column]] <- NULL
    # return(colSums(result))
    return(result)
  }
  else {
    if (missing(time_column)) 
      time_column <- NULL
    if (missing(contact_ids)) {
      
      return(t(apply(parameters, 1, function(pars) vaccination_scenario_epidemic(parameters = pars, 
                                                                    vaccine_calendar = vaccine_calendar,
                                                                    incidence_function = incidence_function, 
                                                                    time_column = time_column, ...))))
    }
    else {
      pc <- cbind(parameters, contact_ids)
      #print("it is here")

      return(t(apply(pc, 1,function(pars_contacts) vaccination_scenario_epidemic(parameters = pars_contacts[1:ncol(parameters)], 
                                                                     contact_ids = pars_contacts[(ncol(parameters) + 
                                                                                                    1):length(pars_contacts)],
                                                                     vaccine_calendar = vaccine_calendar, 
                                                                     incidence_function = incidence_function, time_column = time_column, 
                                                                     ...))))
    }
  }
}




#     Run the function that allows you to change vaccine coverage
change_coverage <- function(data, final_uptake) {
  
  sums <- data[nrow(data),]
  # If final uptake is zero in a group then we need to make some kind of assumption on uptake rate over time
  if (any(sums == 0)) {
    warning("No prior information on uptake rate. Using constant uptake rate")
    #browser()
    col <- which(sums == 0)
    data[,col] <- seq(0, (nrow(data)-1))
    sums <- data[nrow(data),]    
  }
  for(i in 1:nrow(data)) {
    data[i,] <- data[i,]*final_uptake/sums
  }
  data
}

vaccination_scenario_epidemic <- function (vaccine_calendar, parameters, contact_ids, incidence_function, 
                                  time_column, parameter_map, vaccination_ratio_output,..., verbose = T) 
{
  
  if (missing(incidence_function)) {
    print("inside function")
    uk_defaults <- F
    no_risk_groups <- vaccine_calendar$no_risk_groups
    no_age_groups <- vaccine_calendar$no_age_groups
    no_parameters <- length(parameters)
    if (!is.null(nrow(parameters))) 
      no_parameters <- ncol(parameters)
    if (no_risk_groups >= 2 && no_age_groups == 7 && no_parameters == 
        9) 
      uk_defaults <- T
    dots <- list(...)
    var_names <- names(dots)
    if (!"polymod_data" %in% var_names) {
      stop("No polymod_data set provided")
    }
    else {
      polymod_data <- dots[["polymod_data"]]
    }
    if (missing(contact_ids)) {
      stop("No contact_ids set provided")
    }
    if (!"demography" %in% var_names) {
      stop("No demography provided, i.e. a vector with population size by age (starting at age is zero)")
    }
    else {
      demography <- dots[["demography"]]
    }
    time_column = "Time"
    if (missing(parameter_map)) {
      if (uk_defaults) {
        parameter_map <- parameter_mapping(epsilon = c(1, 
                                                       1, 2, 2, 3), psi = 4, transmissibility = 5, 
                                           susceptibility = c(6, 6, 6, 7, 7, 7, 8), initial_infected = 9)
      }
      else if (no_parameters == 2 * no_age_groups + 3) {
        if (is.null(nrow(parameters))) 
          parameter_map <- parameter_mapping(parameters = parameters)
        else parameter_map <- parameter_mapping(parameters = parameters[1, 
        ])
      }
      else {
        stop("Missing parameter map")
      }
    }
    incidence_function <- function(vaccine_calendar, parameters, 
                                   contact_ids, ...) {
      if (!"age_group_limits" %in% var_names) {
        if (uk_defaults) {
          if (verbose) 
            warning("Missing age_group_limits, using default: c(1,5,15,25,45,65)")
          age_group_limits <- c(1, 5, 15, 25, 45, 65)
        }
        else stop("Missing age_group_limits")
      }
      else {
        age_group_limits <- dots[["age_group_limits"]]
      }
      contacts <- contact_matrix(as.matrix(polymod_data[contact_ids, 
      ]), demography, age_group_limits)
      age.groups <- stratify_by_age(demography, age_group_limits)
      if (!"risk_ratios" %in% var_names) {
        if (uk_defaults) {
          risk_ratios <- matrix(c(0.021, 0.055, 0.098, 
                                  0.087, 0.092, 0.183, 0.45, rep(0, no_age_groups * 
                                                                   (no_risk_groups - 2))), ncol = 7, byrow = T)
        }
        else {
          if (no_risk_groups > 1) 
            stop("No risk ratios supplied.")
          risk_ratios <- rep(1, no_age_groups)
        }
      }
      else {
        risk_ratios <- dots[["risk_ratios"]]
      }
      verbose <<- F
      popv <- stratify_by_risk(age.groups, risk_ratios, 
                               no_risk_groups)
      initial.infected <- rep(10^parameters[parameter_map$initial_infected], 
                              no_age_groups)
      initial.infected <- stratify_by_risk(initial.infected, 
                                           risk_ratios, no_risk_groups)
      infectionODEs(popv, initial.infected, vaccine_calendar, 
                    contacts, parameters[parameter_map$susceptibility], 
                    transmissibility = parameters[parameter_map$transmissibility], 
                    c(0.8, 1.8), 7)

    }
  }
  if (is.null(nrow(parameters))) {
    
    if (missing(contact_ids)) {
      result <- incidence_function(vaccine_calendar, parameters, 
                                   ...)
    }
    else {
      result <- incidence_function(vaccine_calendar, parameters, 
                                   contact_ids, ...)
    }
    if (!missing(time_column) && !is.null(time_column)) 
      # result[[time_column]] <- NULL
      # return(colSums(result))
      return((result))
  }
  else {
    if (missing(time_column)) 
      time_column <- NULL
    if (missing(contact_ids)) {
      return(apply(parameters, 1, function(pars)
        vaccination_scenario_epidemic(parameters = pars, 
                             vaccine_calendar = vaccine_calendar, incidence_function = incidence_function, 
                             time_column = time_column, ...)))
    }
    else {
      pc <- cbind(parameters, contact_ids)
      
      return(t(apply(pc, 1, function(pars_contacts)
        vaccination_scenario_epidemic(parameters = pars_contacts[1:ncol(parameters)], 
                             contact_ids = pars_contacts[(ncol(parameters) + 
                                                            1):length(pars_contacts)],
                             vaccine_calendar = vaccine_calendar, 
                             incidence_function = incidence_function, time_column = time_column, 
                             ...))))
    }
  }
}

age_population_1year <- function(population, old_proportions){
  

  #born sucseptible, then age relative to age group span
  proportion_changes <- c(old_proportions[1]*1,
                          old_proportions[2]*0.2,
                          old_proportions[3]*0.1,
                          old_proportions[4]*0.2,
                          old_proportions[5]*(1/30),
                          old_proportions[6]*(1/6)
                          
  )
  # weight by the relative population sizes               
  pop_weighting <- c(1,population[1]/population[2], 
                     population[2]/population[3],
                     population[3]/population[4],
                     population[4]/population[5],
                     population[5]/population[6]
                     
  )  
  
new_proportions <- c(old_proportions[1]-1*proportion_changes[1],
                       old_proportions[2]+pop_weighting[2]*proportion_changes[1] - 1*proportion_changes[2],
                       old_proportions[3]+pop_weighting[3]*proportion_changes[2]- 1*proportion_changes[3],
                       old_proportions[4]+pop_weighting[4]*proportion_changes[3]- 1*proportion_changes[4],
                       old_proportions[5]+pop_weighting[5]*proportion_changes[4]- 1*proportion_changes[5],
                       old_proportions[6]+pop_weighting[6]*proportion_changes[5]- 1*proportion_changes[6]
  )
  
  return(c(new_proportions,rep(0,12)))
  
}


run_epidemic_model <- function(vaccine_scenarios, year_in_question, begin_date, end_date, 
                               epidemics_list, epidemic, scenario, immunity_input, 
                               contact_ids, posterior_samples, flu_type, year_to_run, ...){
  
  #specify the relevant vaccination parameters
  waning_rate = vaccine_scenarios[[scenario]][["waning_rate"]]
  
  if(length(vaccine_scenarios[[scenario]][["dates"]])>1){
    dates = seq(from = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][1])),
                to = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][2])),
                by = 7)
  } else {
    dates = seq(from = as.Date(paste0(years[i], vaccine_scenarios[[scenario]][["dates"]][1])),
                to = as.Date(paste0(years[i+1], vaccine_scenarios[[scenario]][["dates"]][1]))-1,
                by = 7)
  }
  # Check the first date is the same as the start date!!
  # if(dates[1] != begin_date){message("There is a mismatch in vaccine dates, adding the begin date to the start")
  #   dates <- c(begin_date, dates)}
  new_coverage = change_coverage(matrix(rep(0,18*(length(dates))),ncol = 18),
                                 vaccine_scenarios[[scenario]][["coverage"]])
  # specify the demography
  demography.ken <- popken[,which(years == epidemics_list[[epidemic]]["year"])+1]
  # calculate the vaccination calender

  if(virus_type =="AH1N1"){
    efficacy_NH <- vaccine_scenarios[[scenario]][["efficacy_H1"]][,c(year_in_question*2)-1]
    efficacy_SH <- vaccine_scenarios[[scenario]][["efficacy_H1"]][,c(year_in_question*2)]
  } else if(virus_type =="AH3N2"){
    efficacy_NH <- vaccine_scenarios[[scenario]][["efficacy_H3"]][,c(year_in_question*2)-1]
    efficacy_SH <- vaccine_scenarios[[scenario]][["efficacy_H3"]][,c(year_in_question*2)]
  } else if(virus_type =="B"){
    efficacy_NH <- vaccine_scenarios[[scenario]][["efficacy_B"]][,c(year_in_question*2)-1]
    efficacy_SH <- vaccine_scenarios[[scenario]][["efficacy_B"]][,c(year_in_question*2)]
  }
  
  calender = as_vaccination_calendar(efficacy = rep(0,18)
                                     , dates = as.Date(dates) 
                                     , coverage = as.data.frame(new_coverage)
                                     , no_age_groups = 6
                                     , no_risk_groups = 2)
  
# extract immunity inputs
    prop_vacc_start <- list(prop_vaccine_compartments = c(immunity_input[1:6], rep(0,12)), 
                            prop_R_vaccinated = c(immunity_input[13:18], rep(0,12)),
                            prop_R = c(immunity_input[7:12], rep(0,12)))
  # Run the model and calculate the cumulative infections
  # outputs total infections in age group over the time period, for each posterior sample

  total_infections_ages <- epidemic_scenarios(demography = demography.ken,
                                              vaccine_calendar = calender,
                                              polymod_data = as.matrix(polymod.ken),
                                              contact_ids = contact_ids,
                                              parameters = posterior_subset,
                                              age_group_limits = c(1, 6, 15, 20, 50),
                                              risk_ratios = matrix(c(0, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0),
                                                                   ncol = 6, byrow = T),
                                              waning_rate = waning_rate, 
                                              vaccination_ratio_input = prop_vacc_start, 
                                              begin_date = begin_date, 
                                              end_date = end_date, 
                                              year_to_run = year_to_run, 
                                              efficacy_NH = efficacy_NH, 
                                              efficacy_SH = efficacy_SH)
  
  # calculate the sum of cases at the end over the season  
  total_infections <- rowSums(total_infections_ages)
}
