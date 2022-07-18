gen_seeiir_ag_vacc_waning <- odin::odin({
  # Number of groups
  no_groups <- user()
  # INITIAL CONDITIONS
  # Population size by age/risk group
  pop[] <- user()
  # Start vaccinated by age/risk group
  V0[] <- user()
  # R0[] <- user()
  RV0[] <- user()
  # Initial infection by age/risk group
  num_vac_start[] <- user()
  
  # MODEL PARAMETERS
  
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  # efficacy
  alpha[] <- user()
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <-vI[i] #if (sumN[i]>0) vI[i]*pop[i] else 0
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  deriv(S[])  <- + omega*Sv[i] + omega*Rv[i] - v[i] * S[i] 
  deriv(Sv[])  <- - omega*Sv[i] + v[i] * (1-alpha[i]) * S[i] 
  deriv(Rv[])  <- - omega*Rv[i]  + v[i] * (alpha[i] * S[i])
  deriv(VT[]) <- vI[i]*pop[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- pop[i]*(1-V0[i])
  initial(Sv[1:no_groups]) <- (pop[i]*V0[i])*(1-RV0[i])
  initial(Rv[1:no_groups]) <- (pop[i]*V0[i])*(RV0[i])
  initial(VT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  dim(pop) <- no_groups
  dim(V0) <- no_groups
  dim(RV0) <- no_groups
  dim(num_vac_start) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  dim(alpha) <- no_groups
  
  dim(S) <- no_groups
  dim(Sv) <- no_groups
  dim(Rv) <- no_groups
  dim(VT) <- no_groups
  
})

gen_seeiir_ag_vacc_waning_NH <- odin::odin({
  # Number of groups
  no_groups <- user()
  
  # INITIAL CONDITIONS
  # Population size by age/risk group
  pop[] <- user()
  # Initial proportion in each compartment
  allS[] <- user()
  allSv[] <- user()
  allRv[] <- user()
  num_vac_start[] <- user()
  # MODEL PARAMETERS
  # Vaccine related variables 
  dates[] <- user()
  calendar[,] <- user()
  # waning of vaccine immunity
  omega <- user()
  
  # efficacy
  alpha[] <- user()
  
  # Vaccination. The rate is a step function that changes at each date according
  # to the passed calendar
  vI[] <- interpolate(dates, calendar, "constant")
  # Vaccination is given as a fraction vaccination, here we scale it to 
  # a rate
  #sumN[] <- if (vI[i]>0) (S[i]+E1[i]+E2[i]+I1[i]+I2[i]+R[i]) else 0
  v[] <- vI[i]#if (sumN[i]>0) vI[i]*sumN[i]/pop[i] else 0
  
  # THE DERIVATIVES OF THE SEEIIR MODEL
  # Derivatives of the not vaccinated group
  deriv(S[])  <- + omega*Sv[i] + omega*Rv[i] - v[i] * S[i] 
  # Derivatives vaccination group
  deriv(Sv[])  <- - omega*Sv[i]  + v[i] * (1-alpha[i]) * S[i] 
  deriv(Rv[])  <- - omega*Rv[i]  + v[i] * (alpha[i] * S[i])
  deriv(VT[]) <- vI[i]*pop[i]
  
  # Initial value of the variables
  initial(S[1:no_groups]) <- allS[i]
  
  initial(Sv[1:no_groups]) <- allSv[i]
  initial(Rv[1:no_groups]) <- allRv[i]
  initial(VT[1:no_groups]) <- num_vac_start[i]
  
  # Set dimension of all variables/parameters
  dim(dates) <- user()
  dim(calendar) <- user()
  dim(pop) <- no_groups
  dim(allS) <- no_groups
  dim(allSv) <- no_groups
  dim(allRv) <- no_groups
  dim(num_vac_start) <- no_groups
  dim(v) <- no_groups
  dim(vI) <- no_groups
  # dim(sumN) <- no_groups  
  dim(alpha) <- no_groups
  
  dim(S) <- no_groups
  dim(Sv) <- no_groups
  dim(Rv) <- no_groups
  dim(VT) <- no_groups
})


infectionODEs <- function(population, initial_infected, vaccine_calendar, contact_matrix,
                          susceptibility, transmissibility, infection_delays, interval 
                          ,waning_rate, initial_vaccinated_prop, initial_Rv_prop,
                          year_to_run, efficacy_NH, ...
) {
  
  # Extract the date used from the vaccine calendar
  begin_date <- as.Date(paste0(year_to_run, "-03-01"))
  end_date <- as.Date(paste0(year_to_run, "-09-01"))
  t <- as.numeric(seq(begin_date, end_date, interval))
  
  no_groups <- length(population)
  no_risk_groups <- no_groups/nrow(contact_matrix)
  no_age_groups <- no_groups/no_risk_groups
  
  
  # adds a new top row, with the start date od simulation and 0 vaccination
  calendar <- vaccine_calendar$calendar[c(nrow(vaccine_calendar$calendar),1:nrow(vaccine_calendar$calendar)),]
  dates <- as.numeric(c(t[1], vaccine_calendar$dates))
  if(dates[1] == dates[2]){dates <- dates[-2]
  calendar <- calendar[-2,]}
  # Run the model over the first 6 months
  # age the vaccinated population by 1 year. 
  
  initial_vaccinated_prop <- age_population_1year(population, old_proportions=initial_vaccinated_prop)
  initial_Rv_prop <- age_population_1year(population, old_proportions = initial_Rv_prop)
  #Assume that all R become susceptible again at the end of the year.
  initial_R_prop <- rep(0,num_age_groups*3)

  mod <- gen_seeiir_ag_vacc_waning$new(no_groups = no_groups,
                                       pop = population, V0 = initial_vaccinated_prop,
                                       RV0 = initial_Rv_prop,
                                       alpha = vaccine_calendar$efficacy[1:no_groups],
                                       omega = waning_rate,
                                       dates = dates,
                                       calendar = calendar[,1:no_groups],
                                       num_vac_start = rep(0,num_age_groups*3)
  )
  
  # run the model 
  y <- mod$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  y_tail <- tail(y,1)
  
  # Change the dates to NH 
  begin_date <- as.Date(paste0(year_to_run, "-09-01"))
  end_date <- as.Date(paste0(as.character(year_to_run+1), "-03-01"))
  t <- as.numeric(seq(begin_date, end_date, interval))
  #Update the vaccination to NH
  keepers <- which(vaccine_calendar$dates>= begin_date )
  vaccine_calendar$dates <- vaccine_calendar$dates[keepers] 
  
  if(!is.na(vaccine_calendar$dates[1])){ temp <- vaccine_calendar$dates} else {temp <- tail(t,1)}
  input_dates <- c(t[1],temp)
  if(input_dates[1] == input_dates[2]){input_dates <- input_dates[-1]}
  vaccine_calendar$dates <- input_dates
  vaccine_calendar$efficacy <- efficacy_NH
  
  if(length(input_dates)>2){
    vaccine_calendar$calendar <- vaccine_calendar$calendar[c(keepers[1]-1,keepers),]
  } else{
    vaccine_calendar$calendar =matrix(rep(0,num_age_groups*3*length(input_dates)), ncol = num_age_groups*3)
    vaccine_calendar$efficacy <- rep(0,num_age_groups*3)
  }
  
  # recreate the inputs
  calendar <- vaccine_calendar$calendar
  # carry on the runi
  mod2 <- gen_seeiir_ag_vacc_waning_NH$new(no_groups = no_groups,
                                           pop = population,
                                           allS = y_tail[2:((2+no_groups)-1)],
                                           allSv = y_tail[(2+no_groups):((2+(no_groups*2))-1)],
                                           allRv = y_tail[(2+(2*no_groups)):((2+(no_groups*3))-1)],
                                           num_vac_start = y_tail[(2+(3*no_groups)):((2+(no_groups*4))-1)],
                                           alpha = vaccine_calendar$efficacy[1:no_groups],
                                           omega = waning_rate,
                                           dates = input_dates,
                                           calendar = calendar[,1:no_groups]
                                           #    ageing =ageing
  )
  
  y2 <- mod2$run(t, hmax = NULL, method = "euler", hini = 0.25, atol = 1)
  y <- rbind(y, y2[-c(1),])
  y <- data.table(y)
  colnames(y) <- gsub(pattern = "\\[", replacement="", x =colnames(y))
  colnames(y) <- gsub(pattern = "\\]", replacement="", x =colnames(y))
  
  # caclulate the proportion vaccinated in each age group
  for(agp in 1:(num_age_groups*no_risk_groups)){
    prop_v_label <- paste0("prop_v", agp)
    S_label <- paste0("S", agp)
    y[, eval(prop_v_label) := (population[agp]-(get(S_label)))/population[agp]]
  }
  # calculate the proportion in the Rv compartment in each age group
  for(agp in 1:(num_age_groups*no_risk_groups)){
    prop_Rv_label <- paste0("prop_Rv", agp)
    Rv_label <- paste0("Rv", agp)
    Sv_label <- paste0("Sv", agp)
    y[, eval(prop_Rv_label) := get(Rv_label)/(get(Sv_label) + get(Rv_label))]
  }
  # calculate the total number vaccinated in each age group
  for(agp in 1:(num_age_groups*no_risk_groups)){
    Vaccinated_label <- paste0("Vaccinated",agp)
    VT_label <- paste0("VT",agp)
    y[, eval(Vaccinated_label) := get(VT_label)]
  }
  
  
  y[is.na(y)] <- 0
  
  return(y[,((3*num_age_groups*4)+2):ncol(y)])
}


# This function defines the incidence function. 
# Loops round and checks again, if there wasn't an incidence function first time

vacc_scenario_ken <- function (vaccine_calendar, parameters, contact_ids, incidence_function, 
                               time_column,
                               waning_rate,
                               vaccination_ratio_input,year_to_run,efficacy_NH,
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
      #  initial.infected <- rep(10^parameters[9], 6)
      initial.infected <- rep(0, num_age_groups)
      initial.infected <- stratify_by_risk(initial.infected, 
                                           risk_ratios)
      
      
      if(is.null(names(vaccination_ratio_input))){
        pv_input <- vaccination_ratio_input
        pRv_input <- vaccination_ratio_input
      } else {
        pv_input <- vaccination_ratio_input[grep(pattern = "prop_v", names(vaccination_ratio_input))]
        pRv_input <-  vaccination_ratio_input[grep(pattern = "prop_Rv", names(vaccination_ratio_input))]
      }
      
      
      infectionODEs(popv, initial.infected, vaccine_calendar, contacts,
                    susceptibility = c(parameters[6], parameters[6], parameters[6], parameters[7], parameters[7], parameters[8]),
                    transmissibility = parameters[5],
                    infection_delays = c(0.8,1.8), interval = 1,
                    waning_rate = waning_rate,
                    initial_vaccinated_prop = pv_input,
                    initial_Rv_prop = pRv_input,
                    year_to_run = year_to_run, efficacy_NH = efficacy_NH
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
      
      return(lapply(parameters, function(pars) vaccination_scenario(parameters = pars, 
                                                                    vaccine_calendar = vaccine_calendar,
                                                                    incidence_function = incidence_function, 
                                                                    time_column = time_column, ...)))
    }
    else {
      pc <- cbind(parameters, contact_ids)
      #print("it is here")
      return(lapply(pc, function(pars_contacts) vaccination_scenario(parameters = pars_contacts[1:ncol(parameters)], 
                                                                     contact_ids = pars_contacts[(ncol(parameters) + 
                                                                                                    1):length(pars_contacts)],
                                                                     vaccine_calendar = vaccine_calendar, 
                                                                     incidence_function = incidence_function, time_column = time_column, 
                                                                     ...)))
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

vaccination_scenario <- function (vaccine_calendar, parameters, contact_ids, incidence_function, 
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
        vaccination_scenario(parameters = pars, 
                             vaccine_calendar = vaccine_calendar, incidence_function = incidence_function, 
                             time_column = time_column, ...)))
    }
    else {
      pc <- cbind(parameters, contact_ids)
      
      return(t(apply(pc, 1, function(pars_contacts)
        vaccination_scenario(parameters = pars_contacts[1:ncol(parameters)], 
                             contact_ids = pars_contacts[(ncol(parameters) + 
                                                            1):length(pars_contacts)],
                             vaccine_calendar = vaccine_calendar, 
                             incidence_function = incidence_function, time_column = time_column, 
                             ...))))
    }
  }
}


age_population_1year <- function(population, old_proportions){
  

  # - proportion of the age group that will move into the next age group (proportion_ageing)
  proportion_ageing <- c()
  # - relative population size of the age group (pop_weighting)
  pop_weighting <- c()
  # storage 
  new_proportions_all <- c()
  # for each risk group 
  for(l in 1:3){
    
    # - old proportions
    old_proportions_sub <- old_proportions[(l-1)*num_age_groups+(1:num_age_groups)]
    population_sub <- population[l*(1:num_age_groups)]
    # vector for storing new proprotions
    new_proportions <- c()
    # for each age group

    for(k in 1:(length(age_groups)+1)){
      
      # specify end of each age group
      if(k == (length(age_groups)+1)){
        age_grp_end <-max_age } else {
          age_grp_end <- age_groups[k]} 
      # specify start of each ate group and calculate population weightings
      if(k==1){
        age_grp_start <- 0
        # length of age group
        age_grp_length <- age_grp_end-age_grp_start
        #calculate relative population weighting - assumign in bottom same proprtion born
        pop_weighting_temp <- 1/age_grp_length
        #age_grp_length_prev
        age_grp_length_prev <- 1
      } else {
        age_grp_start <- age_groups[k-1]
        # length of age group
        age_grp_length <- age_grp_end-age_grp_start
        # calculate relative population sizes (by length of age group)
        pop_weighting_temp <-c(age_grp_length_prev/age_grp_length)
        # save the previous one for next time
        age_grp_length_prev <- age_grp_length
      }

      # calculate the proportion ageing
      if(l==1){
        proportion_ageing <- c(proportion_ageing,(1/age_grp_length))
        # calculate the relative population sizese
        pop_weighting <-c(pop_weighting,pop_weighting_temp) 
      }
      
      if(k==1){
        
        # in from the previous age group - assume the proportion is 0
        new_top <- ((pop_weighting[k]*0) + 
                      # those already in the current age group
                      (old_proportions_sub[k]*1 )- 
                      # those leaving the age group
                      (old_proportions_sub[k]* proportion_ageing[k])) 
        
        # denominator: orginal pop, add those coming in, minus those leaving
        new_bottom <- (1) + (pop_weighting[k]*1) - 
          (1*proportion_ageing[k])
        
        new_prop_temp <- new_top/new_bottom
      }else{ 
        # in from the previous age group
        new_top <- ((pop_weighting[k]*old_proportions_sub[k-1]*proportion_ageing[k-1]) + 
                      # those already in the current age group
                      (old_proportions_sub[k]*1 )- 
                      # those leaving the age group
                      (old_proportions_sub[k]*proportion_ageing[k] )) 
        
        # denominator: orginal pop, add those coming in, minus those leaving (proportions)
        new_bottom <- 1 + (pop_weighting[k]*proportion_ageing[k-1]) - 
          (1*proportion_ageing[k])
        
        new_prop_temp <- new_top/new_bottom
      }
      new_proportions <- c(new_proportions, new_prop_temp)
    
    }
    new_proportions_all <- c(new_proportions_all, new_proportions)
  }
  
  new_proportions_all[is.nan(new_proportions_all)] <- 0
  if(any(new_proportions_all >1)){browser()}
  if(any(new_proportions_all >1)){stop("proportion in ageing is bigger than one!")}
  return(new_proportions_all)
  
}


