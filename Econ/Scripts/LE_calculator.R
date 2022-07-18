#Updated 5/7/2022

#---------------------------------------------------------------------------------------------------
# FUNCTION: cov_dLE - calculates discounted life expectancy for each of the
# CovidM age bands (equivalent to YLL per death)
#---------------------------------------------------------------------------------------------------
library(data.table)
# load UN population and life tables sourced from https://population.un.org/wpp/Download/Standard/CSV/
# UNPOP <- fread(paste0(path_dropbox,
#                       "WPP2019_PopulationBySingleAgeSex_2020-2100.csv"))[Time == 2022]       # only need 2022 projections

# qx = probability of dying between age x and x+1
# lx = number of 100,000 reference pop surviving to age x
# dx = instantaneous death rate = -ln{1-qx}
# Lx = person years lived between age x and x+1 = [l(x) + l(x+1)]/2 
# Tx = total person years lived above age x
# dLEx = discounted life expectancy at age x

flu_dLE <- function(
    LT,                            # data table with UN life tables,
    r               = 0,           # discount rate
    smr             = 1,           # SMRs adjustment for covid co-morbidities
    selectCountries = c("KEN"),    # vector of iso3 codes of countries to run
    selectTime      = "2010-2015", # which UN life-table time-period to use 
    selectSex       = "Total",     # which UN life-table sex to use
    weight_method   = "lx",   # weight method to average LE by age group: "lx" "lxqx" "equal" "pop_ifr"
    POP             = NULL         # data table of populations to be supplied if using weight_method=="pop_ifr"
){
  
  require(data.table)
  require(ISOcodes)
  
  # age bands for output to match CovidM
  AgeBands = data.table(
    AgeBand = seq(1,16,1),
    start   = seq(0,75,5),
    end     = c(seq(4,74,5),100)
  )
  
  
  # UN M49 country code to ISO3 mapping
  UN_M.49_Countries <- as.data.table(UN_M.49_Countries)[, LocID := as.integer(Code)]
  
  LT <- LT[     # Convert UN M49 country code to ISO3 (also drops regions)           
    UN_M.49_Countries, 
    on = "LocID"
  ][            # only life tables for selected time period, sex, and countries
    Time == selectTime & Sex == selectSex & ISO_Alpha_3 %in% selectCountries    
  ][
    ,
    .(         # keep only selected vars
      country = ISO_Alpha_3,
      AgeGrpStart,
      AgeGrpSpan,
      qx
    )
  ][            # Add AgeGrpEnd
    , AgeGrpEnd := AgeGrpStart + AgeGrpSpan
  ][AgeGrpStart == 100, AgeGrpEnd := 101]
  
  # expand age groups from 0 to maximum age
  out <- data.table(Age = seq(0, max(LT$AgeGrpStart)))  
  
  out <- LT[
    out, 
    on=.(AgeGrpStart <= Age, AgeGrpEnd > Age)
  ][
    order(country, AgeGrpStart)
  ][,AgeGrpEnd := NULL]
  
  setnames(out,"AgeGrpStart","Age")
  
  # convert age group qx to estimated yearly qx
  out[, qx := 1 - (1 - qx)^(1 / AgeGrpSpan)]
  
  # instantaneous death rate (Briggs et al)
  out[, dx := -log((1 - qx))]
  
  # lx
  out[Age == 0, lx := 100000] # starting age
  for (c in selectCountries){
    for (a in 1:max(out$Age)){
      out[
        country == c & Age == a,
        lx := out[country == c & Age == a - 1, lx] *        # alive at start of previous age group
          exp(-out[country == c & Age == a - 1, dx] * smr)  # deaths during previous age group
      ]
    }
  }
  
  # Lx
  for (c in selectCountries){
    for (a in 0:(max(out$Age) - 1)){
      out[
        country == c & Age == a,
        Lx :=  0.5 * (lx + out[country == c & Age == a + 1, lx])       
      ]
    }
  }
  out[Age == max(out$Age), Lx := lx] # final age
  
  # discounted Tx
  for (c in selectCountries){
    for (a in 0:max(out$Age)){
      out[
        country == c & Age == a,
        dTx := sum(out[country == c & Age >= a, Lx / (1 + r)^(Age - a)])
      ]
    }
  }
  
  # discounted LEx
  out[, dLEx := dTx/lx]
  
  # Age groups for output 
  out[, joinAge := Age]
  out <- out[AgeBands, on = .(joinAge >= start, joinAge <= end)]
  
  out <- out[Age != 100] # drop age 100
  return(out)
  
  # dLEx for age bands weighted by lx
  
  if (weight_method == "lx"){
    out <- out[
      ,
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(lx * dLEx)/sum(lx)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "lxqx"){
    out <- out[
      ,
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(lx * qx * dLEx)/sum(lx * qx)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "equal") {
    out <- out[
      ,
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = mean(dLEx)
      ),
      by=.(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "pop_ifr") {
    
    POP <- POP[     # Convert UN M49 country code to ISO3 (also drops regions)
      UN_M.49_Countries,
      on = "LocID"
    ][
      ,
      country := ISO_Alpha_3
    ][ country %in% selectCountries & AgeGrp <= 99][
      ,
      IFR := ifr_levin(AgeGrp)
    ]
    
    out <- out[
      POP[, .(country = country, Age = AgeGrp, Pop = PopTotal, IFR = IFR)],
      on = .(Age == Age, country == country)
    ][
      order(country, AgeBand)
    ]
    
    out <- out[
      ,
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(Pop * IFR * dLEx)/sum(Pop * IFR)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else {out <- "Error: Invalid argument for weight_method"}
  
  
  return(out[, c("disc.rate","SMR") := NULL]) 
  # for now dropping discount rate & SMR from output as this is known implicitly
  
}


