#Updated 5/7/2022
#Function to generate inmb samples and median inmb----
f <- function(df, wtp.df, exclude) {
  inmb<- df %>% 
    #select(Sample, Scenario, inc.costs, dalys.averted) %>% 
    cbind(wtp.df) %>% 
    mutate(across(matches("WTP"),
                  .fns = function(x) ((x*.$dalys.averted) - .$inc.costs),
                  .names = "inmb_{col}"))
  
  median.inmb <- inmb %>% 
    filter(!Scenario == "NO_V") %>% 
    select(-c(Sample, inc.costs, dalys.averted, WTP1:WTP10)) %>% 
    pivot_longer(-Scenario, names_to = "wtp.name", values_to = "inmb") %>% 
    group_by(Scenario, wtp.name) %>% 
    dplyr::summarise(median.inmb = median(inmb),
                     sd.inmb = sd(inmb),
                     lower_ci = round(quantile(inmb, 0.025, na.rm = TRUE)/1e6, 2),
                     upper_ci = round(quantile(inmb, 0.975, na.rm = TRUE)/1e6, 2)) %>% 
    ungroup() %>% 
    mutate(#lower_ci = round((median.inmb - sd.inmb)/1e6,2),
      #upper_ci = round((median.inmb + sd.inmb)/1e6,2),
      median_ci = paste0(round(median.inmb/1e6,2),
                         " (", lower_ci, ", ", upper_ci, ")")) %>% 
    select(Scenario, wtp.name, median_ci) %>% 
    pivot_wider(names_from = wtp.name, values_from = median_ci) %>% 
    select(Scenario, inmb_WTP1, inmb_WTP2:inmb_WTP9, inmb_WTP10)
  
  list(inmb = inmb, 
       median.inmb = median.inmb)
  
}


#Function to calculate data for ceac----
g <- function(df){
  
  sample.length <- nrow(df)/length(unique(df$Scenario))
  inmb.data <- df %>% 
    select(Scenario, contains("WTP"))
  prop.df <- data.frame(Scenario = unique(df$Scenario))
  
  for (i in 2:ncol(inmb.data)){
    empty <- inmb.data[c(1,i)]
    colnames(empty) <- c("Scenario", "WTP")
    
    empty<- empty %>% 
      arrange(Scenario) %>% 
      mutate(my.sample = rep(1:sample.length, length(unique(df$Scenario)))) %>%
      group_by(my.sample, Scenario) %>% 
      dplyr::summarise(max.wtp = max(WTP)) %>% 
      ungroup() %>% 
      group_by(my.sample) %>% 
      slice_max(max.wtp) %>% 
      ungroup() %>% 
      group_by(Scenario) %>% 
      tally() %>% 
      #mutate(row_max = names(.)[which.max(c_across(everything()))]) %>% 
      #group_by(row_max) %>% 
      #tally() %>% 
      mutate(prop = n/sum(n)) %>% 
      select(Scenario, prop)
    
    names(empty) <- c("Scenario", names(inmb.data[i]))
    
    prop.df <- prop.df %>% 
      full_join(empty, by = "Scenario")
    
  }
  print(prop.df)
}
