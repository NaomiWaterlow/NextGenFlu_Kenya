#Updated 5/7/2022
library(here)

#Load data, modify----
#Run only if running this script directly without running 01_Public-health-outcomes_mine_short.T
#source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))
source(here::here("Econ/Scripts", "mydf_functions.R"))

n_int_digits = function(x) {
  result = floor(log10(abs(x)))+1
  result[!is.finite(result)] = 0
  result
}

## Summary of median burden ----

medians <- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, contains("all.ages")) %>% 
  #select(!contains("lost.wages")) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "total") %>% 
  group_by(ay_Scenario3, Measure) %>% 
  dplyr::summarise(median = round(median(total, na.rm = TRUE)),
                   #median = median(value, na.rm = TRUE),
                   lower_ci = round(quantile(total, 0.025, na.rm = TRUE)),
                   upper_ci = round(quantile(total, 0.975, na.rm = TRUE))
  ) %>% 
  mutate(across(c(median, lower_ci, upper_ci),
                ~ case_when(n_int_digits(.) >= 6 ~ ./1e6,
                            n_int_digits(.) < 6 ~ .
                            
                ))) %>% #View()
  ungroup() %>% #View()
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
  select(ay_Scenario3, Measure, median_ci) %>% 
  pivot_wider(names_from = ay_Scenario3, values_from = median_ci) %>% 
  filter(!str_detect(Measure, paste(c("costs", "doses", "10", "13"), collapse = "|")))


# medians <- all.samples %>% 
#   select(Scenario2, Sample, contains("all.ages")) %>% 
#   pivot_longer(-c(Scenario2,Sample), names_to = "Measure", values_to = "value") %>% #View()
#   group_by(Scenario2, Sample, Measure) %>% 
#   dplyr::summarise(total = sum(value)) %>% 
#   ungroup() %>%
#   group_by(Scenario2, Measure) %>% #dplyr::summarise(median.total = median(total))
#   dplyr::summarise(median = round(median(total, na.rm = TRUE)),
#                    #median = median(value, na.rm = TRUE),
#                    lower_ci = round(quantile(total, 0.025, na.rm = TRUE)),
#                    upper_ci = round(quantile(total, 0.975, na.rm = TRUE))
#   ) %>% 
#   ungroup() %>% 
#   mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
#   select(Scenario2, Measure, median_ci) %>% 
#   pivot_wider(names_from = Scenario2, values_from = median_ci) %>% 
#   filter(!str_detect(Measure, paste(c("costs", "10", "13"), collapse = "|")))

row.order <- paste0("ay_", c("cases.all.ages", 
                             "upper.resp.cases.all.ages", "URT.OPC.visit.all.ages",
                             "lower.resp.cases.all.ages", "LRT.non.hosp.all.ages", 
                             "Hosp.all.ages", "total.deaths.all.ages",
                             "OPC.lost.wages.all.ages", "hosp.lost.wages.all.ages",
                             "total.YLD00.all.ages", "total.YLL00.all.ages", 
                             "total.DALY00.all.ages",
                             "disc_total.YLD00.all.ages", "disc_total.YLL03.all.ages", 
                             "disc_total.DALY03.all.ages"
)
)

medians %>% 
  filter(!Measure == "ay_total.YLL03.all.ages") %>% 
  slice(match(row.order, Measure)) %>% #View()
  mutate(Measure = case_when(
    Measure == "ay_cases.all.ages" ~ "Total cases (millions)",
    Measure == "ay_upper.resp.cases.all.ages" ~ "Total URT infections (millions)",
    Measure == "ay_URT.OPC.visit.all.ages" ~ "Total out-patient visits by patients with URT infection (millions)",
    Measure == "ay_lower.resp.cases.all.ages" ~ "Total LRT infections (millions)",
    Measure == "ay_LRT.non.hosp.all.ages" ~ "Total LRT infections that are not hospitalised (millions)",
    Measure == "ay_Hosp.all.ages" ~ "Total patients with severe illness that are hospitalised",
    Measure == "ay_total.deaths.all.ages" ~ "Total deaths",
    Measure == "ay_OPC.lost.wages.all.ages" ~ "Total lost wages (millions USD) due to influenza related illness (non-hospitalised)",
    Measure == "ay_hosp.lost.wages.all.ages" ~ "Total lost wages (millions USD) due to influenza related hospitalisation",
    Measure == "ay_total.YLD00.all.ages" ~ "Total YLD (undiscounted)",
    Measure == "ay_total.YLL00.all.ages" ~ "Total YLL (undiscounted) (millions)",
    Measure == "ay_total.DALY00.all.ages" ~ "Total DALYs (undiscounted) (millions)",
    Measure == "ay_disc_total.YLD00.all.ages" ~ "Total YLD (discounted)",
    Measure == "ay_disc_total.YLL03.all.ages" ~ "Total YLL (discounted) (millions)",
    TRUE ~ "Total DALYs (discounted) (millions)"
  )) -> medians



## Summaries for total costs ----
#total costs and DALYs in each year
unchanged <- all.samples %>% 
  select(Scenario3, Year, Vaccine.doses, cases.all.ages, disc.total.costs, disc_total.DALY03.all.ages, disc_dalys.03.averted) %>% 
  pivot_longer(-c(Scenario3, Year), names_to = "param", values_to = "val") %>% 
  group_by(Scenario3, Year, param) %>% 
  dplyr::summarise(median = round(median(val, na.rm = TRUE)),
                   lower_ci = round(quantile(val, 0.025, na.rm = TRUE)),
                   upper_ci = round(quantile(val, 0.975, na.rm = TRUE))
  ) %>% 
  ungroup() 


changed<-unchanged %>% 
  filter(!str_detect(param, paste0(c("dalys", "DALY03"), collapse = "|"))) %>% 
  mutate(across(c(median, lower_ci, upper_ci),
                ~ case_when(n_int_digits(.) >= 6 ~ ./1e6,
                            n_int_digits(.) < 6 ~ .
                            
                ))) %>% #View()
  mutate(across(where(is.numeric), round, 2))

unchanged %>% 
  filter(str_detect(param, paste0(c("dalys", "DALY03"), collapse = "|"))) %>% 
  rbind(changed) %>% #View()
  mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
  select(Scenario3, Year, param, median_ci) %>% 
  pivot_wider(names_from = param, values_from = median_ci) %>% 
  select(Scenario3, Year, Vaccine.doses,
         cases.all.ages, disc_total.DALY03.all.ages,
         disc.total.costs, disc_dalys.03.averted) -> total.costs.dalys.by.year

names(total.costs.dalys.by.year) <- c("Vaccine", 
                                      "Year",
                                      "Total doses administered (millions)",
                                      "Total cases (millions)",
                                      "Total DALYs (discounted)",
                                      "Total costs (discounted) (millions USD)",
                                      "Total DALYs averted (discounted)")

rm(changed, unchanged)

#across all 10 years combined
ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample,
         ay_Vaccine.doses,
         ay_vaccine.admin.costs.all.ages, ay_vaccine.doses.price.all.ages,
         ay_vaccine.costs.all.ages,
         ay_direct.medical.costs, ay_direct.medical.costs.minus.vaccine.costs,
         ay_healthcare.related.costs, ay_healthcare.related.costs.minus.vaccine,
         ay_indirect.costs, 
         ay_total.costs, ay_total.costs.minus.vacc.costs.vacc.transport, 
         ay_disc.total.costs
  ) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "total") %>% 
  group_by(ay_Scenario3, Measure) %>% 
  dplyr::summarise(median = round(median(total, na.rm = TRUE)/1e6, 2),
                   #median = median(value, na.rm = TRUE),
                   lower_ci = round(quantile(total, 0.025, na.rm = TRUE)/1e6, 2),
                   upper_ci = round(quantile(total, 0.975, na.rm = TRUE)/1e6, 2)
  ) %>% 
  ungroup() %>%
  mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
  select(ay_Scenario3, Measure, median_ci) %>% 
  pivot_wider(names_from = ay_Scenario3, values_from = median_ci) -> all.costs.summ



# all.samples %>% 
#   mutate(r = 0.03,
#          n = Year - base.year,
#          disc.total.costs = total.costs/((1+r)^n)) %>% 
#   select(Scenario2, Sample,
#          direct.medical.costs, direct.medical.costs.minus.vaccine,
#          healthcare.related.costs, indirect.costs,
#          total.costs, disc.total.costs
#   ) %>%
#   pivot_longer(-c(Scenario2, Sample),names_to = "Measure", values_to = "value") %>% #View()
#   group_by(Scenario2, Sample, Measure) %>% 
#   dplyr::summarise(total = sum(value)) %>% 
#   ungroup() %>% #View()
#   group_by(Scenario2, Measure) %>% 
#   dplyr::summarise(median = round(median(total, na.rm = TRUE)/1e6, 2),
#                    #median = median(value, na.rm = TRUE),
#                    lower_ci = round(quantile(total, 0.025, na.rm = TRUE)/1e6, 2),
#                    upper_ci = round(quantile(total, 0.975, na.rm = TRUE)/1e6, 2)
#   ) %>% 
#   ungroup() %>% 
#   mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
#   select(Scenario2, Measure, median_ci) %>% 
#   pivot_wider(names_from = Scenario2, values_from = median_ci) -> all.costs.summ2

row.order <- c(paste0("ay_", c("Vaccine.doses", "vaccine.admin.costs.all.ages", "vaccine.doses.price.all.ages",
                               "vaccine.costs.all.ages",
                               "direct.medical.costs", "direct.medical.costs.minus.vaccine.costs",
                               "healthcare.related.costs", "healthcare.related.costs.minus.vaccine",
                               "indirect.costs", "total.costs", "total.costs.minus.vacc.costs.vacc.transport",
                               "disc.total.costs"))
)

all.costs.summ %>% 
  slice(match(row.order, Measure)) %>% 
  mutate(Measure = case_when(
    Measure == "ay_Vaccine.doses" ~ "Vaccine doses administered (including 15% wastage) (millions)",
    Measure == "ay_vaccine.admin.costs.all.ages" ~ "Vaccine costs (administration) (millions USD)",
    Measure == "ay_vaccine.doses.price.all.ages" ~ "Vaccine purchase costs (total doses) (millions USD)",
    Measure == "ay_vaccine.costs.all.ages" ~ "Total vaccine costs (administration and purchase) (millions USD)",
    Measure == "ay_direct.medical.costs" ~ "Direct medical costs (millions USD)",
    Measure == "ay_direct.medical.costs.minus.vaccine.costs" ~ "Direct medical costs (excl. vaccine purchase and administration costs) (millions USD)",
    Measure == "ay_healthcare.related.costs" ~ "Healthcare related costs (millions USD)",
    Measure == "ay_healthcare.related.costs.minus.vaccine" ~ "Healthcare related costs (excl. transport costs to seek vaccination) (millions USD)",
    Measure == "ay_indirect.costs" ~ "Indirect costs (millions USD)",
    Measure == "ay_total.costs" ~ "Total costs (millions USD)",
    Measure == "ay_total.costs.minus.vacc.costs.vacc.transport" ~ "Total costs (excl. vaccine purchase, administration and transport costs) (millions USD)",
    TRUE ~ "Discounted total costs (millions USD)"
  )) -> all.costs.summ2


## Summaries for incremental costs, dalys averted and icers ----

ay.outcomes.costs %>% 
  # mutate(icer.total.per.daly.00 = ay_incremental.total.costs/ay_dalys.00.averted,
  #        icer.total.per.daly.03 = ay_incremental.total.costs/ay_dalys.03.averted,
  #        icer.disc.total.per.daly.00 = ay_disc.incremental.total.costs/ay_dalys.00.averted,
  #        icer.disc.total.per.daly.03 = ay_disc.incremental.total.costs/ay_dalys.03.averted
  # ) %>% #View()
  select(ay_Scenario3, ay_Sample,
         ay_incremental.total.costs, ay_disc.incremental.total.costs,
         ay_dalys.00.averted, ay_disc_dalys.03.averted,
         icer.disc.total.per.daly.00, icer.disc.total.per.disc.daly.03) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "value") %>% #View()
  group_by(ay_Scenario3, Measure) %>%  
  dplyr::summarise(median = round(median(value, na.rm = TRUE)),
                   #median = median(value, na.rm = TRUE),
                   lower_ci = round(quantile(value, 0.025, na.rm = TRUE)),
                   upper_ci = round(quantile(value, 0.975, na.rm = TRUE))) %>%
  mutate(across(c(median, lower_ci, upper_ci),
                ~ case_when(n_int_digits(.) >= 5 ~ ./1e6,
                            n_int_digits(.) < 5 ~ .
                            
                ))) %>%
  ungroup() %>% #View()
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
  select(ay_Scenario3, Measure, median_ci) %>% #View()
  pivot_wider(names_from = ay_Scenario3, values_from = median_ci) -> all.icers.summ

# all.samples %>% 
#   #group_by(Scenario) %>% 
#   select(Scenario2, Sample,
#          incremental.total.costs, disc.incremental.total.costs,
#          dalys.00.averted, dalys.03.averted,
#          icer.total.per.daly.00, icer.total.per.daly.03,
#          icer.disc.total.per.daly.00, icer.disc.total.per.daly.03) %>%
#   pivot_longer(-c(Scenario2, Sample), names_to = "Measure", values_to = "value") %>% #View()
#   group_by(Scenario2, Sample, Measure) %>% 
#   dplyr::summarise(total = sum(value)) %>% 
#   ungroup() %>% #View()
#   group_by(Scenario2, Measure) %>%  
#   dplyr::summarise(median = round(median(total, na.rm = TRUE)),
#                    #median = median(value, na.rm = TRUE),
#                    lower_ci = round(quantile(total, 0.025, na.rm = TRUE)),
#                    upper_ci = round(quantile(total, 0.975, na.rm = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(median_ci = paste0(median, " (", lower_ci, ", ", upper_ci, ")")) %>% 
#   select(Scenario2, Measure, median_ci) %>% 
#   pivot_wider(names_from = Scenario2, values_from = median_ci) %>% 
#   select(-NO_V) -> all.summ


row.order <- c("ay_incremental.total.costs", "ay_disc.incremental.total.costs", 
               "ay_dalys.00.averted",  "icer.disc.total.per.daly.00", 
               "ay_disc_dalys.03.averted",  "icer.disc.total.per.disc.daly.03")

all.icers.summ %>% 
  slice(match(row.order, Measure)) %>% 
  mutate(Measure = case_when(
    Measure == "ay_incremental.total.costs" ~ "Incremental total costs (millions USD)",
    Measure == "ay_disc.incremental.total.costs" ~ "Incremental total costs (discounted) (millions USD)",
    Measure == "ay_dalys.00.averted" ~ "DALYs averted (undiscounted) (millions)",
    Measure == "ay_disc_dalys.03.averted" ~ "DALYs averted (discounted) (millions)",
    Measure == "icer.disc.total.per.daly.00" ~ "ICER per DALY averted (discounted costs)",
    TRUE ~ "ICER per DALY averted (discounted costs & DALYs)"
  ))  -> all.icers.summ

#All three summaries in one table ----
all.combined <- as.data.frame(mapply(c, medians,all.costs.summ2, all.icers.summ))

play.df <- df.wordtable.loop(list(medians, total.costs.dalys.by.year, all.costs.summ2, all.icers.summ, all.combined))

print(play.df, target = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                                   paste0(filename, "_costs_table_updated.docx")
))
