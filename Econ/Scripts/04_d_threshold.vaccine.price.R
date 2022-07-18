#Updated 5/7/2022
library(here)

#Load data, modify----
#Run only if running this script directly without running 01_Public-health-outcomes_mine_short.T

################NOTE - ALWAYS RUN run_scripts.R first
#source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))
source(here::here("Econ/Scripts", "mydf_functions.R"))

#preparing the df for threhold price calculation----
t.price.calc <- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, #total.costs, vaccine.costs.all.ages,#vaccine.transport.costs.all.ages,
         ay_total.costs.minus.vaccine.price, ay_disc.total.costs.minus.vaccine.price,
         ay_dalys.00.averted, ay_disc_dalys.03.averted, ay_Vaccine.doses)

#the dataframe with base scenario costs and dalys
base.costs <- t.price.calc %>%
  filter(ay_Scenario3 == "No vaccination") %>%
  select(ay_Scenario3, ay_Sample, ay_total.costs.minus.vaccine.price, ay_disc.total.costs.minus.vaccine.price)

names(base.costs) <- paste0("base_", names(base.costs))


t.price.calc <- t.price.calc %>% 
  left_join(base.costs, by = c("ay_Sample" = "base_ay_Sample")) %>% 
  mutate(costs.offset1 = base_ay_total.costs.minus.vaccine.price - ay_total.costs.minus.vaccine.price,
         disc.costs.offset1 = base_ay_disc.total.costs.minus.vaccine.price - ay_disc.total.costs.minus.vaccine.price)


#preparing the wtp dataframe----
wtp <- c(19.13,100,491,497.29,542,623,647,975.45,1912.65,5737.95)
names(wtp) <- paste0("WTP", 1:length(wtp))

wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(t.price.calc)), nrow = nrow(t.price.calc)))
colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))

wtp.to.bind <- data.frame(wtp) %>% 
  mutate(Threshold = rownames(.),
         Threshold.label = paste0("$",round(wtp)))

#function to calculate threshold vaccine price----
#this function will require a dataframe with columns named 
#Scenario, Sample, costs.offset, dalys.averted and Vaccine.doses
tp_calc <- function(df, wtp.df){
  
  nsample <- length(unique(df$Sample))
  tprice.all.samples<- df %>% 
    cbind(wtp.df) %>% 
    mutate(across(matches("WTP"), 
                  .fns = function(x) ((.$costs.offset + (.$dalys.averted*x))/.$Vaccine.doses),
                  .names = "tp_{col}"
    )
    )
  tprice <- tprice.all.samples %>% 
    select(Scenario, Sample, contains("tp_")) %>% 
    pivot_longer(-c(Scenario, Sample), names_to = "Threshold", values_to = "val") %>% #View()
    group_by(Scenario, Threshold) %>%
    dplyr::summarise(median.tp = round(median(val), 2),
                     # sd.tp = sd(val, na.rm = T),
                     # margin.error = 1.96*(sd.tp/sqrt(nsample)),
                     # lower_ci = round(median.tp - margin.error,2),
                     # upper_ci = round(median.tp + margin.error,2),
                     lower_ci = round(quantile(val, 0.025, na.rm = TRUE), 2),
                     upper_ci = round(quantile(val, 0.975, na.rm = TRUE), 2))
  
  tprice.table <- tprice %>% 
    filter(!Scenario == "No vaccination") %>% 
    mutate(median_ci = paste0(median.tp, " (", lower_ci, ", ", 
                              upper_ci, ")")) %>% 
    select(Scenario, Threshold, median_ci) %>% 
    pivot_wider(names_from = "Threshold", values_from = "median_ci", names_sort = F) %>% 
    select(Scenario, tp_WTP1, tp_WTP2:tp_WTP9, tp_WTP10)
  
  colnames(tprice.table) <- c("Vaccine",
                              paste0("Threshold = $", round(unique(wtp.df)))
  )
  list(tprice.all.samples = tprice.all.samples,
       tprice = tprice, 
       tprice.table = tprice.table)
  
}

## Discounted costs and dalys----
dftouse.disc<- t.price.calc %>% 
  select(ay_Scenario3, ay_Sample, disc.costs.offset1, ay_disc_dalys.03.averted, ay_Vaccine.doses)
tp.calc_col.names <- c("Scenario", "Sample", "costs.offset", "dalys.averted", "Vaccine.doses")

names(dftouse.disc) <- tp.calc_col.names

median.tp.discounted <- tp_calc(dftouse.disc, wtp.df)

median.tp.discounted.short <- median.tp.discounted$tprice.table %>% 
  select(contains(c("Vaccine", "100", "623", "1913", "5738")))

names(median.tp.discounted.short) <- c("Vaccine", 
                                       "WHO best buy ($100)",
                                       "45% per capita GDP ($623)",
                                       "1x per capita GDP ($1913)",
                                       "3x per capita GDP ($5738)")


#plot median threshold prices
# median.tp.discounted$tprice %>% 
#   select(Scenario, Threshold, median.tp, lower_ci, upper_ci) %>% 
#   filter(!Scenario == "No vaccination") %>% 
#   left_join(wtp.to.bind) %>% 
#   ggplot(aes(factor(wtp), median.tp, group = Scenario, col = Scenario))+
#   geom_boxplot()+
#   # geom_line() +
#   # geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci,
#   #                 fill = Scenario), alpha = 0.3)+
#   theme_classic()+
#   theme(legend.position = "bottom")+
#   xlab("Willingness to pay threshold (USD)")+
#   ylab("Threshold vaccine price")+
#   ggtitle("Threshold vaccine prices at various WTPs \n(discounted costs and dalys)")
# 
# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
#                   "tvprice_discounted.jpeg"),
#        height = 1200, width = 1950, units = "px", limitsize = FALSE)
# 
#labs(fill = "Vaccination scenario")

## Discounted costs and undiscounted dalys----
dftouse.undisc.dalys<- t.price.calc %>% 
  select(ay_Scenario3, ay_Sample, disc.costs.offset1, ay_dalys.00.averted, ay_Vaccine.doses)

names(dftouse.undisc.dalys) <- tp.calc_col.names

median.tp.undisc.dalys <- tp_calc(dftouse.undisc.dalys, wtp.df)

median.tp.undisc.dalys.short <- median.tp.undisc.dalys$tprice.table %>% 
  select(contains(c("Vaccine", "100", "623", "1913", "5738")))


names(median.tp.undisc.dalys.short) <- c("Vaccine", 
                                         "WHO best buy ($100)",
                                         "45% per capita GDP ($623)",
                                         "1x per capita GDP ($1913)",
                                         "3x per capita GDP ($5738)")


#plot median threshold prices
# median.tp.undisc.dalys$tprice %>% 
#   select(Scenario, Threshold, median.tp, margin.error) %>% 
#   filter(!Scenario == "No vaccination") %>% 
#   left_join(wtp.to.bind) %>% 
#   ggplot(aes(wtp, median.tp, group = Scenario, col = Scenario))+
#   geom_line() +
#   geom_ribbon(aes(ymin = median.tp-margin.error, ymax = median.tp+margin.error,
#                   fill = Scenario), alpha = 0.3)


#write both prices to word----
play.df <- df.wordtable.loop(list(median.tp.discounted$tprice.table,
                                  median.tp.discounted.short,
                                  median.tp.undisc.dalys$tprice.table,
                                  median.tp.undisc.dalys.short))

print(play.df, target = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                                   "vacc_tprice_updated.docx")
)

# group_by(Scenario2) %>%
# dplyr::summarise(median.wtp1 = median(WTP1),
#                  sd.wtp1 = sd(WTP1), 
#                  median.wtp2 = median(WTP2),
#                  sd.wtp2 = sd(WTP2), 
#                  median.wtp3 = median(WTP3),
#                  sd.wtp3 = sd(WTP3), 
#                  median.wtp4 = median(WTP4),
#                  sd.wtp4 = sd(WTP4), 
#                  median.wtp5 = median(WTP5),
#                  sd.wtp5 = sd(WTP5), 
#                  median.wtp6 = median(WTP6),
#                  sd.wtp6 = sd(WTP6), 
#                  median.wtp7 = median(WTP7),
#                  sd.wtp7 = sd(WTP7), 
#                  median.wtp8 = median(WTP8),
#                  sd.wtp8 = sd(WTP8), 
#                  median.wtp9 = median(WTP9),
#                  sd.wtp9 = sd(WTP9), 
#                  median.wtp10 = median(WTP10),
#                  sd.wtp10 = sd(WTP10) 
# ) %>% view()

# OR

# mutate(across(matches("WTP"), function(x) 
#   ((.$ay.costs.offset1 + (.$ay.dalys.00.averted*x))/.$ay.Vaccine.doses),
#   .names = "tp_{col}")) %>%
#   #select(-contains("ay.")) %>% #View()
#   select(Scenario2, contains("tp_WTP")) %>% 
#   group_by(Scenario2) %>% 
#   dplyr::summarise(median = across(matches("WTP"), median),
#                    sd = across(matches("WTP"), sd))