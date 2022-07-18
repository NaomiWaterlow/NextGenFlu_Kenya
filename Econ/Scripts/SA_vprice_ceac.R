library(tidyverse)
ay.outcomes.costs_vacc1.5 <- read.csv(here::here("Econ/Model_outcomes_output/Final/Naomi", 
                                                 "SA-vprice-1.5_2022-07-18/Naomi_ay.outcomes.costs.csv"))
ay.outcomes.costs_vacc6 <- read.csv(here::here("Econ/Model_outcomes_output/Final/Naomi", 
                                                 "SA-vprice-6_2022-07-18/Naomi_ay.outcomes.costs.csv"))
ay.outcomes.costs_vacc10 <- read.csv(here::here("Econ/Model_outcomes_output/Final/Naomi", 
                                                 "SA-vprice-10_2022-07-18/Naomi_ay.outcomes.costs.csv"))
ay.outcomes.costs_vacc3 <- read.csv(here::here("Econ/Model_outcomes_output/Final/Naomi", 
                                                 "2022-07-15/Naomi_ay.outcomes.costs.csv"))
list <- list(ay.outcomes.costs_vacc1.5, ay.outcomes.costs_vacc3, ay.outcomes.costs_vacc6, ay.outcomes.costs_vacc10)
vprice <- c(1.5, 3, 6, 10)

wtp <- c(19.13, #1% GDP
         100, #WHO best buy
         491, #36% 2015 GDP (Ochalek 2018)
         497.29, # 26% GDP - median WTP (Woods 2016)
         542, #39% GDP (Ochalek 2018)
         623, #45% GDP (Ochalek 2018)
         647, #47% GDP (Ochalek 2018)
         975.45, #51% GDP
         1912.65,#1X GDP
         5737.95 #3x GDP
)
names(wtp) <- paste0("WTP", 1:length(wtp))
source(here::here("Econ/Scripts", "my_icer_nmb_fns.R"))
ceac.df <- data.frame()

for (i in 1:length(list)){
  dftouse<- list[[i]] %>% 
    select(ay_Scenario3, ay_Sample, ay_disc.incremental.total.costs, ay_disc_dalys.03.averted)
  
  names(dftouse)
  names(dftouse) <- c("Scenario","Sample", "inc.costs", "dalys.averted")
  
  
  wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(dftouse)), nrow = nrow(dftouse)))
  colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))
  
  
  inmb.disc.both <- f(dftouse, wtp.df, exclude = "No vaccination")
  val3 <- g(inmb.disc.both$inmb)
  
  val3 <- val3 %>% mutate(Vacc_price = vprice[i])
  
  ceac.df <- rbind(ceac.df, val3)
  
}

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)

filename <- "Naomi"
tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename),
                "sa-vprice-ceac.tiff"),
     height = 2000, width = 3000, res = 300)
ceac <- ceac.df %>%  
  select(Scenario, Vacc_price, contains("inmb_")) %>% 
  pivot_longer(-c(Scenario, Vacc_price), names_to = "inmb_name", values_to = "prop") %>% 
  filter(!is.na(prop)) %>%
  rowwise() %>% 
  mutate(WTP = str_split(inmb_name, "_")[[1]][2]) %>% 
  full_join(wtp.named) %>% 
  ggplot(aes(x = wtp, y = prop, col = factor(Scenario)))+
  geom_point()+
  geom_line(lwd = 1)+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    #panel.border = element_blank(),
    panel.background = element_blank())+
  coord_cartesian(xlim = c(0, 6500)) +
  scale_x_continuous(breaks=c(100, 623, 1913, 5738))+
  geom_vline(xintercept = c(100, 623, 1913, 5738),
             lty = c("dashed"))+
  ylim(0,1) +
  facet_wrap(Vacc_price~., nrow = 2)+
  xlab("Willingness to pay per DALY averted (USD)")+
  scale_color_manual(values = c("#d73027",
                                #"#fc8d59", 
                                # "orange1", 
                                # "#91cf60", 
                                #"#1a9850"
                                #"#92c5de",
                                #"#4393c3"
                                #"#3288bd"
                                "purple"
  ))+
  ylab("Probability most cost-effective")+
  labs(col = "Vaccine")

print(ceac)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename),
                "sa-vprice-ceac.jpeg"),
     height = 600, width = 1050, units = "px")

print(ceac)
dev.off()
