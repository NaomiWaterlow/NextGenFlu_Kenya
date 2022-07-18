#to conduct SAs by vaccine price----

library(here)
source(here::here("Econ/Scripts", "00_load_data.R"))
read.date <- lubridate::today()

vaccine.doses <- vaccine.doses %>% 
  mutate(Vaccines.millions = Total.Vaccines/10^6) #%>% 
#filter(!Scenario == 1)


#Vacc_price $3----
#vacc_price <- 6 #1.5, 6, 10
filename <- "Naomi"
date <- as.Date("2022-05-18")#Sys.Date()-1

all.samples3 <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", date),
                                          paste0(filename, "_all_samples_all_years_scenarios.csv")
))

all.samples3 <- all.samples3 %>% 
  mutate(Vacc_price = 3,
         durn_illness = 4)


#Vacc_price $1.5----

date <- as.Date("2022-04-29")#Sys.Date()-1

all.samples1.5 <- read.csv(file = here::here(paste0("Econ/Sensitivity_analyses/Vacc_price_1.5", "/", date),
                                             paste0(filename, "_all_samples_all_years_scenarios.csv")
))

all.samples1.5 <- all.samples1.5 %>% 
  mutate(Vacc_price = 1.5,
         durn_illness = 4)

#Vacc_price $6----
all.samples6 <- read.csv(file = here::here(paste0("Econ/Sensitivity_analyses/Vacc_price_6", "/", date),
                                             paste0(filename, "_all_samples_all_years_scenarios.csv")
))

all.samples6 <- all.samples6 %>% 
  mutate(Vacc_price = 6,
         durn_illness = 4)

#Vacc_price $10----
all.samples10 <- read.csv(file = here::here(paste0("Econ/Sensitivity_analyses/Vacc_price_10", "/", date),
                                             paste0(filename, "_all_samples_all_years_scenarios.csv")
))

all.samples10 <- all.samples10 %>% 
  mutate(Vacc_price = 10,
         durn_illness = 4)

#L = 7 days----
date <- as.Date("2022-05-09")#Sys.Date()-1

all.samplesL7 <- read.csv(file = here::here(paste0("Econ/Sensitivity_analyses/L7", "/", date),
                                            paste0(filename, "_all_samples_all_years_scenarios.csv")
))

all.samplesL7 <- all.samplesL7 %>% 
  mutate(Vacc_price = 3,
         durn_illness = 7)

#combine and format data----
all.data <- rbind(all.samples1.5, all.samples3, all.samples6, all.samples10,
                  all.samplesL7)


all.data <- all.data %>% 
  mutate(Scenario2 = case_when(
    Scenario == 1 ~ "NO_V",
    Scenario == 4 ~ "CU_V",
    Scenario == 51~ "IB_V",
    Scenario == 53~ "IE_V",
    TRUE ~ "U_V"
  ))

all.data$Scenario2 <- factor(all.data$Scenario2, 
                                levels = c("NO_V", "CU_V", "IE_V", "IB_V", "U_V"))
all.data$durn_illness <- factor(all.data$durn_illness,
                                levels = c(4, 7))


all.data <- all.data %>% 
  left_join(vaccine.doses, by = c("Year", "Scenario")) %>% 
  # select(Scenario, Year, direct.medical.costs, Total.Vaccines) %>% 
  # group_by(Scenario, Year) %>% 
  # slice(1) %>% View()
  mutate(direct.medical.costs.minus.vaccine = 
           direct.medical.costs - (inputs[47, "Mean"]*Total.Vaccines))# %>% 
#select(Scenario, Year, direct.medical.costs, direct.medical.costs.minus.vaccine) %>% View()

#$3
library(hesim)

vacc_price <- unique(all.data$Vacc_price)
durn_illness <- unique(all.data$durn_illness)

df.filter <- expand.grid(v_price = vacc_price,
                d_illness = durn_illness) %>% 
  filter(v_price %in% vacc_price & d_illness == 4 |
           v_price == 3 & d_illness == 7)

for (i in 1:nrow(df.filter)) {

v_price <- df.filter$v_price[i]
d_illness <- df.filter$d_illness[i]
  
my.ce.dis.costs.dalys <- all.data %>% 
  filter(Vacc_price == v_price &
           durn_illness == d_illness) %>%# dim()
  arrange(Scenario) %>% #select(Scenario, Year) %>% View()
  select(Scenario2, #strategy
         disc.total.costs, #cost
         dalys.03.averted
         #total.DALY00.all.ages#qalys
  ) %>% #dim()
  mutate(sample = rep(1:10000, 5),#sample
         grp = "group.all") #grp

names(my.ce.dis.costs.dalys)
names(my.ce.dis.costs.dalys) <- c("strategy", "dis.cost", "dalys.averted", "sample", "grp")


ktop <- 6000
cea_out_dis_costs_dalys <-  cea(my.ce.dis.costs.dalys, k = seq(0, ktop, 100), sample = "sample", strategy = "strategy",
                                grp = "grp", e = "dalys.averted", c = "dis.cost")


cea_pw_out_dis_costs_dalys <-  cea_pw(my.ce.dis.costs.dalys,  k = seq(0, ktop, 100), comparator = "NO_V",
                                      sample = "sample", strategy = "strategy", grp = "grp",
                                      e = "dalys.averted", c = "dis.cost")


icer(cea_pw_out_dis_costs_dalys, k = ktop) %>%
  format() #-> icer.table


##ceplane_plot
p1 <- plot_ceplane(cea_pw_out_dis_costs_dalys, k = ktop) + 
  xlab("Incremental DALYS averted") +
  geom_jitter(size = 0.5, alpha = 0.2) + 
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) + 
  scale_x_continuous(limits = c(-5000, 60000), breaks = pretty(seq(-5000, 60000, length.out = 10))
  )


##mce_plot, warning = FALSE, message = FALSE
p2 <- plot_ceac(cea_out_dis_costs_dalys) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

##ceac_plot---
p3 <- plot_ceac(cea_pw_out_dis_costs_dalys) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

##ceaf_plot---
p4 <- plot_ceaf(cea_out_dis_costs_dalys)+ 
  geom_line(lwd= 0.8)+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

library(gridExtra)
library(ggpubr)

text <- paste0("Vaccine price = $", v_price, ", L = ", d_illness)
tgrob <- text_grob(text, face = "bold", color = "black")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2,
                 top = tgrob)

a<- arrangeGrob(p1, p2, p3, p4, nrow = 2, ncol = 2,
                 top = tgrob)
ggsave(here::here("Econ/Sensitivity_analyses", paste0("v.price_", v_price,
                                                      "_L_", d_illness, ".jpeg")), 
                  a, height = 2000, width = 3000, units = "px", limitsize = FALSE)

  
}

##all together on same plot ----
empty <- data.frame()
for (i in 1:nrow(df.filter)) {
  
  v_price <- df.filter$v_price[i]
  d_illness <- df.filter$d_illness[i]
  library(hesim)
  
  my.ce <- all.data %>% 
    filter(Vacc_price == v_price &
             durn_illness == d_illness) %>%# dim()
    arrange(Scenario) %>% #select(Scenario, Year) %>% View()
    select(Scenario2, #strategy
           disc.total.costs, #cost
           dalys.03.averted
           #total.DALY00.all.ages#qalys
    ) %>% #dim()
    mutate(sample = rep(1:10000, 5),#sample
           grp = "group.all") #grp
  
  names(my.ce)
  names(my.ce) <- c("strategy", "dis.cost", "dalys.averted", "sample", "grp")
  
  
  ktop <- 6000
  cea_out <-  cea(my.ce, k = seq(0, ktop, 100), sample = "sample", strategy = "strategy",
                                  grp = "grp", e = "dalys.averted", c = "dis.cost")
  
  towrite <- cea_out$mce %>% mutate(param = paste0(v_price, "_", d_illness))
  
  empty <- rbind(empty, towrite)
  
}
  
ceacs.all<- ggplot(empty)+
  geom_line(aes(k, prob, col = strategy, lty = param), lwd = 0.5)#+
#theme(legend.position = "bottom", legend.box = "vertical")#+
#guides(col=guide_legend(nrow=2,byrow=TRUE))

best <- empty %>% filter(best == 1)

ceafs.all<- ggplot(best)+
  geom_line(aes(k, prob, col = strategy, lty = param), lwd = 0.5)+
  ylim(0,1)  
  
all<- arrangeGrob(ceacs.all, ceafs.all, nrow = 2)
ggsave("all.vacc.durn.jpeg", all,
       height = 2200, width = 2500, units = "px", limitsize = FALSE)



#SA - discounting-----
#doing cea using package hesim

#no discouting of costs or dalys----
library(hesim)
my.ce <- all.samples %>% 
  #filter(Year == "2010") %>% 
  arrange(Scenario) %>% #select(Scenario, Year) %>% View()
  select(Scenario2, #strategy
         total.costs, #cost
         dalys.00.averted
         #total.DALY00.all.ages, #this does not work!
  ) %>% #dim()
  mutate(sample = rep(1:10000, 5),#sample
         grp = "group.all") #grp

names(my.ce)
names(my.ce) <- c("strategy", "cost", 
                  "dalys.averted", 
                  #"total.dalys", 
                  "sample", "grp")


ktop <- 6000
cea_out <-  cea(my.ce, 
                #k = 19.13, 
                k = seq(0, ktop, 100), 
                sample = "sample", strategy = "strategy",
                grp = "grp", e = "dalys.averted", c = "cost")


cea_pw_out <-  cea_pw(my.ce,  
                      #k = ktop,
                      k = seq(0, ktop, 100), 
                      comparator = "NO_V",
                      sample = "sample", strategy = "strategy", grp = "grp",
                      e = "dalys.averted", c = "cost")


icer(cea_pw_out, 
     k = ktop
     #k = 647
) %>%
  format()

##ceplane_plot
theme_set(theme_bw())
plot_ceplane(cea_pw_out, k = 19.13) + xlab("Incremental DALYS averted")


##mce_plot, warning = FALSE, message = FALSE
p2.undiscounted <- plot_ceac(cea_out) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Undiscounted costs and DALYs")
##ceac_plot---
plot_ceac(cea_pw_out) + geom_vline(xintercept = 491, lty = "dotted")

##ceaf_plot---
p4.undiscounted <- plot_ceaf(cea_out)+ 
  geom_line(lwd= 0.8)+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Undiscounted costs and DALYs")


#Since 4 is better than 1, exclude 1 and set 4 as the base scenario
all.samples %>% 
  filter(Scenario == 4) %>% 
  select(Sample, Year, 
         total.costs, disc.total.costs, 
         total.DALY00.all.ages, total.DALY03.all.ages) -> base4

names(base4) <- paste0("base4_", names(base4))  

all.samples %>% 
  filter(!Scenario == 1) %>% 
  left_join(base4, by = c("Sample" = "base4_Sample",
                          "Year" = "base4_Year"),
            keep = TRUE) %>% #select(Sample, base4_Sample, Year, base4_Year) %>% View()
  mutate(inc.total.costs.4 = total.costs - base4_total.costs,
         inc.disc.total.costs.4 = disc.total.costs - base4_disc.total.costs,
         daly00.averted.4 = base4_total.DALY00.all.ages - total.DALY00.all.ages,
         daly03.averted.4 = base4_total.DALY03.all.ages - total.DALY03.all.ages) -> df4


my.ce.4 <- df4 %>% 
  filter(!Scenario == 1) %>% 
  arrange(Scenario) %>% #select(Scenario, Year) %>% View()
  select(Scenario, #strategy
         total.costs, #cost
         daly00.averted.4
         #total.DALY00.all.ages, #this does not work!
  ) %>% #dim()
  mutate(sample = rep(1:10000, 4),#sample
         grp = "group.all") #grp

names(my.ce.4)
names(my.ce.4) <- c("strategy", "cost", 
                    "dalys.averted", 
                    #"total.dalys", 
                    "sample", "grp")


ktop <- 647
cea_out <-  cea(my.ce.4, 
                k = ktop, 
                #k = wtp, #seq(0, ktop, 100), 
                sample = "sample", strategy = "strategy",
                grp = "grp", e = "dalys.averted", c = "cost")


cea_pw_out <-  cea_pw(my.ce.4,  
                      k = ktop,
                      #k = wtp, #seq(0, ktop, 100), 
                      comparator = "4",
                      sample = "sample", strategy = "strategy", grp = "grp",
                      e = "dalys.averted", c = "cost")


icer(cea_pw_out, 
     k = ktop
     #k = 647
) %>%
  format()

##ceplane_plot
theme_set(theme_bw())
plot_ceplane(cea_pw_out, k = ktop) + xlab("Incremental DALYS averted")


##mce_plot, warning = FALSE, message = FALSE
plot_ceac(cea_out) + geom_vline(xintercept = 19.13, lty = "dotted")

##ceac_plot---
plot_ceac(cea_pw_out) + geom_vline(xintercept = 491, lty = "dotted")

##ceaf_plot---
plot_ceaf(cea_out) + geom_vline(xintercept = 491, lty = "dotted")




#costs discounted----
library(hesim)
my.ce.dis.costs <- all.samples %>% 
  #filter(Year == "2010") %>% 
  arrange(Scenario) %>% #select(Scenario, Year) %>% View()
  select(Scenario2, #strategy
         disc.total.costs, #cost
         dalys.00.averted
         #total.DALY00.all.ages#qalys
  ) %>% #dim()
  mutate(sample = rep(1:10000, 5),#sample
         grp = "group.all") #grp

names(my.ce.dis.costs)
names(my.ce.dis.costs) <- c("strategy", "disc.cost", "dalys.averted", "sample", "grp")


ktop <- 6000
cea_out_dis_costs <-  cea(my.ce.dis.costs, k = seq(0, ktop, 100), sample = "sample", strategy = "strategy",
                          grp = "grp", e = "dalys.averted", c = "disc.cost")


cea_pw_out_dis_costs <-  cea_pw(my.ce.dis.costs,  k = seq(0, ktop, 100), comparator = "1",
                                sample = "sample", strategy = "strategy", grp = "grp",
                                e = "dalys.averted", c = "disc.cost")


icer(cea_pw_out_dis_costs, k = 1000) %>%
  format()

##ceplane_plot
theme_set(theme_bw())
plot_ceplane(cea_pw_out_dis_costs, k = 500) + xlab("Incremental DALYS averted")

p2.dis.costs <- plot_ceac(cea_out_dis_costs) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Discounted costs")

##ceaf_plot---
p4.dis.costs <- plot_ceaf(cea_out_dis_costs)+ 
  geom_line(lwd= 0.8)+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Discounted costs")


#Since 4 is better than 1, exclude 1 and set 4 as the base scenario
all.samples %>% 
  filter(Scenario == 4) %>% 
  select(Sample, Year, 
         total.costs, disc.total.costs, 
         total.DALY00.all.ages, total.DALY03.all.ages) -> base4
##mce_plot, warning = FALSE, message = FALSE
plot_ceac(cea_out_dis_costs)

##ceac_plot---
plot_ceac(cea_pw_out_dis_costs)

##ceaf_plot---
plot_ceaf(cea_out_dis_costs)

#dalys discounted----
library(hesim)
my.ce.dis.dalys <- all.samples %>% 
  #filter(Year == "2010") %>% 
  arrange(Scenario) %>% #select(Scenario, Year) %>% View()
  select(Scenario2, #strategy
         total.costs, #cost
         dalys.03.averted
         #total.DALY00.all.ages#qalys
  ) %>% #dim()
  mutate(sample = rep(1:10000, 5),#sample
         grp = "group.all") #grp

names(my.ce.dis.dalys)
names(my.ce.dis.dalys) <- c("strategy", "cost", "dalys.averted", "sample", "grp")


ktop <- 6000
cea_out_dis_dalys <-  cea(my.ce.dis.dalys, k = seq(0, ktop, 100), sample = "sample", strategy = "strategy",
                          grp = "grp", e = "dalys.averted", c = "cost")


cea_pw_out_dis_dalys <-  cea_pw(my.ce.dis.dalys,  k = seq(0, ktop, 100), comparator = "NO_V",
                                sample = "sample", strategy = "strategy", grp = "grp",
                                e = "dalys.averted", c = "cost")


icer(cea_pw_out_dis_dalys, k = 1000) %>%
  format()

##ceplane_plot
plot_ceplane(cea_pw_out_dis_dalys, k = 500) + xlab("Incremental DALYS averted")

p2.dis.dalys <- plot_ceac(cea_out_dis_dalys) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Discounted DALYs")

##ceaf_plot---
p4.dis.dalys <- plot_ceaf(cea_out_dis_dalys)+ 
  geom_line(lwd= 0.8)+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Discounted DALYs")

##mce_plot, warning = FALSE, message = FALSE
plot_ceac(cea_out_dis_dalys)

##ceac_plot---
plot_ceac(cea_pw_out_dis_dalys)

##ceaf_plot---
plot_ceaf(cea_out_dis_dalys)

#costs and dalys discounted----
library(hesim)
my.ce.dis.costs.dalys <- all.samples %>% 
  #filter(Year == "2010") %>% 
  arrange(Scenario) %>% #select(Scenario, Year) %>% View()
  select(Scenario2, #strategy
         disc.total.costs, #cost
         dalys.03.averted
         #total.DALY00.all.ages#qalys
  ) %>% #dim()
  mutate(sample = rep(1:10000, 5),#sample
         grp = "group.all") #grp

names(my.ce.dis.costs.dalys)
names(my.ce.dis.costs.dalys) <- c("strategy", "dis.cost", "dalys.averted", "sample", "grp")


ktop <- 6000
cea_out_dis_costs_dalys <-  cea(my.ce.dis.costs.dalys, k = seq(0, ktop, 100), sample = "sample", strategy = "strategy",
                                grp = "grp", e = "dalys.averted", c = "dis.cost")


cea_pw_out_dis_costs_dalys <-  cea_pw(my.ce.dis.costs.dalys,  k = seq(0, ktop, 100), comparator = "NO_V",
                                      sample = "sample", strategy = "strategy", grp = "grp",
                                      e = "dalys.averted", c = "dis.cost")


icer(cea_pw_out_dis_costs_dalys, k = ktop) %>%
  format() #-> icer.table


##ceplane_plot
plot_ceplane(cea_pw_out_dis_costs_dalys, k = 647) + xlab("Incremental DALYS averted") +
  geom_jitter(size = 0.7) + 
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) + 
  scale_x_continuous(limits = c(-5000, 60000), breaks = pretty(seq(-5000, 60000, length.out = 10))
  )

p2.dis.costs.dalys <- plot_ceac(cea_out_dis_costs_dalys) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Discounted costs and DALYs")

##ceaf_plot---
p4.dis.costs.dalys <- plot_ceaf(cea_out_dis_costs_dalys)+ 
  geom_line(lwd= 0.8)+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())+
  ggtitle("Discounted costs and DALYs")

##mce_plot, warning = FALSE, message = FALSE
plot_ceac(cea_out_dis_costs_dalys) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

##ceac_plot---
plot_ceac(cea_pw_out_dis_costs_dalys) + 
  geom_line(lwd= 0.8) +
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())

##ceaf_plot---
plot_ceaf(cea_out_dis_costs_dalys)+ 
  geom_line(lwd= 0.8)+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())


a.ceacs <- arrangeGrob(p2.undiscounted,
                       p2.dis.costs, 
                       p2.dis.dalys,
                       p2.dis.costs.dalys, nrow = 2, ncol = 2)

a.ceafs <- arrangeGrob(p4.undiscounted,
                       p4.dis.costs, 
                       p4.dis.dalys,
                       p4.dis.costs.dalys, nrow = 2, ncol = 2)


ggsave(here::here("Econ/Sensitivity_analyses", "ceacs.jpeg"), 
       a.ceacs, height = 2000, width = 3000, units = "px", limitsize = FALSE)


ggsave(here::here("Econ/Sensitivity_analyses", "ceafs.jpeg"), 
       a.ceafs, height = 2000, width = 3000, units = "px", limitsize = FALSE)




#all in one plot ----

a1 <- cea_out$mce %>% mutate(lty = "Undiscounted costs or DALYs")
a2 <- cea_out_dis_costs$mce %>% mutate(lty = "Discounted costs")
a3 <- cea_out_dis_dalys$mce %>% mutate(lty = "Discounted DALYs")
a4 <- cea_out_dis_costs_dalys$mce %>% mutate(lty = "Discounted costs and DALYs")

ceacs.all<- ggplot(rbind(a1, a2, a3, a4))+
  geom_line(aes(k, prob, col = strategy, lty = lty), lwd = 0.5)#+
#theme(legend.position = "bottom", legend.box = "vertical")#+
#guides(col=guide_legend(nrow=2,byrow=TRUE))


a1.best <- a1 %>% filter(best == 1) 
a2.best <- a2 %>% filter(best == 1) 
a3.best <- a3 %>% filter(best == 1) 
a4.best <- a4 %>% filter(best == 1) 

ceafs.all<- ggplot(rbind(a1.best, a2.best, 
                         a3.best, a4.best))+
  geom_line(aes(k, prob, col = strategy, lty = lty), lwd = 0.5)+
  ylim(0,1) #+
#theme(legend.position = "bottom", legend.box = "vertical")#+

all<- arrangeGrob(ceacs.all, ceafs.all, nrow = 2)
ggsave("all.jpeg", all,
       height = 2000, width = 3000, units = "px", limitsize = FALSE)
