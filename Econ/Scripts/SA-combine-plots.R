#combining plots for SA in supplement
library(tidyverse)
library(gridExtra)
library(grid)
ay.outcomes.costs <- read.csv(file = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", read.date),
                                               paste0(filename, "_ay.outcomes.costs.csv")
))

ay.outcomes.costs$ay_Scenario3 <- factor(ay.outcomes.costs$ay_Scenario3, 
                                levels = c("No vaccination",
                                           "Current seasonal",
                                           "Improved (minimal)",
                                           "Improved (breadth)", 
                                           "Improved (efficacy)",
                                           "Universal"))


ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample,
         ay_disc.incremental.total.costs,
         ay_disc_cases.averted, ay_Vaccine.doses) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "value") %>% #View()
  group_by(ay_Scenario3, Measure) %>%  
  dplyr::summarise(median = round(median(value, na.rm = TRUE)/1e6,2),
                   #median = median(value, na.rm = TRUE),
                   lower_ci = round(quantile(value, 0.025, na.rm = TRUE)/1e6,2),
                   upper_ci = round(quantile(value, 0.975, na.rm = TRUE)/1e6,2)) %>%
  ungroup() %>% 
  filter(!ay_Scenario3 == "No vaccination") %>% 
  pivot_longer(-c(ay_Scenario3, Measure), names_to = "stat", values_to = "val") %>% 
  pivot_wider(names_from = c(Measure, stat), values_from = val, names_sep = "_") -> medians.to.plot2

#a - Total incremental costs by cases averted----
a<- medians.to.plot2 %>% ggplot(aes(y = ay_disc.incremental.total.costs_median, 
                                    x = ay_disc_cases.averted_median))+
  geom_point(aes(col = ay_Scenario3, shape = ay_Scenario3), size = 3)+
  geom_errorbar(aes(ymin=ay_disc.incremental.total.costs_lower_ci, 
                    ymax = ay_disc.incremental.total.costs_upper_ci,
                    col = ay_Scenario3), width=.1, lwd = 1)+
  geom_errorbar(aes(xmin=ay_disc_cases.averted_lower_ci, 
                    xmax = ay_disc_cases.averted_upper_ci,
                    col = ay_Scenario3), width=.7, lwd = 1)+
  xlab("Reduction in number of cases (millions)")+
  ylab("Discounted incremental total costs (2010 - 2019) \n(millions USD)")+
  labs(col = "Vaccine", shape = "Vaccine")+
  #guides(guide_legend(title="New Legend Title"))+
  #xlim(-5,15)+
  #ylim(-100,1000)+
  # geom_pointrange(aes(y = median, ymin = `lower 95% CI limit`,
  #                     ymax = `upper 95% CI limit`,
  #                     color = factor(Scenario)), size = 1)+
  #geom_hline(yintercept = 1710*3)+
  theme_bw()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values = c(#"#d73027",
    #"#fc8d59", 
    "orange1", 
    "#91cf60", 
    #"#1a9850"
    "#92c5de",
    #"#4393c3"
    "#3288bd",
    "purple"
  ))+
  theme(#axis.line = element_line(colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  guides(color = guide_legend(nrow = 2))
#facet_grid(.~Year, scales = "free_x")
print(a)

#b - Boxplot of icer per daly - discounted costs and dalys----
df.plot <- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample,
         icer.disc.total.per.daly.00,
         icer.disc.total.per.disc.daly.03) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "value") %>% #View()
  filter(!ay_Scenario3 == "No vaccination" & 
           str_detect(Measure, "icer.disc")) %>%
  mutate(Measure2 = case_when(
    Measure == "icer.disc.total.per.daly.00" ~ "Discounted costs, \nundiscounted DALYs",
    # Measure == "icer.disc.total.per.daly.03" ~ "Discounted costs and DALYs",
    # Measure == "icer.total.per.daly.00" ~ "Undiscounted costs or DALYs",
    TRUE ~ "Discounted costs \nand DALYs"
  )) #select(Measure, Measure2) %>% unique()

icer_bp <-  df.plot %>% 
  ggplot(aes(x = Measure2, y = value, fill = ay_Scenario3))+
  geom_boxplot()+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  scale_fill_manual(values = c(#"#d73027",
    #"#fc8d59", 
    "orange1", 
    "#91cf60", 
    #"#1a9850"
    "#92c5de",
    #"#4393c3"
    "#3288bd",
    "purple"
  ))+
  xlab("") + ylab("ICER per DALY averted (2010 - 2019)")+ labs(fill = "Vaccine")+
  coord_cartesian(ylim = quantile(df.plot$value, c(0.05, 0.95)))+
  scale_y_continuous(breaks=c(0, #100, 
                              #623, 
                              1913, 5738))+
  geom_hline(yintercept = c(0, #100, 
                            #623, 
                            1913, 5738), 
             lty = c("solid", #"dotted", 
                     #"dashed", 
                     "dotdash", "twodash"))+
  geom_vline(xintercept=0, lty = "solid")+
  guides(fill = guide_legend(nrow = 2))

print(icer_bp)

#c - INMB boxplot ----
# this is how to calculate INMB for two scenarios - base scenario A and new scenario B
# Measures - QALY.A, QALY.B, WTP, TOTAL.COST.A, TOTAL.COST.B

#ICER = (TOTAL.COST.B - TOTAL.COST.A)/(QALY.B - QALY.A)
#Absolute NMB A = (QALY.A * WTP) - TOTAL.COST.A
#Absolute NMB B = (QALY.B * WTP) - TOTAL.COST.B
#Incremental NMB = Absolute NMB B - Absolute NMB A 
#                = Incremental Qalys/Dalys * wTP - (Incremental costs)

#Therefore, a new scenario B is cost-effective if:
# a. ICER/QALY > WTP/QALY, &
# b. INMB > 0

# Kenya 2019 GDP pc - $1912.65
# WTP thresholds to use
# a. Best buy threshold - $100
# b. 1% GDP = 19.13
# c. 51% GDP = 975.45
# d. median% GDP (26%) = 497.23
# e. OCHALEK - 4 values - $542 	39%	$491 	36%	$623 	45%	$647 	47%
# f. 1X GDP = 1912.65
# g. 3X GDP = 5737.95

# inmb.calculator <- function(df, wtp, inc.costs, dalys.averted){
#   inmb <- matrix()
#   # inc.costs <- df$incremental.total.costs
#   # dalys.averted <- df$dalys.00.averted
#   
#   for (i in 1:length(wtp)){
#     df <- data.frame((dalys.averted*wtp[i])-inc.costs)
#     colnames(df) <- paste0("T", wtp[i])
#     inmb <- cbind(inmb, df)
#   }
#   print(inmb)
# }

#inmb.calc <- function(x) ((x*dalys.00.averted) - incremental.total.costs)
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


dftouse<- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, ay_disc.incremental.total.costs, ay_disc_dalys.03.averted)

names(dftouse)
names(dftouse) <- c("Scenario","Sample", "inc.costs", "dalys.averted")


wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(dftouse)), nrow = nrow(dftouse)))
colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))

source(here::here("Econ/Scripts", "my_icer_nmb_fns.R"))

inmb.disc.both <- f(dftouse, wtp.df, exclude = "No vaccination")

df <- inmb.disc.both$inmb %>% 
  filter(!Scenario == "No vaccination") %>% 
  select(-c(Sample, inc.costs, dalys.averted, WTP1:WTP10)) 

names(df) <- c("Scenario", round(wtp))

df <- df %>% 
  pivot_longer(-Scenario, names_to = "wtp.name", values_to = "inmb")#View()

df$wtp.name <- as.numeric(df$wtp.name)

df3 <- df %>% 
  filter(wtp.name %in% c(100, 623, 1913, 5738))

inmb_select<- ggplot(df3, aes(x = factor(wtp.name), y = inmb/1e6))+
  geom_boxplot(aes(fill = factor(Scenario)))+#, notch = T, notchwidth = 0.5) +
  coord_cartesian(ylim = quantile(df3$inmb, c(0.025, 0.975))/1e6)+
  geom_hline(yintercept = 0, lty = "solid")+
  theme_bw()+
  scale_x_discrete(labels= c(
    "WHO best buy \n($100)",
    "45% per capita GDP \n($623)",
    "1x per capita GDP \n($1913)",
    "3x per capita GDP \n($5738)"
  ))+
  xlab("Willingness to pay per DALY averted") + 
  ylab("Incremental net monetary benefits \n(millions USD)")+
  labs(fill = "Vaccine") +
  theme(#axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  scale_fill_manual(values = c(#"#d73027",
    #"#fc8d59", 
    "orange1", 
    "#91cf60", 
    #"#1a9850"
    "#92c5de",
    #"#4393c",
    "#3288bd",
    "purple"
    
  ))+
  guides(fill = guide_legend(nrow = 2))

print(inmb_select)

#d - CEAC - U_V
val3 <- g(inmb.disc.both$inmb)

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)


ceac <- val3 %>%  
  select(Scenario, contains("inmb_")) %>% 
  pivot_longer(-Scenario, names_to = "inmb_name", values_to = "prop") %>% 
  filter(!is.na(prop)) %>%
  rowwise() %>% 
  mutate(WTP = str_split(inmb_name, "_")[[1]][2]) %>% 
  full_join(wtp.named) %>% 
  ggplot(aes(x = wtp, y = prop, group = Scenario, col = factor(Scenario)))+
  geom_point()+
  geom_line(lwd = 1)+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    #panel.border = element_blank(),
    panel.background = element_blank())+
  coord_cartesian(xlim = c(0, 6500)) +
  scale_x_continuous(breaks=c(100, 623, 1913, 5738))+
  geom_vline(xintercept = c(100, 623, 1913, 5738),
             lty = c("dotted", "dashed", "dotdash", "twodash"))+
  ylim(0,1) +
  xlab("Willingness to pay per DALY averted (USD)")+
  scale_color_manual(values = c("#d73027",
                                "orange1", 
                                "#91cf60", 
                                #"#1a9850"
                                "#92c5de",
                                #"#4393c",
                                "#3288bd",
                                "purple"
  ))+
  ylab("Probability most cost-effective")+
  labs(col = "Vaccine")

print(ceac)


#Combining all -0----
gA <- ggplotGrob(a + ggtitle("A"))
gB <- ggplotGrob(icer_bp + ggtitle("B"))
gC <- ggplotGrob(inmb_select + ggtitle("C"))
gD <- ggplotGrob(ceac + ggtitle("D"))
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)
#gC$widths[2:5] <- as.list(maxWidth)


tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "four-way-plot-supplement.tiff"),
     height = 5000, width = 6000, res = 300)

grid.arrange(gA, gB, gC, gD, nrow = 2)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "four-way-plot-supplement.jpeg"),
     height = 900, width = 1250, units = "px")

grid.arrange(gA, gB, gC, gD, nrow = 2)
dev.off()
