#Updated 5/7/2022
library(here)

#Load data, modify----
#Run only if running this script directly without running 01_Public-health-outcomes_mine_short.T
#source(here::here("Econ/Scripts", "00_1_read.all.samples.R"))
source(here::here("Econ/Scripts", "mydf_functions.R"))

#Fig. 1b----
#Boxplot of incremental costs - both types

ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample,
         ay_incremental.total.costs, ay_disc.incremental.total.costs) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "value") %>% #View()
  filter(!ay_Scenario3 == "No vaccination" & str_detect(Measure, "costs")) %>% #View()
  mutate(Measure2 = case_when(
    Measure == "ay_incremental.total.costs" ~ "Undiscounted costs",
    # Measure == "icer.disc.total.per.daly.03" ~ "Discounted costs and DALYs",
    # Measure == "icer.total.per.daly.00" ~ "Undiscounted costs or DALYs",
    TRUE ~ "Discounted costs"
  )) %>% #select(Measure, Measure2) %>% unique()
  ggplot(aes(x = Measure2, y = value/1e6, fill = ay_Scenario3))+
  geom_boxplot()+
  theme_bw()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  theme(#axis.line = element_line(colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  scale_y_continuous(breaks=c(-500, -250, 0, 250, 500))+
  xlab("") + ylab("Incremental total costs (2010 - 2019) (millions USD)")+ labs(fill = "Vaccine")+
  #coord_cartesian(ylim = c(-1000, 5800)) +
  # scale_y_continuous(breaks=c(0, 100, 491, 975, 1913, 5738, 10000, 
  #                             20000))+
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
  guides(fill = guide_legend(nrow = 2))#+
#guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
#ggtitle("Incremental total costs - discounted and undiscounted")

ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                  "incremental.costs_boxplot.jpeg"),
       height = 2200, width = 2550, units = "px", limitsize = FALSE)

#Fig. 1c----
#Boxplot of dalys averted - both types

ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample,
         ay_dalys.00.averted, ay_disc_dalys.03.averted) %>% 
  pivot_longer(-c(ay_Scenario3, ay_Sample), names_to = "Measure", values_to = "value") %>% #View()
  filter(!ay_Scenario3 == "No vaccination" & str_detect(Measure, "averted")) %>% #View()
  mutate(Measure2 = case_when(
    Measure == "ay_dalys.00.averted" ~ "Undiscounted DALYs",
    # Measure == "icer.disc.total.per.daly.03" ~ "Discounted costs and DALYs",
    # Measure == "icer.total.per.daly.00" ~ "Undiscounted costs or DALYs",
    TRUE ~ "Discounted DALYs"
  )) %>% #select(Measure, Measure2) %>% unique()
  ggplot(aes(x = Measure2, y = value/1e6, fill = ay_Scenario3))+
  geom_boxplot()+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_blank())+
  #scale_y_continuous(breaks=c(-500, -250, 0, 250, 500))+
  xlab("") + ylab("DALYs averted (2010 - 2019) (millions)")+ labs(fill = "Vaccine")+
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
  guides(fill = guide_legend(nrow = 2))+
  coord_cartesian(ylim = c(0, 1)) +
  # scale_y_continuous(breaks=c(0, 100, 491, 975, 1913, 5738, 10000, 
  #                             20000))+
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept=0, lty = "solid")

ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                  "daly.averted_boxplot.jpeg"),
       height = 2200, width = 2550, units = "px", limitsize = FALSE)

#Fig. 2 ----
#Boxplot of icer per daly - discounted costs and dalys
icer_bp <- ay.outcomes.costs %>% 
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
  )) %>% #select(Measure, Measure2) %>% unique()
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
  coord_cartesian(ylim = c(-1000, 6000)) +
  scale_y_continuous(breaks=c(0, 100, 623, 1913, 5738))+
  geom_hline(yintercept = c(0, 100, 623, 1913, 5738), 
             lty = c("solid", "dotted", "dashed", "dotdash", "twodash"))+
  geom_vline(xintercept=0, lty = "solid")+
  guides(fill = guide_legend(nrow = 2))

print(icer_bp)

ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                  "disc.costs_icers_boxplot.jpeg"),
       height = 3000, width = 2550, units = "px", limitsize = FALSE)

#Combining plots----
#two-way plot and icer boxplot ----
gA <- ggplotGrob(a + ggtitle("A"))
gB <- ggplotGrob(icer_bp + ggtitle("B"))
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "two-way-plot_icer_bp_combined.tiff"),
     height = 2000, width = 4000, res = 300)

grid.arrange(gA, gB, nrow=1)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "two-way-plot_icer_bp_combined.jpeg"),
     height = 500, width = 1000, units = "px")
grid.arrange(gA, gB, nrow=1)

dev.off()

#Fig. 4 ----
#Density plot of vaccine costs ---
ay.outcomes.costs %>% 
  filter(!ay_Scenario3 == "No vaccination") %>% 
  select(ay_Scenario3, contains("vaccine")) %>%
  pivot_longer(-c(ay_Scenario3), names_to = "param", values_to = "val") %>% #View()
  #filter(param == "ay_vaccine.doses.price.all.ages") %>% 
  ggplot(aes(val, fill = factor(param)))+
  geom_density()+
  facet_grid(ay_Scenario3~param, scales = "free")+
  theme(legend.position = "bottom")
#Strange!!!!!


# INMB table ----
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

median.inmb <- inmb.disc.both$median.inmb

names(median.inmb) <- c("Vaccine", paste0("Threshold = $", round(wtp)))
median.inmb.short <- median.inmb %>% 
  select(contains(c("Vaccine", "100", "623", "1913", "5738")))

names(median.inmb.short) <- c("Vaccine", 
                              "WHO best buy ($100)",
                              "45% per capita GDP ($623)",
                              "1x per capita GDP ($1913)",
                              "3x per capita GDP ($5738)")

towrite2 <- df.wordtable.loop(list(median.inmb, median.inmb.short))

print(towrite2, target = here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                                    "inmb_updated2.docx")
)

#INMB boxplot----
df <- inmb.disc.both$inmb %>% 
  filter(!Scenario == "No vaccination") %>% 
  select(-c(Sample, inc.costs, dalys.averted, WTP1:WTP10)) 

names(df) <- c("Scenario", round(wtp))

df <- df %>% 
  pivot_longer(-Scenario, names_to = "wtp.name", values_to = "inmb")#View()

df$wtp.name <- as.numeric(df$wtp.name)

df3 <- df %>% 
  filter(wtp.name %in% c(100, 623, 1913, 5738))

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "inmb_select.tiff"),
     height = 2000, width = 3000, res = 300)

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

dev.off()

print(inmb_select)

ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                  "inmb_select.jpeg"),
       height = 2200, width = 2750, units = "px", limitsize = FALSE)

#Combining plots----
#two-way plot, icer boxplot and INMB boxplot ----
gA <- ggplotGrob(a + ggtitle("A"))
gB <- ggplotGrob(icer_bp + ggtitle("B"))
gC <- ggplotGrob(inmb_select + ggtitle("C"))
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
#gC$widths[2:5] <- as.list(maxWidth)

two.together <- grid.arrange(gA, gB, nrow=1)
grid.arrange(two.together, gC, nrow = 2)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "two-way-plot_icer_bp_inmb_select_combined.tiff"),
     height = 4000, width = 4000, res = 300)

grid.arrange(two.together, gC, nrow = 2)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "two-way-plot_icer_bp_inmb_select_combined.jpeg"),
     height = 1000, width = 1000, units = "px")
grid.arrange(two.together, gC, nrow = 2)

dev.off()


#The code below is only to check that the inmb values generated in output above 
#are correct 

# output2 <- all.samples %>% 
#   select(Sample, Scenario, Year, incremental.total.costs, dalys.00.averted) %>% #View()
#   cbind(wtp.df) %>% 
#   mutate(
#     inmb1 = (WTP1*dalys.00.averted) - incremental.total.costs,
#     inmb2 = (WTP2*dalys.00.averted) - incremental.total.costs,
#     inmb3 = (WTP3*dalys.00.averted) - incremental.total.costs,
#     inmb4 = (WTP4*dalys.00.averted) - incremental.total.costs,
#     inmb5 = (WTP5*dalys.00.averted) - incremental.total.costs,
#     inmb6 = (WTP6*dalys.00.averted) - incremental.total.costs,
#     inmb7 = (WTP7*dalys.00.averted) - incremental.total.costs,
#     inmb8 = (WTP8*dalys.00.averted) - incremental.total.costs,
#     inmb9 = (WTP9*dalys.00.averted) - incremental.total.costs,
#     inmb10 = (WTP10*dalys.00.averted) - incremental.total.costs
#   )
#   

# CEAC ----

val3 <- g(inmb.disc.both$inmb)

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "ceac_select.tiff"),
     height = 2000, width = 3000, res = 300)

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

print(ceac)
ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                  "ceac_select.jpeg"),
       height = 1200, width = 1950, units = "px", limitsize = FALSE)


# #Combining plots
# #inmb and ceac ----
# gC <- ggplotGrob(inmb_select + ggtitle("A"))
# gD <- ggplotGrob(ceac + ggtitle("B"))
# maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
# gA$widths[2:5] <- as.list(maxWidth)
# gB$widths[2:5] <- as.list(maxWidth)
# 
# tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                 "inmb_ceac_combined.tiff"),
#      height = 3200, width = 2800, res = 300)
# 
# grid.arrange(gC, gD, ncol=1)
# dev.off()
# 
# jpeg(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                 "inmb_ceac_combined.jpeg"),
#      height = 700, width = 650, units = "px")
# grid.arrange(gC, gD, ncol=1)
# 
# dev.off()


#CEACs for each individual scenario----
#CU_V
dftouse.cuv<- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, ay_disc.incremental.total.costs, ay_disc_dalys.03.averted) %>% 
  filter(ay_Scenario3 %in% c("No vaccination", "Current seasonal"))

names(dftouse.cuv)
names(dftouse.cuv) <- c("Scenario","Sample", "inc.costs", "dalys.averted")


wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(dftouse.cuv)), nrow = nrow(dftouse.cuv)))
colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))

inmb.disc.both <- f(dftouse.cuv, wtp.df, exclude = "No vaccination")

val3 <- g(inmb.disc.both$inmb)

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "ceac_cuv.tiff"),
     height = 2000, width = 3000, res = 300)

ceac.cuv <- val3 %>%  
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
                                #"#fc8d59", 
                                "orange1"#, 
                                #"#91cf60", 
                                #"#1a9850"
                                #"#92c5de",
                                #"#4393c3"
                                #"#3288bd"
  ))+
  ylab("Probability most cost-effective")+
  labs(col = "Vaccine")

print(ceac.cuv)
dev.off()

# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                   "ceac_disc_nov.cuv.jpeg"),
#        height = 1200, width = 1950, units = "px", limitsize = FALSE)


#IE_V
dftouse.IEV<- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, ay_disc.incremental.total.costs, ay_disc_dalys.03.averted) %>% 
  filter(ay_Scenario3 %in% c("No vaccination", "Improved (efficacy)"))

names(dftouse.IEV)
names(dftouse.IEV) <- c("Scenario","Sample", "inc.costs", "dalys.averted")


wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(dftouse.IEV)), nrow = nrow(dftouse.IEV)))
colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))

inmb.disc.both <- f(dftouse.IEV, wtp.df, exclude = "No vaccination")

val3 <- g(inmb.disc.both$inmb)

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "ceac_iev.tiff"),
     height = 2000, width = 3000, res = 300)

ceac.iev <- val3 %>%  
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
                                #"#fc8d59", 
                                #"orange1", 
                                #"#91cf60", 
                                #"#1a9850"
                                #"#92c5de"#,
                                #"#4393c3"
                                "#3288bd"
  ))+
  ylab("Probability most cost-effective")+
  labs(col = "Vaccine")

print(ceac.iev)
dev.off()

# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                   "ceac_disc_nov.iev.jpeg"),
#        height = 1200, width = 1950, units = "px", limitsize = FALSE)


#IB_V
dftouse.IBV<- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, ay_disc.incremental.total.costs, ay_disc_dalys.03.averted) %>% 
  filter(ay_Scenario3 %in% c("No vaccination", "Improved (breadth)"))

names(dftouse.IBV)
names(dftouse.IBV) <- c("Scenario","Sample", "inc.costs", "dalys.averted")


wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(dftouse.IBV)), nrow = nrow(dftouse.IBV)))
colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))

inmb.disc.both <- f(dftouse.IBV, wtp.df, exclude = "No vaccination")

val3 <- g(inmb.disc.both$inmb)

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "ceac_ibv.tiff"),
     height = 2000, width = 3000, res = 300)

ceac.ibv <- val3 %>%  
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
                                #"#fc8d59", 
                                #"orange1", 
                                #"#91cf60"#, 
                                #"#1a9850"
                                "#92c5de"#,
                                #"#4393c3"
                                #"#3288bd"
  ))+
  ylab("Probability most cost-effective")+
  labs(col = "Vaccine")

print(ceac.ibv)
dev.off()

#IM_V
dftouse.IMV<- ay.outcomes.costs %>% 
  select(ay_Scenario3, ay_Sample, ay_disc.incremental.total.costs, ay_disc_dalys.03.averted) %>% 
  filter(ay_Scenario3 %in% c("No vaccination", "Improved (minimal)"))

names(dftouse.IMV)
names(dftouse.IMV) <- c("Scenario","Sample", "inc.costs", "dalys.averted")


wtp.df <- as.data.frame(matrix(rep(wtp, each = nrow(dftouse.IMV)), nrow = nrow(dftouse.IMV)))
colnames(wtp.df) <- paste0("WTP", seq(1:length(wtp)))

inmb.disc.both <- f(dftouse.IMV, wtp.df, exclude = "No vaccination")

val3 <- g(inmb.disc.both$inmb)

wtp.named <- as.data.frame(wtp)
wtp.named$WTP <- names(wtp)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "ceac_imv.tiff"),
     height = 2000, width = 3000, res = 300)

ceac.imv <- val3 %>%  
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
                                #"orange1", 
                                "#91cf60"#, 
                                #"#1a9850"
                                #"#92c5de"#,
                                #"#4393c3"
                                #"#3288bd"
  ))+
  ylab("Probability most cost-effective")+
  labs(col = "Vaccine")

print(ceac.imv)
dev.off()

# ggsave(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
#                   "ceac_disc_nov.IBV.jpeg"),
#        height = 1200, width = 1950, units = "px", limitsize = FALSE)


#combining individual ceacs----
g1 <- ggplotGrob(ceac.cuv + ggtitle("A") + xlab(""))
g2 <- ggplotGrob(ceac.imv + ggtitle("B") + xlab("") + ylab(""))
g3 <- ggplotGrob(ceac.iev + ggtitle("C") + xlab(""))
g4 <- ggplotGrob(ceac.ibv + ggtitle("D") + ylab(""))
g5 <- ggplotGrob(ceac + ggtitle("E"))

maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5])
g1$widths[2:5] <- as.list(maxWidth)
g2$widths[2:5] <- as.list(maxWidth)
g3$widths[2:5] <- as.list(maxWidth)
g4$widths[2:5] <- as.list(maxWidth)
g5$widths[2:5] <- as.list(maxWidth)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "all_ceacs_combined.tiff"),
     height = 3000, width = 3800, res = 300)

grid.arrange(g1, g2, g3, g4, g5, nrow = 3, ncol=2)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/Final/", filename, "/", create.date),
                "all_ceacs_combined.jpeg"),
     height = 1000, width = 1000, units = "px")
grid.arrange(g1, g2, g3, g4, g5, nrow = 3, ncol=2)

dev.off()


