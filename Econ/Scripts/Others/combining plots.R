
#Combining plots
#two-way plot and icer boxplot ----
gA <- ggplotGrob(a + ggtitle("A"))
gB <- ggplotGrob(icer_bp + ggtitle("B"))
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
                "two-way-plot_icer_bp_combined.tiff"),
     height = 2000, width = 4000, res = 300)

grid.arrange(gA, gB, nrow=1)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
                "two-way-plot_icer_bp_combined.jpeg"),
     height = 500, width = 1000, units = "px")
grid.arrange(gA, gB, nrow=1)

dev.off()

#inmb and ceac ----
gC <- ggplotGrob(inmb_select + ggtitle("A"))
gD <- ggplotGrob(ceac + ggtitle("B"))
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
                "inmb_ceac_combined.tiff"),
     height = 3200, width = 2800, res = 300)

grid.arrange(gC, gD, ncol=1)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
                "inmb_ceac_combined.jpeg"),
     height = 700, width = 650, units = "px")
grid.arrange(gC, gD, ncol=1)

dev.off()

#individual ceacs----
g1 <- ggplotGrob(ceac.cuv + ggtitle("A") + xlab(""))
g2 <- ggplotGrob(ceac.ibv + ggtitle("B") + xlab("") + ylab(""))
g3 <- ggplotGrob(ceac.iev + ggtitle("C"))
g4 <- ggplotGrob(ceac + ggtitle("D") + ylab(""))

maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5])
g1$widths[2:5] <- as.list(maxWidth)
g2$widths[2:5] <- as.list(maxWidth)
g3$widths[2:5] <- as.list(maxWidth)
g4$widths[2:5] <- as.list(maxWidth)

tiff(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
                "all_ceacs_combined.tiff"),
     height = 2800, width = 3800, res = 300)

grid.arrange(g1, g2, g3, g4, nrow = 2, ncol=2)
dev.off()

jpeg(here::here(paste0("Econ/Model_outcomes_output", "/", filename, "/", create.date),
                "all_ceacs_combined.jpeg"),
     height = 700, width = 1000, units = "px")
grid.arrange(g1, g2, g3, g4, nrow = 2, ncol=2)

dev.off()
