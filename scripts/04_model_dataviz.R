# Plotting results of our model.
rm(list = ls())

library(visreg)
library(ggplot2)
library(patchwork)
library(visreg)
library(performance)
library(see)
library(sf)
library(tmap)


load("outputs/Models.rda")

# Marginal effects
g1 = visreg(m1_gam, xvar = "log10_dist_logging",
             scale = "response", gg = TRUE) + 
  xlab("Distance to log ponds (log10)") + 
  ylab("Topa abundance")
g1


flow_pred = visreg(m1_gam, xvar = "flow",
                    scale = "response", gg = TRUE, plot = FALSE)

g2 = ggplot(flow_pred$fit) + 
  aes(x = flow, y = visregFit) + 
  geom_point() + 
  geom_linerange(aes(ymin = visregLwr, ymax = visregUpr)) + 
  xlab("Flow") + 
  ylab("Topa abundance")

gboth = g1 + g2 + 
  plot_layout(nrow = 1, widths = c(1, 0.5)) + 
  plot_annotation(tag_levels = "A")

gboth

ggsave("images/29_m1_gam_predictions.png", plot = gboth,
       width = 6, 
       height = 3)

