# Plotting results of our model.
rm(list = ls())

library(MASS)
library(visreg)
library(ggplot2)
library(patchwork)
library(performance)
library(see)
library(sf)
library(tmap)
library(nlme)
library(mgcv)
library(terra)


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

# How to generate predictions at the original sample sites

sdat2$gam1_pred = predict(m1_gam, type = "response")

# retrieve the standard error as well
sdat2_se = predict(m1_gam, type = "response", se=TRUE)

# append to sdat2
sdat2$gam1_pred_se = sdat2_se[[2]]

tland + 
  tm_shape(sdat2) +
  tm_symbols(col = "gam1_pred", size = 0.25)

tland + 
  tm_shape(sdat2) +
  tm_symbols(col = "gam1_pred_se", size = 0.25) 

# Generating predictions across the whole map (using rasters)

# Convert land and the crs to a terra object

land_terra = vect(land)

kia_crs_terra = crs(land_terra)

# Create a blank raster with the same extent and crs. If you want a smoother
# raster you need to go with smaller sizes for the resolution.
# I am using 300x300m instead of 500x500m only to see the difference

rlogponds = rast(extent = ext(land_terra), crs = crs(land_terra),
                  res  = 300)

# assign grids to logponds a value of 1
xy = st_coordinates(logponds)
icell = cellFromXY(rlogponds, xy)
rlogponds[icell] = 1

# and plot
tland + 
  tm_shape(rlogponds) + 
  tm_raster(palette = "Dark2")

# Now with the 500x500m raster
rlogponds2 = rast(extent = ext(land_terra), crs = crs(land_terra),
                 res  = 500)

xy = st_coordinates(logponds)
icell = cellFromXY(rlogponds2, xy)
rlogponds2[icell] = 1

tland + 
  tm_shape(rlogponds2) + 
  tm_raster(palette = "Dark2")

# The convenience of this is that we can calculate distance from every cell 
# with a value of 1 to every other cell

rdist = distance(rlogponds2)

# Convert to the same scale as our covariate (log10 kilometres) and plot

rdist_log10 = rast(rdist)
rdist_log10[] = log10(rdist[]/1000)
tm_shape(rdist_log10) + 
  tm_raster(palette = "-YlOrBr", n = 7) + 
  tland 

# Without transformation, the gradient does not concentrate as well around the
# logponds
tm_shape(rdist) + 
  tm_raster(palette = "-YlOrBr", n = 7) + 
  tland 

# Predicting to a raster grid

# 1) set up a dataframe with the distance to logging ponds* from the raster and
# cell numbers. Cells are numbered 1 to the total number of cells starting at
# the top leftmost cell
# * (if we use the Gaussian Process splines we also need xy coordinates) 

icell = 1:ncell(rdist_log10)
pred = data.frame(log10_dist_logging = rdist_log10[icell][,1],
                   cells = icell, 
                   x = xFromCell(rdist_log10, icell),
                   y = yFromCell(rdist_log10, icell), 
                   flow = "Mild") 
library(dplyr)
pred = pred %>% filter(log10_dist_logging !='-Inf')

# Chosen the response type so that prediction of units is of topa counts, not
# log topa counts (this is because of the log link in the negative binomial
# model that corresponds to m1_gam)

pred$topa_pred = predict(m1_gam, 
                         newdata = pred, 
                         type = "response")

# Assign the predictions to an empty copy of the rdist_log10 raster
rpred = rast(rdist_log10)

# specify rpred[pred$cells] so that values are only added to the cells that
# were assigned a predicted value
rpred[pred$cells] = matrix(pred$topa_pred, ncol = 1)

tm_shape(rpred) + 
  tm_raster(palette = "Blues",
            title= "Predicted abundance of Topa parrot fish", 
            alpha = 0.7, n=10) + 
  tm_shape(land) +
  tm_fill(col = "black") +
  tm_shape(logponds) + 
  tm_symbols(col = "yellow", size = 0.3) + 
  tm_layout(bg.color = "grey20", 
            legend.position = c("LEFT", "bottom"),
            legend.text.color = "white", 
            legend.title.color = "white")

# Can't include coral cover and distance to logging 
# because we don't have maps of coral cover. Furthermore the two are confounded
# Confounding won't affect predictions, but it does impact the interpretation
# of effects of each covariate.

# Chris Brown offers the following example:

# Abundance, flow and CB cover
mCB = glm.nb(pres.topa ~  flow + Sum_CB_cover, 
              data = sdat2)

# Abundance, flow, and log10 distance from logging ponds
mdist = glm.nb(pres.topa ~  flow +log10_dist_logging, 
                data = sdat2)

# Abundance, flow, CB cover, and log10 distance from logging ponds
mdist_CB = glm.nb(pres.topa ~  flow + Sum_CB_cover + log10_dist_logging, 
                   data = sdat2)

# With just CB cover
visreg(mCB, xvar = "Sum_CB_cover")


# With just log10 distance from logging ponds
visreg(mdist, xvar = "log10_dist_logging")


# with CB cover and log10 distance from logging ponds 
visreg(mdist_CB, xvar = "CB_cover")

# It is evidence CB cover and log10 distance are confounded as the effect
# disappears

# What does the AIC look like?

AIC(mCB, mdist, mdist_CB)
##          df      AIC
## mCB       4 207.0706
## mdist     4 184.8777
## mdist_CB  5 186.8581

# the lowest AIC is indeed associated with the distance only model

# Bonus: making rasters from polygons
rland = rast(extent = ext(land_terra), crs = crs(land_terra),
              res  = 200)
rland = rasterize(land_terra, rland)
plot(rland)


