# Using SDMs because machine learning models are not exactly interpretable,
# nor is the underlying process generating the data easy to tease apart, nor
# can they handle spatial autocorrelation.

# The following example is part of the reason for doing this tutorial. It models
# the effect of heat on fish populations. Particularly since the effect on
# corals is well established. One could imagine a similar approach to coastal
# bird populations would be tractable as temperature would also have an effect
# upstream on birds that predate on fish.

load("outputs/2021-09-20_cleaned-data.rda")
library(visreg)
library(performance)
library(see)
library(MASS)
library(sf)
library(tmap)
library(ggplot2)

# Let's start with an exploratory glm

# log transform distance to log ponds
sdat2$log10_dist_logging = log10(sdat2$dist_to_logging)

# What we are doing here is fitting a Gaussian glm to test whether the presence
# of Topa fish can be explained by the log distance from log ponds, we are also
# adding the 'flow' variable as differences in flow could explain
# differences in water quality

glm1_gaus = glm(pres.topa ~ log10_dist_logging + flow, data = sdat2)

# Generate diagnostic plots
diagnostic_glm1_gaus = check_model(glm1_gaus)
diagnostic_glm1_gaus

# check for outliers in the data
check_outliers(glm1_gaus)

# check whether residuals for our model are normally distributed (if applicable)
check_normality(glm1_gaus)

# Clearly from the diagnostic plots we can see that as expected for most
# ecological/biological data, normal distributions are not appropriate.

# A Poisson distribution would be more appropriate.
glm1_pois = glm(pres.topa ~ log10_dist_logging + flow, data = sdat2,
                 family = "poisson")

# Generate diagnostic plots
diagnostic_glm1_pois = check_model(glm1_pois)
diagnostic_glm1_pois

# check for outliers in the data
check_outliers(glm1_pois)
# 3 outliers detected

# How about the negative binomial for overdispersed counts?

glm1_nb = glm.nb(pres.topa ~ log10_dist_logging + flow, data = sdat2)

# Generate diagnostic plots
diagnostic_glm1_nb = check_model(glm1_nb)
diagnostic_glm1_nb

# check for outliers in the data
check_outliers(glm1_nb)
# No outliers detected

# The negative binomial is a better fit but we can compare all three models
# using the Akaike Information Criterion (AIC)

AIC(glm1_gaus, glm1_pois, glm1_nb)
# Indeed, the negative binomial has the lowest AIC of all 3 (184.8777)

summary(glm1_nb)

# Testing spatial autocorrelation (Only autocorrelation in the residuals matters)
# Endogenous AC would be caused by species dispersal dynamics for example,
# whereas exogenous AC would be caused by environmental variables not accounted
# for in the model

sdat2$resid_glm_pois = resid(glm1_pois)

tland = tm_shape(land) + 
  tm_fill()

tland + 
  tm_shape(sdat2) + 
  tm_symbols(col = "resid_glm_pois", size = 0.25)

# The residuals in the Poisson and negative binomial models are similarly 
# distributed but I am curious as to how AC looks for the NB model as well

sdat2$resid_glm_nb = resid(glm1_nb)

tland + 
  tm_shape(sdat2) + 
  tm_symbols(col = "resid_glm_nb", size = 0.25)

# The data clusters slightly differently in the NB model and personally I feel
# it describes slightly less variance than the Poisson residuals. Chris Brown
# provides a script to compute semivariance based on Legendre and Legendre's 
# Numerical Ecology book. We can load the function like so:

source("data/semivariance.R")
site_distmat = st_distance(sdat2)/1000
dim(site_distmat)

# Semivariance is a measure of the weighted pairwise deviations between residual
# values at different distances from each other.

glm1_pois_semivar = semivariance(site_distmat, 
                                  sdat2$resid_glm_pois, 
                                  ncats = 15)

ggplot(glm1_pois_semivar) + 
  aes(x = distances, y = semivar) + 
  geom_point() + 
  stat_smooth() +
  theme_minimal()


# What about the semivariance on the NB model?
glm1_nb_semivar = semivariance(site_distmat, 
                                  sdat2$resid_glm_nb, 
                                  ncats = 15)

ggplot(glm1_nb_semivar) + 
  aes(x = distances, y = semivar) + 
  geom_point() + 
  stat_smooth() +
  theme_minimal()

# Semivariance of the residuals on the Poisson model indicates there is some
# AC at shorter distances (increasing trend), whereas semivariance for the
# negative binomial model does not. Dr. Brown suggests the Spatial AC for the
# NB model deals better with the high value clusters.

glm_nb_plots = plot_spatial_AC(sdat2, glm1_nb, site_distmat)
glm_poisson_plots = plot_spatial_AC(sdat2, glm1_pois, site_distmat)

tland + glm_nb_plots[[1]]
glm_nb_plots[[2]]

tland + glm_poisson_plots[[1]]
glm_poisson_plots[[2]]

# Results

visreg(glm1_nb)
visreg(m1, scale = "response")

# Logging is bad mkay? Greater distance from logging leads to higher counts of
# Topa parrot fish
