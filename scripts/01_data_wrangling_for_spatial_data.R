# Data wrangling part of the species dist modelling tutorial

library(readr)
library(ggplot2)
library(dplyr)

dat_1 = read_csv("data/JuvUVCSites_with_ReefTypes_16Jun2016.csv")
# "Topa" parrot fish dataset

dat = as_tibble(dat_1)

rm(dat_1)

bendat = read_csv("data/BenthicCoverSurveys.csv")

# Summarise benthic cover data.
# First for cover of branching corals. CB is included because it is also
# a branching coral. This is where domain knowledge matters.

CB_dat = filter(bendat, code %in% c("ACB", "CB"))
# Group by site
CB_dat = group_by(CB_dat, site)
# Summarise for both benthic categories. Save to new object if you would like
# to keep both categories separate for some reason.
# I have changed the names of the variables to be more informative of what 
# statistics we are reporting.
CB_dat_summ = summarize(CB_dat, Sum_CB_cover = sum(cover),
                    Mean_n_pts = mean(n_pts))

# Alternatively, if only interested in the summary stats for a given category 
# (set of categories) pipe the whole thing like so:

# CB_dat_summ = bendat %>%
#   filter(code %in% c("ACB", "CB")) %>% # Branching coral
#   group_by(site) %>%
#   summarize(Sum_CB_cover = sum(cover),
#             Mean_n_pts = mean(n_pts))

# Let's do that for soft cover

soft_dat <- bendat %>%
  filter(code %in% c("S", "SI")) %>% #Sand or Silt
  group_by(site) %>% 
  summarize(Sum_soft_cover = sum(cover),
            Mean_n_pts = mean(n_pts))

# Join the Topa fish dataset with the benthic dataset. A commonly desired 
# operation when testing say, species abundance with temperature, or some other
# suspected covariate. Since we have generate a summary stat per site, we can
# now join on that common field much as we would in Bash and get 49 rows.

# As usual, inspect the datasets to ensure consistency in naming conventions for
# things like sites, species, individuals, IDs, etc. This will help to prevent
# issues with the dataset for downstream analyses. 

# One way you could easily do this is retrieving a unique vector of all 
# character data for a given variable for example, you can then easily use SED
# in BASH to replace inconsistencies across large datasets.

dat_Topa_benthic = left_join(dat, CB_dat_summ, by = "site")

# Execute a second join operation to add soft cover, remember the join is to
# the object above "dat_Topa_benthic" and not "dat".

dat_Topa_benthic = left_join(dat_Topa_benthic, soft_dat, 
                              by = c("site", "Mean_n_pts"))


# you can check the join worked is to plot the outputs:

ggplot(dat_Topa_benthic) + 
  aes(x = logged, y = Sum_CB_cover) + 
  geom_boxplot() +
  theme_minimal()

ggplot(dat_Topa_benthic) + 
  aes(x = Sum_CB_cover, y = pres.topa) + 
  geom_point() +
  theme_minimal()

ggplot(dat_Topa_benthic) + 
  aes(x = Sum_CB_cover, y = Sum_soft_cover) + 
  geom_point() +
  theme_minimal()

# Checking correlations between variables and potential confounders

icol = sapply(dat_Topa_benthic, is.numeric)
library(corrplot)
corrplot(cor(dat_Topa_benthic[,icol]))

# Next we need to convert the survey dataframe to a spatial layer (spatial
# point file)

library(sf)
logponds = st_read("data/Kia_Logging_Ponds/Kia_Logging_Ponds.shp")

# Store the CRS to a separate object
kia_crs = st_crs(logponds)

# Convert to simple features
sdat2 = st_as_sf(dat_Topa_benthic, coords = c("coordx", "coordy"),
                  crs = kia_crs)

plot(sdat2["pres.topa"])

# Transform CRS 
# Read the land polygon data
land = st_read("data/LandPoly/LandPoly.shp")

# Correct CRS of land to that of the logponds
land = st_transform(land, kia_crs)

st_crs(land) == kia_crs

# Using a measure of distance from logponds as a proxy for pollution (in the
# absence of actual direct measurements of pollution)

# Make a distance matrix from every sample site (sdat2 object) to every logpond
# (logponds object)

distmat = st_distance(sdat2, logponds)
dim(distmat)

# Find the nearest minimum distance for each site (individual rows) and assign
# back to the sdat2 dataframe
# apply(distmat, 1, min)[1:5]

sdat2$dist_to_logging = apply(distmat, 1, min)/1000
# dividing by 1000 transforms the units to km

ggplot(sdat2) + 
  aes(x = dist_to_logging, y = pres.topa) + 
  geom_point() +
  theme_minimal()

save(sdat2, dat_Topa_benthic, land, logponds, kia_crs, 
     file = "outputs/2021-09-20_cleaned-data.rda")
