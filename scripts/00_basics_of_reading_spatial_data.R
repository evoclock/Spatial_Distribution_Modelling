# These scripts are preliminary training scripts seemingly generated for students
# unfamiliar with R/RStudio without much apparent actually scientific significance
# for the project.

library(readr)
library(ggplot2)
library(dplyr)

dat_1 = read_csv("data/JuvUVCSites_with_ReefTypes_16Jun2016.csv")
# "Topa" parrot fish dataset

dat = as_tibble(dat_1)
rm(dat_1)

str(dat)
# flow and logged are character data types, the rest are numeric

names(dat)
nrow(dat)
length(unique(dat$site))
# 49 unique sites in the data.
range(dat$pres.topa)
# range of "Topa" parrot fish 0-32
summary(dat)

# If the dataset does not have a coordinate reference system, the coordinates
# are very much useless for aligning to other datasets.

# Plot the abundance of Topa fish and also plot whether Topa fish were logged
# at a given site or not across all given sites.

ggplot(dat) + 
  aes( x = secchi, y = pres.topa) + 
  geom_point() +
  stat_smooth() +
  theme_minimal()

ggplot(dat) + 
  aes(x = logged, y = pres.topa) + 
  geom_boxplot() +
  theme_minimal()

# Turns out there were far more Topa fish not logged across different sites
# than those that were logged for some reason.
# We know from the summary stats that the max number of Topa (32) were spotted 
# at site #35 but not logged for some reason not explained in the dataset.

# Reading benthic data

bendat = read_csv("data/BenthicCoverSurveys.csv")

str(bendat)
# code and category are character data types, the rest are numeric

head(bendat)

nrow(bendat)
# 1,519 rows

range(bendat$cover)
# range for the benthic data is 0-287

summary(bendat)
# 49 sites

# Why are there more than 49 rows?

ggplot(bendat) + 
  aes(x = category, y = cover) + 
  geom_boxplot() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# There are 32 categories of reef cover 

# Now use the sf library for reading spatial data.

library(sf)
logponds = st_read("data/Kia_Logging_Ponds/Kia_Logging_Ponds.shp")

str(logponds)
names(logponds)
# 3 variable types, OBJECTID is integer type, Years_of_0 is character type, 
# while the last one (geometry) is a list with numerical elements that correspond
# to XY coordinates

names(logponds)
st_crs(logponds)
# The dataset also has the aforementioned coordinate reference system that is 
# needed. In this case it uses WGS84 which was developed from GPS measurements.
# Alternatively, we can use EPSG 4326 which is a common code for 
# unprojected lon/lat coordinates

# Now read polygons

land = st_read("data/LandPoly/LandPoly.shp")

# and plot it so that each row is associated with a polygon (island)
plot(land[1])



