# Required packages: "tmap", "tidyverse", "terra", "sf", "nlme", "patchwork", "visreg"

#If on ubuntu you need the following first via the command line interface

# sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
# sudo apt-get update
# sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsqlite0-dev

# You can then proceed to install the "remotes" package if needed and install
# tmap from the github repo.
# install the other packages after as needed.

library(remotes)
install_github("r-tmap/tmap")
