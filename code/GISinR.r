
#------------------------------------------------------------------------------
# GISinR.r
#
# An example script to show the utility of sf package for GIS analysis. This
# script will color-code watersheds in BC based on the number of hydrometric
# stations that are contained within each
#
# uses the 'bcmaps' package for access to the sub-basin shape files
# see https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html for useful
# guidance and examples as well.
#
# D.Hutchinson 2020-04-29
#------------------------------------------------------------------------------

# library references
library(bcmaps)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyhydat)

# let's get the bc watersheds data 
wsheds <- bcmaps::wsc_drainages(class = "sf")

# the above statement could be replaced by reading in a shape file
# myshape <- read_sf() # for example...

# now read in the active hydrometric network in BC
bc_network <- hy_stations(prov_terr_state_loc = "BC") %>%
              filter(HYD_STATUS == "ACTIVE") %>%
              mutate(SUB_SUB_BSN = substr(STATION_NUMBER, 1,4))

# summarize the number of stations by SUB_SUB_BASIN
sub_bsn <- bc_network %>% 
           group_by(SUB_SUB_BSN) %>% summarise(N = n()) %>%
           mutate(N = as.factor(N))

# merge summary with watershed data
wsheds <- wsheds %>% left_join(sub_bsn, by = c("SUB_SUB_DRAINAGE_AREA_CD" = "SUB_SUB_BSN")) 

# generate map
ggplot(wsheds) +
  geom_sf(aes(fill = N)) +
  theme_bw() + 
  ggtitle("BC hydrometric network - number of stations per sub-sub-basin")

# 