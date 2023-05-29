#### Compile climate records from BNZ LTER database

library(tidyverse)
library(ggplot2)

### 2015-2022 data
BNZ_climate <- read.csv("Raw data/BNZ_climate_2019-2022.csv", skip = 5)
head(BNZ_climate)
names(BNZ_climate) <- c("DatetimeAK", "Cari_peak_airT", "Cari_peak_precip_tot_mm", "Main_adj_BP_mBar", "Main_airT", "Main_PAR_Wm2", "Main_precip_tot_mm")
BNZ_climate$DatetimeAK <- as.POSIXct(BNZ_climate$DatetimeAK, tz = "America/Anchorage")


# Average or sum for 15 minute intervals
min15<-cut(BNZ_climate$DatetimeAK, breaks="15 min")
aggregate(inst_rainfall_mm ~ min, data = VAUL.2022.gauge, FUN=function(x) 
  sum=sum(x)))