#### Compile climate records from BNZ LTER database

library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(data.table)


### Load data through 2021
PAR_CRREL <- read.table("Raw data/155_PAR_CRREL_2010-2021.txt", sep = ",", header = T) %>% filter(flag == "G")
names(PAR_CRREL) <- c("site_id", "date", "hour", "measurement", "PAR_uMsm2", "units", "flag")
#AT_CPEAK <-read.table("Raw data/164_AT_CPEAK_1992-2021.txt", sep = ",", header = T)%>% filter(mean_flag == "G", height == 100) %>% rename("site_id" = "site") # Only a couple years in the 90's
AT_CRREL <- read.table("Raw data/164_AT_CRREL_1992-2021.txt", sep = ",", header = T)%>% filter(airtemp_flag == "G", height == 100)
PPT_CPEAK <-read.table("Raw data/167_TIPBUCK_CPEAK_1993-2021.txt", sep = ",", header = T)%>% filter(flag == "G")
names(PPT_CPEAK) <- c("site_id", "date", "hour", "measurement", "PPT_tip_mm", "units", "flag")
PPT_CRREL <- read.table("Raw data/167_TIPBUCK_CRREL_2007-2021.txt", sep = ",", header = T)%>% filter(flag == "G")
names(PPT_CRREL) <- c("site_id", "date", "hour", "measurement", "PPT_tip_mm", "units", "flag")

# Merge data
BNZ.1 <- full_join(PAR_CRREL, AT_CRREL, by = c("site_id", "date", "hour"))
BNZ.2 <- full_join(BNZ.1, PPT_CRREL, by = c("site_id", "date", "hour", "measurement", "flag"))
BNZ_climate <- full_join(BNZ.2, PPT_CPEAK) %>% filter(is.na(site_id) == F) %>% select(site_id, date, hour, PPT_tip_mm, airtemp, PAR_uMsm2)


BNZ_climate$PPT_tip_mm <- as.numeric(BNZ_climate$PPT_tip_mm)
BNZ_climate$PAR_uMsm2 <- as.numeric(BNZ_climate$PAR_uMsm2)
BNZ_climate$airtemp <- as.numeric(BNZ_climate$airtemp)

# Set time zone
BNZ_climate <- BNZ_climate %>% mutate(across(hour, ~ifelse(nchar(hour) <4, paste("0", hour, sep = ""),.))) # Make all hours have 4 digits
BNZ_climate$datetime <- paste(BNZ_climate$date, BNZ_climate$hour, sep = " ")
BNZ_climate$datetimeAK <- as.POSIXct(BNZ_climate$datetime, format = "%Y-%m-%d %H%M", tz = "America/Anchorage")


BNZ_climate$year <- format(BNZ_climate$datetimeAK, format = "%Y", tz = "America/Anchorage")

write.csv(BNZ_climate, "BNZ_Climate_1992-2021.csv", row.names = F)

## Plot raw data
p.ppt <- BNZ_climate %>% ggplot(aes(datetimeAK, PPT_tip_mm, color = site_id)) + geom_point()
p.temp <- BNZ_climate %>% ggplot(aes(datetimeAK, airtemp, color = site_id)) + geom_point()
p.par <- BNZ_climate %>% ggplot(aes(datetimeAK, PAR_uMsm2, color = site_id)) + geom_point()


## 2019-2021 rolling windows
# Select data
Climate_19_21 <- BNZ_climate %>% filter(datetimeAK >= "2019-01-01") 
Climate_19_21 <- Climate_19_21[order(Climate_19_21$datetimeAK),]
check <- Climate_19_21 %>% group_by(site_id) %>% mutate(diff = datetimeAK - lag(datetimeAK), datetimeAK = datetimeAK)
unique(check$diff)

# Data is missing only a few single hours. Add them in:
DateTimeFill <- data.frame(datetimeAK = seq(ymd_hms("2019-01-01 00:00:00", tz = "America/Anchorage"),ymd_hms("2021-12-30 00:00:00", tz = "America/Anchorage"), by = '1 hour'))
DateTimeFill <- data.frame(datetimeAK = rep(DateTimeFill$datetimeAK,2), 
                           site_id = c(rep("CRREL-Met", length(DateTimeFill$datetimeAK)), rep("CPEAK", length(DateTimeFill$datetimeAK))))
Climate_19_21 <- full_join(DateTimeFill, Climate_19_21) %>% group_by(site_id, datetimeAK) %>% summarise(PPT_tip_mm = sum(PPT_tip_mm, na.rm = TRUE),
                                                                                                        PAR_uMsm2 = mean(PAR_uMsm2, na.rm = TRUE),
                                                                                                        airtemp = mean(airtemp, na.rm = TRUE))
Climate_19_21 <- Climate_19_21[order(Climate_19_21$datetimeAK),]
Climate_19_21 <- Climate_19_21 %>% mutate(across(PPT_tip_mm, ~ifelse(PPT_tip_mm %in% NA, 0, .))) %>% unique()

# Calculate rolling windows
Climate_19_21 <- Climate_19_21 %>% group_by(site_id) %>% mutate(PPT_day_sum = rollapply(PPT_tip_mm, width=24, FUN=function(x) sum(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            PPT_week_sum = rollapply(PPT_tip_mm, width=168, FUN=function(x) sum(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            PPT_month_sum = rollapply(PPT_tip_mm, width=672, FUN=function(x) sum(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            PAR_day_mean = rollapply(PAR_uMsm2, width=24, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            PAR_week_mean = rollapply(PAR_uMsm2, width=168, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            PAR_month_mean = rollapply(PAR_uMsm2, width=672, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            AirT_day_mean = rollapply(airtemp, width=24, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            AirT_week_mean = rollapply(airtemp, width=168, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
                            AirT_month_mean = rollapply(airtemp, width=672, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

Climate_19_21 %>% ggplot(aes(datetimeAK, airtemp, color = site_id)) + geom_point()

write.csv(Climate_19_21, "BNZ_Climate_2019-2021.csv", row.names = F)



### ALD from "Bonanza Creek LTER: Active Layer Depth or Permafrost Presence for the Regional Site Network"
library(data.table)
ALD <- fread("Raw data/605_RSN_ActiveLayerDepths_2021.txt", select = c(1:5)) %>% filter(flag == "G", depth > 0) %>% group_by(date, site) %>%
  summarise(depth = mean(depth, na.rm = T))
head(ALD)
length(unique(ALD$site))

ALD_site <- fread("Raw data/knb-lter-bnz.605.5-sites.txt", select = 1, blank.lines.skip = TRUE, fill = T)

ALD_sites <- data.frame(site = ALD_site[seq(1, length(ALD_site$V1), 5)], lon = ALD_site[seq(2, length(ALD_site$V1), 5)], lat = ALD_site[seq(4, length(ALD_site$V1), 5)])
names(ALD_sites) <- c("site", "lon", "lat")
ALD_sites$lat <- as.numeric(ALD_sites$lat)
ALD_sites$lon <- as.numeric(ALD_sites$lon)

ALD_2015 <- ALD %>% filter(date > "2015-01-01")

ALD_2015 %>% filter(site == "DCM1") %>% ggplot(aes(date, depth, color = site)) + geom_point()
ALD_2015 %>% group_by(site) %>% filter(is.na(depth) == F) %>% summarise(n = length(depth)) %>% filter(n > 4) %>% View()

# Find closest site
ALD_sites$Vault <- sqrt(((ALD_sites$lon + 147.714949)^2) + ((ALD_sites$lat-65.029315)^2))
ALD_sites$Moose <- sqrt(((ALD_sites$lon + 147.0539195)^2) + ((ALD_sites$lat-64.71415308)^2))
ALD_sites$French <- sqrt(((ALD_sites$lon + 146.916364)^2) + ((ALD_sites$lat-64.60667379)^2))
ALD_sites$Stuart <- sqrt(((ALD_sites$lon + 146.479933)^2) + ((ALD_sites$lat-64.75408)^2))
ALD_sites$Poker <- sqrt(((ALD_sites$lon + 147.486655)^2) + ((ALD_sites$lat-65.152855)^2))



## Snow depth from Jake's storms repo (Snowtel?)

filelist = list.files(path = "Raw data/SWE", full.names = TRUE, recursive = TRUE )
filelist
df.list <- lapply(filelist, read.csv) # set file names
SWE <- rbindlist(df.list)
head(SWE)
names(SWE) <- c("site.id", "date", "time", "depth_in", "prec_in")
SWE$date <- as.Date(SWE$date, format = "%m/%d/%Y")
SWE$year <- format(SWE$date, "%y")
SWE$julian <- yday(SWE$date)

SWE %>% filter(depth_in >= 0) %>% ggplot(aes(date, depth_in, color = year)) + geom_line()

SWE <- SWE %>% filter(depth_in >= 0)
SWE$diff <- c(NA, diff(SWE$depth_in))
max <- SWE %>% group_by(year) %>% filter(diff == min(diff))

SWE %>% ggplot(aes(date, depth_in, color = year)) + geom_line(linetype = 2) +
  geom_vline(data = max[-c(2),], aes(xintercept = date, color = year)) + geom_text(data = max[-c(2),], aes(label = julian), hjust = -0.5) + xlab("Date") + ylab("SWE (in)")

SWE %>% group_by(year) %>% filter(depth_in == max(depth_in)) %>% select(year, depth_in) %>% unique()

# SWE from USDA (only through 2020)
USDA_SWE <- read.csv("Raw data/USDA_swe.txt", skip = 59, header = T)
head(USDA_SWE)
names(USDA_SWE) <- c("Date", "SWE", "QC")
USDA_SWE$Date <- as.Date(USDA_SWE$Date)
USDA_SWE$Year <- format(USDA_SWE$Date, "%Y")
USDA_SWE %>% ggplot(aes(Date, SWE)) + geom_point()

USDA_SWE$diff <- c(NA, diff(USDA_SWE$SWE))

USDA_SWE %>% ggplot(aes(Date, diff)) + geom_point()

USDA_SWE %>% group_by(Year) %>% filter(diff == min(diff))


yday(as.Date('2015-05-06'))



## Other snow depth for USDA
USDA_SD <- read.csv("Raw data/snow free days.csv", header = T)
head(USDA_SD)
names(USDA_SD) <- c("Year", "Snotel", "Gilmore", "Upper_Chena", "Chena_Ridge", "Munson", "Monument")
USDA_SD[,3] <- yday(as.Date(USDA_SD[,3], format = "%m/%d/%Y"))
USDA_SD[,4] <- yday(as.Date(USDA_SD[,4], format = "%m/%d/%Y"))
USDA_SD[,5] <- yday(as.Date(USDA_SD[,5], format = "%m/%d/%Y"))
USDA_SD[,6] <- yday(as.Date(USDA_SD[,6], format = "%m/%d/%Y"))
USDA_SD[,7] <- yday(as.Date(USDA_SD[,7], format = "%m/%d/%Y"))

# Select 4 lower elevation sites
USDA_SD <- USDA_SD %>% select(Year, Snotel, Gilmore, Chena_Ridge, Monument)
USDA_SD$mean <- round(rowMeans( USDA_SD[,2:5] ))

USDA_SD %>% ggplot(aes(x=Year)) + geom_line(aes(y=Snotel)) +
  geom_line(aes(y=Gilmore)) +
  #geom_line(aes(y=Keystone), color = "blue") +
  #geom_line(aes(y=Upper_Chena)) +
  geom_line(aes(y=Chena_Ridge)) +
  #geom_line(aes(y=Munson)) +
  geom_line(aes(y=Monument)) +
  geom_line(aes(y=mean), color = 2) + ylab("Julian day")

ggsave("Last snow day plot.png")

write.csv(USDA_SD, "Last snow day.csv", row.names = F)
