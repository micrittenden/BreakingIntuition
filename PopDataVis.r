rm(list=ls(all=TRUE))

setwd("FILEPATH")

# install.packages("sp", dependencies = TRUE)
# install.packages("rgdal", dependencies = TRUE)
# install.packages("raster", dependencies = TRUE)
# install.packages("rgeos", dependencies = TRUE)
# install.packages("ggmap", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages("gridExtra", dependencies = TRUE)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(ggmap)
library(dplyr)
library(maptools)
library(gridExtra)

liberia <- get_map(location = c(-11.65, 4.25, -7.25, 8.6),
                   maptype = "watercolor")
liberia <- ggmap(liberia)


counties <- readOGR(dsn="shapefiles", layer=
                              "counties", stringsAsFactors=FALSE, verbose=FALSE)
#I need to add # of Households in Bomi
counties@data[1,7] <- 20508


proj4string(counties)
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
counties <- spTransform(counties, CRS(projection))
counties_f <- fortify(counties, region = "CCNAME")
county_names <- counties@data$CCNAME
county_cpts <- gCentroid(counties, byid=TRUE)
county_labels <- cbind.data.frame(name = county_names,
                                  x = county_cpts$x, y = county_cpts$y)
#I want to push the label for Margibi up and to the right
county_labels[9,2] <- -10.1
county_labels[9,3] <- 6.6
county_labels[10,2] <- -7.65

counties_f <- left_join(x = counties_f, y = counties@data,by = c("id" = "CCNAME"))
# #population
# lbr_cnty_pop <- liberia + geom_map(data=counties_f, map=counties_f,
#                                    aes(x=long, y=lat, map_id=id, fill = SUM_TOTAL/1000),
#                                    color ="white", alpha = .5, size = .1)
# lbr_cnty_pop <- lbr_cnty_pop + scale_fill_gradient2(low = "purple",
#                                                     mid="yellow", high="red", midpoint = 600, space="Lab",
#                                                     guide = guide_legend(title = NULL))
# lbr_cnty_pop <- lbr_cnty_pop + annotate('text', x = county_labels$x,
#                                         y = county_labels$y, label = county_labels$name, size = 2)
# lbr_cnty_pop <- lbr_cnty_pop + ggtitle("Population across Liberia by County", subtitle = "by thousands")
# # plot(lbr_cnty_pop)
# #population density
# lbr_cnty_dens <- liberia + geom_map(data=counties_f, map=counties_f,
#                                     aes(x=long, y=lat, map_id=id, fill = SUM_TOTAL/Sq_km),
#                                     color ="white", alpha = .5, size = .1)
# lbr_cnty_dens <- lbr_cnty_dens + scale_fill_gradient2(low = "purple",
#                                                       mid="yellow", high="red", midpoint = 300, space="Lab",
#                                                       guide = guide_legend(title = NULL))
# lbr_cnty_dens <- lbr_cnty_dens + annotate('text', x = county_labels$x,
#                                           y = county_labels$y, label = county_labels$name, size = 2)
# lbr_cnty_dens <- lbr_cnty_dens + ggtitle("Population Density across Liberia by County", subtitle = "by square kilometer")
# # plot(lbr_cnty_dens)
# #percentage female
# lbr_cnty_perfem <- liberia + geom_map(data=counties_f, map=counties_f,
#                                       aes(x=long, y=lat, map_id=id, fill = SUM_FEMALE/SUM_TOTAL),
#                                       color ="white", alpha = .5, size = .1)
# lbr_cnty_perfem <- lbr_cnty_perfem + scale_fill_gradient(low = "blue",
#                                                          high="red", space="Lab", guide = guide_legend(title = NULL))
# lbr_cnty_perfem <- lbr_cnty_perfem + annotate('text', x = county_labels$x,
#                                               y = county_labels$y, label = county_labels$name, size = 2)
# lbr_cnty_perfem <- lbr_cnty_perfem + ggtitle("Population Percentage Female across Liberia by County", subtitle = "Red = Female & Blue = Male")
# # plot(lbr_cnty_perfem)
# #persons per household
# lbr_cnty_hh <- liberia + geom_map(data=counties_f, map=counties_f,
#                                   aes(x=long, y=lat, map_id=id, fill = SUM_TOTAL/SUM_HH),
#                                   color ="white", alpha = .5, size = .1)
# lbr_cnty_hh <- lbr_cnty_hh + scale_fill_gradient2(low = "purple",
#                                                   mid="yellow", high="red", midpoint = 6, space="Lab",
#                                                   guide = guide_legend(title = NULL))
# lbr_cnty_hh <- lbr_cnty_hh + annotate('text', x = county_labels$x,
#                                       y = county_labels$y, label = county_labels$name, size = 2)
# lbr_cnty_hh <- lbr_cnty_hh + ggtitle("Persons per Household across Liberia by County")
# # plot(lbr_cnty_hh)
# # county4 <- grid4 <- grid.arrange(lbr_cnty_pop, lbr_cnty_dens, lbr_cnty_perfem, lbr_cnty_hh, nrow = 2, ncol = 2)
# # ggsave("Final_Deliverable1.pdf", county4, width = 11, height = 8.5)



districts <- readOGR(dsn="shapefiles", layer=
                                "districts", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(districts)
districts <- spTransform(districts, CRS(projection))
districts_f <- fortify(districts, region = "DNAME")
district_names <- districts@data$DNAME
district_cpts <- gCentroid(districts, byid=TRUE)
district_labels <- cbind.data.frame(name = district_names,
                                  x = district_cpts$x, y = district_cpts$y)
districts_f <- left_join(x = districts_f, y = districts@data,
                         by = c("id" = "DNAME"))
# #population
# lbr_dstr_pop <- liberia + geom_map(data=districts_f, map=districts_f,
#                                    aes(x=long, y=lat, map_id=id, fill = log(TOTAL)),
#                                    color ="white", alpha = .5, size = .2)
# lbr_dstr_pop <- lbr_dstr_pop + geom_map(data=counties_f,
#                                         map=counties_f, aes(x=long, y=lat, map_id=id), color ="white",
#                                         alpha = 0, size = .7)
# lbr_dstr_pop <- lbr_dstr_pop + scale_fill_gradient2(low = "purple",
#                                                     mid="yellow", high="red", midpoint=10, space="Lab",
#                                                     guide = guide_legend(title = NULL))
# lbr_dstr_pop <- lbr_dstr_pop + annotate('text', x = district_labels$x,
#                                         y = district_labels$y, label = district_labels$name, size = 1)
# lbr_dstr_pop <- lbr_dstr_pop + ggtitle("Log for Population across Liberia by District")
# # plot(lbr_dstr_pop)
# #population density
# lbr_dstr_dens <- liberia + geom_map(data=districts_f, map=districts_f,
#                                     aes(x=long, y=lat, map_id=id, fill = log(TOTAL/Area)),
#                                     color ="white", alpha = .5, size = .2)
# lbr_dstr_dens <- lbr_dstr_dens + geom_map(data=counties_f,
#                                           map=counties_f, aes(x=long, y=lat, map_id=id), color ="white",
#                                           alpha = 0, size = .7)
# lbr_dstr_dens <- lbr_dstr_dens + scale_fill_gradient2(low = "purple",
#                                                       mid="yellow", high="red", midpoint = 5, space="Lab",
#                                                       guide = guide_legend(title = NULL))
# lbr_dstr_dens <- lbr_dstr_dens + annotate('text', x = district_labels$x,
#                                           y = district_labels$y, label = district_labels$name, size = 1)
# lbr_dstr_dens <- lbr_dstr_dens + ggtitle("Log for Population Density across Liberia by District")
# # plot(lbr_dstr_dens)
# #percentage female
# lbr_dstr_perfem <- liberia + geom_map(data=districts_f, map=districts_f,
#                                       aes(x=long, y=lat, map_id=id, fill = FEMALE/TOTAL),
#                                       color ="white", alpha = .5, size = .2)
# lbr_dstr_perfem <- lbr_dstr_perfem + geom_map(data=counties_f,
#                                               map=counties_f, aes(x=long, y=lat, map_id=id), color ="white",
#                                               alpha = 0, size = .7)
# lbr_dstr_perfem <- lbr_dstr_perfem + scale_fill_gradient2(low = "yellow",
#                                                           mid="orange", high="red", midpoint= 0.45, space="Lab",
#                                                           guide = guide_legend(title = NULL))
# lbr_dstr_perfem <- lbr_dstr_perfem + annotate('text', x = district_labels$x,
#                                               y = district_labels$y, label = district_labels$name, size = 1)
# lbr_dstr_perfem <- lbr_dstr_perfem + ggtitle("Population Percentage Female across Liberia by District", subtitle = "Red = Female & Yellow = Male")
# # plot(lbr_dstr_perfem)
# #persons per household
# lbr_dstr_hh <- liberia + geom_map(data=districts_f, map=districts_f,
#                                   aes(x=long, y=lat, map_id=id, fill = TOTAL/HHOLDS),
#                                   color ="white", alpha = .5, size = .2)
# lbr_dstr_hh <- lbr_dstr_hh + geom_map(data=counties_f,
#                                       map=counties_f, aes(x=long, y=lat, map_id=id), color ="white",
#                                       alpha = 0, size = .7)
# lbr_dstr_hh <- lbr_dstr_hh + scale_fill_gradient2(low = "yellow",
#                                                   mid= "orange", high="red", midpoint = 7.5, space="Lab",
#                                                   guide = guide_legend(title = NULL))
# lbr_dstr_hh <- lbr_dstr_hh + annotate('text', x = district_labels$x,
#                                       y = district_labels$y, label = district_labels$name, size = 1)
# lbr_dstr_hh <- lbr_dstr_hh + ggtitle("Persons per Household across Liberia by District")
# # plot(lbr_dstr_hh)
# # district4 <- grid4 <- grid.arrange(lbr_dstr_pop, lbr_dstr_dens, lbr_dstr_perfem, lbr_dstr_hh, nrow = 2, ncol = 2)
# # ggsave("Final_Deliverable2.pdf", district4, width = 11, height = 8.5)



clans <- readOGR(dsn="shapefiles", layer=
                            "clans", stringsAsFactors=FALSE, verbose=FALSE)
#I need to add # of Households in Bomi (this will make all households the same size, but it's the best we can do since we don't have data past the county level)
clans@data[which(clans@data$FIRST_CCNA == "Bomi"),8] <- (clans@data[which(clans@data$FIRST_CCNA == "Bomi"),5]/4)
#I need to correct # of Households for Mimmoken Clan of River Gee, so I will take the mean of HH of clans in River Gee (removing Mimmoken) and assign that to Mimmoken
#because the actual clan-level data is unavailable, th best I can do is fill the misinformation with the mean valu so it does not throw off the range
clans@data[388,8] <- 0
df <- clans@data[which(clans@data$FIRST_CCNA == "River Gee"),]
df <- df[-29,]
clans@data[388,8] <- mean(df$SUM_HH)
proj4string(clans)
clans <- spTransform(clans, CRS(projection))
clans_f <- fortify(clans, region = "CLNAME")
clan_names <- clans@data$CLNAME
clan_cpts <- gCentroid(clans, byid=TRUE)
clan_dist <- clans@data$FIRST_DNAM
clan_labels <- cbind.data.frame(name = clan_names, dist = clan_dist,
                                x = clan_cpts$x, y = clan_cpts$y)
mnrv_labels <- clan_labels
bchn_labels <- clan_labels

n <- nrow(clan_labels[which(clan_labels$dist == "Greater Monrovia"), ])
remove <- rep("", n)
clan_labels[which(clan_labels$dist == "Greater Monrovia"),]$name <- remove
s <- nrow(clan_labels[which(clan_labels$dist == "Commonwealth"), ])
remove <- rep("", s)
clan_labels[which(clan_labels$dist == "Commonwealth"),]$name <- remove
#I need to add missing density info
# clans@data$DENSITY <- clans@data$SUM_TOTAL/(clans@data$Area)
clans_f <- left_join(x = clans_f, y = clans@data, by = c("id" = "CLNAME"))
# #population
# lbr_clan_pop <- liberia + geom_map(data=clans_f, map=clans_f,
#                                    aes(x=long, y=lat, map_id=id, fill = SUM_TOTAL),
#                                    color ="white", alpha = .5, size = .1)
# lbr_clan_pop <- lbr_clan_pop + geom_map(data=districts_f, map=districts_f,
#                                         aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_clan_pop <- lbr_clan_pop + geom_map(data=counties_f, map=counties_f,
#                                         aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_clan_pop <- lbr_clan_pop + scale_fill_gradient2(low = "yellow",
#                                                     mid = "orange",high="red", midpoint = 25000, space="Lab",
#                                                     guide = guide_legend(title = NULL))
# lbr_clan_pop <- lbr_clan_pop + annotate('text', x = county_labels$x,
#                                         y = county_labels$y, label = county_labels$name, size = 1.5)
# lbr_clan_pop <- lbr_clan_pop + annotate('text', x = district_labels$x,
#                                   y = district_labels$y, label = district_labels$name, size = .7)
# lbr_clan_pop <- lbr_clan_pop + annotate('text', x = clan_labels$x,
#                                   y = clan_labels$y, label = clan_labels$name, size = .2)
# lbr_clan_pop <- lbr_clan_pop + ggtitle("Population across Liberia by Clan")
# # plot(lbr_clan_pop)
# #population density
# lbr_clan_dens <- liberia + geom_map(data=clans_f, map=clans_f,
#                                     aes(x=long, y=lat, map_id=id, fill = log(DENSITY)),
#                                     color ="white", alpha = .5, size = .1)
# lbr_clan_dens <- lbr_clan_dens + geom_map(data=districts_f, map=districts_f,
#                                           aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_clan_dens <- lbr_clan_dens + geom_map(data=counties_f, map=counties_f,
#                                           aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_clan_dens <- lbr_clan_dens + scale_fill_gradient2(low = "yellow",
#                                                       mid="orange", high="red", midpoint = 5, space="Lab",
#                                                       guide = guide_legend(title = NULL))
# lbr_clan_dens <- lbr_clan_dens + annotate('text', x = county_labels$x,
#                                           y = county_labels$y, label = county_labels$name, size = 1.5)
# lbr_clan_dens <- lbr_clan_dens + annotate('text', x = district_labels$x,
#                                           y = district_labels$y, label = district_labels$name, size = .7)
# lbr_clan_dens <- lbr_clan_dens + annotate('text', x = clan_labels$x,
#                                           y = clan_labels$y, label = clan_labels$name, size = .2)
# lbr_clan_dens <- lbr_clan_dens + ggtitle("Log for Population Density across Liberia by Clan")
# # plot(lbr_clan_dens)
# #percentage female
# lbr_clan_perfem <- liberia + geom_map(data=clans_f, map=clans_f,
#                                       aes(x=long, y=lat, map_id=id, fill = (SUM_FEMALE/SUM_TOTAL)),
#                                       color ="white", alpha = .5, size = .1)
# lbr_clan_perfem <- lbr_clan_perfem + geom_map(data=districts_f, map=districts_f,
#                                               aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_clan_perfem <- lbr_clan_perfem + geom_map(data=counties_f, map=counties_f,
#                                               aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_clan_perfem <- lbr_clan_perfem + scale_fill_gradient2(low = "yellow",
#                                                           mid="orange", high="red", midpoint = 0.4, space="Lab",
#                                                           guide = guide_legend(title = NULL))
# lbr_clan_perfem <- lbr_clan_perfem + annotate('text', x = county_labels$x,
#                                               y = county_labels$y, label = county_labels$name, size = 1.5)
# lbr_clan_perfem <- lbr_clan_perfem + annotate('text', x = district_labels$x,
#                                               y = district_labels$y, label = district_labels$name, size = .7)
# lbr_clan_perfem <- lbr_clan_perfem + annotate('text', x = clan_labels$x,
#                                               y = clan_labels$y, label = clan_labels$name, size = .2)
# lbr_clan_perfem <- lbr_clan_perfem + ggtitle("Population Percentage Female across Liberia by Clan", subtitle = "Red = Female & Yellow = Male")
# # plot(lbr_clan_perfem)
# #persons per household
# clans@data$AVG_HH <- (clans@data$SUM_TOTAL/clans@data$SUM_HH)
# lbr_clan_hh <- liberia + geom_map(data=clans_f, map=clans_f,
#                                   aes(x=long, y=lat, map_id=id, fill = (SUM_TOTAL/SUM_HH)),
#                                   color ="white", alpha = .5, size = .1)
# lbr_clan_hh <- lbr_clan_hh + geom_map(data=districts_f, map=districts_f,
#                                       aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_clan_hh <- lbr_clan_hh + geom_map(data=counties_f, map=counties_f,
#                                       aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_clan_hh <- lbr_clan_hh + scale_fill_gradient2(low = "green",
#                                                   mid="yellow", high="red", midpoint = 15, space="Lab",
#                                                   guide = guide_legend(title = NULL))
# lbr_clan_hh <- lbr_clan_hh + annotate('text', x = county_labels$x,
#                                       y = county_labels$y, label = county_labels$name, size = 1.5)
# lbr_clan_hh <- lbr_clan_hh + annotate('text', x = district_labels$x,
#                                       y = district_labels$y, label = district_labels$name, size = .7)
# lbr_clan_hh <- lbr_clan_hh + annotate('text', x = clan_labels$x,
#                                       y = clan_labels$y, label = clan_labels$name, size = .2)
# lbr_clan_hh <- lbr_clan_hh + ggtitle("Persons per Household across Liberia by Clan")
# # plot(lbr_clan_hh)
# # clan4 <- grid4 <- grid.arrange(lbr_clan_pop, lbr_clan_dens, lbr_clan_perfem, lbr_clan_hh, nrow = 2, ncol = 2)
# #I just made the pdf bigger so that the labels would not overlap
# # ggsave("Final_Deliverable3.pdf", clan4, width = 17, height = 15)



monrovia <- get_map(location = c(-10.82, 6.24, -10.67, 6.41),
                    maptype = "watercolor")
monrovia <- ggmap(monrovia)
mnrv <- clans@data[which(clans@data$FIRST_DNAM == "Greater Monrovia"),]
#some of the clans ar missing Density information
#these clans have also had incorrectly inputed Area (it is 10 times to small)
#to correct this Density problem while also solving the Area problem, the clans with Density = 0 have to be isolated
#then the Area must be multiplied by 10 for those clans (I did not actually alter the Area info because it was not important to the graphics)
mnrv_dens2 <- mnrv[which(mnrv$DENSITY == "0"),]
mnrv_dens2$DENSITY <- (mnrv_dens2$SUM_TOTAL/(mnrv_dens2$Area*10))
mnrv[which(mnrv$DENSITY == "0"),] <- mnrv_dens2
mnrv_f <- fortify(clans, region = "CLNAME")
mnrv_f <- left_join(x = mnrv_f, y = mnrv, by = c("id" = "CLNAME"))
mnrv_f <- mnrv_f[which(mnrv_f$FIRST_DNAM == "Greater Monrovia"),]

# #population
# lbr_mnrv_pop <- monrovia + geom_map(data=mnrv_f, map=mnrv_f,
#                                    aes(x=long, y=lat, map_id=id, fill = SUM_TOTAL),
#                                    color ="white", alpha = .5, size = .1)
# lbr_mnrv_pop <- lbr_mnrv_pop + geom_map(data=districts_f, map=districts_f,
#                                         aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_mnrv_pop <- lbr_mnrv_pop + geom_map(data=counties_f, map=counties_f,
#                                         aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_mnrv_pop <- lbr_mnrv_pop + scale_fill_gradient2(low = "yellow",
#                                                     mid = "orange",high="red", midpoint = 12500, space="Lab",
#                                                     guide = guide_legend(title = NULL))
# lbr_mnrv_pop <- lbr_mnrv_pop + annotate('text', x = mnrv_labels$x,
#                                         y = mnrv_labels$y, label = mnrv_labels$name, size = .7)
# lbr_mnrv_pop <- lbr_mnrv_pop + ggtitle("Population across Monrovia by Clan")
# # plot(lbr_mnrv_pop)
# #population density
# lbr_mnrv_dens <- monrovia + geom_map(data=mnrv_f, map=mnrv_f,
#                                      aes(x=long, y=lat, map_id=id, fill = log(DENSITY)),
#                                      color ="white", alpha = .5, size = .1)
# lbr_mnrv_dens <- lbr_mnrv_dens + geom_map(data=districts_f, map=districts_f,
#                                           aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_mnrv_dens <- lbr_mnrv_dens + geom_map(data=counties_f, map=counties_f,
#                                           aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_mnrv_dens <- lbr_mnrv_dens + scale_fill_gradient2(low = "yellow",
#                                                       mid = "orange",high="red", midpoint = 7.5, space="Lab",
#                                                       guide = guide_legend(title = NULL))
# lbr_mnrv_dens <- lbr_mnrv_dens + annotate('text', x = mnrv_labels$x,
#                                           y = mnrv_labels$y, label = mnrv_labels$name, size = .7)
# lbr_mnrv_dens <- lbr_mnrv_dens + ggtitle("Log of Population Density across Monrovia by Clan")
# # plot(lbr_mnrv_dens)
# #population percentage female
# lbr_mnrv_perfem <- monrovia + geom_map(data=mnrv_f, map=mnrv_f,
#                                        aes(x=long, y=lat, map_id=id, fill = (SUM_FEMALE/SUM_TOTAL)),
#                                        color ="white", alpha = .5, size = .1)
# lbr_mnrv_perfem <- lbr_mnrv_perfem + geom_map(data=districts_f, map=districts_f,
#                                               aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_mnrv_perfem <- lbr_mnrv_perfem + geom_map(data=counties_f, map=counties_f,
#                                               aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_mnrv_perfem <- lbr_mnrv_perfem + scale_fill_gradient2(low = "yellow",
#                                                           mid = "orange", high="red", midpoint = 0.45, space="Lab",
#                                                           guide = guide_legend(title = NULL))
# lbr_mnrv_perfem <- lbr_mnrv_perfem + annotate('text', x = mnrv_labels$x,
#                                               y = mnrv_labels$y, label = mnrv_labels$name, size = .7)
# lbr_mnrv_perfem <- lbr_mnrv_perfem + ggtitle("Population Percentage Female across Monrovia by Clan", subtitle = "Red = Female & Yellow = Male")
# # plot(lbr_mnrv_perfem)
# #persons per household
# lbr_mnrv_hh <- monrovia + geom_map(data=mnrv_f, map=mnrv_f,
#                                    aes(x=long, y=lat, map_id=id, fill = (SUM_TOTAL/SUM_HH)),
#                                    color ="white", alpha = .5, size = .1)
# lbr_mnrv_hh <- lbr_mnrv_hh + geom_map(data=districts_f, map=districts_f,
#                                       aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .75)
# lbr_mnrv_hh <- lbr_mnrv_hh + geom_map(data=counties_f, map=counties_f,
#                                       aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .9)
# lbr_mnrv_hh <- lbr_mnrv_hh + scale_fill_gradient2(low = "yellow",
#                                                   mid = "orange", high="red", midpoint = 4.5, space="Lab",
#                                                   guide = guide_legend(title = NULL))
# lbr_mnrv_hh <- lbr_mnrv_hh + annotate('text', x = mnrv_labels$x,
#                                       y = mnrv_labels$y, label = mnrv_labels$name, size = .7)
# lbr_mnrv_hh <- lbr_mnrv_hh + ggtitle("Persons per Household across Monrovia by Clan")
# # plot(lbr_mnrv_hh)
# # monrovia4 <- grid4 <- grid.arrange(lbr_mnrv_pop, lbr_mnrv_dens, lbr_mnrv_perfem, lbr_mnrv_hh, nrow = 2, ncol = 2)
# # ggsave("Final_Deliverable4.pdf", monrovia4, width = 17, height = 15)
