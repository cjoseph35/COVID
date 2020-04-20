require(ggplot2)
require(reshape2)
require(gganimate)
require(gifski)
require(readxl, quietly = TRUE)
require(xlsx, quietly = TRUE)
require(dplyr)
require(cowplot, quietly = TRUE)
require(googleway, quietly = TRUE)
require(ggrepel, quietly = TRUE)
require(ggspatial, quietly = TRUE)
require(libwgeom, quietly = TRUE)
require(sf, quietly = TRUE)
require(rnaturalearth, quietly = TRUE)
require(rnaturalearthdata, quietly = TRUE)
library(leaflet)
library(maps)
library(htmlwidgets)




setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set WD to saved script directory


dat <- read_excel('USCOVID_TimeSeries.xlsx',sheet = 'covid') ## Reads Data


dat$iso2 <- NULL
dat$iso3 <- NULL
dat$code3 <- NULL
dat$FIPS <- NULL
dat$Country_Region <- NULL ##Removes unnecessary fields


dat.melt <- melt(dat, id.vars = c('UID','Admin2','Province_State', 'Lat','Long_','Combined_Key'))
dat.melt$date <- dat.melt$variable
dat.melt$variable <- NULL

dat.melt$value <- as.numeric(dat.melt$value)
#dat.melt$date <- gsub("X","0",dat.melt$date)



dat.melt$date <- as.Date(dat.melt$date,"%m/%d/%y") ##Sets date to date type
dat.melt.NY <- dat.melt[dat.melt$Province_State == 'New York',] ##Removes all other dates


##Long Island Only##
dat.melt.LI <- dat.melt.NY[dat.melt.NY$Admin2 == 'Suffolk' | dat.melt.NY$Admin2 == 'Nassau',]
dat.melt.LI <-dat.melt.LI[dat.melt.LI$value > 1,]



##Downstate Only##
dat.melt.DS <- dat.melt.NY[dat.melt.NY$Admin2 == 'Suffolk' | dat.melt.NY$Admin2 == 'Nassau'|dat.melt.NY$Admin2 == 'New York'|
                             dat.melt.NY$Admin2 == 'Westchester'|dat.melt.NY$Admin2 == 'Rockland'|dat.melt.NY$Admin2 == 'Orange',]


DSonly <-
  ggplot(dat.melt.DS, aes(x = date, y = value, color = Admin2, fill = Admin2))+
  geom_line(size = 1.5)+
  geom_point(size = 3)+
  geom_vline(data = NULL, xintercept = as.Date("3/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick4')+
  annotate('text', x = as.Date("3/20/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*9/10,label = 'NYS on Pause',
           color = 'firebrick4', fontface = c('bold'))+
  geom_vline(data = NULL, xintercept = as.Date("3/1/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick3')+
  annotate('text', x = as.Date("3/1/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*9/10,label = 'First Case in NY',
           color = 'firebrick3', fontface = c('bold'))+
  # geom_vline(data = NULL, xintercept = as.Date("2/24/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick2')+
  #annotate('text', x = as.Date("2/24/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*9/10,label = 'Trump - "[COVID] nothing to worry about"',
  #         color = 'firebrick2', fontface = c('bold'), hjust = -1)+
  geom_vline(data = NULL, xintercept = as.Date("1/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick1')+
  annotate('text', x = as.Date("1/20/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*9/10,label = 'First case in US',
           color = 'firebrick1', fontface = c('bold'),hjust = -.05)+
  ylab('Count')+
  xlab('Date')+
  scale_y_continuous(label = scales::comma,
                     breaks = seq(from = 0, to = 200000, by = 5000),
                     expand = c(0,0))+
  scale_x_date(date_breaks = '14 days',
               date_labels = "%m/%d")+
  transition_reveal(date)+
  theme_bw()+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = 'bottom')

animate(DSonly, nframes = 100, renderer = gifski_renderer("Downstate.gif")) ## Gif 


##Downstate Log Chart##

dat.melt.DS.log <- dat.melt.DS
dat.melt.DS.log$value<- ifelse(dat.melt.DS$value == 0, .001,dat.melt.DS$value) ## Imputes small values so they plot


DSonlylog <-
  ggplot(dat.melt.DS.log, aes(x = date, y = value, color = Admin2, fill = Admin2))+
  geom_line(size = 1.5)+
  geom_point(size = 3)+
  geom_vline(data = NULL, xintercept = as.Date("3/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick4')+
  annotate('text', x = as.Date("3/20/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*7/10,label = 'NYS on Pause',
           color = 'firebrick4', fontface = c('bold'))+
  geom_vline(data = NULL, xintercept = as.Date("3/1/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick3')+
  annotate('text', x = as.Date("3/1/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*7/10,label = 'First Case in NY',
           color = 'firebrick3', fontface = c('bold'))+
  #  geom_vline(data = NULL, xintercept = as.Date("2/24/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick2')+
  #annotate('text', x = as.Date("2/24/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*9/10,label = 'Trump - "[COVID] nothing to worry about"',
  #        color = 'firebrick2', fontface = c('bold'), hjust = -1)+
  geom_vline(data = NULL, xintercept = as.Date("1/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick1')+
  annotate('text', x = as.Date("1/20/20",'%m/%d/%y'), y = max(dat.melt.DS$value)*7/10,label = 'First case in US',
           color = 'firebrick1', fontface = c('bold'),hjust = -.05)+
  ggtitle('Down State New York Log Cases')+
  ylab('Log(Count)')+
  xlab('Date')+
  transition_reveal(date)+
  scale_y_log10(expand = c(0,0))+
  scale_x_date(date_breaks = '14 days',
               date_labels = "%m/%d")+
  theme_bw()+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = 'bottom')


animate(DSonlylog, nframes = 100, renderer = gifski_renderer("DownstateLog.gif"))


RateGrowthDS <- dat.melt.DS %>%
  group_by(Admin2) %>%
  mutate(Diff_growth = value - lag(value),
         Rate_growth = Diff_growth/lag(value)*100)




RateGrowthDS <- RateGrowthDS %>%
  group_by(Admin2) %>%
  mutate(Rate_growth_3day = (Rate_growth + lag(Rate_growth) + lag(Rate_growth,2))/3)



LongIsland <- c('Nassau', 'Suffolk')


RateGrowthDS <- filter(RateGrowthDS, value > 0 & Admin2 %in% LongIsland)



LIOnlyRG <-
  ggplot(RateGrowthDS, aes(x = date, y = Rate_growth/100, fill = Admin2)) + geom_tile(aes(height = Rate_growth/100, y = Rate_growth/100/2, width = .9), alpha = .9, position='dodge')+
  scale_x_date(date_breaks = '7 days',
               date_labels = "%m/%d")+
  scale_y_continuous(label = scales::percent,
                     breaks = seq(from = 0, to = 10000, by = .25),
                     expand = c(0,0))+
  geom_vline(data = NULL, xintercept = as.Date("3/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick4')+
  annotate('text', x = as.Date("3/20/20",'%m/%d/%y'), y = .1,label = 'NYS on Pause',
           color = 'firebrick4', fontface = c('bold'))+
  labs(title='{closest_state}') +
  ylab('% Increase in Confirmed Cases')+
  xlab('Date')+
  transition_states(date,transition_length = 4, state_length = 5)+
  ease_aes("cubic-in-out") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = 'bottom')
animate(LIOnlyRG, nframes = 100, renderer = gifski_renderer("LIOnlyRG.gif"))

RateGrowthDS$rg.lim <- ifelse(RateGrowthDS$Rate_growth> 100, 100, RateGrowthDS$Rate_growth)

LIOnlyRGstatic <-
  ggplot(RateGrowthDS, aes(x = date, y = rg.lim/100, fill = Admin2)) + geom_tile(aes(height = rg.lim/100, y = rg.lim/100/2, width = .9), alpha = .9, position='dodge')+
  scale_x_date(date_breaks = '3 days',
               date_labels = "%m/%d")+
  scale_y_continuous(label = scales::percent,
                     breaks = seq(from = 0, to = 10000, by = .05),
                     limits = c(0,1),
                     expand = c(0,0))+
  geom_vline(data = NULL, xintercept = as.Date("3/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick4')+
  facet_wrap(.~Admin2, ncol = 1)+
  annotate('text', x = as.Date("3/20/20",'%m/%d/%y'), y = .1,label = 'NYS on Pause',
           color = 'firebrick4', fontface = c('bold'))+
  labs(title='Day-by-day Confirmed Case Growth Rate -- Long Island Only') +
  ylab('% Increase in Confirmed Cases')+
  xlab('Date')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = 'bottom')


ggsave('LIonlyRGstatic.jpg', plot = LIOnlyRGstatic)


##=============================---Deaths---========================================================##

dat.death <- read_excel('USCOVID_TimeSeries.xlsx',sheet = 'covid_deaths')


dat.death$iso2 <- NULL
dat.death$iso3 <- NULL
dat.death$code3 <- NULL
dat.death$FIPS <- NULL
dat.death$Country_Region <- NULL ##Removes unnecessary fields


dat.death.melt <- melt(dat.death, id.vars = c('UID','Admin2','Province_State', 'Lat','Long_','Combined_Key','Population'))
dat.death.melt$date <- dat.death.melt$variable
dat.death.melt$variable <- NULL

dat.death.melt$value <- as.numeric(dat.death.melt$value)
#dat.melt$date <- gsub("X","0",dat.melt$date)



dat.death.melt$date <- as.Date(dat.death.melt$date,"%m/%d/%y") ##Sets date to date type

dat.death.melt.NY <- dat.death.melt[dat.death.melt$Province_State == 'New York',] ##Removes all other States

dat.death.melt.LI <- dat.death.melt.NY[dat.death.melt.NY$Admin2 == 'Suffolk' | dat.death.melt.NY$Admin2 == 'Nassau',]


RateGrowthDeath.LI <-
  dat.death.melt.LI %>%
  group_by(Admin2) %>%
  mutate(Diff_growth = value - lag(value),
         Rate_growth = Diff_growth/lag(value)*100)

RateGrowthDeath.LI <- RateGrowthDeath.LI[RateGrowthDeath.LI$value > 0,]


LIonlyRGstaticdeath <-
  ggplot(RateGrowthDeath.LI, aes(x = date, y = Rate_growth/100, fill = Admin2, color = Admin2))+
  geom_bar(stat='identity', position = 'dodge')+
  scale_x_date(date_breaks = '1 days',
               date_labels = "%m/%d")+
  facet_wrap(.~Admin2, ncol = 1)+
  scale_y_continuous(label = scales::percent,
                     expand = c(0,0))+
  geom_vline(data = NULL, xintercept = as.Date("3/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick4')+
  annotate('text', x = as.Date("3/20/20",'%m/%d/%y'), y = 1.5,label = 'NYS on Pause',
           color = 'firebrick4', fontface = c('bold'))+
  labs(title='Day-by-day Death Growth Rate -- Long Island Only') +
  ylab('% Increase in Deaths')+
  xlab('Date')+
  theme_bw()+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = 'bottom')

ggsave('LIonlyRGstaticdeath.jpg', plot = LIonlyRGstaticdeath, width = 15, height = 15)





LIonlyRG3day <- 
  ggplot(RateGrowthDS, aes(x = date, y = Rate_growth_3day/100, color = Admin2, fill = Admin2))+
  geom_point(aes(x = date, y = Rate_growth/100, color = Admin2, fill = Admin2),alpha = .5)+
  geom_line(size = 1.5)+
  geom_vline(data = NULL, xintercept = as.Date("3/20/20",'%m/%d/%y'),linetype = 'dashed', color = 'firebrick4')+
  scale_x_date(date_breaks = '2 days',
               date_labels = "%m/%d")+
  scale_y_continuous(label = scales::percent,
                     expand = c(0,0))+
  annotate('text', x = as.Date("3/20/20",'%m/%d/%y'), y = 1.5,label = 'NYS on Pause',
           color = 'firebrick4', fontface = c('bold'))+
  labs(title='3-Day Moving Average Confirmed Cases -- Long Island Only') +
  ylab('% Increase in Cases - 3 day average')+
  xlab('Date')+
  theme_bw()+
  theme(plot.title = element_text(hjust = .5),
        legend.title = element_blank(),
        legend.position = 'bottom')

ggsave('LIonlyRG3day.jpg', plot = LIonlyRG3day, width = 15, height = 15)


## --- MAPS --- #

dat.death.melt.NY <- dat.death.melt.NY[dat.death.melt.NY$Lat != 0,]
dat.melt.NY$Lat <- as.numeric(dat.melt.NY$Lat)
dat.melt.NY$Long_ <- as.numeric(dat.melt.NY$Long_)
dat.melt.NY <- dat.melt.NY[dat.melt.NY$Lat != 0,]

sum.ny.data.deaths <- dat.death.melt.NY %>%
  group_by(Admin2, Lat, Long_, Population) %>%
  summarise(TotalDeaths = max(value))

sum.ny.data <- dat.melt.NY %>%
  group_by(Admin2, Lat, Long_) %>%
  summarise(Total = max(value))


sum.ny.data <- merge(sum.ny.data, sum.ny.data.deaths, by = c('Admin2','Lat','Long_'))
sum.ny.data <- sum.ny.data[sum.ny.data$Total > 0, ]

sum.ny.data$pct <- round(sum.ny.data$Total/sum.ny.data$Population*100,2)

sum.ny.data$pctrank <- percent_rank(sum.ny.data$pct)



qpal <- colorNumeric("YlOrRd", sum.ny.data$pct, n = 3)

covidny <- 
  leaflet(data = sum.ny.data) %>%
  addProviderTiles('CartoDB.DarkMatterNoLabels')%>%
  addCircleMarkers(lng = ~Long_, 
                   lat = ~Lat,
                   popup = ~paste0(Admin2,
                                   '<br/>Cases: ', format(Total,big.mark=","),
                                   '<br/>Deaths:', format(TotalDeaths,big.mark=","),
                                   '<br/>% of Pop Infected: ',paste0(pct,"%")),
                   fillColor = ~qpal(pct),
                   color = ~qpal(pct), fillOpacity = .7,
                   radius = ~5 + pctrank*15,
                   label = ~Admin2)%>%
  addLegend("bottomleft", pal = qpal, values = ~pct,
            title = "% of Pop Infected")%>%
  fitBounds(lng1 = -79.8, lng2 = -72.7, lat1 = 40.5, lat2 = 44.5)

saveWidget(covidny, file="covidny.html")

#library(rsconnect)

#result <- rpubsUpload("Covid-19 Cases and Deaths - New York", "covidny.html", originalDoc = NULL)

#updateResult <- rpubsUpload("COVID-19 Cases and Deaths - New York", "covidny.html", result$id)

#browseURL(updateResult$continueUrl) #Slug - c35
