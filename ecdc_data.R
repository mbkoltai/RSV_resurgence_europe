# settings
rm(list=ls())
library(tidyverse); library(wpp2019); library(RcppRoll); library(lubridate); library(qs) # library(here)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); getwd()
source("fcns/fcns.R"); data(pop)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# non-sentinel

# total number of tests
epi_yr_start_wk<-28
ECDC_surveillance_data_test_number <- 
  read_csv("data/ecdc_data/ECDC_surveillance_data_Respiratory_Syncytial_Virus_total_spec_tested.csv") %>% 
  select(!c(TxtValue,Unit)) %>% 
  mutate(year=as.numeric(substr(Time,1,4)), week=as.numeric(substr(Time,7,8)),
  date=as.Date(paste(year,week,1, sep="-"),"%Y-%U-%u"),
  epi_year=ifelse(as.numeric(week)>=epi_yr_start_wk,year,year-1)) %>%
  rename(n_test=NumValue) %>% filter(!is.na(n_test)  & !RegionName %in% "EU/EEA")

pop_ecdc <- pop %>% select(c(name,`2020`)) %>% 
  filter(name %in% unique(ECDC_surveillance_data_test_number$RegionName)) %>%
  rename(popul=`2020`) %>% mutate(popul=popul*1e3)

# median/mean testing numbers
mean_test_nums <- left_join(ECDC_surveillance_data_test_number,
                            pop_ecdc %>% rename(RegionName=name)) %>% 
  group_by(RegionName,epi_year) %>% 
  mutate(peak_date=min(date[n_test==max(n_test)])) %>% 
  filter(date>=peak_date-10*7 & date<=peak_date+10*7) %>%
  summarise(mean_test=mean(n_test),max_test=max(n_test),
            n_weeks=n(),popul=unique(popul)) %>% filter(n_weeks>11) %>%
  group_by(RegionName) %>% 
  summarise(n_yr=n(),
            mean_test_around_peak=mean(mean_test),
            mean_tests_at_peak=mean(max_test),
            tests_per1e6=1e6*mean_test_around_peak/unique(popul))

cntrs_test_lt_100 <- mean_test_nums %>%  filter(mean_test_around_peak>100 & tests_per1e6>25 & n_yr>2)

num_tests <- ECDC_surveillance_data_test_number %>% 
  filter(date>=as.Date("2016-09-15") & RegionName %in% cntrs_test_lt_100$RegionName) 

# PLOT total # tests
ggplot(num_tests) + geom_col(aes(x=Time,y=n_test,group=1)) + facet_wrap(~RegionName,scales="free_y")  + 
  geom_vline(data=num_tests %>% 
               filter(week %in% 1),aes(xintercept=Time),size=1/3,linetype="dashed",color="red") +
  geom_rect(data=num_tests %>% filter(week>=40|week<=14) %>% 
              group_by(RegionName,epi_year) %>% 
              summarise(start=max(Time),end=min(Time)),
            aes(xmin=end,xmax=start,ymin=0,ymax=Inf),color=NA,fill="red",alpha=0.1) +
  theme_bw() + standard_theme + theme(strip.text=element_text(size=14)) +
  scale_x_discrete(breaks=every_nth(n=14)) + 
  scale_y_continuous(expand=expansion(0.01,0)) + xlab("") + ylab("# tests")

# save
# ggsave(paste0("ecdc_rsv_nonsentinel_testnumber_min_test_no",test_nr_min,".png"),
#   width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# % positive tests
posit_data_file <- "data/ecdc_data/ECDC_surveillance_data_Respiratory_Syncytial_Virus_nonsent_positivity.csv"
epi_yr_start_wk=28
ECDC_surveillance_data_positivity <- read_csv(posit_data_file) %>% select(!c(TxtValue,Unit)) %>% 
  mutate(year=as.numeric(substr(Time,1,4)),week=as.numeric(substr(Time,7,8)),
         date=as.Date(paste(year,week,1, sep="-"),"%Y-%U-%u"),
         epi_year=ifelse(as.numeric(week)>=epi_yr_start_wk,year,year-1),
         NumValue=as.numeric(NumValue)) %>% filter(!is.na(NumValue))

nonsent_posit_filt <- ECDC_surveillance_data_positivity %>% 
  filter(date>=as.Date("2016-09-01") & !RegionName %in% "EU/EEA" & 
           RegionName %in% unique(num_tests$RegionName))

# seasons as defined as 10% of peak value
seas_thresh_perc=0.1
posit_season_lims <- nonsent_posit_filt %>% group_by(RegionName,epi_year) %>% arrange(date) %>% 
  mutate(smooth_posit=roll_mean(NumValue,n=5,na.rm=T,fill=T)) %>% filter(epi_year<=2019) %>% 
  group_by(RegionName,epi_year) %>% mutate(data_wks=sum(NumValue>1)) %>% filter(data_wks>5) %>% 
  summarise(max_val=max(smooth_posit),peak_week=week[smooth_posit==max(smooth_posit)],
            peak_date=date[smooth_posit==max(smooth_posit)],
            start_week=min(date[smooth_posit>=seas_thresh_perc*max(smooth_posit,na.rm=T)],na.rm=T),
            end_week=max(date[smooth_posit>=seas_thresh_perc*max(smooth_posit,na.rm=T)],na.rm=T),
            peak_week=ifelse(peak_week>26,peak_week-53,peak_week)) %>%
  # mean season limits
  group_by(RegionName) %>% summarise(mean_peak=mean(max_val),peak_week=mean(peak_week),
                                     peak_week=ifelse(peak_week<0,peak_week+53,peak_week),
                                     start_from_peak_wk=mean(as.numeric(peak_date-start_week))/7,
                                     end_from_peak_wk=mean(as.numeric(end_week-peak_date))/7) %>%
  mutate(epi_year_start_wk=ifelse(peak_week+26<=53,peak_week+26,peak_week+26-53))

# create peaks dataframe
l <- lapply(2016:2022, function(x) as.Date(paste(x,round(posit_season_lims$peak_week),1, sep="-"),"%Y-%U-%u"))
names(l)=as.character(2016:2022)
peak_weeks_years <- bind_rows(l) %>% mutate(RegionName=posit_season_lims$RegionName,
                                            start_from_peak_wk=posit_season_lims$start_from_peak_wk,
                                            end_from_peak_wk=posit_season_lims$end_from_peak_wk) %>% 
  pivot_longer(!c(RegionName,start_from_peak_wk,end_from_peak_wk)) %>% rename(peak_week=value) %>% 
  mutate(season_start=peak_week-start_from_peak_wk*7,season_stop=peak_week+end_from_peak_wk*7) %>%
  select(!c(start_from_peak_wk,end_from_peak_wk,name)) %>%
  filter(season_start>=min(nonsent_posit_filt$date) & peak_week<=as.Date("2022-05-01") ) %>%
  mutate(epi_year=ifelse(week(peak_week)<27,year(peak_week)-1,year(peak_week)))

# correct epi_year limits
nonsent_posit_filt <- left_join(nonsent_posit_filt, posit_season_lims %>%
                                  select(!c(mean_peak,peak_week,start_from_peak_wk,end_from_peak_wk))) %>% 
  mutate(epi_year=ifelse(week>=epi_year_start_wk,year(date),year(date)-1))

# plot positivity with seasons
ggplot(nonsent_posit_filt %>% group_by(RegionName,epi_year) %>% 
         arrange(date) %>% mutate(smooth_posit=roll_mean(NumValue,n=3,na.rm=T,fill=NA))) + 
  geom_col(aes(x=date,y=NumValue,group=1)) + geom_line(aes(x=date,y=smooth_posit,group=1)) +  
  facet_wrap(~RegionName,scales="free_y") +
  geom_vline(data=peak_weeks_years,aes(xintercept=peak_week),size=1/2.5,linetype="dashed",color="red") +
  geom_rect(data=peak_weeks_years,aes(xmax=season_stop,xmin=season_start,
                                      ymin=0,ymax=Inf),color=NA,fill="red",alpha=0.1) +
  xlab("") + ylab("% positive") + 
  scale_x_date(date_breaks="3 month",expand=expansion(0.01,0)) + 
  scale_y_continuous(expand=expansion(0.01,0)) + 
  theme_bw() + standard_theme + theme(strip.text=element_text(size=14))
# nonsentinel positivity
# ggsave(paste0("ecdc_rsv_nonsentinel_positivity.png"),width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# nr of positive samples

ECDC_surveillance_data_nonsent <- 
  left_join(
            read_csv("data/ecdc_data/ECDC_surveillance_data_Respiratory_Syncytial_Virus_nonsent_detections.csv") %>% 
              select(!c(TxtValue,Unit)) %>% 
              mutate(year=as.numeric(substr(Time,1,4)),
                     week=as.numeric(substr(Time,7,8)),
                     date=as.Date(paste(year,week,1, sep="-"),"%Y-%U-%u"),
                     epi_year=ifelse(as.numeric(week)>=40,year,year-1)),
            num_tests %>% select(c(Time,RegionName,n_test,year,week,epi_year)),
            by=c("RegionName","epi_year","Time","year","week"))

nonsent_positive_samples <- ECDC_surveillance_data_nonsent %>% 
  filter(date>=as.Date("2016-09-15") & !RegionName %in% "EU/EEA" & 
           RegionName %in% c(unique(num_tests$RegionName),"Netherlands"))

# seasonal peaks
peak_weeks_smoothed_nr_positvs <- nonsent_positive_samples %>% filter(epi_year<2020) %>% 
  group_by(RegionName,epi_year) %>% arrange(date) %>%
  mutate(smooth_posit=roll_mean(NumValue,n=5,na.rm=T,fill=T)) %>% 
  group_by(RegionName,epi_year) %>% 
  summarise(peak_week_smooth=max(smooth_posit),
            date=date[smooth_posit==max(smooth_posit)],
            week=week[smooth_posit==max(smooth_posit)],
            week_transp=ifelse(week>20,week-54,week)) %>% 
  group_by(RegionName) %>% 
  summarise(mean_peak_week=mean(week_transp,na.rm=T),
            mean_peak_week=ifelse(mean_peak_week<0,mean_peak_week+54,mean_peak_week))
# create peaks dataframe
l <- lapply(2016:2023, function(x) 
  as.Date(paste(x,round(peak_weeks_smoothed_nr_positvs$mean_peak_week),1, sep="-"),"%Y-%U-%u"))
names(l)=as.character(2016:2023)
peak_weeks_year_nr_positvs <- bind_rows(l) %>% 
  mutate(RegionName=peak_weeks_smoothed_nr_positvs$RegionName) %>% 
  pivot_longer(!RegionName) %>% 
  mutate(week=week(value),season_start=value-10*7,season_stop=value+10*7,name=as.numeric(name)) %>% 
  rename(peak=value) %>% 
  filter(season_start>=min(nonsent_positive_samples$date) & peak<=as.Date("2023-05-01") ) # 

# PLOT
ggplot(nonsent_positive_samples %>% filter(!RegionName %in% c("Latvia","Estonia")) %>%
         complete(date,RegionName) ) + #  ) #  & date>=as.Date("2017-09-15")
  geom_line(aes(x=date,y=NumValue,group=1),size=1/3) + # geom_col(aes(x=date,y=NumValue,group=1)) +
  facet_wrap(~RegionName,scales="free_y") + 
  scale_x_date(date_breaks="3 month",expand=expansion(0.01,0)) + scale_y_continuous(expand=expansion(0.01,0)) +
  geom_vline(data=peak_weeks_year_nr_positvs %>% filter(!RegionName %in% c("Latvia","Estonia")),
             aes(xintercept=peak),size=1/2.5,linetype="dashed",color="red") +
  geom_rect(data=peak_weeks_year_nr_positvs %>% 
              filter(!RegionName %in% c("Latvia","Estonia")), # season_start>=as.Date("2017-09-15")
            aes(xmin=season_start,xmax=season_stop,ymin=0,ymax=Inf),fill="red",alpha=0.1) +
  xlab("") + ylab("# positives") + theme_bw() + standard_theme + theme(strip.text=element_text(size=14))
# geom_text(data=left_join(peak_weeks_smoothed_nr_positvs %>% 
#                           mutate(mean_peak_week=round(mean_peak_week)),
#                           nonsent_positive_samples %>% group_by(RegionName) %>% 
#                           summarise(maxval=max(NumValue))),
#           aes(y=maxval*0.9,label=paste0("peak wk: ",mean_peak_week)),
#           x=min(nonsent_positive_samples$date)+25*30) +

# nonsentinel
ggsave(paste0("data/ecdc_data/ecdc_rsv_nonsentinel_detections.png"),width=38,height=22,units="cm")

# how much earlier did off-season outbreaks happen?
# resurgence timing # epi_year_start_wk
resurgence_start <- left_join(
  left_join(nonsent_posit_filt %>% select(!year), 
            posit_season_lims %>% select(!c(start_from_peak_wk,end_from_peak_wk)),
            by=c("RegionName","epi_year_start_wk")) %>% 
    group_by(RegionName,epi_year) %>% arrange(date) %>% 
    mutate(smooth_posit=roll_mean(NumValue,n=3,fill=NA,na.rm=T)) %>% group_by(RegionName) %>%
    summarise(date_resurg=min(date[smooth_posit>=mean_peak/3 & epi_year>=2020],na.rm=T),
              date_last_pre_resurg=max(date[smooth_posit>=mean_peak/3 & epi_year<=2019],na.rm=T),
              epi_year_start_wk=unique(epi_year_start_wk),
              epi_year=min(epi_year[smooth_posit>=mean_peak/3 & epi_year>=2020],na.rm=T)) %>%
    mutate(week_resurg=week(date_resurg)),
  peak_weeks_years %>% filter(epi_year==2021) %>% select(!epi_year),by=c("RegionName")) %>%
  mutate(early_outbreak=date_resurg<season_start & date_resurg>date_last_pre_resurg,
         # resurg_epiyear=ifelse(week_resurg<epi_year_start_wk,year(date_resurg)-1,year(date_resurg)),
         forward_shift=ifelse(early_outbreak,peak_week-date_resurg,NA))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# RKI (Germany) data
inzidenz_anteile_neu_2015_2021 <- read_csv("../../data/inzidenz_anteile_neu_2015_2021.csv")

# PLOT
ggplot(inzidenz_anteile_neu_2015_2021 %>%
         filter(!AGE.GROUP %in% NA & !CAL.WEEK %in% NA & grepl("mo",AGE.GROUP) & KHS_Fall==1) %>%
         mutate(epi_year=ifelse(CAL.WEEK<39,YEAR-1,YEAR),
                epi_week=ifelse(CAL.WEEK>=39,CAL.WEEK-38,CAL.WEEK+15)) # %>%
       # group_by(epi_year,epi_week) %>% summarise(INCID_1000=sum(INCID_1000)) 
) + # filter(epi_year==2017)
  geom_line(aes(x=epi_week,y=INCID_1000,color=factor(epi_year)),size=1/3) + # ,linetype="dashed"
  geom_point(aes(x=epi_week,y=INCID_1000,color=factor(epi_year)),size=1/3) +
  facet_wrap(~AGE.GROUP,scales="free_y") + xlab("epi-week (1=w39)") + ylab("incidence/1000") + theme_bw() + 
  standard_theme + theme(strip.text=element_text(size=14))


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# sentinel data (very small sample sizes)
#
# ECDC_surveillance_data_sent <- read_csv("ECDC_surveillance_data_Respiratory_Syncytial_Virus_sentinel.csv") %>%
#   mutate(year=substr(Time,1,4),week=substr(Time,7,8)) 
# 
# ECDC_surveillance_data_sent %>% group_by(RegionName,year) %>% 
#   mutate(yr_peaks=max(NumValue)) %>% group_by(RegionName) %>% filter(median(yr_peaks)>5 & year>=2017) %>%
#   ggplot() + geom_line(aes(x=Time,y=NumValue,group=1)) + facet_wrap(~RegionName,scales="free_y")  + 
#   scale_x_discrete(breaks=every_nth(n=13)) + xlab("") +
#   theme_bw() + standard_theme + theme(axis.text.x=element_text(size=7,angle=90))
# # save
# ggsave(paste0("ecdc_sentinel.png"),width=30,height=22,units="cm")

# identify seasonal peaks
# peak_weeks_smoothed <- nonsent_posit_filt %>% filter(epi_year<2020) %>% 
#   group_by(RegionName,epi_year) %>% arrange(date) %>% 
# mutate(smooth_posit=roll_mean(NumValue,n=5,na.rm=T,fill=T)) %>% 
#   group_by(RegionName,epi_year) %>% 
#       summarise(peak_week_smooth=max(smooth_posit),date=date[smooth_posit==max(smooth_posit)],
#             week=week[smooth_posit==max(smooth_posit)],week_transp=ifelse(week>20,week-53,week)) %>% 
#   group_by(RegionName) %>% summarise(mean_peak_week=mean(week_transp,na.rm=T),
#                                      mean_peak_week=ifelse(mean_peak_week<0,mean_peak_week+53,mean_peak_week))
# timing of off-season outbreak
# posit_mean_peak <- nonsent_posit_filt %>% filter(epi_year<=2019) %>% group_by(RegionName,epi_year) %>% 
#   mutate(data_wks=sum(NumValue>1)) %>% filter(data_wks>5) %>% 
#   summarise(max_val=max(NumValue),week=week[NumValue==max(NumValue)],week=ifelse(week>26,week-52,week)) %>% 
#   group_by(RegionName) %>% summarise(mean_peak=mean(max_val),peak_week=mean(week),
#                                      peak_week=round(ifelse(peak_week<0,peak_week+52,peak_week)),
#                                      epi_year_start_wk=ifelse(peak_week-26>0,peak_week-26,peak_week-26+53))
