# settings
rm(list=ls())
library(tidyverse); library(wpp2019); library(RcppRoll); library(lubridate); library(qs) # library(here)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("fcns/fcns.R"); data(pop)
# standard_theme<-theme(plot.title=element_text(hjust=0.5,size=16),
#   axis.text.x=element_text(size=9,angle=90,vjust=1/2),axis.text.y=element_text(size=9),
#   axis.title=element_text(size=14), text=element_text(family="Calibri"))
# panel.grid=element_line(linetype="dashed",colour="black",size=0.1)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# non-sentinel

# total number of tests
epi_yr_start_wk<-28
ECDC_surveillance_data_test_number <- 
  read_csv("ECDC_surveillance_data_Respiratory_Syncytial_Virus_total_spec_tested.csv") %>% 
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
posit_data_file <- "ECDC_surveillance_data_Respiratory_Syncytial_Virus_nonsent_positivity.csv"
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
  left_join(read_csv("ECDC_surveillance_data_Respiratory_Syncytial_Virus_nonsent_detections_2022oct25.csv") %>% 
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
l <- lapply(2016:2022, function(x) 
  as.Date(paste(x,round(peak_weeks_smoothed_nr_positvs$mean_peak_week),1, sep="-"),"%Y-%U-%u"))
names(l)=as.character(2016:2022)
peak_weeks_year_nr_positvs <- bind_rows(l) %>% 
  mutate(RegionName=peak_weeks_smoothed_nr_positvs$RegionName) %>% 
  pivot_longer(!RegionName) %>% 
  mutate(week=week(value),season_start=value-10*7,season_stop=value+10*7,name=as.numeric(name)) %>% 
  rename(peak=value) %>% 
  filter(season_start>=min(nonsent_positive_samples$date) & peak<=as.Date("2022-05-01") ) # 

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
ggsave(paste0("ecdc_rsv_nonsentinel_detections.png"),width=38,height=22,units="cm")

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

# Oxford stringency index 
# OxCGRT_latest <- read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")
# process
OxCGRT_input <- fcn_get_OxCGRT(
  OxCGRT_url="https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv",
  cntr_name=resurgence_start$RegionName,
  sel_cols=c("date","StringencyIndex","NPI_on","CountryName","CountryCode",
             "C1_School closing","C2_Workplace closing"),
  start_date="2020-02-01") %>% group_by(CountryName) %>% 
  mutate(stringency_scaled_smoothed=1-roll_mean(OxCGRT_scaled,30,align="center",fill=NA)) %>% 
  filter(!is.na(CountryCode))

# plot
OxCGRT_input %>% filter(NPI_on==1 & date>=as.Date("2020-02-25") & date<as.Date("2021-12-01")) %>% 
  select(c(date,CountryName,StringencyIndex,`C1_School closing`)) %>% mutate(StringencyIndex=StringencyIndex/100,
                                                                             `C1_School closing`=`C1_School closing`/max(OxCGRT_input$`C1_School closing`)) %>% 
  pivot_longer(!c(date,CountryName)) %>%
  ggplot() + geom_line(aes(x=date,y=value,color=name)) + facet_wrap(~CountryName) + 
  scale_color_manual(values=c("black","blue")) + scale_x_date(date_breaks="2 month",expand=expansion(0.01,0)) + 
  geom_vline(data=resurgence_start %>% 
               rename(CountryName=RegionName),aes(xintercept=date_resurg),color="red") + 
  geom_vline(xintercept=as.Date("2021-01-01"),color="grey") +
  xlab("") + ylab("") + theme_bw() + standard_theme + 
  theme(strip.text=element_text(size=15),legend.title=element_blank(),
        legend.text=element_text(size=14),legend.position="top")
# ggsave
ggsave(paste0("resurgence_stringency_school_clos.png"),width=38,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# CoMix

# all URLs
# comix_url_list <- c(uk="https://zenodo.org/record/6542524",it="https://zenodo.org/record/6362888",
#   ch="https://zenodo.org/record/6542657",dk="https://zenodo.org/record/6362899",
#   sk="https://zenodo.org/record/6535357",at="https://zenodo.org/record/6362906",
#   hu="https://zenodo.org/record/6535344",ee="https://zenodo.org/record/6535313",
#   fr="https://zenodo.org/record/6362893",fi="https://zenodo.org/record/6542664",
#   si="https://zenodo.org/record/6362865",pl="https://zenodo.org/record/6362879",
#   nl="https://zenodo.org/record/4790347",es="https://zenodo.org/record/6362898",
#   be="https://zenodo.org/record/7086043")

# contact_common: https://zenodo.org/record/6362898/files/CoMix_es_contact_common.csv?download=1
# participant_common: https://zenodo.org/record/6362898/files/CoMix_es_participant_common.csv?download=1
# sday: https://zenodo.org/record/6362898/files/CoMix_es_sday.csv?download=1

# if COMIX datafile does not exist yet (this downloads files from Zenodo, takes some time!)
source("fcns/load_comix.R")
# if exists
comix_aggr_all <- read_csv("data/comix/comix_aggr_all.csv")

# plot
for (k_url in 1:length(comix_url_list)) {
  cntr_name=unique(comix_aggr_all$country)[k_url]
  plot_df <- comix_aggr_all %>% filter(metric %in% "value" &(country %in% cntr_name)&(!name %in% "all") & 
                                         !(part_age %in% "Prefer not to answer")) %>%
    mutate(part_age_num=as.numeric(gsub(".*-","",part_age)), 
           part_age_num=ifelse(is.na(part_age_num),0,part_age_num)+1)
  plot_df$part_age_num=as.numeric(factor(plot_df$part_age_num))
  proper_order=unlist(sapply(1:length(unique(plot_df$part_age_num)), 
                             function(x) which(unique(plot_df$part_age_num) %in% x)))
  plot_df$part_age=factor(plot_df$part_age, levels=unique(plot_df$part_age)[proper_order])
  plot_df %>% 
    ggplot(aes(x=date,y=value_mean,fill=name)) + ggtitle(cntr_name) +
    geom_bar(stat="identity",color="black",width=6) + facet_wrap(~part_age) + labs(fill="") +
    scale_x_date(date_breaks="month") + xlab("") + theme_bw() + standard_theme + theme(legend.position="top")
  # save
  ggsave(paste0("data/comix/plots/y_fixed/comix_mean_cont_",cntr_name,".png"),
         width=30*ifelse(cntr_name %in% "uk",1.3,1),height=22,units="cm")
  message(paste0("plotting ",cntr_name))
}

# # part_id
# comix_raw_participant_common <- read_csv("comix_raw_participant_common.csv")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# COVIDDATAHUB from Imperial https://github.com/YouGov-Data/covid-19-tracker

list_files_coviddatahub <- list.files("data/coviddatahub/cntr_data",pattern=".csv",full.names=T)
l_coviddatahub_mask_avoid=list(); l_coviddatahub_num_contact=list();  
mask_avoid=F; contact_num_flag=T
for (k_file in 1:length(list_files_coviddatahub)) {
  
  country_name=gsub("\\.csv","",gsub("data/coviddatahub/cntr_data/","",list_files_coviddatahub))[k_file]
  record_varname<-ifelse(country_name %in% "sweden","record","RecordNo")
  # masking variables
  if (mask_avoid){
    temp_coviddatahub <- read_csv(list_files_coviddatahub[k_file]) %>% 
      select(c(!!record_varname,endtime,qweek,weight,
               i12_health_1,i12_health_21,i12_health_22,i12_health_23,i12_health_24,i12_health_25,
               i6_health,i12_health_5,i12_health_6,i12_health_7,i12_health_8,i12_health_9,
               i12_health_10,i12_health_11,i12_health_12,i12_health_13,i12_health_14,
               i12_health_15,i12_health_16,i12_health_20)) 
    if (country_name %in% "sweden"){ temp_coviddatahub <- temp_coviddatahub %>% rename(RecordNo=record) }
    # selecting columns
    l_coviddatahub_mask_avoid[[k_file]] <- temp_coviddatahub %>% mutate(cntr=country_name) %>%
      pivot_longer(!c(RecordNo,endtime,qweek,cntr,weight)) %>% 
      mutate(value=factor(value,levels=c("Always","Frequently","Sometimes","Rarely","Not at all"))) %>% 
      pivot_wider(names_from=name,values_from=value) %>% 
      mutate(date=dmy(substr(endtime,1,nchar(endtime)-6)),qweek=as.numeric(gsub("week ","",qweek))) %>% 
      pivot_longer(!c(RecordNo,endtime,qweek,date,cntr,weight)) %>% filter(!is.na(value)) %>% 
      group_by(qweek,cntr,name,value) %>%
      summarise(date=max(date),n_mask=n(),n_mask_weighted=sum(weight)) %>% group_by(qweek,name,cntr) %>% 
      mutate(week_date=max(date),percentage=n_mask/sum(n_mask,na.rm=F)) %>% 
      mutate(name=case_when(name %in% "i12_health_1" ~ "mask_outside_home",
                            name %in% "i12_health_22" ~ "mask_grocery",
                            name %in% "i12_health_23" ~ "mask_shops",
                            name %in% "i12_health_24" ~ "mask_work",
                            name %in% "i12_health_25" ~ "mask_publ_transp",
                            name %in% "i12_health_21" ~ "mask_at_home",
                            # avoidance behaviours:
                            # i6_health: self-isolate when sick
                            # i12_health_5: avoided contact w/ ppl w/ symptoms
                            # i12_health_6: Avoided going out in general
                            # i12_health_7: avoided going to hosp/hc setting
                            # i12_health_8: Avoided taking public transport
                            # i12_health_9: Avoided working outside your home
                            # i12_health_10: Avoided letting your children go to school/ university
                            # i12_health_11: Avoided having guests to your home
                            # i12_health_12: Avoided small social gatherings (not more than 2 people),
                            # i12_health_13: Avoided medium-sized social gatherings (between 3 and 10 people)
                            # i12_health_14: Avoided large-sized social gatherings (more than 10 people)
                            # i12_health_15: Avoided crowded areas
                            # i12_health_16: Avoided going to shops
                            # i12_health_20: Avoided touching objects in public (e.g. elevator buttons or doors)
                            name %in% "i6_health" ~ "self_isol",
                            name %in% "i12_health_5" ~ "avoid_contact_sympt",
                            name %in% "i12_health_6" ~ "avoid_go_out",
                            name %in% "i12_health_7" ~ "avoid_hosp",
                            name %in% "i12_health_8" ~ "avoid_publ_transp",
                            name %in% "i12_health_9" ~ "avoid_work_outside",
                            name %in% "i12_health_10" ~ "avoid_let_child_school",
                            name %in% "i12_health_11" ~ "avoid_guests_home",
                            name %in% "i12_health_12" ~ "avoid_small_gath",
                            name %in% "i12_health_13" ~ "avoid_med_gath",
                            name %in% "i12_health_14" ~ "avoid_large_gath",
                            name %in% "i12_health_15" ~ "avoid_crowd_area",
                            name %in% "i12_health_16" ~ "avoid_shops",
                            name %in% "i12_health_20" ~ "avoid_touch_surf"))
  }
  ### ### ### ### ### 
  # numerical variables (contacts): 
  # i1_health: # physical contact w/ household members
  # i2_health: # physical contact w/ ppl outside household
  # i7a_health: # times did you leave home yestday? 
  if (contact_num_flag){
    temp_coviddatahub <- read_csv(list_files_coviddatahub[k_file]) %>% 
      select(c(!!record_varname,endtime,age,qweek,weight,i1_health,i2_health,i7a_health)) %>% 
      mutate(cntr=country_name)
    if (country_name %in% "sweden"){ temp_coviddatahub <- temp_coviddatahub %>% rename(RecordNo=record) }
    l_coviddatahub_num_contact[[k_file]] <- temp_coviddatahub %>% 
      mutate(date=dmy(substr(endtime,1,nchar(endtime)-6)),qweek=as.numeric(gsub("week ","",qweek))) %>% 
      pivot_longer(!c(RecordNo,endtime,qweek,date,cntr,weight)) %>% 
      filter(!is.na(value)) %>% group_by(qweek,cntr,name) %>%
      summarise(date=max(date),mean_num=mean(value),mean_num_weighted=mean(value*weight),
                median_num=median(value),median_num_weighted=median(value*weight)) %>%
      mutate(name=case_when(name %in% "i1_health" ~ "contact_in_hh",
                            name %in% "i2_health" ~ "contact_out_hh",
                            name %in% "i7a_health" ~ "left_home"))
  }
  message(k_file)
  if (k_file==length(list_files_coviddatahub)) {
    coviddatahub <- bind_rows(l_coviddatahub_mask_avoid)
    coviddatahub_num_contact <- bind_rows(l_coviddatahub_num_contact) 
  }
}

# write_csv(coviddatahub_num_contact,"data/coviddatahub/coviddatahub_num_contact.csv")
# write_csv(coviddatahub,"data/coviddatahub/coviddatahub_mask_avoid_aggreg.csv")

# plot yes/no % for masking + avoided X behaviours
for (k_plot in unique(coviddatahub$name)) {
  
  coviddatahub %>% filter(!is.na(percentage) & name %in% k_plot) %>% 
    group_by(qweek,cntr) %>% mutate(week_date=min(date)) %>%
    select(!date) %>% pivot_wider(names_from=value,values_from=percentage) %>%
    group_by(week_date,qweek,cntr) %>% 
    summarise(Always=min(Always,na.rm=T),Frequently=min(Frequently,na.rm=T),
              `Not at all`=min(`Not at all`,na.rm=T),Rarely=min(Rarely,na.rm=T),
              Sometimes=min(Sometimes,na.rm=T)) %>% 
    mutate(`always+frequently`=Always+Frequently,`sometimes+rarely+never`=`Not at all`+Rarely+Sometimes) %>%
    pivot_longer(!c(week_date,qweek,cntr)) %>% 
    filter(name %in% c("always+frequently","sometimes+rarely+never")) %>%
    mutate(name=factor(name,levels=rev(c("always+frequently","sometimes+rarely+never"))),
           year_week=paste0(year(week_date),"/w",week(week_date))) %>% 
ggplot(aes(x=week_date,y=value,fill=name),alpha=2/3) + 
    geom_bar(stat="identity",width=4) + # geom_line(size=1) + geom_point(shape=21) + #geom_col(width=5)+
    facet_wrap(~cntr) + scale_x_date(date_breaks="3 month",expand=expansion(0.01,0)) + 
    scale_y_continuous(expand=expansion(0.01,0)) + scale_fill_manual(values=c("blue","red2")) + 
    xlab("") + ylab(paste0(k_plot," (%)")) + theme_bw() + standard_theme + labs(fill="") +
    theme(axis.text.x=element_text(size=10),legend.position="top")
  # mask wearing %
  ggsave(paste0("data/coviddatahub/",k_plot,".png"),width=30,height=22,units="cm")
}

# time courses of masking components (work, publ transp etc)
var_sel=1
coviddatahub %>% filter(grepl("mask",name) & value %in% list("Always",c("Always","Frequently"))[[var_sel]]) %>% 
  group_by(cntr,qweek,name,week_date) %>%
  summarise(mask_w_n=sum(mask_w_n),percentage=sum(percentage)) %>%
  mutate(name=factor(name,levels=c("mask_outside_home","mask_work","mask_publ_transp",
                                   "mask_grocery","mask_shops","mask_at_home"))) %>%
  # group_by(year_week) %>% complete(cntr,name) %>%
  ggplot(aes(x=week_date,xend=week_date+14,yend=100*percentage,
             y=100*percentage,color=name,size=grepl("out",name))) +
  geom_segment(size=1) + facet_wrap(~cntr) + labs(size="",color="") + # 
  scale_x_date(date_breaks="3 month",expand=expansion(0.01,0)) + #scale_size_manual(values=c(1,1.4),guide="none") +
  scale_color_manual(values=c("black","blue","red","green","orange","grey")) + 
  xlab("") + ylab("mask wearing (%)") + theme_bw() + standard_theme + 
  theme(axis.text.x=element_text(size=10),legend.position = "top")
# mask wearing %
ggsave(paste0("data/coviddatahub/mask_wearing_",c("always_","always_frequently_")[var_sel],"all_cntrs.png"),
       width=32,height=20,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### 
# contact numbers

plot_var=c("mean_num_weighted","median_num_weighted")[2]; plot_var_type=1
coviddatahub_num_contact %>% filter(grepl(c("contact","left_home")[plot_var_type],name) & !cntr %in% "australia") %>%
ggplot(aes(x=date,y=get(plot_var),fill=name),alpha=2/3) + 
  geom_bar(stat="identity",width=3) + # geom_line(size=1) + geom_point(shape=21) + # geom_col(width=5) +
  facet_wrap(~cntr) + scale_x_date(date_breaks="3 month",expand=expansion(0.01,0)) + 
  scale_y_continuous(expand=expansion(0.01,0)) + scale_fill_manual(values=c("blue","red2")) + labs(fill="") +
  xlab("") + ylab(c("number of contacts","# times left home")[plot_var_type]) + theme_bw() + standard_theme + 
  theme(axis.text.x=element_text(size=10),legend.position="top",strip.text=element_text(size=14))
# contact numbers
ggsave(paste0("data/coviddatahub/",ifelse(plot_var_type==1,"contacts_","left_home_"),plot_var,".png"),
       width=32,height=20,units="cm")


### ### ### ### ### ### ### ### ### ### ### ### 
# single country (France)
# coviddatahub_france <- read_csv("../coviddatahub/france.csv") %>% 
#   select(c(RecordNo,endtime,qweek,i12_health_1, 
#            i12_health_21,i12_health_22,i12_health_23,i12_health_24,i12_health_25)) %>% 
#   pivot_longer(!c(RecordNo,endtime,qweek)) %>% 
#   mutate(value=factor(value,levels=c("Always","Frequently","Sometimes","Rarely","Not at all"))) %>% 
#   pivot_wider(names_from=name,values_from=value)
# # plot ALL vs subcategs
# coviddatahub_france %>% select(!endtime) %>% pivot_longer(!c(RecordNo,qweek,i12_health_1)) %>%
#   filter(!(is.na(i12_health_1)|is.na(value))) %>% # rename(other_var=name) %>% 
#   group_by(i12_health_1,name) %>% mutate(n_all=n()) %>% group_by(i12_health_1,name,value) %>%
#   summarise(n_comb=n(),freq_comb=n()/unique(n_all)) %>% 
#   mutate(name=case_when(name %in% "i12_health_21" ~ "home",
#                         name %in% "i12_health_22" ~ "grocery",
#                         name %in% "i12_health_23" ~ "clothes/footwear shops",
#                         name %in% "i12_health_24" ~ "work",
#                         name %in% "i12_health_25" ~ "publ. transport")) %>%
#   ggplot(aes(x=i12_health_1,y=freq_comb*100,fill=value)) + facet_wrap(~name) +
#   geom_bar(stat="identity",position=position_dodge(),color="black",size=1/5) + ggtitle("mask wearing in France") +
#   geom_vline(xintercept=(1:4)+1/2,size=1/2,linetype="dashed") + xlab("") + ylab("%") + 
#   scale_y_continuous(expand=expansion(0.01,0)) + labs(fill="") + theme_bw() + standard_theme
# # save
# ggsave("data/coviddatahub/mask_wearing_comps_FR.png",width=30,height=22,units="cm")
# subcategs correlate quite well with the aggregate index
### ### ### ### ### ### ### ### ### ### ### ### 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
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
# google mobility
# 
gmob_url<-
  "https://github.com/ActiveConclusion/COVID19_mobility/raw/master/google_reports/mobility_report_countries.csv"
google_mobility <- read_csv("mobility_report_europe.csv") %>% 
  filter(country %in% unique(num_tests$RegionName) & `sub region 1` %in% "Total") %>%
  select(!c(`sub region 1`,`sub region 2`,world_region))

google_mobility %>% pivot_longer(!c(date,country)) %>% group_by(name,country) %>% 
  mutate(smooth_value=roll_mean(value,7,align="center",fill=NA,na.rm=T)) %>%
  ggplot(aes(x=date,y=smooth_value,color=name)) + geom_line() + 
  scale_x_date(date_breaks="2 month",expand=expansion(0.01,0)) +
  geom_hline(yintercept=0,size=1/2) + 
  facet_wrap(~country,scales="free_y") + xlab("") + theme_bw() + standard_theme



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
