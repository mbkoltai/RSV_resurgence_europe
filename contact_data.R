### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# COVIDDATAHUB from Imperial https://github.com/YouGov-Data/covid-19-tracker

# list_files_coviddatahub <- list.files("data/coviddatahub/cntr_data",pattern=".csv",full.names=T)
# l_coviddatahub_mask_avoid=list(); l_coviddatahub_num_contact=list();  
# mask_avoid=F; contact_num_flag=T
# for (k_file in 1:length(list_files_coviddatahub)) {
#   
#   country_name=gsub("\\.csv","",gsub("data/coviddatahub/cntr_data/","",list_files_coviddatahub))[k_file]
#   record_varname<-ifelse(country_name %in% "sweden","record","RecordNo")
#   # masking variables
#   if (mask_avoid){
#     temp_coviddatahub <- read_csv(list_files_coviddatahub[k_file]) %>% 
#       select(c(!!record_varname,endtime,qweek,weight,
#                i12_health_1,i12_health_21,i12_health_22,i12_health_23,i12_health_24,i12_health_25,
#                i6_health,i12_health_5,i12_health_6,i12_health_7,i12_health_8,i12_health_9,
#                i12_health_10,i12_health_11,i12_health_12,i12_health_13,i12_health_14,
#                i12_health_15,i12_health_16,i12_health_20)) 
#     if (country_name %in% "sweden"){ temp_coviddatahub <- temp_coviddatahub %>% rename(RecordNo=record) }
#     # selecting columns
#     l_coviddatahub_mask_avoid[[k_file]] <- temp_coviddatahub %>% mutate(cntr=country_name) %>%
#       pivot_longer(!c(RecordNo,endtime,qweek,cntr,weight)) %>% 
#       mutate(value=factor(value,levels=c("Always","Frequently","Sometimes","Rarely","Not at all"))) %>% 
#       pivot_wider(names_from=name,values_from=value) %>% 
#       mutate(date=dmy(substr(endtime,1,nchar(endtime)-6)),qweek=as.numeric(gsub("week ","",qweek))) %>% 
#       pivot_longer(!c(RecordNo,endtime,qweek,date,cntr,weight)) %>% filter(!is.na(value)) %>% 
#       group_by(qweek,cntr,name,value) %>%
#       summarise(date=max(date),n_mask=n(),n_mask_weighted=sum(weight)) %>% group_by(qweek,name,cntr) %>% 
#       mutate(week_date=max(date),percentage=n_mask/sum(n_mask,na.rm=F)) %>% 
#       mutate(name=case_when(name %in% "i12_health_1" ~ "mask_outside_home",
#                             name %in% "i12_health_22" ~ "mask_grocery",
#                             name %in% "i12_health_23" ~ "mask_shops",
#                             name %in% "i12_health_24" ~ "mask_work",
#                             name %in% "i12_health_25" ~ "mask_publ_transp",
#                             name %in% "i12_health_21" ~ "mask_at_home",
#                             # avoidance behaviours:
#                             # i6_health: self-isolate when sick
#                             # i12_health_5: avoided contact w/ ppl w/ symptoms
#                             # i12_health_6: Avoided going out in general
#                             # i12_health_7: avoided going to hosp/hc setting
#                             # i12_health_8: Avoided taking public transport
#                             # i12_health_9: Avoided working outside your home
#                             # i12_health_10: Avoided letting your children go to school/ university
#                             # i12_health_11: Avoided having guests to your home
#                             # i12_health_12: Avoided small social gatherings (not more than 2 people),
#                             # i12_health_13: Avoided medium-sized social gatherings (between 3 and 10 people)
#                             # i12_health_14: Avoided large-sized social gatherings (more than 10 people)
#                             # i12_health_15: Avoided crowded areas
#                             # i12_health_16: Avoided going to shops
#                             # i12_health_20: Avoided touching objects in public (e.g. elevator buttons or doors)
#                             name %in% "i6_health" ~ "self_isol",
#                             name %in% "i12_health_5" ~ "avoid_contact_sympt",
#                             name %in% "i12_health_6" ~ "avoid_go_out",
#                             name %in% "i12_health_7" ~ "avoid_hosp",
#                             name %in% "i12_health_8" ~ "avoid_publ_transp",
#                             name %in% "i12_health_9" ~ "avoid_work_outside",
#                             name %in% "i12_health_10" ~ "avoid_let_child_school",
#                             name %in% "i12_health_11" ~ "avoid_guests_home",
#                             name %in% "i12_health_12" ~ "avoid_small_gath",
#                             name %in% "i12_health_13" ~ "avoid_med_gath",
#                             name %in% "i12_health_14" ~ "avoid_large_gath",
#                             name %in% "i12_health_15" ~ "avoid_crowd_area",
#                             name %in% "i12_health_16" ~ "avoid_shops",
#                             name %in% "i12_health_20" ~ "avoid_touch_surf"))
#   }
#   ### ### ### ### ### 
#   # numerical variables (contacts): 
#   # i1_health: # physical contact w/ household members
#   # i2_health: # physical contact w/ ppl outside household
#   # i7a_health: # times did you leave home yestday? 
#   if (contact_num_flag){
#     temp_coviddatahub <- read_csv(list_files_coviddatahub[k_file]) %>% 
#       select(c(!!record_varname,endtime,age,qweek,weight,i1_health,i2_health,i7a_health)) %>% 
#       mutate(cntr=country_name)
#     if (country_name %in% "sweden"){ temp_coviddatahub <- temp_coviddatahub %>% rename(RecordNo=record) }
#     l_coviddatahub_num_contact[[k_file]] <- temp_coviddatahub %>% 
#       mutate(date=dmy(substr(endtime,1,nchar(endtime)-6)),qweek=as.numeric(gsub("week ","",qweek))) %>% 
#       pivot_longer(!c(RecordNo,endtime,qweek,date,cntr,weight)) %>% 
#       filter(!is.na(value)) %>% group_by(qweek,cntr,name) %>%
#       summarise(date=max(date),mean_num=mean(value),mean_num_weighted=mean(value*weight),
#                 median_num=median(value),median_num_weighted=median(value*weight)) %>%
#       mutate(name=case_when(name %in% "i1_health" ~ "contact_in_hh",
#                             name %in% "i2_health" ~ "contact_out_hh",
#                             name %in% "i7a_health" ~ "left_home"))
#   }
#   message(k_file)
#   if (k_file==length(list_files_coviddatahub)) {
#     coviddatahub <- bind_rows(l_coviddatahub_mask_avoid)
#     coviddatahub_num_contact <- bind_rows(l_coviddatahub_num_contact) 
#   }
# }

coviddatahub_num_contact <- read_csv("data/coviddatahub/coviddatahub_num_contact.csv")
coviddatahub <- read_csv("data/coviddatahub/coviddatahub_mask_avoid_aggreg.csv")

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
  summarise(n_mask_weighted=sum(n_mask_weighted),percentage=sum(percentage)) %>%
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
# contact numbers (this is strange...increasing in spring/summer 2020)
# 
# plot_var=c("mean_num_weighted","median_num_weighted")[2]; plot_var_type=1
# coviddatahub_num_contact %>% filter(grepl(c("contact","left_home")[plot_var_type],name) & !cntr %in% "australia") %>%
#   ggplot(aes(x=date,y=get(plot_var),fill=name),alpha=2/3) + 
#   geom_bar(stat="identity",width=3) + # geom_line(size=1) + geom_point(shape=21) + # geom_col(width=5) +
#   facet_wrap(~cntr) + scale_x_date(date_breaks="3 month",expand=expansion(0.01,0)) + 
#   scale_y_continuous(expand=expansion(0.01,0)) + scale_fill_manual(values=c("blue","red2")) + labs(fill="") +
#   xlab("") + ylab(c("number of contacts","# times left home")[plot_var_type]) + theme_bw() + standard_theme + 
#   theme(axis.text.x=element_text(size=10),legend.position="top",strip.text=element_text(size=14))
# # contact numbers
# ggsave(paste0("data/coviddatahub/",ifelse(plot_var_type==1,"contacts_","left_home_"),plot_var,".png"),
#        width=32,height=20,units="cm")


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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# google mobility
# 
gmob_url<- "data/mobility/mobility_report_europe.csv"
# "https://github.com/ActiveConclusion/COVID19_mobility/raw/master/google_reports/mobility_report_countries.csv"
google_mobility <- read_csv("data/mobility/mobility_report_europe.csv") %>% 
  filter(country %in% c(unique(num_tests$RegionName),"United Kingdom") & `sub region 1` %in% "Total") %>%
  select(!c(`sub region 1`,`sub region 2`,world_region))

google_mobility %>% pivot_longer(!c(date,country)) %>% group_by(name,country) %>% 
  mutate(smooth_value=roll_mean(value,7,align="center",fill=NA,na.rm=T)) %>%
ggplot(aes(x=date,y=smooth_value,color=name)) + geom_line() + 
  scale_x_date(date_breaks="2 month",expand=expansion(0.01,0)) +
  geom_hline(yintercept=0,size=1/2) + 
  facet_wrap(~country,scales="free_y") + xlab("") + theme_bw() + standard_theme

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Oxford stringency index
OxCGRT_latest <- read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_nat_latest.csv")
# process
OxCGRT_input <- fcn_get_OxCGRT(
  # https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_nat_latest.csv
  OxCGRT_url="data/OxCGRT_nat_latest.csv", 
  cntr_name=c(resurgence_start$RegionName,"United Kingdom"),
  sel_cols=c("date","StringencyIndex_Average","CountryName","CountryCode","NPI_on",
             "C1M_School closing","C2M_Workplace closing"),
  start_date="2020-02-01") %>% group_by(CountryName) %>% 
  mutate(stringency_scaled_smoothed=1-roll_mean(OxCGRT_scaled,30,align="center",fill=NA)) %>% 
  filter(!is.na(CountryCode)) # %>% rename(StringencyIndex_Average)

# plot
OxCGRT_input %>% filter(NPI_on==1 & date>=as.Date("2020-02-25") & date<as.Date("2022-05-01")) %>% 
  select(c(date,CountryName,StringencyIndex_Average,`C1M_School closing`)) %>% 
  mutate(StringencyIndex_Average=StringencyIndex_Average/100,
         `C1M_School closing`=`C1M_School closing`/max(OxCGRT_input$`C1M_School closing`)) %>% 
  pivot_longer(!c(date,CountryName)) %>%
  ggplot() + geom_line(aes(x=date,y=value,color=name)) + facet_wrap(~CountryName) + 
  scale_color_manual(values=c("black","blue")) + 
  scale_x_date(date_breaks="2 month",expand=expansion(0.01,0)) + 
  geom_vline(data=resurgence_start %>% 
               rename(CountryName=RegionName),aes(xintercept=date_resurg),color="red") + 
  geom_vline(xintercept=as.Date(c("2021-01-01","2022-01-01")),color="grey") +
  xlab("") + ylab("") + theme_bw() + standard_theme + 
  theme(strip.text=element_text(size=15),legend.title=element_blank(),
        legend.text=element_text(size=14),legend.position="top")
# ggsave
ggsave(paste0("resurgence_stringency_school_clos.png"),width=38,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# CoMix

# England mean # contacts for entire period (data as csv received from C Jarvis)
# subset for plot
mean_cont_plot = read_csv("data/comix/2022-03-02_bs_means_2w_open.csv") %>% 
  filter(part_region %in% "All" & setting %in% "All" & 
           !(part_age_group %in% c("All-adults","All","5-17","60+","18-59")))
# order
x_age=as.numeric(sapply(sapply(unique(mean_cont_plot$part_age_group), strsplit,"-|\\+"),`[[`,1))
mean_cont_plot$part_age_group=factor(mean_cont_plot$part_age_group,
                                     levels=unique(mean_cont_plot$part_age_group)[order(x_age)])
ylim_dummy = mean_cont_plot %>% ungroup() %>% select(part_age_group) %>% distinct() %>% 
  arrange(part_age_group) %>% mutate(mid_date=as.Date("2020-04-01"),mean=c(rep(18,3),rep(7,6)))

# plot
mean_cont_plot %>% 
ggplot() + facet_wrap(~part_age_group,scales="free_y") +
  geom_line(aes(x=mid_date,y=mean)) + geom_point(aes(x=mid_date,y=mean),shape=21) + 
  geom_ribbon(aes(x=mid_date,ymin=lci,ymax=uci),alpha=1/4) +
  geom_point(data=ylim_dummy,aes(x=mid_date,y=mean),color=NA) +
  scale_x_date(expand=expansion(0.01,0),date_breaks="2 month") + 
  scale_y_continuous(expand=expansion(0.01,0)) + xlab("") + ylab("mean # contacts") + 
  theme_bw() + standard_theme + theme(strip.text=element_text(size=15))
# save
ggsave(paste0("data/comix/england_mean_contacts_partipage_2020_2022.png"),width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# COMIX data aggregated by Munday 2021 article
# from https://zenodo.org/record/4677018
uk_contact_matrices_9_periods <- read_csv("data/comix/munday2021/contact_matrices_9_periods.csv") %>%
  select(!`...1`)
# period
uk_ld_inds=lapply(list("Lockdown 1"=c("23/03/2020","03/06/2020"),
                       "Lockdown 1 easing"=c("04/06/2020","29/07/2020"),
                       "Reduced restrictions"=c("30/07/2020","03/09/2020"),
                       "Schools open"=c("04/09/2020","26/10/2020"),
                       "Lockdown 2"=c("05/11/2020","02/12/2020"),
                       "Lockdown 2 easing"=c("03/12/2020","19/12/2020"),
                       "Christmas"=c("20/12/2020","02/01/2021"),
                       "Lockdown 3"=c("05/01/2021","08/03/2021"),
                       "Lockdown 3 + schools"=c("08/03/2021","16/03/2021")),dmy)
names(uk_ld_inds)=unique(uk_contact_matrices_9_periods$Period)
# create full dataframe 
uk_contact_matrices_9_periods <- uk_contact_matrices_9_periods %>% rowwise() %>%
  mutate(date_start=uk_ld_inds[[which(names(uk_ld_inds) %in% Period)]][1],
         date_stop=uk_ld_inds[[which(names(uk_ld_inds) %in% Period)]][2],
         mean_date=mean(c(date_start,date_stop)),
         period_length=as.numeric(abs(date_start-date_stop))) %>% ungroup() %>%
  mutate(`Contact age`=factor(`Contact age`,levels=unique(`Contact age`))) %>% ungroup() %>% 
  group_by(`Participant age`,Period) %>% mutate(cumul_contact=cumsum(`mean contacts`))
uk_contact_matrices_9_periods$`Participant age`=factor(uk_contact_matrices_9_periods$`Participant age`,
                                                       levels=unique(uk_contact_matrices_9_periods$`Participant age`))
ylim_dummy = uk_contact_matrices_9_periods %>% ungroup() %>% select(`Participant age`) %>% distinct() %>% 
  mutate(mean_date=as.Date("2020-04-01"),`mean contacts`=c(rep(15,3),rep(6,6)))

# barplot: absolute contact numbers
uk_contact_matrices_9_periods %>%
ggplot() + facet_wrap(~`Participant age`,scales="free_y") + 
  # geom_segment(aes(x=date_start,xend=date_stop,y=cumul_contact,yend=cumul_contact,color=`Contact age`)) + 
  geom_bar(aes(x=mean_date,y=`mean contacts`,fill=`Contact age`,width=period_length*3/4),
           stat="identity",color="black",size=1/2,position=position_stack(reverse=TRUE)) +
  geom_point(data=ylim_dummy,aes(x=mean_date,y=`mean contacts`),color=NA) +
  geom_vline(aes(xintercept=date_start),size=1/3,linetype="dashed") + xlab("") + 
  # geom_vline(aes(xintercept=date_stop),size=1/3,linetype="dashed") +
  scale_x_date(expand=expansion(0.02),breaks=unique(uk_contact_matrices_9_periods$mean_date)) +
  scale_y_continuous(expand=expansion(0.02)) + # date_breaks="1 month",
  theme_bw() + standard_theme + theme(strip.text=element_text(size=15),axis.text.x=element_text(size=14),
                                      axis.text.y=element_text(size=14))
# save
ggsave(paste0("data/comix/munday2021/contact_matr_uk_paneldates.png"),width=33,height=22,units="cm")

# line/area plot: num/shares of contacts by age groups (faceted by participant age)
sel_var=list(c("share_contacts","cumul_share"),c("mean contacts","cumul_contact"))[[1]]
uk_contact_matrices_9_periods %>% ungroup() %>% group_by(`Participant age`,Period) %>%
  mutate(share_contacts=(`mean contacts`/sum(`mean contacts`)),
         cumul_share=cumsum(share_contacts)) %>% # 
ggplot() + facet_wrap(~`Participant age`,scales="free_y") + 
  geom_area(aes(x=mean_date,y=get(sel_var[1]),fill=`Contact age`),color="black",size=1/3,
            position=position_stack(reverse=TRUE)) +
  geom_point(aes(x=mean_date,y=get(sel_var[2]),group=`Contact age`),size=4/5,shape=21) + # 
  scale_x_date(breaks=unique(uk_contact_matrices_9_periods$mean_date), # date_breaks="1 month",
               expand=expansion(0.01)) + scale_y_continuous(expand=expansion(0.02)) + 
  scale_fill_grey() + xlab("") + ylab("% of all contacts") + theme_bw() + 
  standard_theme + theme(strip.text=element_text(size=15))
# save
ggsave(paste0("data/comix/munday2021/contact_matr_uk_",gsub(" ","_",sel_var[1]),".png"),
       width=33,height=22,units="cm")
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# compare contact (Comix) data with GoogleMob/OxCGRT

# comix vs NPI stringency
uk_npi_periods <- uk_contact_matrices_9_periods %>% 
  ungroup() %>% select(Period,date_start,date_stop,mean_date) %>% unique()
UK_NPI_period_meanvals <- OxCGRT_input %>% filter(CountryName %in% "United Kingdom") %>%
  pivot_longer(!c(date,CountryName,CountryCode,NPI_on)) %>% 
  mutate(npi_period=findInterval(date,uk_npi_periods$date_start)) %>%
  rowwise() %>% mutate(mean_date=ifelse(npi_period>0 & npi_period<=nrow(uk_npi_periods),
                                        as.character(uk_npi_periods$mean_date[npi_period]),NA)) %>%
  group_by(npi_period,mean_date,name) %>% summarise(mean_value=mean(value)) %>%
  pivot_wider(names_from=name,values_from=mean_value)

# join and plot: stringency indices vs contacts per participant age group
n_sel_var=3; varlist=c("C1M_School closing","C2M_Workplace closing","StringencyIndex_Average")
left_join(UK_NPI_period_meanvals %>% filter(!is.na(mean_date)) %>% mutate(mean_date=as.Date(mean_date)), 
          uk_contact_matrices_9_periods, by="mean_date") %>% filter(!is.na(`Participant age`)) %>%
ggplot(aes(x=get(varlist[n_sel_var]),y=`mean contacts`,color=`Contact age`)) +
  geom_point(size=3,alpha=1/2) + geom_line() + # ,position=position_dodge(width=1/3)
  facet_wrap(~`Participant age`,scales="free_y") + xlab(varlist[n_sel_var]) +
  theme_bw() + standard_theme + theme(panel.grid.major=element_blank(),panel.spacing=unit(0.2,"lines"))
# save
ggsave(paste0("data/comix/munday2021/contacts_",gsub(" ","_",varlist[n_sel_var]),".png"),
       width=33,height=22,units="cm")

# concatenate with continuous CoMix data
# weekly averages of NPI stringencies
uk_npi_weeks <- mean_cont_plot %>% select(start_date,end_date,mid_date) %>% unique() %>%
  arrange(start_date) %>% mutate(n_week=row_number())
UK_NPI_weeks_meanvals <- OxCGRT_input %>% filter(CountryName %in% "United Kingdom") %>%
  pivot_longer(!c(date,CountryName,CountryCode,NPI_on)) %>% rowwise() %>%
  mutate(npi_period_start=findInterval(date,sort(unique(mean_cont_plot$start_date))),
         npi_period_end=findInterval(date,sort(unique(mean_cont_plot$end_date))),
         npi_period=mean(c(npi_period_start))) %>%
  rowwise() %>% mutate(mid_date=as.Date(ifelse(npi_period>0 & npi_period<=nrow(uk_npi_weeks),
                                        as.character(uk_npi_weeks$mid_date[npi_period]),NA))) %>%
  group_by(npi_period,mid_date,name) %>% summarise(mean_value=mean(value)) %>%
  pivot_wider(names_from=name,values_from=mean_value)

# UK_NPI_weeks_meanvals %>% select(!StringencyIndex_Average) %>% pivot_longer(!c(npi_period,mid_date)) %>% 
#   ggplot() + geom_line(aes(x=mid_date,y=value,color=name,group=name)) + scale_x_date(date_breaks="month") +
#   theme_bw() + standard_theme

# join and plot
varlist=c("C1M_School closing","C2M_Workplace closing","StringencyIndex_Average"); n_sel_var=3
left_join(UK_NPI_weeks_meanvals, mean_cont_plot, by="mid_date") %>% 
  filter(!is.na(part_age_group)) %>%
ggplot(aes(x=get(varlist[n_sel_var]),y=mean,color=mid_date)) +
  geom_point(size=3,alpha=1/3) + # geom_line() + # ,position=position_dodge(width=1/3)
  facet_wrap(~part_age_group,scales="free_y") + xlab(varlist[n_sel_var]) +
  theme_bw() + standard_theme + 
  theme(panel.grid.major=element_blank(),panel.spacing=unit(0.2,"lines"),strip.text=element_text(size=13))
# save
ggsave(paste0("data/comix/uk_contacts_",gsub(" ","_",varlist[n_sel_var]),".png"),
       width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### compare with Google Mobility


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Extracted from Socrates

all_comix_matr <- read_csv("data/comix/contact_matrs_socrates/all_comix_matr_4agegroups.csv")

# plot all together
all_comix_matr %>% filter(contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            particip %in% c("[0,2)","[2,5)","[5,18)","18+")) %>%
  filter(!wave %in% "All") %>% # group_by(country) %>% 
  mutate(# wave_nr=ifelse(wave %in% "All",wave,as.numeric(factor(wave))),
         date=sapply(wave, function(x) strsplit(x, split = " ")[[1]][2]),
         year_month=paste0(year(date),"/",ifelse(month(date)<10,0,""),month(date))) %>%
  filter(!is.na(mean)) %>%
ggplot(aes(particip,contact,fill=log10(median))) + facet_grid(date~country) + 
  geom_tile() + geom_text(aes(label=round(median,2),color=median>8),size=2.75) +
  scale_color_manual(values=c("grey","black"),guide="none") + 
  scale_x_discrete(expand=expansion(0.01,0.01)) + scale_y_discrete(expand=expansion(0.01,0.01)) +
  geom_hline(yintercept=(1:3)+1/2,color="black",size=1/4) +
  geom_vline(xintercept=(1:3)+1/2,color="black",size=1/4) +
  theme_bw() + standard_theme + theme(panel.grid.major=element_blank(),panel.spacing=unit(0.2,"lines"))
# save
# ggsave("data/comix/contact_matrs_socrates/heatmaps/all_comix_matr_yearmonth.png",width=33,height=22,units="cm")
# ggsave("data/comix/contact_matrs_socrates/heatmaps/all_comix_matr_alldates.png",width=33,height=44,units="cm")
ggsave("data/comix/contact_matrs_socrates/heatmaps/all_comix_matr_log_alldates.png",width=33,height=44,units="cm")

# barplot
sel_var=c("mean","median")[2]
all_comix_matr %>% filter(contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            particip %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            !is.na(mean) & !wave %in% "All") %>% 
  mutate(particip=paste0(particip," (part. age)")) %>% rename(`age of participant`=particip) %>% 
  rowwise() %>% mutate(wave_date=(ifelse(wave %in% "All",wave,
                                         substr(x=wave,4,nchar(wave)-ifelse(country %in% "Belgium",0,5)))))  %>% 
ggplot() + facet_grid(`age of participant`~country,scales="free_x") + # scales="free_x"
  geom_bar(aes(x=as.Date(wave_date),y=get(sel_var),fill=contact),
           stat="identity",size=1/4,color="black",width=10) +
  # geom_linerange(aes(x=mean,ymin=as.Date(wave)-5,ymax=as.Date(wave)+5,
  #                    color=contact),size=1.5) + coord_flip() + # ,size=2
  scale_x_date(expand=expansion(0.02,0),date_breaks="month") + scale_y_continuous(expand=expansion(0.02,0)) + 
  # scale_x_log10() + # breaks=(0:(max(df_plot$mean)/2))*2, #breaks=1:n_wave,labels=sort(unique(df_plot$wave)),
  xlab("") + ylab("# of daily contacts") + labs(color="contact age",fill="contact age") +
  theme_bw() + standard_theme + theme(strip.text=element_text(size=13),panel.grid.minor=element_blank(),
                                      panel.grid.major.x=element_blank(),legend.position="top")
# save
ggsave(paste0("data/comix/contact_matrs_socrates/barplots/all_cntrs_",sel_var,".png"),
       width=32,height=18,units="cm")


# HEATMAP
# plot by country
# for (cntr_name in unique(all_comix_matr$country)) {
#   # cntr_name=unique(all_comix_matr$country)[k_cntr]
#   # heatmap
#   all_comix_matr %>% filter(country %in% cntr_name & 
#                               contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
#                               particip %in% c("[0,2)","[2,5)","[5,18)","18+")) %>%
#     mutate(wave=factor(wave,levels=unique(wave))) %>%
#     ggplot(aes(particip,contact,fill=mean)) + 
#     geom_tile() + geom_text(aes(label=round(mean,2),color=mean>8),size=4) +
#     scale_color_manual(values=c("grey","black"),guide="none") + 
#     scale_x_discrete(expand=expansion(0.01,0.01)) + scale_y_discrete(expand=expansion(0.01,0.01)) +
#     geom_hline(yintercept=(1:3)+1/2,color="white") + geom_vline(xintercept=(1:3)+1/2,color="white") +
#     facet_wrap(~wave) + ggtitle(cntr_name) + theme_bw() + standard_theme + 
#     theme(panel.grid.major=element_blank())
#   # ggsave
#   ggsave(paste0("data/comix/contact_matrs/heatmaps/by_cntr/",cntr_name,".png"),width=33,height=22,units="cm")
# }

# # all URLs
# # comix_url_list <- c(uk="https://zenodo.org/record/6542524",it="https://zenodo.org/record/6362888",
# #   ch="https://zenodo.org/record/6542657",dk="https://zenodo.org/record/6362899",
# #   sk="https://zenodo.org/record/6535357",at="https://zenodo.org/record/6362906",
# #   hu="https://zenodo.org/record/6535344",ee="https://zenodo.org/record/6535313",
# #   fr="https://zenodo.org/record/6362893",fi="https://zenodo.org/record/6542664",
# #   si="https://zenodo.org/record/6362865",pl="https://zenodo.org/record/6362879",
# #   nl="https://zenodo.org/record/4790347",es="https://zenodo.org/record/6362898",
# #   be="https://zenodo.org/record/7086043")
# 
# # contact_common: https://zenodo.org/record/6362898/files/CoMix_es_contact_common.csv?download=1
# # participant_common: https://zenodo.org/record/6362898/files/CoMix_es_participant_common.csv?download=1
# # sday: https://zenodo.org/record/6362898/files/CoMix_es_sday.csv?download=1
# 
# # if COMIX datafile does not exist yet (this downloads files from Zenodo, takes some time!)
# source("fcns/load_comix.R")
# # if exists
# comix_aggr_all <- read_csv("data/comix/comix_aggr_all.csv")
# 
# # plot
# for (k_url in 1:length(comix_url_list)) {
#   cntr_name=unique(comix_aggr_all$country)[k_url]
#   plot_df <- comix_aggr_all %>% filter(metric %in% "value" &(country %in% cntr_name)&(!name %in% "all") & 
#                                          !(part_age %in% "Prefer not to answer")) %>%
#     mutate(part_age_num=as.numeric(gsub(".*-","",part_age)), 
#            part_age_num=ifelse(is.na(part_age_num),0,part_age_num)+1)
#   plot_df$part_age_num=as.numeric(factor(plot_df$part_age_num))
#   proper_order=unlist(sapply(1:length(unique(plot_df$part_age_num)), 
#                              function(x) which(unique(plot_df$part_age_num) %in% x)))
#   plot_df$part_age=factor(plot_df$part_age, levels=unique(plot_df$part_age)[proper_order])
#   plot_df %>% 
#     ggplot(aes(x=date,y=value_mean,fill=name)) + ggtitle(cntr_name) +
#     geom_bar(stat="identity",color="black",width=6) + facet_wrap(~part_age) + labs(fill="") +
#     scale_x_date(date_breaks="month") + xlab("") + theme_bw() + standard_theme + theme(legend.position="top")
#   # save
#   ggsave(paste0("data/comix/plots/y_fixed/comix_mean_cont_",cntr_name,".png"),
#          width=30*ifelse(cntr_name %in% "uk",1.3,1),height=22,units="cm")
#   message(paste0("plotting ",cntr_name))
# }

# # part_id
# comix_raw_participant_common <- read_csv("comix_raw_participant_common.csv")
