# using socrates-comix shiny app
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("data/comix/socrates_rshiny/")
source('R/socrates_main.R'); source('R/load_config_base.R') # re-load settings (without CoMix-based selection)
detach("package:wpp2015",unload=TRUE); detach("package:plyr") 
library(plyr); library(dplyr)
# setwd("../../../")
comix_opt_country=opt_country[c(sapply("CoMix", grepl,names(opt_country)))]
l_matrix_out_recipr=list(); l_demogr=list(); l_inner=list()

for (k_cntr in 1:length(comix_opt_country)) { # 
  cntr_name = unlist(strsplit(names(comix_opt_country[k_cntr])," "))[1]
  for (k_wave in 1:10){
# aggregate the input parameters in a list
input <- list(age_breaks_num = c(0,2,5,18),
              country     = comix_opt_country[k_cntr],
              daytype     = opt_day_type[1],
              touch       = opt_touch[[1]],
              duration    = opt_duration[[1]],
              gender      = opt_gender[1],
              cnt_location = opt_location,
              cnt_matrix_features=opt_matrix_features,
              bool_transmission_param=FALSE,bool_reciprocal=TRUE,bool_suppl_professional_cnt=TRUE,
              wave=list("All",1,2,3,4,5,6,7,8,9)[[k_wave]],
              cnt_reduction=data.frame(Transport=0,Leisure=0,Otherplace=0) ) 
# no reductions for this example
# include age breaks as text (required for SOCRATES)
input$age_breaks_text = paste(input$age_breaks_num,sep=',')
input$age_susceptibility_text = paste(rep(1,length(input$age_breaks_num)),collapse=',')
input$age_infectiousness_text = input$age_susceptibility_text

# attach all atributes from the input list, so you can use their names directly
attach(input,warn.conflicts = F)

if (k_wave==1) {
temp_survey_obj <- get_survey_object(country,daytype,touch,duration,gender,cnt_location,
                                bool_reciprocal="Reciprocal" %in% cnt_matrix_features,
                                bool_suppl_professional_cnt=TRUE,bool_hhmatrix_selection=FALSE,
                                wave=wave) 
  wave_string=sort(unique(temp_survey_obj$participants$wave) )
  }

# get survey object
if ((wave<=length(wave_string))|wave %in% "All"){
survey_obj <- get_survey_object(country, daytype,
                                touch, duration,
                                gender, cnt_location,
                                bool_reciprocal = "Reciprocal" %in% cnt_matrix_features,
                                bool_suppl_professional_cnt = TRUE, bool_hhmatrix_selection = FALSE,
                                wave = ifelse(wave %in% "All",wave,wave_string[wave]))

# get (weighted) contact matrix based on previous input values
# sampled 50x
for (k_l in 1:50) {
  mm <- as.data.frame(
    contact_matrix(survey_obj, age.limits=age_breaks_num,quiet=TRUE,
                           symmetric="Reciprocal" %in% cnt_matrix_features, #TRUE,
                           estimated.contact.age='sample',# estimated.participant.age = 'sample',
                           weigh.dayofweek="Weigh by week/weekend" %in% cnt_matrix_features, #TRUE,
                           weigh.age="Weigh by age" %in% cnt_matrix_features,
                           weight.threshold=weight_threshold)$matrix,col.names="cont") %>% 
    mutate(particip=row_number()) %>% pivot_longer(!c(particip),names_to="contact") %>%
    mutate(particip=unique(contact)[particip],k_l=k_l)
  # mm %>% pivot_wider(values_from=value,names_from=contact)
  # store in list
  l_mm[[k_l]]=mm
  if (k_l %% 10==0) {
    message(paste0(cntr_name," (cntr ",k_cntr,"/8)",", wave ",wave,", iter ",k_l)) }
  if (all(is.na(mm$value))) {break}
}
# calculate the mean contact matrix
mean_c_m <- l_mm %>% bind_rows %>% group_by(particip,contact) %>%
  summarise(mean=mean(value,na.rm=T),median=median(value,na.rm=T),n_sample=sum(!is.na(value)))


l_inner[[k_wave]] <- mean_c_m %>%
  mutate(country=cntr_name,wave=ifelse(wave %in% "All",wave,wave_string[wave])) %>% 
  relocate(contact,.before=country) # list("All",1,2,3,4,5,6,7,8,9)[[k_wave]]
}

} # loop for waves
  l_matrix_out_recipr[[k_cntr]] <- l_inner %>% bind_rows()
  # names(l_matrix_out_recipr[[k_cntr]]) <- names(comix_opt_country[k_cntr])
  }
### end of loop
all_comix_matr=bind_rows(l_matrix_out_recipr) %>% distinct() %>% relocate(contact,.after=particip) %>%
  mutate(wave_year_month=ifelse(wave %in% "All","All",substr(start=4,stop=10,wave)) ) # ,
         # wave_year_month=factor(wave_year_month,levels=sort(unique(all_comix_matr$wave_year_month))))
# write_csv(all_comix_matr,"data/comix/all_comix_matr_4agegroups.csv")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# HEATMAP
# plot by country
for (cntr_name in unique(all_comix_matr$country)) {
# cntr_name=unique(all_comix_matr$country)[k_cntr]
# heatmap
all_comix_matr %>% filter(country %in% cntr_name & 
                            contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            particip %in% c("[0,2)","[2,5)","[5,18)","18+")) %>%
  mutate(wave=factor(wave,levels=unique(wave))) %>%
ggplot(aes(particip,contact,fill=mean)) + 
    geom_tile() + geom_text(aes(label=round(mean,2),color=mean>8),size=4) +
    scale_color_manual(values=c("grey","black"),guide="none") + 
    scale_x_discrete(expand=expansion(0.01,0.01)) + scale_y_discrete(expand=expansion(0.01,0.01)) +
    geom_hline(yintercept=(1:3)+1/2,color="white") + geom_vline(xintercept=(1:3)+1/2,color="white") +
    facet_wrap(~wave) + ggtitle(cntr_name) + theme_bw() + standard_theme + 
    theme(panel.grid.major=element_blank())
# ggsave
ggsave(paste0("data/comix/contact_matrs/heatmaps/by_cntr/",cntr_name,".png"),width=33,height=22,units="cm")
}

# plot all together
all_comix_matr %>% filter(contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                          particip %in% c("[0,2)","[2,5)","[5,18)","18+")) %>%
  group_by(country) %>% mutate(wave_nr=ifelse(wave %in% "All",wave,as.numeric(factor(wave)))) %>%
ggplot(aes(particip,contact,fill=mean)) + facet_grid(wave_nr~country) + 
  geom_tile() + geom_text(aes(label=round(mean,2),color=mean>8),size=2.75) +
  scale_color_manual(values=c("grey","black"),guide="none") + 
  scale_x_discrete(expand=expansion(0.01,0.01)) + scale_y_discrete(expand=expansion(0.01,0.01)) +
  geom_hline(yintercept=(1:3)+1/2,color="black",size=1/4)+geom_vline(xintercept=(1:3)+1/2,color="black",size=1/4)+
  ggtitle(cntr_name) + theme_bw() + standard_theme + 
  theme(panel.grid.major=element_blank())
# save
ggsave("data/comix/contact_matrs/heatmaps/all_comix_matr.png",width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ###
# plot by wave
flag_month_aggreg=F
for (wave_yr_month in unique(all_comix_matr$wave_year_month)) {
  # heatmap
  df_plot <- all_comix_matr %>% 
    filter(wave_year_month %in% wave_yr_month & 
           contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
           particip %in% c("[0,2)","[2,5)","[5,18)","18+")) %>%
    mutate(cntr_wave=paste0(country,"_",wave)) 
  
    # take the mean values per month
  if (flag_month_aggreg){
    df_plot <- df_plot %>% group_by(country,wave_year_month,particip,contact) %>% 
                           summarise(mean=mean(mean,na.rm = T)) }
  
df_plot %>% ggplot(aes(particip,contact,fill=mean)) + facet_wrap(~country) +
    geom_tile() + geom_text(aes(label=round(mean,2),color=mean>8),size=4) +
    scale_color_manual(values=c("grey","black"),guide='none') + labs(color="") +
    scale_x_discrete(expand=expansion(0.01,0.01)) + scale_y_discrete(expand=expansion(0.01,0.01)) +
    geom_hline(yintercept=(1:3)+1/2,color="white") + geom_vline(xintercept=(1:3)+1/2,color="white") +
    ggtitle(wave_yr_month) + theme_bw() + standard_theme + 
    theme(panel.grid.major=element_blank(),strip.text=element_text(size=13))
  # ggsave
  ggsave(paste0("data/comix/contact_matrs/by_wave_month_aggreg/",gsub("-","_",wave_yr_month),".png"),
         width=33,height=22,units="cm")
  message(wave_yr_month)
}

### ### ### ### ### ### ### ### ### ### ### ###
# bar/segment plot
for (cntr_name in unique(all_comix_matr$country)) {
  df_plot <- all_comix_matr %>% filter(country %in% cntr_name & # !(wave %in% "All") &
                            contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            particip %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            !is.na(mean)) %>% rename(`age of participant`=particip) %>% 
    rowwise() %>% mutate(wave=(ifelse(wave %in% "All",wave,
                          substr(x=wave,4,nchar(wave)-ifelse(cntr_name %in% "Belgium",0,5)))))
n_wave=length(unique(df_plot$wave))
plot_type=2
# plot
df_plot %>% 
ggplot() + facet_wrap(~`age of participant`,labeller=label_both) + # scales="free_x"
  geom_bar(aes(x=wave,y=mean,fill=contact),stat="identity",position="dodge",size=1/4,color="black") +
  #
  # geom_linerange(aes(x=mean,ymin=as.numeric(factor(wave))-0.45,ymax=as.numeric(factor(wave))+0.45,
  #                  color=contact),size=2) + coord_flip() + # ,size=2
  # geom_hline(yintercept=(1:(n_wave-1))+1/2,size=1/3) + 
  # geom_hline(yintercept=n_wave+c(-1/2,1/2),size=1/2,color="red") + 
  # scale_x_continuous(breaks=(0:(max(df_plot$mean)/2))*2,expand=expansion(0.01,0)) + # scale_x_log10() +
  # scale_y_continuous(breaks=1:n_wave,labels=sort(unique(df_plot$wave)),expand=expansion(0.01,0)) +
  #
  scale_y_continuous(breaks=(0:(max(df_plot$mean)/2))*2,expand=expansion(0.01,0)) + # scale_x_log10() +
  geom_vline(xintercept=(1:(n_wave-1))+1/2,size=1/4) + 
  geom_vline(xintercept=n_wave+c(-1/2,1/2),size=1/2,color="red") + 
  xlab("") + ylab("# of daily contacts") +  # ylab("") + xlab("# of daily contacts") + 
  ggtitle(cntr_name) + labs(fill="age of contact") + theme_bw() + standard_theme + 
  theme(strip.text=element_text(size=13),
        panel.grid.minor=element_blank(),panel.grid.major.x=element_blank())
message(cntr_name)
# save
ggsave(paste0(c("data/comix/contact_matrs/linerange/fixed_x/",
                "data/comix/contact_matrs/barplots/fixed_x/")[plot_type],cntr_name,".png"), 
       width=25,height=18,units="cm")
}

### ### ### ### ### ### ### ### ### ### ### ###
# all on one plot
# barplot
all_comix_matr %>% filter(contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            particip %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            !is.na(mean) & !wave %in% "All") %>% 
  mutate(particip=paste0(particip," (part. age)")) %>% rename(`age of participant`=particip) %>% 
  rowwise() %>% mutate(wave_date=(ifelse(wave %in% "All",wave,
              substr(x=wave,4,nchar(wave)-ifelse(country %in% "Belgium",0,5)))))  %>% 
ggplot() + facet_grid(`age of participant`~country,scales="free_x") + # scales="free_x"
  geom_bar(aes(x=as.Date(wave_date),y=mean,fill=contact),stat="identity",size=1/4,color="black",width=10) +
  # geom_linerange(aes(x=mean,ymin=as.Date(wave)-5,ymax=as.Date(wave)+5,
  #                    color=contact),size=1.5) + coord_flip() + # ,size=2
  scale_x_date(expand=expansion(0.02,0)) + scale_y_continuous(expand=expansion(0.02,0)) + 
  # scale_x_log10() + # breaks=(0:(max(df_plot$mean)/2))*2, #breaks=1:n_wave,labels=sort(unique(df_plot$wave)),
  xlab("") + ylab("# of daily contacts") + labs(color="contact age",fill="contact age") +
  theme_bw() + standard_theme + theme(strip.text=element_text(size=13),panel.grid.minor=element_blank(),
                                      panel.grid.major.x=element_blank(),legend.position="top")
# save
ggsave(paste0("data/comix/contact_matrs/barplots/all_cntrs.png"), width=32,height=18,units="cm")

###
# segment
all_comix_matr %>% filter(contact %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            particip %in% c("[0,2)","[2,5)","[5,18)","18+") & 
                            !is.na(mean) & !wave %in% "All") %>% rename(`age of participant`=particip) %>% 
  rowwise() %>% mutate(wave_date=(ifelse(wave %in% "All",wave,
                substr(x=wave,4,nchar(wave)-ifelse(country %in% "Belgium",0,5))))) %>% 
  ggplot() + facet_grid(`age of participant`~country,scales="free") + # scales="free_x"
  geom_linerange(aes(x=mean,ymin=as.Date(wave_date)-5,ymax=as.Date(wave_date)+5,
                      color=contact),size=1.5) + coord_flip() + # ,size=2
  # scale_x_log10(expand=expansion(0.02,0),limits=c(0.1/2,100)) + 
  scale_x_continuous(expand=expansion(0.03,0)) + scale_y_date(expand=expansion(0.02,0)) + 
  # scale_x_log10() + # breaks=(0:(max(df_plot$mean)/2))*2, #breaks=1:n_wave,labels=sort(unique(df_plot$wave)),
  ylab("") + xlab("# of daily contacts") + labs(color="age of contact",fill="age of contact") +
  theme_bw() + standard_theme + theme(strip.text=element_text(size=13),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),legend.position="top")
# save
# ggsave(paste0("data/comix/contact_matrs/linerange/all_cntrs_log10.png"), width=32,height=18,units="cm")
ggsave(paste0("data/comix/contact_matrs/linerange/all_cntrs.png"), width=32,height=18,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# by downloading raw data from zenodo 
# we don't need to do this for thosecountries where there's data through the shiny app
# @ https://github.com/lwillem/socrates_rshiny can create the matrices
#
# all URLs
comix_url_list <- c(uk="https://zenodo.org/record/6542524",it="https://zenodo.org/record/6362888",
                    ch="https://zenodo.org/record/6542657",dk="https://zenodo.org/record/6362899",
                    sk="https://zenodo.org/record/6535357",at="https://zenodo.org/record/6362906",
                    hu="https://zenodo.org/record/6535344",ee="https://zenodo.org/record/6535313",
                    fr="https://zenodo.org/record/6362893",fi="https://zenodo.org/record/6542664",
                    si="https://zenodo.org/record/6362865",pl="https://zenodo.org/record/6362879",
                    nl="https://zenodo.org/record/4790347",es="https://zenodo.org/record/6362898",
                    be="https://zenodo.org/record/7086043")

# contact_common: https://zenodo.org/record/6362898/files/CoMix_es_contact_common.csv?download=1
# participant_common: https://zenodo.org/record/6362898/files/CoMix_es_participant_common.csv?download=1
# sday: https://zenodo.org/record/6362898/files/CoMix_es_sday.csv?download=1

for (k_url in which(names(comix_url_list) %in% c("uk","nl","be"))){

  base_url=paste0(array(comix_url_list[k_url]),"/files/CoMix_",names(comix_url_list[k_url]),"_")
  cntr_name=names(comix_url_list)[k_url]
  # join the table with contact and participant information
  left_joined_input <- left_join(
    left_join(
      read_csv(paste0(ifelse(cntr_name %in% "be", gsub("be","BE",base_url),base_url),
                      "contact_common.csv?download=1")),
      read_csv(paste0(base_url,"participant_common.csv?download=1")), by="part_id"),
    read_csv(paste0(base_url,"sday.csv?download=1")) , by="part_id")
  # particip age groups
  age_lims=data.frame(age_min=as.numeric(gsub("-.*","",
                            gsub("Under 1","0-1",unique(left_joined_input$part_age)))),
                      age_max=as.numeric(gsub(".*-","",
                            gsub("Under 1","0-1",unique(left_joined_input$part_age))))) %>%
    mutate(rank=rank(age_min,ties.method="first")) %>%
  arrange(rank) %>% filter(!(is.na(age_min)|is.na(age_max)))
  
  # duration of contact by category
  cont_dur_vals=c(2.5/60,10/60,37.5/60,2.5,4)
  # start aggregating
  comix_indiv_contact <- left_joined_input %>% 
    select(where(~!all(is.na(.x)))) %>% filter(!is.na(part_age)) %>%
    filter(!is.na(cnt_age_est_min)) %>% filter(!is.na(cnt_age_est_max)) %>%
    mutate(date=as.Date(gsub("\\.","-",sday_id)),
           cnt_age_group=paste0(cnt_age_est_min,"-",cnt_age_est_max)) %>%
    select(part_id,part_age,cnt_age_group,date,wave,phys_contact,duration_multi,cont_id,
           cnt_home,cnt_work,cnt_school,cnt_transport,cnt_leisure,cnt_otherplace) %>%
    pivot_longer(!c(part_id,part_age,date,wave,phys_contact,
                    duration_multi,cont_id,cnt_age_group)) %>% group_by(cont_id) %>%
    relocate(cont_id,.before=cnt_age_group) %>%
    mutate(part_id=unique(part_id),part_age=unique(part_age),
           cnt_age_group=unique(cnt_age_group),wave=unique(wave),date=unique(date),
           value_phys=value*ifelse(phys_contact==1,1,0),
           value_dur_hr=value*cont_dur_vals[duration_multi],
           value_phys_dur_hr=value*cont_dur_vals[duration_multi]*ifelse(phys_contact==1,1,0))
  #
  # write_csv(comix_raw,file=paste0("data/comix/comix_raw_",cntr_name,".csv"))

  message(paste0("PROCESSING: ",cntr_name))
  # write_csv(comix_fr_contacts,file = "data/comix/comix_fr_contacts.csv")
  
  # do aggregation in batches not to run out of memory
  l_comix_aggr=list() # ; n_segm=100
  all_part_age=unique(comix_indiv_contact$part_age)
  for (k_row in 1:length(all_part_age)) {
    # AGGREGATE by CONTACT age group
    xx <- comix_indiv_contact %>% ungroup() %>%
    filter(part_age %in% all_part_age[k_row]) %>%
    mutate(contact_exist=value) %>% 
    pivot_longer(!c(part_id,part_age,cnt_age_group,cont_id,contact_exist,
                    date,wave,name),names_to="metric") %>% mutate(value=value*contact_exist) %>%
    # sum of all types of contacts (keeping particip and contact separate)
    select(!contact_exist) %>% ungroup() %>% group_by(cont_id,metric) %>% 
    mutate(all=sum(value),part_id=unique(part_id),part_age=unique(part_age),
           # contact_exist=unique(contact_exist),
           cnt_age_group=unique(cnt_age_group),wave=unique(wave)) %>% ungroup() %>%
    pivot_longer(c(all,value),names_to="nn") %>%
    mutate(name=ifelse(nn %in% "all","all",name)) %>% distinct() %>% select(!nn)
    message(paste0("aggreg by contact age done, part age group: ", k_row,"/",length(all_part_age)))
    gc()
    
    # aggregate by participant
    l_comix_aggr[[k_row]] <- xx %>% ungroup() %>%
    group_by(part_id,cnt_age_group,date,wave,name,metric) %>%
    summarise(value=sum(value,na.rm=T),part_age=unique(part_age)) %>% ungroup() %>%
    # take the mean # contacts by particip age group and contact age group 
    group_by(part_age,cnt_age_group,date,wave,name,metric) %>%
    summarise(n_part=n(), # date=unique(date),
              mean=mean(value),median=median(value,na.rm=T),
              stdev=sd(value,na.rm=T),
              ci95_l=quantile(value,na.rm=T,probs=0.05),ci95_u=quantile(value,na.rm=T,probs=0.95)) %>%
    # remove NAs, format
    filter(!is.na(part_age)|!is.na(cnt_age_group)) %>% mutate(country=cntr_name)

  message(k_row); gc()
  }
  ### end of loop
  
  comix_aggr <- l_comix_aggr %>% bind_rows() %>% distinct() %>% 
                        mutate(part_age=factor(part_age),cnt_age_group=factor(cnt_age_group))
  qsave(comix_aggr,file = "data/comix/comix_aggr_uk.csv")
  # order of age groups
  order_part_agegrps <- order(as.numeric(gsub(".*-","",levels(comix_aggr$part_age))),na.last=F)
  levels(comix_aggr$part_age) <- levels(comix_aggr$part_age)[order_part_agegrps]
  #
  order_cnt_age_grp <- order(as.numeric(gsub(".*-","",levels(comix_aggr$cnt_age_group))),na.last=F)
  levels(comix_aggr$cnt_age_group) <- levels(comix_aggr$cnt_age_group)[order_cnt_age_grp]
  
  if (length(unique(comix_aggr$part_age))>18){
    message("agegroups need to be merged")
    agegrs=c("0-1","1-4","5-11","12-15","16-17","18-29","20-24","25-34",
             "30-39","40-49","45-54","50-59","60-69","70-120")
    agegrs_lims_num=as.numeric(gsub("-.*","",agegrs))
    comix_aggr <- comix_aggr %>%
      mutate(part_age=agegrs[sapply(part_age, function(x) findInterval(x,agegrs_lims_num))]) %>%
      group_by(part_age,cnt_age_group,wave,name,metric) %>%
      # this should be checked (means of means?)
      summarise(n_part=sum(n_part),date=mean(date),
                value_mean=mean(value_mean),value_median=median(value_median),
                stdev=mean(stdev),ci95_l=mean(ci95_l),ci95_u=mean(ci95_u),country=unique(country))
  }
  # write_csv(comix_aggr,file=paste0("data/comix/comix_aggr_",cntr_name,".csv"))

  gc()
  message(paste0("DONE WITH: ",cntr_name))
  l_comix_aggr[[k_url]] = comix_aggr
  if (k_url==length(comix_url_list)) {comix_aggr_all = bind_rows(l_comix_aggr)}
}


# there are some age groups w only little data
agegrp_sample_size <- comix_aggr %>% filter(name %in% "all" & metric %in% "value") %>%
  group_by(part_age) %>% mutate(n_part=n()) %>% group_by(cnt_age_group) %>% mutate(n_cnt=n()) %>%
  select(cnt_age_group,part_age,n_part,n_cnt) %>% 
  pivot_longer(c(cnt_age_group,part_age)) %>% distinct() %>% rename(agegrp=value) %>%
  mutate(value=ifelse(grepl("cnt",name),n_cnt,n_part)) %>% select(!c(n_part,n_cnt)) %>% distinct()

sel_cnt_agegrps=c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                  "45-49","50-54","55-59","60-64","65-69","70-74","75-79")
# plot contact matrix changes in time
comix_aggr %>% filter(name %in% "all" & metric %in% "value" & 
                      !(part_age %in% (agegrp_sample_size %>% filter(value<100))$agegrp) & 
                        cnt_age_group %in% sel_cnt_agegrps) %>% 
  # & date<=as.Date("2020-04-16")
ggplot() + facet_wrap(~part_age) + # scales="free_x"
  # geom_area(aes(x=date,y=value_median,fill=cnt_age_group),position="stack") + # ,size=1/4
  geom_bar(aes(x=date,y=value_median,fill=cnt_age_group),stat="identity",color=NA) + # width=1/5,
  scale_x_date(expand=expansion(0.02,0)) + scale_y_continuous(expand=expansion(0.02,0)) + 
  xlab("") + ylab("# of daily contacts") + labs(color="contact age",fill="contact age") +
  theme_bw() + standard_theme + theme(strip.text=element_text(size=13),panel.grid.minor=element_blank(),
                                      panel.grid.major.x=element_blank()) # ,legend.position="top"
# save
ggsave(paste0("data/comix/raw_data/contact_matr_uk_barplot_median.png"),width=33,height=22,units="cm")

