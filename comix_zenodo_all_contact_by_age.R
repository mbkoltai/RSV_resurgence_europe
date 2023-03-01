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

for (k_url in  1:length(comix_url_list)){
  
  base_url=paste0(array(comix_url_list[k_url]),"/files/CoMix_",names(comix_url_list[k_url]),"_")
  cntr_name=names(comix_url_list)[k_url]
  comix_raw <- left_join(
    left_join(
      read_csv(paste0(ifelse(cntr_name %in% "be", gsub("be","BE",base_url),base_url),
                      "contact_common.csv?download=1")),
      read_csv(paste0(base_url,"participant_common.csv?download=1")), by="part_id"),
    read_csv(paste0(base_url,"sday.csv?download=1")) , by="part_id") %>% 
    mutate(date=as.Date(gsub("\\.","-",sday_id))) %>% 
    mutate(cnt_age=paste0(cnt_age_est_min,"_",cnt_age_est_max)) %>% 
    select(part_id,part_age,date,wave,phys_contact,duration_multi,cont_id,cnt_age,
           cnt_home,cnt_work,cnt_school,cnt_transport,cnt_leisure,cnt_otherplace) %>%
    pivot_longer(!c(part_id,part_age,date,wave,phys_contact,duration_multi,cont_id,cnt_age)) %>% # filter(value) %>%
    group_by(part_id,part_age,wave,date) %>% 
    mutate(value_phys=value*ifelse(phys_contact==1,1,0),
           value_dur_hr=value*c(2.5/60,10/60,37.5/60,2.5,4)[duration_multi],
           value_phys_dur_hr=value*c(2.5/60,10/60,37.5/60,2.5,4)[duration_multi]*ifelse(phys_contact==1,1,0))
  # write_csv(comix_raw,file=paste0("data/comix/comix_raw_",cntr_name,".csv"))
  
  message(paste0("PROCESSING: ",cntr_name))
  
  # write_csv(comix_fr_contacts,file = "data/comix/comix_fr_contacts.csv")
  
  comix_aggr <- comix_raw %>% group_by(part_id,part_age,date,wave,name) %>% 
    summarise(value=sum(value),value_phys=sum(value_phys),
              value_dur_hr=sum(value_dur_hr),value_phys_dur_hr=sum(value_phys_dur_hr)) %>% 
    pivot_longer(!c(part_id,part_age,date,wave,name),names_to="metric") %>%
    # sum of all types of contacts
    group_by(part_id,part_age,wave,metric) %>% mutate(all=sum(value)) %>%
    pivot_longer(c(all,value),names_to="nn") %>% 
    mutate(name=ifelse(nn %in% "all","all",name)) %>% distinct() %>% select(!nn) %>%
    # sum indiv participants
    group_by(part_age,wave,name,metric) %>% 
    summarise(n_part=n(),date=mean(date),value_mean=mean(value,na.rm=T),
              value_median=median(value,na.rm=T),stdev=sd(value,na.rm=T),
              ci95_l=quantile(value,na.rm=T,probs=0.05),ci95_u=quantile(value,na.rm=T,probs=0.95)) %>%
    filter(!is.na(part_age)) %>% mutate(country=cntr_name)
  # %>% mutate(part_age=factor(part_age),country=cntr_name)
  # levels(comix_fr_aggr$part_age)=levels(comix_fr_aggr$part_age)[c(length(levels(comix_fr_aggr$part_age)),
  #                                                                1:(length(levels(comix_fr_aggr$part_age))-1))]
  
  if (length(unique(comix_aggr$part_age))>18){
    message("agegroups need to be merged")
    agegrs=c("0-1","1-4","5-11","12-15","16-17","18-29","20-24","25-34",
             "30-39","40-49","45-54","50-59","60-69","70-120")
    agegrs_lims_num=as.numeric(gsub("-.*","",agegrs))
    comix_aggr <- comix_aggr %>% 
      mutate(part_age=agegrs[sapply(part_age, function(x) findInterval(x,agegrs_lims_num))]) %>%
      group_by(part_age,wave,name,metric) %>%
      # this should be checked (means of means?)
      summarise(n_part=sum(n_part),date=mean(date),value_mean=mean(value_mean),value_median=median(value_median),
                stdev=mean(stdev),ci95_l=mean(ci95_l),ci95_u=mean(ci95_u),country=unique(country))
  }
  write_csv(comix_aggr,file=paste0("data/comix/comix_aggr_",cntr_name,".csv"))
  
  gc()
  message(paste0("DONE WITH: ",cntr_name))
  l_comix_aggr[[k_url]] = comix_aggr
  if (k_url==length(comix_url_list)) {comix_aggr_all = bind_rows(l_comix_aggr)}
}


