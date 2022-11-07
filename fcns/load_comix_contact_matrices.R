# using scorates-comix shiny app
setwd("data/comix/socrates_rshiny/")
source('R/socrates_main.R'); source('R/load_config_base.R') # re-load settings (without CoMix-based selection)

comix_opt_country=opt_country[c(sapply("CoMix", grepl,names(opt_country)))]
l_matrix_out_recipr=list(); l_demogr=list(); l_inner=list()

for (k_cntr in 1:length(comix_opt_country)) {
for (k_wave in 1:10){
# aggregate the input parameters in a list
input <- list(age_breaks_num = c(0,2,5,18),
              country     = comix_opt_country[k_cntr],
              daytype     = opt_day_type[1],
              touch       = opt_touch[[1]],
              duration    = opt_duration[[1]],
              gender      = opt_gender[1],
              cnt_location = opt_location,
              cnt_matrix_features   = opt_matrix_features,
              bool_transmission_param = FALSE,
              bool_reciprocal = TRUE,
              bool_suppl_professional_cnt = TRUE,
              wave = list("All",1,2,3,4,5,6,7,8,9)[[k_wave]],
              cnt_reduction = data.frame(Transport=0,Leisure=0,Otherplace=0) ) # no reductions for this example

# include age breaks as text (required for SOCRATES)
input$age_breaks_text = paste(input$age_breaks_num,sep=',')
input$age_susceptibility_text = paste(rep(1,length(input$age_breaks_num)),collapse=',')
input$age_infectiousness_text = input$age_susceptibility_text

# attach all atributes from the input list, so you can use their names directly
attach(input,warn.conflicts = F)

# get survey object
survey_obj <- get_survey_object(country,
                                daytype,
                                touch,
                                duration,
                                gender,
                                cnt_location,
                                bool_reciprocal = "Reciprocal" %in% cnt_matrix_features,
                                bool_suppl_professional_cnt = TRUE,
                                bool_hhmatrix_selection = FALSE,
                                wave = wave)

# inspect survey object
# lapply(survey_obj,dim) # head(survey$contacts)
# summary(survey_obj$contacts)

# generate default contact matrix, based on the survey object
# cnt_obj <- contact_matrix(survey_obj, age.limits = c(0,2,5,18))

# inspect contact matrix object
# lapply(cnt_obj,dim)
# table(is.na(cnt_obj$matrix))
# sum(cnt_obj$participants$contacts_reported)

# get (default) symmetric contact matrix
# matrix_out <- contact_matrix(survey_obj,age.limits=age_breaks_num,symmetric=TRUE,quiet=TRUE)
# names(matrix_out)

# inspect contact matrix using internal function(s)  
# plot_cnt_matrix(matrix_out$matrix)

# get (weighted) contact matrix based on previous input values
matrix_out_recipr <- contact_matrix(survey_obj, 
                                    age.limits=age_breaks_num,
                                    symmetric="Reciprocal" %in% cnt_matrix_features, #TRUE,
                                    estimated.contact.age='sample',quiet=TRUE,
                                    weigh.dayofweek="Weigh by week/weekend" %in% cnt_matrix_features, #TRUE,
                                    weigh.age="Weigh by age" %in% cnt_matrix_features, #TRUE,
                                    weight.threshold=weight_threshold)

l_inner[[k_wave]] <- matrix_out_recipr$matrix
message(paste0(k_cntr,"/",k_wave))
if (k_wave==1) {l_demogr[[k_cntr]]=matrix_out_recipr$demography}
}
  l_matrix_out_recipr[[k_cntr]] <- l_inner
  # names(l_matrix_out_recipr[[k_cntr]])<-comix_opt_country[k_cntr]
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# by downloading raw data from zenodo - we don't need to do this, 
# bc the shiny app tool at https://github.com/lwillem/socrates_rshiny can create the matrices
#
# all URLs
# comix_url_list <- c(uk="https://zenodo.org/record/6542524",it="https://zenodo.org/record/6362888",
#                     ch="https://zenodo.org/record/6542657",dk="https://zenodo.org/record/6362899",
#                     sk="https://zenodo.org/record/6535357",at="https://zenodo.org/record/6362906",
#                     hu="https://zenodo.org/record/6535344",ee="https://zenodo.org/record/6535313",
#                     fr="https://zenodo.org/record/6362893",fi="https://zenodo.org/record/6542664",
#                     si="https://zenodo.org/record/6362865",pl="https://zenodo.org/record/6362879",
#                     nl="https://zenodo.org/record/4790347",es="https://zenodo.org/record/6362898",
#                     be="https://zenodo.org/record/7086043")
# 
# # contact_common: https://zenodo.org/record/6362898/files/CoMix_es_contact_common.csv?download=1
# # participant_common: https://zenodo.org/record/6362898/files/CoMix_es_participant_common.csv?download=1
# # sday: https://zenodo.org/record/6362898/files/CoMix_es_sday.csv?download=1
# 
# for (k_url in  1:length(comix_url_list)){
#   
#   base_url=paste0(array(comix_url_list[k_url]),"/files/CoMix_",names(comix_url_list[k_url]),"_")
#   cntr_name=names(comix_url_list)[k_url]
#   # join the table with contact and participant information
#   left_joined_input <- left_join(
#     left_join(
#       read_csv(paste0(ifelse(cntr_name %in% "be", gsub("be","BE",base_url),base_url),
#                       "contact_common.csv?download=1")),
#       read_csv(paste0(base_url,"participant_common.csv?download=1")), by="part_id"),
#     read_csv(paste0(base_url,"sday.csv?download=1")) , by="part_id")
#   # particip age groups
#   age_lims=data.frame(age_min=as.numeric(gsub("-.*","",gsub("Under 1","0-1",unique(left_joined_input$part_age)))),
#                       age_max=as.numeric(gsub(".*-","",gsub("Under 1","0-1",unique(left_joined_input$part_age))))) %>%
#     mutate(rank=rank(age_min,ties.method="first")) %>% arrange(rank) %>% filter(!(is.na(age_min)|is.na(age_max)))
#   
#   # start aggregating
#   comix_raw <- left_joined_input %>% select(where(~!all(is.na(.x)))) %>% filter(!is.na(part_age)) %>%
#     filter(!is.na(cnt_age_est_min)) %>% filter(!is.na(cnt_age_est_max)) %>%
#     mutate(date=as.Date(gsub("\\.","-",sday_id)),
#            cnt_age_mean=(cnt_age_est_min+cnt_age_est_max)/2) %>% #paste0(cnt_age_est_min,"-",cnt_age_est_max)
#     select(part_id,part_age,cnt_age_mean,date,wave,phys_contact,duration_multi,cont_id,
#            cnt_home,cnt_work,cnt_school,cnt_transport,cnt_leisure,cnt_otherplace) %>%
#     pivot_longer(!c(part_id,part_age,date,wave,phys_contact,duration_multi,cont_id,cnt_age_mean)) %>% 
#     # 
#     # rowwise() %>% mutate(cnt_age_group=ifelse(cnt_age_mean>1,findInterval(cnt_age_mean,age_lims$age_min),1))
#     comix_raw$cnt_age_group=sapply(comix_raw$cnt_age_mean, 
#                                    function(x) ifelse(x>1,findInterval(x,age_lims$age_min),1))
#   
#   comix_raw <- comix_raw %>% group_by(part_id,part_age,cnt_age_group,wave,date) %>% 
#     mutate(value_phys=value*ifelse(phys_contact==1,1,0),
#            value_dur_hr=value*c(2.5/60,10/60,37.5/60,2.5,4)[duration_multi],
#            value_phys_dur_hr=value*c(2.5/60,10/60,37.5/60,2.5,4)[duration_multi]*ifelse(phys_contact==1,1,0))
#   # write_csv(comix_raw,file=paste0("data/comix/comix_raw_",cntr_name,".csv"))
#   
#   message(paste0("PROCESSING: ",cntr_name))
#   
#   # write_csv(comix_fr_contacts,file = "data/comix/comix_fr_contacts.csv")
#   
#   comix_aggr <- comix_raw %>% # filter(!is.na(part_age)) %>% filter(!is.na(cnt_age)) %>%
#     group_by(part_id,cont_id,name) %>% 
#     summarise(value=sum(value),value_phys=sum(value_phys),part_age=unique(part_age),
#               date=unique(date),wave=unique(wave),
#               value_dur_hr=sum(value_dur_hr),value_phys_dur_hr=sum(value_phys_dur_hr)) %>%
#     pivot_longer(!c(part_id,part_age,cnt_age,date,wave,name),names_to="metric") %>%
#     # sum of all types of contacts
#     group_by(part_id,part_age,cnt_age,wave,metric) %>% mutate(all=sum(value)) %>%
#     pivot_longer(c(all,value),names_to="nn") %>% 
#     mutate(name=ifelse(nn %in% "all","all",name)) %>% distinct() %>% select(!nn) %>%
#     # sum indiv participants
#     group_by(part_age,cnt_age,wave,name,metric) %>% 
#     summarise(n_part=n(),date=mean(date),value_mean=mean(value,na.rm=T),
#               value_median=median(value,na.rm=T),stdev=sd(value,na.rm=T),
#               ci95_l=quantile(value,na.rm=T,probs=0.05),ci95_u=quantile(value,na.rm=T,probs=0.95)) %>%
#     filter(!is.na(part_age)|!is.na(cnt_age)) %>% mutate(country=cntr_name)
#   # %>% mutate(part_age=factor(part_age),country=cntr_name)
#   # levels(comix_fr_aggr$part_age)=levels(comix_fr_aggr$part_age)[c(length(levels(comix_fr_aggr$part_age)),
#   #                                                                1:(length(levels(comix_fr_aggr$part_age))-1))]
#   
#   if (length(unique(comix_aggr$part_age))>18){
#     message("agegroups need to be merged")
#     agegrs=c("0-1","1-4","5-11","12-15","16-17","18-29","20-24","25-34",
#              "30-39","40-49","45-54","50-59","60-69","70-120")
#     agegrs_lims_num=as.numeric(gsub("-.*","",agegrs))
#     comix_aggr <- comix_aggr %>% 
#       mutate(part_age=agegrs[sapply(part_age, function(x) findInterval(x,agegrs_lims_num))]) %>%
#       group_by(part_age,wave,name,metric) %>%
#       # this should be checked (means of means?)
#       summarise(n_part=sum(n_part),date=mean(date),value_mean=mean(value_mean),value_median=median(value_median),
#                 stdev=mean(stdev),ci95_l=mean(ci95_l),ci95_u=mean(ci95_u),country=unique(country))
#   }
#   write_csv(comix_aggr,file=paste0("data/comix/comix_aggr_",cntr_name,".csv"))
#   
#   gc()
#   message(paste0("DONE WITH: ",cntr_name))
#   l_comix_aggr[[k_url]] = comix_aggr
#   if (k_url==length(comix_url_list)) {comix_aggr_all = bind_rows(l_comix_aggr)}
# }


