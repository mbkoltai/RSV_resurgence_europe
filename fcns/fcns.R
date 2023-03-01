### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ###

# for graphs with factos x-axis
every_nth = function(n) { return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]}) }

# use as: g(a,b,c) %=% c(1,2,3)
'%=%' = function(l, r, ...) UseMethod('%=%')
# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)
  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")
  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }
  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)
  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}
# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

### for plots with factors as x-axis, show every nth tick --------------
show_every_nth = function(n) { return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]}) }

standard_theme <- theme(plot.title=element_text(hjust=0.5,size=16),
                        axis.text.x=element_text(size=11,angle=90,vjust=1/2),axis.text.y=element_text(size=11),
                        axis.title=element_text(size=14),text=element_text(family="Calibri"),
                        legend.text=element_text(size=12))
# panel.grid=element_line(linetype="dashed",colour="black",size=0.1),
# every_nth = function(n) { return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]}) }


### get objects larger than x Mb (memory) --------------
fcn_objs_mem_use <- function(min_size){
  mem_use_df=round(data.frame(unlist(sapply(ls(envir=.GlobalEnv), 
                                            function(n) object.size(get(n)), simplify = FALSE)))/1e6,1)
  colnames(mem_use_df)[1]<-"size (Mb)"; mem_use_df[,"objs"]=rownames(mem_use_df)
  mem_use_df<-mem_use_df[order(mem_use_df$size,decreasing=T),]; rownames(mem_use_df)<-c()
  mem_use_df[mem_use_df$size>min_size,c(2,1)]
}


# delay equation
fcn_delay_eq <- function(t_span,params,df_delay,waning_distr,imports_mt) {
  # params
  daily_births=params["daily_births"]; N=params["N"]; gamma=params["gamma"]
  aging_rate=params["aging_rate"]; death_rate=params["death_rate"]; beta_scale=params["beta_scale"]
  for (k_t in t_span) {
    # increments
    inf_term=beta_scale*forcing_fcn(k_t)*df_delay[k_t-1,"S"]*df_delay[k_t-1,"I"]/N #
    new_recov=gamma*df_delay[k_t-1,"I"]
    # immunity waning on indivs in R compartment not 'aged out' of the age bracket. aging is exponential process!
    R_aging=df_delay[(k_t-1):(k_t-length(waning_distr)),"new_recov"]*exp(-aging_rate*(1:length(waning_distr)))
    # waning
    waning_term=sum(R_aging*waning_distr) # df_delay$new_recov[(k_t-1):(k_t-length(waning_distr))]
    dS = as.numeric(daily_births - inf_term - (aging_rate+death_rate)*df_delay[k_t-1,"S"] + waning_term)
    if (k_t %% 30==0) {import_inf=imports_mt} else {import_inf=0} #new_introd(k_t)
    dI = as.numeric(inf_term - new_recov - (aging_rate+death_rate)*df_delay[k_t-1,"I"] + import_inf)
    dR = as.numeric(new_recov - (aging_rate+death_rate)*df_delay[k_t-1,"R"] - waning_term)
    # update variables
    df_delay[k_t,2:ncol(df_delay)]=c(df_delay[k_t-1,"S"]+dS,df_delay[k_t-1,"I"]+dI,
                                     df_delay[k_t-1,"R"]+dR,inf_term,new_recov)
  }
  return(df_delay)
}
# if (k_t %% 1e3==0) {print(paste0(k_t,":",forcing_fcn(k_t)))}

### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# ode model
sir_rsv <- function(t, y, parms) {
  S=y[1]; I=y[2]; R=y[3] # E=y[2]
  daily_births=parms["daily_births"]; N=parms["N"]; gamma=parms["gamma"]; beta_scale=parms["beta_scale"]
  aging_rate=parms["aging_rate"]; death_rate=parms["death_rate"]; waning_rate=parms["waning_rate"]
  # new infections
  inf_term=beta_scale*forcing_fcn(t)*S*I/N # 
  dS = daily_births - inf_term - (aging_rate+death_rate)*S + waning_rate*R # dE = inf_term - (mu + sigma)*E
  dI = inf_term - (aging_rate+gamma+death_rate)*I + new_introd(t); new_inf=inf_term
  dR = gamma*I - (aging_rate+death_rate+waning_rate)*R # res = c(dS, dE, dI, dR)
  list(c(dS, dI, dR, new_inf)) 
}

# create reduced contact matrix --------------
# fun_create_red_C_m=function(C_m_full,rsv_agegroups,orig_age_groups_duration,orig_age_groups_sizes){
#   C_m=matrix(0,nrow=nrow(rsv_agegroups),ncol=nrow(rsv_agegroups)); rownames(C_m)=rsv_agegroups$agegroup_name
#   colnames(C_m)=rsv_agegroups$agegroup_name
#   n_age=nrow(rsv_agegroups)
#   for (i_row in 1:n_age){
#     for (j_col in 1:n_age){
#       # we are merging or splitting age groups, there are 3 possibilities for a new age group:
#       # same OR smaller than (ST) OR larger than (LT) the original
#       # (it is an *average* of contacts per person, and we have no resolution within age bands)
#       #
#       # if the 'i' group (C[i,j]) is the *same* as original or *smaller*, this (in itself) does not change the contact rate
#       if (rsv_agegroups$wpp_agegroup_low[i_row]==rsv_agegroups$wpp_agegroup_high[i_row]) {
#         # 'j' group same or smaller as original
#         if (rsv_agegroups$wpp_agegroup_low[j_col]==rsv_agegroups$wpp_agegroup_high[j_col]) {
#           f_dur=rsv_agegroups$duration[j_col]/orig_age_groups_duration[rsv_agegroups$wpp_agegroup_high[j_col]]
#           C_m[i_row,j_col]=(C_m_full[rsv_agegroups$wpp_agegroup_low[i_row],rsv_agegroups$wpp_agegroup_low[j_col]])*f_dur
#         } else { # if 'j' is larger than original group
#           group_span=rsv_agegroups$wpp_agegroup_low[j_col]:rsv_agegroups$wpp_agegroup_high[j_col]
#           agegroup_weights=orig_age_groups_sizes[group_span]/sum(orig_age_groups_sizes[group_span])
#           C_m[i_row,j_col]=sum(agegroup_weights*C_m_full[i_row,group_span])
#         } # end of 'i' smaller or same as original
#       } else { # if 'i' in C[i,j] is a bigger age band -> weighted average of the contact rates of constituent groups
#         group_span=rsv_agegroups$wpp_agegroup_low[i_row]:rsv_agegroups$wpp_agegroup_high[i_row]
#         agegroup_weights=orig_age_groups_sizes[group_span]/sum(orig_age_groups_sizes[group_span])
#         # if 'j' is same/smaller -> contact rate with original group proportionally divided
#         if (rsv_agegroups$wpp_agegroup_low[j_col]==rsv_agegroups$wpp_agegroup_high[j_col]) {
#           f_dur=rsv_agegroups$duration[j_col]/orig_age_groups_duration[rsv_agegroups$wpp_agegroup_high[j_col]]
#           C_m[i_row,j_col]=
#             sum((orig_age_groups_sizes[group_span]/sum(orig_age_groups_sizes[group_span]))*C_m_full[group_span,j_col])*f_dur
#         } else {# if 'j' larger -> weighted average of the contact rates of the constituent groups
#           # print(c(i_row,j_col)); print(agegroup_weights); print(group_span)
#           C_m[i_row,j_col]=sum(rep(agegroup_weights,length(agegroup_weights))*unlist(
#             lapply(group_span,function(x) {agegroup_weights*C_m_full[x,group_span]})))
#         }
#       }
#       # C_m[i_row,j_col]=mean(C_m_full[rsv_agegroups$wpp_agegroup_low[i_row]:rsv_agegroups$wpp_agegroup_high[i_row],
#       #                                rsv_agegroups$wpp_agegroup_low[j_col]:rsv_agegroups$wpp_agegroup_high[j_col]])   
#     } }
#   C_m }
# 
# 
# # funcn create reciprocal matrix  --------------
fun_recipr_contmatr<-function(C_m_full,age_group_sizes){
  all_perms=expand.grid(1:nrow(C_m_full),1:nrow(C_m_full)) # permutations(n=nrow(C_m_full),r=2,repeats.allowed=T);
  N_tot=sum(age_group_sizes)
  C_m_full_symm=matrix(0,nrow=nrow(C_m_full),ncol=nrow(C_m_full))
  for (k in 1:nrow(all_perms)) {
    i=all_perms[k,1]; j=all_perms[k,2]
    C_m_full_symm[i,j]=(C_m_full[i,j] + C_m_full[j,i]*(age_group_sizes[j]/age_group_sizes[i]))/2
  }
  colnames(C_m_full_symm)=colnames(C_m_full); rownames(C_m_full_symm)=rownames(C_m_full)
  C_m_full_symm
}


### get_OxCGRT -----------------------------
fcn_get_OxCGRT <- function(OxCGRT_url,cntr_name,sel_cols,start_date){
  OxCGRT_sel=subset(read_csv(OxCGRT_url),CountryName %in% cntr_name)
  # convert date format
  OxCGRT_sel[,"date"]=as.Date(paste(sapply(strsplit(as.character(OxCGRT_sel$Date), "(?<=[0-9]{4})", perl=TRUE), "[[",1),
                        sapply(strsplit(sapply(strsplit(as.character(OxCGRT_sel$Date), "(?<=[0-9]{4})", perl=TRUE), "[[",2), 
                                        "(?<=[0-9]{2})", perl=TRUE),function(x){paste(x,collapse="-")}),sep="-"))
  
  OxCGRT_sel[,"NPI_on"]=0; OxCGRT_sel$NPI_on[min(which(OxCGRT_sel$StringencyIndex_Average>0)):nrow(OxCGRT_sel)]=1
  # timespan of model
  OxCGRT_sel=merge(data.frame(date=seq(as.Date(start_date),max(OxCGRT_sel$date),1)),
                   OxCGRT_sel[,sel_cols],by="date",all=TRUE)
  OxCGRT_sel$StringencyIndex_Average[1:which.min(is.na(OxCGRT_sel$StringencyIndex_Average))-1]=0
  OxCGRT_sel=OxCGRT_sel[!is.na(OxCGRT_sel$StringencyIndex_Average),]
  OxCGRT_sel$NPI_on[is.na(OxCGRT_sel$NPI_on)]=0
  # # need to convert it into [0,1] to scale susceptibility (assume: pre-data period had no restrictions)
  OxCGRT_sel[,"OxCGRT_scaled"]=1-(OxCGRT_sel$StringencyIndex_Average)/100
  OxCGRT_sel
}


# 
# 
# # age structure of country ----------------------------------------------------------
# fun_cntr_agestr <- function(i_cntr,i_year,age_low_vals,age_high_vals){
#   age_groups=data.frame(age_low=seq(0,75,5), age_high=c(seq(4,74,5),100))
#   if (!any((.packages()) %in% "wpp2019")) {library(wpp2019)}; if (!exists("popF")) {data("pop")}
#   cntr_agestr=data.frame(agegroups=popF[popF$name %in% i_cntr,"age"],values=popF[popF$name %in% i_cntr,i_year] +
#                            popM[popM$name %in% i_cntr,i_year])
#   agegr_truthvals=sapply(strsplit(as.character(cntr_agestr$agegroups),"-"),"[[",1) %in% age_groups$age_low
#   N_tot=cntr_agestr$values[agegr_truthvals]
#   N_tot[length(N_tot)]=N_tot[length(N_tot)]+sum(cntr_agestr$values[!agegr_truthvals])
#   N_tot=N_tot*1e3; # N_tot
#   data.frame(age_low=age_low_vals, age_high=age_high_vals,values=N_tot, duration=(age_high_vals-age_low_vals)+1) %>%
#     mutate(proportion=values/sum(values))
# }
# 
# 
# ### country full popul struct ----------------------------------------------------------
# fcn_cntr_fullpop <- function(n_year,country_sel){
#   uk_popul=left_join(subset(popF,name %in% country_sel)[,c("age",n_year)],
#                      subset(popM,name %in% country_sel)[,c("age",n_year)],by="age",suffix=c("F","M"))
#   uk_popul[,"totalpop"]=uk_popul[,2]+uk_popul[,3]
#   uk_popul=uk_popul %>% mutate(lower=as.numeric(gsub("-\\d+","",age)),upper=as.numeric(gsub("\\d+-","",age))+0.9)
#   if (any(is.na(uk_popul$lower))){
#     uk_popul[grepl("95",uk_popul$age),c(paste0(n_year,"F"),paste0(n_year,"M"),"totalpop")]=
#       (uk_popul[grepl("95",uk_popul$age),c(paste0(n_year,"F"),paste0(n_year,"M"),"totalpop")]+
#          uk_popul[uk_popul$age=="100+",c(paste0(n_year,"F"),paste0(n_year,"M"),"totalpop")])
#     uk_popul=uk_popul[-which(uk_popul$age=="100+"),] %>% 
#       mutate(mean_age=(lower+upper+0.1)/2,fraction_pop=totalpop/sum(totalpop)) }
#   uk_popul
# }
# 
# 
# ### fcn RSV age groups ----------------------------------------------------------
# fun_rsv_agegroups<-function(standard_age_groups,popul_struct,rsv_age_groups_low,rsv_age_group_sizes){
#   
#   rsv_age_groups=data.frame(age_low=rsv_age_groups_low,age_high=rsv_age_groups_low+rsv_age_group_sizes)
#   
#   truthvals=which(match(rsv_age_groups$age_low,standard_age_groups$age_low)==
#                     match(rsv_age_groups$age_high,standard_age_groups$age_high))
#   rsv_age_groups[,c("wpp_agegroup_low","wpp_agegroup_high")]=NA
#   rsv_age_groups[,c("wpp_agegroup_low","wpp_agegroup_high")]=data.frame(t(sapply(1:length(rsv_age_groups_low), function(x) 
#   {c(max(which(rsv_age_groups_low[x]>=standard_age_groups$age_low)),
#      max(which(rsv_age_groups_low[x]+rsv_age_group_sizes[x]>=standard_age_groups$age_low)))})))
#   
#   agelim_diffs=rsv_age_groups$age_high-rsv_age_groups$age_low; agelim_diffs_increm=rep(NA,length(agelim_diffs))
#   agelim_diffs_increm[agelim_diffs %% 1==0]=1; agelim_diffs_increm[agelim_diffs %% 1>0]=0.1 
#   rsv_age_groups[,"duration"]=(rsv_age_groups$age_high-rsv_age_groups$age_low) + agelim_diffs_increm
#   scaling_fact=rsv_age_groups$duration/sapply(1:nrow(rsv_age_groups),function(x) {
#     sum(standard_age_groups$duration[rsv_age_groups$wpp_agegroup_low[x]:rsv_age_groups$wpp_agegroup_high[x]])})
#   popul_custom_agegroups=sapply(1:nrow(rsv_age_groups),function(x) {sum(standard_age_groups$values[
#     rsv_age_groups$wpp_agegroup_low[x]:rsv_age_groups$wpp_agegroup_high[x]])}); rsv_age_groups[,"value"]=NA
#   rsv_age_groups$value=popul_custom_agegroups*scaling_fact; rsv_age_groups[,"fraction"]=
#     round(rsv_age_groups$value/sum(rsv_age_groups$value),4)
#   rsv_age_groups[,"agegroup_name"]=paste(rsv_age_groups$age_low,rsv_age_groups$age_low+rsv_age_groups$duration,sep='-'); 
#   rsv_age_groups
#   
#   agegroup_match=data.frame(model_agegroup=1:nrow(rsv_age_groups),
#                             age_low=rsv_age_groups$age_low,age_high=rsv_age_groups$age_high,
#                             wpp_agegroup_low=unlist(lapply(rsv_age_groups$age_low,
#                                       function(x){which(x>=popul_struct$lower & x<=popul_struct$upper)})),
#                             wpp_agegroup_high=unlist(lapply(rsv_age_groups$age_high,
#                                       function(x){which(x>=popul_struct$lower & x<=popul_struct$upper)}))) %>%
#     mutate(age_high=ifelse(model_agegroup<max(model_agegroup),age_low[model_agegroup+1],age_high),
#            mean_age_arithm=(age_low+age_high)/2, mean_age_weighted=sapply(1:max(model_agegroup),function(x) {
#              sum((popul_struct$totalpop[wpp_agegroup_low[x]:wpp_agegroup_high[x]]*
#                     popul_struct$mean_age[wpp_agegroup_low[x]:wpp_agegroup_high[x]])/
#                    sum(popul_struct$totalpop[wpp_agegroup_low[x]:wpp_agegroup_high[x]]))})) %>%
#     mutate(mean_age_weighted=ifelse(wpp_agegroup_low==wpp_agegroup_high,mean_age_arithm,mean_age_weighted)) %>% 
#     select(-mean_age_arithm)
#   rsv_age_groups %>% mutate(mean_age_weighted=agegroup_match$mean_age_weighted)
# }
