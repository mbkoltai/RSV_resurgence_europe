### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
rm(list=ls())
lapply(c("tidyverse","wpp2019","RcppRoll","lubridate","deSolve","tictoc","pracma","Rcpp","RcppArmadillo"), 
       require, character.only=TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("fcns.R"); sourceCpp("rcpp_files/diff_delay_eq.cpp")
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# DDE Rcpp model

params_dde=c("daily_births"=650e3/365,"gamma"=1/7,"beta_scale"=1/5,"I_init"=10,"imp_val"=10)
par_perms=expand.grid(birthrate=c(round(700e3/365)),waning_shape=c(8,10,16,20),t_waning=c(200,350))
k_gamma=4; waning_prob=dgamma(x=(1:round(2*par_perms$t_waning[k_gamma])),shape=par_perms$waning_shape[k_gamma],
                   rate=par_perms$waning_shape[k_gamma]/par_perms$t_waning[k_gamma])
waning_prob=waning_prob/sum(waning_prob); plot(waning_prob,cex=1/2)
# source: 
# sourceCpp("rcpp_files/diff_delay_eq.cpp")
# run
agegr_pop_size=c(1.48,2.38,10.27,52.67)*1e6; pop_stat_sol=c(1295,1941,8395,55664)*1e3
init_conds=unlist(sapply(1:4, function(x) sapply(c(pop_stat_sol[x]/c(2,2,1,1)[x]-10,10,0), 
                                                 function(x_var) rep(x_var,c(2,2,1,1)[x])) ))
I_age_table=data.frame(fcn_get_seq_inds(vec_inf_byage=c(2,2,1,1),n_age=4,comp_list=c("S","I","R"),sel_var="I"))
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# CONTACT MATRIX
# socialmixr package
library('socialmixr') # devtools::install_github('sbfnk/socialmixr')
# load polymod
data(polymod)
c_m_polymod=contact_matrix(polymod, countries="United Kingdom", age.limits=c(0,2,5,18),symmetric=T)$matrix
c_m_polymod_recipr=fun_recipr_contmatr(c_m_polymod,age_group_sizes = agegr_pop_size)
# test if reciprocal
# cc=expand.grid(1:4,1:4)[-(c(0:3)*5+1),]
# sapply(1:12, function(x) (c_m_polymod_recipr[cc[x,1],cc[x,2]]*agegr_pop_size[cc[x,1]])/(
#   c_m_polymod_recipr[cc[x,2],cc[x,1]]*agegr_pop_size[cc[x,2]]))

# synth contact matr (Prem 2021)
load("data/contact_all.rdata")
c_m_synth=contact_all$GBR

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# run
# init_conds=out_m[nrow(out_m),]
tic();
out_m=rcpp_age_struct_delay_eq(t_span=1:(60*365),contmatr=c_m_polymod_recipr,
                    pop_size=agegr_pop_size,agegr_dur=c(2,3,13,80)*365,
                    susc_pars=c(age1=c(1/5,1/5)*2,age2=c(1/5,1/5),age3=1/10,age4=1/10)/4,init_vals=init_conds,
                    vec_inf_byage=c(2,2,1,1),death_rates=c(2/1e3,0.2/1e3,0.15/1e3,11.6/1e3)/365,
                    params=params_dde,waning_distr=waning_prob,comp_list=c("S","I","R"),
                    out_type=c("all_var","incid")[1])
toc(); 

stat_agegr_pop_size=sapply(1:4, function(x) 
  sum(init_conds[ifelse(x>1,cumsum(c(2,2,1,1)*3)[x-1]+1,1):(cumsum(c(2,2,1,1)*3))[x]]))

# PLOT inf incidence
data.frame(t=1:nrow(out_m),out_m) %>% filter(t>0.95*nrow(out_m)) %>% 
  pivot_longer(!t) %>% mutate(k_var=I_age_table$X2[as.numeric(gsub("X","",name))],
      n_age=sapply(k_var, function(x) I_age_table$X1[I_age_table$X2 %in% x])+1,
      n_inf=k_var-(3*sapply(n_age, function(x) sum(c(0,2,2,1,1)[1:x]))+c(2,2,1,1)[n_age])) %>% #
   mutate(perc_value=100*value/stat_agegr_pop_size[n_age],n_age=paste0("agegroup ",n_age)) %>%
ggplot() + geom_line(aes(x=t/365,y=perc_value,color=factor(n_inf))) + facet_wrap(~n_age,scales="free_y") + 
  xlab("year") + ylab(c("infxs (1000)","% age group")[2]) + scale_x_continuous(expand=expansion(0.01,0)) + 
  labs(color="infect. level") + theme_bw() + standard_theme

# attack rate (1yr period)
data.frame(t=1:nrow(out_m),out_m) %>% filter(t>=58*365 & t<=(58+1)*365) %>% pivot_longer(!t) %>% 
  mutate(k_var=I_age_table$X2[as.numeric(gsub("X","",name))],
         n_age=sapply(k_var, function(x) I_age_table$X1[I_age_table$X2 %in% x])+1) %>% 
  group_by(n_age) %>% summarise(tot_inf=sum(value),n_age=unique(n_age),att_rate=tot_inf/agegr_pop_size[n_age])

# plot infection prevalence
data.frame(t=1:nrow(out_m),out_m[,I_age_table$X2]) %>% filter(t>(0.98*nrow(out_m)/365)*365) %>% pivot_longer(!t) %>%
  mutate(k_var=I_age_table$X2[as.numeric(gsub("X","",name))],
   n_age=sapply(k_var, function(x) I_age_table$X1[I_age_table$X2 %in% x])+1,
   n_inf=k_var-(3*sapply(n_age, function(x) sum(c(0,2,2,1,1)[1:x]))+c(2,2,1,1)[n_age])) %>% 
  mutate(perc_value=100*value/stat_agegr_pop_size[n_age]) %>%
ggplot() + geom_line(aes(x=t/365,y=value/1e3,color=factor(n_inf))) + facet_wrap(~n_age,scales="free_y") +
  xlab("year") + ylab(c("infxs (1000)","% age group")[1]) + theme_bw() + standard_theme

# % susceptible
S_age_table=data.frame(fcn_get_seq_inds(vec_inf_byage=c(2,2,1,1),n_age=4,comp_list=c("S","I","R"),sel_var="S"))
data.frame(t=1:nrow(out_m),out_m[,S_age_table$X2]) %>% pivot_longer(!t) %>% 
  mutate(k_var=S_age_table$X2[as.numeric(gsub("X","",name))],
     n_age=sapply(k_var, function(x) S_age_table$X1[S_age_table$X2 %in% x])+1,
     n_inf=k_var-(3*sapply(n_age, function(x) sum(c(0,2,2,1,1)[1:x])))) %>% filter(t<20*365) %>%
  mutate(perc_value=100*value/stat_agegr_pop_size[n_age]) %>%
ggplot() + geom_line(aes(x=t/365,y=perc_value,color=factor(n_inf))) + facet_wrap(~n_age,scales="free_y") + 
  xlab("year") + ylab(c("infxs (1000)","% age group")[2]) + theme_bw() + standard_theme

# multiple vars
# t_end=1255; n_var=7
# for (i in (1:6)){ 
#   i_var=c(1,3,5,2,4,6)[i]-1
#   if (i==1) {
#     plot(1:t_end,round(out_m[1:t_end,n_var+i_var])/1e3,type="l",
#         col=c("black","blue","red")[3-i%%3],ylab="thousand") } else {
#     lines(1:t_end,round(out_m[1:t_end,n_var+i_var])/1e3,type="l",col=c("black","blue","red")[3-i%%3]) }
# }

# age group totals
bind_cols(lapply(list(1:6,7:12,13:15,16:18,1:18), function(x) rowSums(out_m[,x])),.name_repair="unique") %>%
  rename(agegr1=`...1`,agegr2=`...2`,agegr3=`...3`,agegr4=`...4`,total=`...5`) %>% 
  mutate(t=1:nrow(out_m)) %>% pivot_longer(!t) %>%
ggplot() + geom_line(aes(x=t/365,y=value/1e3)) + facet_wrap(~name,scales="free_y") + ylab("thousands") +
  theme_bw() + standard_theme

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ODE model (1 pop group)

# params
n_years=30
pop_size=1.475e6
# calc R0 at baseline
# R0 = expression(beta_scale/(gamma + aging_rate)); with(as.list(params), eval(R0))

# i adjusted the birth rate so that pop size is stable and 
# death rate is consistent with ONS data of ~2800deaths/year = 7/day
params_ode = c(daily_births=2028, N=pop_size, beta_scale=1/5,gamma=1/7,
               aging_rate=1/(2*365),waning_rate=1/300)
params_ode["death_rate"]=params_ode["daily_births"]/pop_size - 1/(2*365)
# new introductions (to avoid extinction)
val_introd=10; new_introd <- approxfun(x=1:(365*n_years), 
                                       y=rep(rep(c(val_introd,rep(0,30)),12)[1:365],n_years),
                                       method="linear",rule=2)
# parameter combinations
partable=expand.grid(t_waning=c(200,350,450,1/0),forcing_pk=c(1.05,1.1,1.25,1.5,2))

tic("ode")
# 0.45-0.9 sec to simulate one model for 30 yrs -> 15-30ms/yr of simulation
for (k_p in 1:nrow(partable)) {
  if (!exists("list_out")) {list_out=list()}
  # set waning rate
  params_ode["waning_rate"]=1/partable$t_waning[k_p]
  # seas forcing
  beta_seas_ode = spline(x=1:52, y=c(rep(1,7),rep(NA,8),
                                     c(rep(partable$forcing_pk[k_p],1)),rep(NA,10),rep(1,26)), 
                         xout=seq.int(1,52,length.out=365))$y; beta_seas_ode[beta_seas_ode<1]=1
  # interpolation fcns
  forcing_fcn <- approxfun(x=1:(365*n_years), y=rep(beta_seas_ode,n_years),method="linear",rule=2)
  # plot(forcing_fcn(1:(1*365)),cex=1/3) # print(params_ode)
  list_out[[k_p]] = as.data.frame(
    ode(c(S=1-10/pop_size, I=10/pop_size, R=0,new_inf=0)*pop_size,times=seq(0, 365*n_years, by=1),
        sir_rsv,params_ode)) %>%
    mutate(incid_I=new_inf-lag(new_inf),date=as.Date(time,origin=paste0(2022-n_years,"-06-22")),
           t_waning=partable$t_waning[k_p],forcing_pk=partable$forcing_pk[k_p])
  message(paste0(c(k_p,partable[k_p,]),collapse=","))
  # collect
  if (k_p==nrow(partable)) {out_ode = bind_rows(list_out); rm(list_out)}
}
toc()

# plot
out_ode %>% select(!c(new_inf,I,R)) %>% pivot_longer(!c(time,date,t_waning,forcing_pk)) %>% 
  filter(time>(n_years-5)*365) %>% #  & time<max(time)-30
  mutate(type=ifelse(name %in% c("S","R"),"cumul","inc")) %>% filter(!name %in% "S") %>%
  ggplot(aes(x=date,y=100*value/pop_size,color=factor(100*(forcing_pk-1)),group=forcing_pk)) +
  geom_line() + # geom_point(aes(x=max(out_ode$date)-365,y=0),color=NA) +
  facet_wrap(~t_waning,scales="free_y",labeller=label_both) + 
  labs(color="forcing (%)") + xlab("") + ylab("incident infections (% population)") +
  scale_x_date(date_breaks="4 month",expand=expansion(0.01,0)) + scale_color_viridis_d() +
  theme_bw() + standard_theme
# save
ggsave("output/ode_dynamics_forcing_waning_effect.png",width=33,height=22,units="cm")

# attack rate
out_ode %>% filter(date>as.Date(paste0(year(max(out_ode$date))-2,"-06-01")) & 
                     date<as.Date(paste0(year(max(out_ode$date)),"-06-01"))) %>%
  mutate(year=ifelse(month(date)<6,year(date)-1,year(date))) %>% group_by(t_waning,forcing_pk) %>%
  summarise(AR_yr1=100*sum(incid_I[year==year(max(out_ode$date))-2])/pop_size,
            AR_yr2=100*sum(incid_I[year==year(max(out_ode$date))-1])/pop_size) %>%
  pivot_longer(!c(t_waning,forcing_pk)) %>%
  ggplot() + 
  geom_point(aes(x=factor(t_waning),y=value,color=name),position=position_dodge(width=0.5),size=3) + 
  facet_wrap(~forcing_pk,labeller=label_both) + labs(color="attack rate \nodd/even year") + 
  xlab("waning time") + geom_vline(xintercept=(1:4)+1/2,size=1/2,linetype="dashed") + 
  theme_bw() + standard_theme
# save
ggsave("output/ode_ARs_forcing_waning_effect.png",width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Single popul group delay difference equation with delay distribution for waning immunity

# length of simulation
n_years=30
# seasonal forcing (1=baseline)
forcing_pk_val=1
beta_seas_ode=spline(x=1:52, y=c(rep(1,9),rep(NA,6),c(rep(forcing_pk_val,1)),rep(NA,10),rep(1,26)), 
                     xout=seq.int(1,52,length.out=365))$y; beta_seas_ode[beta_seas_ode<1]=1
forcing_fcn <- approxfun(x=1:(365*n_years), y=rep(beta_seas_ode,n_years),method="linear",rule=2)
# 
par_perms=expand.grid(birthrate=c(round(700e3/365)),waning_shape=c(8,10,16,20),t_waning=c(200,350))
params_dde=c("daily_births"=par_perms$birthrate[1],"gamma"=1/7,"N"=pop_size,"beta_scale"=1/5,
             "aging_rate"=1/(pop_size/par_perms$birthrate[1]),"death_rate"=0)
# create matrix for t<0
var_list=c("time","S","I","R","new_inf","new_recov")
init_inf=10
df_prep=matrix(nrow=n_years*365,ncol=length(var_list))
colnames(df_prep)=var_list;df_prep[,"time"]=1:(n_years*365)
df_prep[1:length(waning_prob),2:ncol(df_prep)]=matrix(rep(c(0.7*pop_size-init_inf,init_inf,0.3*pop_size,0,0),
                                                          length(waning_prob)),length(waning_prob),byrow=T)
# loop thru parameters (~0.55 secs to simulate one model for 50 yrs --> 10 ms/yr)
tic(paste0("DDE, ",nrow(par_perms)," iters"))
for (k_par in 1:nrow(par_perms)) {
  if (!exists("l_out_delay")) {l_out_delay=list()}
  # immunity waning
  waning_prob=dgamma(x=(1:round(2*par_perms$t_waning[k_par])),
                     shape=par_perms$waning_shape[k_par],
                     rate=par_perms$waning_shape[k_par]/par_perms$t_waning[k_par])
  waning_prob=waning_prob/sum(waning_prob)
  dgamma_plot=F
  if (dgamma_plot){ plot(waning_prob,type="b",cex=1/3,
                         col=ifelse(par_perms$t_waning[k_par]>min(par_perms$t_waning),"blue","red")) }
  # run calc
  l_out_delay[[k_par]] <- data.frame(
    fcn_delay_eq(t_span=(length(waning_prob)+1):(n_years*365),
                 params=params_dde,df_delay=df_prep,
                 waning_distr=waning_prob,imports_mt=val_introd)) %>% 
    mutate(birth_rate=params_dde[names(params_dde)[1]],
           gamma_shape_par=round(par_perms$waning_shape[k_par],3),
           t_waning=par_perms$t_waning[k_par])
  message(k_par,"/",nrow(par_perms))
  # collect results
  if (k_par==nrow(par_perms)) {out_delay <- bind_rows(l_out_delay); rm(l_out_delay) }
}
toc()

# plot
out_delay %>% mutate(N=S+I+R,date=as.Date(time,origin=as.Date(paste0(2022-n_years,"-06-22")))) %>% 
  select(!c(new_recov,R,I,N)) %>% # filter(birth_rate>0) %>%
  pivot_longer(!c(time,date,birth_rate,gamma_shape_par,t_waning)) %>% 
  filter(time>round(n_years-4)*365) %>% #  & time<365*round(n_years*3/4)
  mutate(type=ifelse(name %in% c("new_inf"),"infection (incid.)","S (I,R,N)")) %>% 
  ggplot(aes(x=date,y=100*value/pop_size,color=factor(t_waning),group=t_waning)) + # ,group=t_waning
  geom_line(size=1.1) + facet_grid(type~gamma_shape_par,scales="free_y",labeller=label_both) + 
  labs(linetype="births/day",color="immunity duration (mean)") +
  scale_x_date(date_breaks="6 months",expand=expansion(0.01,0)) + 
  xlab("") + ylab(c("incident inf. (% popul.)","% popul.")[2]) + theme_bw() + standard_theme +
  theme(legend.position="top",legend.box="vertical",legend.margin=margin())
#
# save
ggsave(paste0("output/delay_eq_noforcing_waningduration_gammashape_effect_",val_introd,"imports.png"),
       width=28,height=22,units="cm")

# bind_rows(out_delay_all_susc %>% mutate(init_susc="all"),out_delay %>% mutate(init_susc="70%")) %>% 
#   mutate(N=S+I+R,date=as.Date(time,origin=as.Date(paste0(2022-n_years,"-06-22")))) %>% 
#   select(!c(new_recov,R,I,N)) %>% # filter(birth_rate>0) %>%
#   pivot_longer(!c(time,date,birth_rate,gamma_shape_par,t_waning,init_susc)) %>% 
#   filter(time>round(n_years-4)*365) %>% #  & time<365*round(n_years*3/4)
#   mutate(type=ifelse(name %in% c("new_inf"),"infection (incid.)","S (I,R,N)")) %>% 
# ggplot(aes(x=date,y=100*value/pop_size,linetype=factor(init_susc),color=factor(t_waning))) + # ,group=t_waning
#   geom_line(size=1.03) + facet_grid(type~gamma_shape_par,scales="free_y",labeller=label_both) + 
#   labs(linetype="initial suscept.",color="immunity duration (mean)") +
#   scale_x_date(date_breaks="6 months",expand=expansion(0.01,0)) + 
#   xlab("") + ylab(c("incident inf. (% popul.)","% popul.")[2]) + theme_bw() + standard_theme + 
#   theme(legend.position="top",legend.box="vertical",legend.margin=margin())

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# compare ODE and DDE outputs
bind_rows(out_delay %>% select(time,new_inf,t_waning,gamma_shape_par) %>% 
            mutate(forcing_pk=1,modeltype="dde"),
          out_ode %>% select(c(time,incid_I,t_waning,forcing_pk)) %>% rename(new_inf=incid_I) %>% 
            mutate(modeltype="ode")) %>% 
  filter(time>(n_years-3)*365 & t_waning<450 & forcing_pk<=1.5 & forcing_pk!=1.05 & 
           (is.na(gamma_shape_par)|gamma_shape_par>8)) %>%
  mutate(type_fact=ifelse(modeltype %in% "ode",paste0("t_w",t_waning,"_f",forcing_pk),
                          paste0("t_w",t_waning,"_f",gamma_shape_par)),
         colorvar=ifelse(modeltype %in% "ode",paste0("ode_f",forcing_pk),
                         paste0("dde_g",gamma_shape_par)),
         colorvar=factor(colorvar,levels=unique(colorvar))) %>%
  ggplot(aes(x=time/365,y=100*new_inf/pop_size,color=colorvar,group=type_fact)) + geom_line(size=1.1) + 
  facet_wrap(~t_waning,nrow=2) + labs(color="") + guides(color=guide_legend(ncol=2)) +
  scale_x_continuous(expand=expansion(0.01,0)) + scale_y_continuous(expand=expansion(0.01,0)) +
  scale_color_manual(values=c(colorRampPalette(c("brown1", "darkred"))(3),
                              colorRampPalette(c("green", "blue"))(3))) + 
  xlab("year") + ylab("incid. infx (% popul.)") +
  theme_bw() + standard_theme + theme(legend.position="top",legend.text=element_text(size=14))
# save
ggsave("output/ode_dde_comparison.png",width=28,height=22,units="cm")

# rcpp # sourceCpp("rcpp_files/diff_delay_eq.cpp")
# 2-3ms/year of simul
tic(); xx <- rcpp_delay_eq(t_span=(length(waning_prob)+1):(n_years*365),
                           params=params_dde,I_init=10,imp_val=10,waning_distr=waning_prob); toc()
### ### ### ### ### ### ### ### 
colnames(xx)=c(colnames(df_prep)) # ,"waning","all_recovs","R_age","range_R_length","l_exp_vect"
# View(data.frame(xx) %>% mutate(N=S+I+R))
data.frame(xx) %>% mutate(N=S+I+R) %>% select(!c(new_recov)) %>% pivot_longer(!time) %>%
  filter(time>0.8*n_years*365) %>% 
  mutate(type=ifelse(name %in% c("S","I","R","N"),"prev","incid")) %>%
  ggplot(aes(x=time/365,y=100*value/pop_size,color=name)) +
  geom_line() + facet_wrap(~type,scales="free_y",labeller=label_both) + 
  xlab("yr") + ylab("% population") + theme_bw() + standard_theme +
  theme(legend.position="top",legend.box="vertical",legend.margin=margin())
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
# compare R and rcpp models
bind_rows(l_out_delay[[length(l_out_delay)]][-(1:length(waning_prob)),1:6] %>% 
            mutate(time=time-700,code="R"),
          data.frame(xx) %>% mutate(code="rcpp")) %>% 
  mutate(N=S+I+R) %>% select(!c(new_recov)) %>% pivot_longer(!c(time,code)) %>%
  filter(time>15e3) %>% mutate(type=ifelse(name %in% c("S","I","R","N"),"prev","incid")) %>% # 
  ggplot(aes(x=time,y=value,color=name,linetype=code)) + # 100*/pop_size
  geom_line() + facet_wrap(~type,scales="free_y",labeller=label_both) + 
  xlab("yr") + ylab("% population") + theme_bw() + standard_theme +
  theme(legend.position="top",legend.box="vertical",legend.margin=margin())

# are the oscillations shifting? lets calculate distance btwn peaks
# peaks after stabilisation (use pracma pckg)
peaks_by_par = left_join(out_delay %>% filter(time>max(time)*4/5) %>%
                           group_by(gamma_shape_par,t_waning) %>%
                           summarise(peak_new_inf=max(new_inf),
                                     peak_new_inf_perc_pop=100*peak_new_inf/pop_size,trough=min(new_inf)),
                         par_perms %>% rename(gamma_shape_par=waning_shape) %>% mutate(n=row_number()), 
                         by=c("gamma_shape_par","t_waning")) %>% arrange(n)

init_t_cutoff=20*365
df_periods <- bind_rows(
  lapply(1:nrow(par_perms), function(x) data.frame(p=x,
                        t_peak=(data.frame(
                        findpeaks(out_delay$new_inf[out_delay$gamma_shape_par %in% par_perms$waning_shape[x] & 
                        out_delay$time>init_t_cutoff & (out_delay$t_waning %in% par_perms$t_waning[x])],
                        minpeakheight=peaks_by_par$peak_new_inf[x]*0.9))  ) ) )) %>% 
  select(c(p,t_peak.X1,t_peak.X2)) %>% 
  rename(t_peak=t_peak.X2,peak_value=t_peak.X1) %>% relocate(t_peak,.before=peak_value) %>%
  group_by(p) %>% mutate(t_peak=t_peak+init_t_cutoff,t_interpk=t_peak-lag(t_peak)) %>% 
  filter(!is.na(t_interpk) & t_interpk>10) %>%
  mutate(waning_shape=par_perms$waning_shape[p],t_waning=par_perms$t_waning[p],
         param=factor(paste0("shape par.=",waning_shape,"\n imm. dur.=",t_waning)),
         date=as.Date(t_peak,origin=as.Date(paste0(2022-n_years,"-06-22")))) %>% 
  relocate(date,.after=t_peak)

# plot peak distrib
df_periods %>% 
  ggplot() + geom_jitter(aes(x=param,y=t_interpk,color=factor(p),size=peak_value),alpha=1/2) + 
  theme_bw() + standard_theme + theme(axis.text.x = element_text(angle=0)) + xlab("") + labs(color="")
# save
ggsave(paste0("delay_eq_noforcing_period_length.png"),width=28,height=20,units="cm")

# attack rates
# find troughs
df_periods %>% group_by(p) %>% mutate(peak_periods=ceiling(row_number()/2))

for (k_par in 1:nrow(par_perms)){
  if (!exists("l_troughs")) {l_troughs=list()}
  
  peaks_by_par = df_periods %>% filter(p==k_par) %>% 
    select(p,t_peak,waning_shape,t_waning) %>% mutate(n=row_number()) # %>% group_by(p,peak_periods) %>%
  # mutate(n=n()) %>% filter(n!=1)
  # 
  l_troughs[[k_par]] <- bind_rows(lapply(1:(max(peaks_by_par$n)-1), function(x)
    left_join(out_delay,par_perms %>% rename(gamma_shape_par=waning_shape) %>% mutate(p=row_number())) %>%
      select(c(time,p,new_inf,gamma_shape_par,t_waning)) %>% 
      filter(p==k_par & time>=min(peaks_by_par$t_peak[x]) & time<=max(peaks_by_par$t_peak[x+1])) %>%
      summarise(min_time=time[new_inf==min(new_inf)],min_new_inf=min(new_inf),
                sum_inf=sum(new_inf)) %>% mutate(p=k_par) ))
}
df_troughs <- bind_rows(l_troughs) %>% group_by(p) %>% mutate(t_intermin=min_time-lag(min_time))

# plot
left_join(df_troughs,df_periods %>% select(p,param) %>% distinct()) %>% 
  ggplot() + geom_jitter(aes(x=param,y=100*sum_inf/pop_size,color=factor(p),size=t_intermin),alpha=1/2) + 
  theme_bw() + standard_theme + theme(axis.text.x=element_text(angle=0)) + 
  xlab("") + ylab("attack rate (%)") + labs(color="") + guides(color="none")
# save
ggsave(paste0("output/delay_eq_noforcing_AR_period_length.png"),width=25,height=18,units="cm")
