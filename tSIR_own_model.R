### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
rm(list=ls())
lapply(c("tidyverse","wpp2019","RcppRoll","lubridate","deSolve","tictoc","pracma","Rcpp","RcppArmadillo"), 
       require, character.only=TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("fcns.R")
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# tSIR toy model for UK <=2y RSV
# attack rate ~70%
# season: weeks w40 to w07
# population: 1.475e6

# time starts at week 26
pop_size=1.475e6
n_yr=200 # simulate for n years
n_t_yr=52 # how many time units is 1 year?
# seas fprc
forcing_pk=1.55
beta_seas = spline(x=1:52, y=c(rep(1,9),rep(NA,6),c(rep(forcing_pk,1)),rep(NA,10),rep(1,26)), 
                   xout=seq.int(1,52,length.out=n_t_yr))$y
# spline(x=1:52, y=c(rep(1,10),rep(NA,7),c(rep(forcing_pk,2)),rep(NA,6),rep(1,27)), 
#             xout=seq.int(1,52,length.out=n_t_yr))$y
plot((1:52)+26,beta_seas)
I_t = c(100,rep(NA,n_yr*n_t_yr))
S_t = (2/3)*pop_size-I_t; beta_t=rep(NA,length(S_t))
# weekly births
birth_rate_wk = round(pop_size/(2*n_t_yr))*1.1
ageout_rate = birth_rate_wk
alpha_par = 1
scale_beta=1 # 1/20 # 1.094 for pk of 1.5

for (k_t in 2:length(I_t)) {
  S_t_try = (1-1/(2*n_t_yr))*S_t[k_t-1] + birth_rate_wk - I_t[k_t-1]; S_t[k_t] = max(S_t_try,0) # aging: 
  week_nr=ifelse(k_t %% n_t_yr>0,k_t %% n_t_yr,n_t_yr)
  I_t[k_t] = scale_beta*beta_seas[week_nr]*(I_t[k_t-1]^alpha_par)*S_t[k_t-1]/pop_size + runif(1,0,5)
  beta_t[k_t]=beta_seas[week_nr]
}

# S_t
# plot((50*52):(55*52),(S_t[(50*52):(55*52)])/1e6,ylab="(S+I)/1e6",type="b") # +I_t[(50*52):(55*52)]
plot((S_t+I_t)/1e6,type="l")
plot((S_t)/1e6,type="l")

# plot all I_t
plot(1:length(I_t),I_t/1e3,type="l",xlab="week no", ylab="thousand infections (incid.)")
# I_t last X yrs
n_sel_yr=5; t_wk_sel = ((n_yr-n_sel_yr)*n_t_yr):((n_yr)*n_t_yr)
# datafr
df_sel_incid = data.frame(t_wk_sel,I_t=I_t[t_wk_sel],S_t=S_t[t_wk_sel]) %>% 
  mutate(wk_nr=ifelse(t_wk_sel %% 52>0,t_wk_sel %% 52,52),cal_wk=ifelse(wk_nr<28,wk_nr+25,wk_nr-27),
         Beta_t=beta_t[t_wk_sel],
         yr_epiwk=paste0("w",wk_nr,"_yr",1+floor(t_wk_sel/52)),
         yr_epiwk=factor(yr_epiwk,levels=unique(yr_epiwk)),
         yr_calwk=paste0("w",cal_wk,"_yr",1+floor(t_wk_sel/52)+ifelse(wk_nr>27 & wk_nr<52,1,0)),
         yr_calwk=factor(yr_calwk,levels=unique(yr_calwk))) 
# %>% # pivot_longer(!c(t_wk_sel,Beta_t,yr_wk)) %>%
# plot
ggplot(df_sel_incid,aes(x=yr_calwk,y=I_t/1e3,group=1)) + geom_line() + geom_point(shape=21,size=5/6) +
  scale_x_discrete(breaks=show_every_nth(n=13)) + 
  geom_vline(data = df_sel_incid %>% filter(cal_wk %in% 49),aes(xintercept=yr_calwk),color="red",size=1/2 ) + 
  theme_bw() + standard_theme + ylab("infections (1000s)")
# plot(t_wk_sel, I_t[t_sel_int],type="b",xlab = "week nr",ylab = "incid infects")

# attack rate
sum(I_t[t_wk_sel[(17:66)]])
emp_pop_size=median(S_t[520:length(S_t)]+I_t[520:length(S_t)])
100*sum(I_t[t_wk_sel[(17:66)]])/emp_pop_size

# S(t)
plot((S_t[520:length(S_t)]+I_t[520:length(S_t)])/1e6,type = "l",ylab = "million pop")
plot(S_t[round(52*n_yr/2):(round(n_yr/2+5)*52)],type="l")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# compare to ODE model

# params
n_years=30
# calc R0 at baseline
# R0 = expression(beta_scale/(gamma + aging_rate)); with(as.list(params), eval(R0))

# i adjusted the birth rate so that pop size is stable and 
# death rate is consistent with ONS data of ~2800deaths/year = 7/day
params_ode = c(daily_births=2028, N=pop_size, beta_scale=1/5,gamma=1/7,
           aging_rate=1/(2*365),waning_rate=1/300)
params_ode["death_rate"]=params_ode["daily_births"]/pop_size - 1/(2*365)
# new introductions (to avoid extinction)
val_introd=10; new_introd <- approxfun(x=1:(365*n_years), 
      y=rep(rep(c(val_introd,rep(0,30)),12)[1:365],n_years),method="linear",rule=2)
# parameter combinations
partable=expand.grid(t_waning=c(200,350,450,1/0),forcing_pk=c(1.05,1.1,1.25,1.5,2))

tic("ode")
# 0.45-0.9 sec to simulate one model for 30 yrs -> 15-30ms/yr of simulation
for (k_p in 1:nrow(partable)) {
  if (!exists("list_out")) {list_out=list()}
  # set waning rate
  params_ode["waning_rate"]=1/partable$t_waning[k_p]
  # seas forcing
  beta_seas_ode = spline(x=1:52, y=c(rep(1,7),rep(NA,8),c(rep(partable$forcing_pk[k_p],1)),rep(NA,10),rep(1,26)), 
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
  facet_wrap(~forcing_pk,labeller=label_both) + labs(color="attack rate \nodd/even year") + xlab("waning time") +
  geom_vline(xintercept=(1:4)+1/2,size=1/2,linetype="dashed") + theme_bw() + standard_theme
# save
ggsave("output/ode_ARs_forcing_waning_effect.png",width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# delay difference equation with delay distribution for waning immunity

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
df_prep=matrix(nrow=n_years*365,ncol=length(var_list));colnames(df_prep)=var_list;df_prep[,"time"]=1:(n_years*365)
df_prep[1:length(waning_prob),2:ncol(df_prep)]=matrix(rep(c(0.7*pop_size-init_inf,init_inf,0.3*pop_size,0,0),
                length(waning_prob)),length(waning_prob),byrow=T)
# loop thru parameters (~0.55 secs to simulate one model for 50 yrs --> 10 ms/yr)
tic(paste0("DDE, ",nrow(par_perms)," iters"))
for (k_par in 1:nrow(par_perms)) {
  if (!exists("l_out_delay")) {l_out_delay=list()}
  # immunity waning
  waning_prob=dgamma(x=(1:round(2*par_perms$t_waning[k_par])),shape=par_perms$waning_shape[k_par],
              rate=par_perms$waning_shape[k_par]/par_perms$t_waning[k_par])
  waning_prob=waning_prob/sum(waning_prob)
  dgamma_plot=F
  if (dgamma_plot){ plot(waning_prob,type="b",cex=1/3,
       col=ifelse(par_perms$t_waning[k_par]>min(par_perms$t_waning),"blue","red")) }
# run calc
l_out_delay[[k_par]] <- data.frame(
  fcn_delay_eq(t_span=(length(waning_prob)+1):(n_years*365),
                params=params_dde,df_delay=df_prep,waning_distr=waning_prob,imports_mt=val_introd)) %>% 
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
  xlab("") + ylab(c("incident inf. (% popul.)","% popul.")[2]) +theme_bw() + standard_theme + 
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
bind_rows(out_delay %>% select(time,new_inf,t_waning,gamma_shape_par) %>% mutate(forcing_pk=1,modeltype="dde"),
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
            colorRampPalette(c("green", "blue"))(3))) + xlab("year") + ylab("incid. infx (% popul.)") +
  theme_bw() + standard_theme + theme(legend.position="top",legend.text=element_text(size=14))
# save
ggsave("output/ode_dde_comparison.png",width=28,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# rcpp
sourceCpp("rcpp_files/diff_delay_eq.cpp")
# 2-3ms/year of simul
tic(); xx <- rcpp_delay_eq(t_span=(length(waning_prob)+1):(n_years*365),
                           params=params_dde,I_init=10,imp_val=10,waning_distr=waning_prob); toc()
### ### ### ### ### ### ### ### 
colnames(xx)=c(colnames(df_prep)) # ,"waning","all_recovs","R_age","range_R_length","l_exp_vect"
# View(data.frame(xx) %>% mutate(N=S+I+R))
data.frame(xx) %>% mutate(N=S+I+R) %>% select(!c(new_recov)) %>% pivot_longer(!time) %>%
  filter(time>0.8*n_years*365) %>% mutate(type=ifelse(name %in% c("S","I","R","N"),"prev","incid")) %>%
  ggplot(aes(x=time/365,y=100*value/pop_size,color=name)) +
  geom_line() + facet_wrap(~type,scales="free_y",labeller=label_both) + 
  xlab("yr") + ylab("% population") + theme_bw() + standard_theme +
  theme(legend.position="top",legend.box="vertical",legend.margin=margin())
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
# compare R and rcpp models
bind_rows(l_out_delay[[length(l_out_delay)]][-(1:length(waning_prob)),1:6] %>% mutate(time=time-700,code="R"),
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
  t_peak=(data.frame(findpeaks(out_delay$new_inf[out_delay$gamma_shape_par %in% par_perms$waning_shape[x] & 
    out_delay$time>init_t_cutoff & (out_delay$t_waning %in% par_perms$t_waning[x])],
      minpeakheight=peaks_by_par$peak_new_inf[x]*0.9))  ) ) )) %>% select(c(p,t_peak.X1,t_peak.X2)) %>% 
  rename(t_peak=t_peak.X2,peak_value=t_peak.X1) %>% relocate(t_peak,.before=peak_value) %>%
  group_by(p) %>% mutate(t_peak=t_peak+init_t_cutoff,t_interpk=t_peak-lag(t_peak)) %>% 
  filter(!is.na(t_interpk) & t_interpk>10) %>%
  mutate(waning_shape=par_perms$waning_shape[p],t_waning=par_perms$t_waning[p],
         param=factor(paste0("shape par.=",waning_shape,"\n imm. dur.=",t_waning)),
         date=as.Date(t_peak,origin=as.Date(paste0(2022-n_years,"-06-22")))) %>% relocate(date,.after=t_peak)

# plot peak distrib
df_periods %>% ggplot() + geom_jitter(aes(x=param,y=t_interpk,color=factor(p),size=peak_value),alpha=1/2) + 
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
