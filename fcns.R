### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ###

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
