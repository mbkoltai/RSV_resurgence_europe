#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
//#include <Rcpp.h>

//#include <numeric>
//#include <cmath>
//#include <RcppArmadillo.h>

// [[Rcpp::export]]
NumericMatrix rcpp_delay_eq_no_agestruct(NumericVector t_span, List params, 
                                         float I_init, float imp_val, NumericVector waning_distr){
  // parameters
  float N = as<float>(params["N"]);
  float daily_births = as<float>(params["daily_births"]);
  float beta_scale=as<float>(params["beta_scale"]); float gamma = as<float>(params["gamma"]);
  float aging_rate=as<float>(params["aging_rate"]); float death_rate=as<float>(params["death_rate"]);
  int l_wane = waning_distr.size(); int t_n = t_span.size(); 
  int n_var_disp=6;
  NumericMatrix out_delay(t_n,n_var_disp);
  NumericVector init_v(n_var_disp,0.0); init_v[1]=N-I_init; init_v[2]=I_init;
  out_delay(0,_)=init_v;
  NumericVector exp_vect(l_wane,0.0); // NumericVector new_state(n_var_disp);
  // set up vars
  float inf_term=0.0; float new_recov=0.0; int n_R_start=0; float sum_exp=0; float t_imp=0.0;
  // LOOP
  for(int i_t = 1; i_t < t_n; ++i_t) {
    // infect term
    inf_term=beta_scale*out_delay(i_t-1,1)*out_delay(i_t-1,2)/N;
    // new recoveries
    new_recov=gamma*out_delay(i_t-1,2);
    n_R_start=i_t-l_wane; 
    if (n_R_start<0) n_R_start=0;
    // waning of immunity
    NumericMatrix recov_all = out_delay(Range(n_R_start,i_t-1),Range(5,5));
    int range_R_length=seq(n_R_start,i_t-1).size();
    for (int k=0; k<range_R_length-1; k++) {exp_vect[k]=exp(-aging_rate*(k+1));}; 
    // IntegerVector x_int=Range(1,range_R_length); NumericVector t_exp = as<NumericVector>(x_int); // slower 
    float d_waning=sum(rev(recov_all)*exp_vect[Range(0,range_R_length-1)]*waning_distr[Range(0,range_R_length-1)]);
    // importation
    div_t divresult; divresult=std::div(i_t,30); if (divresult.rem==0) {t_imp=imp_val;} else {t_imp=0;};
    // if (k_t %% 30==0) {import_inf=10} else {import_inf=0}
    // increments
    float dS=daily_births - inf_term - (aging_rate+death_rate)*out_delay(i_t-1,1) + d_waning;
    float dI=inf_term - new_recov - (aging_rate+death_rate)*out_delay(i_t-1,2) + t_imp;
    float dR=new_recov - (aging_rate+death_rate)*out_delay(i_t-1,3) - d_waning;
    // update variables
    out_delay(i_t,_)=NumericVector::create(1.0*i_t, // std::max(out_delay(i_t-1,1)+dS,0.0)
              out_delay(i_t-1,1)+dS, 
              out_delay(i_t-1,2)+dI,
              out_delay(i_t-1,3)+dR,
              inf_term,
              new_recov);
  }
  return out_delay;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec fcn_matr_subset(arma::mat index_matr,arma::vec target_vect,arma::vec inds) {
  arma::vec out_v(inds.size());
  for (int i_v=0;i_v<inds.size();i_v++){
    double sum_v=0;
    for (int j_sum=0;j_sum<index_matr.n_rows;j_sum++) {
      if (index_matr(j_sum,0)==inds[i_v]) {sum_v+=target_vect(index_matr(j_sum,1));}   }
    out_v[i_v]=sum_v;
  }
  return out_v;
}

// this fcn converts age, inf level & compartmnt type to the sequential index of the variable
// (S,I,R), 
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
int fcn_ind_seq(int k_age, String k_comp, int k_inf,
           int n_age, StringVector n_comp, arma::vec v_inf) {
  arma::vec k_lev(n_age); int i_comp;
  for (int i=0;i<n_comp.size();i++) {if (n_comp[i]==k_comp) {i_comp=i+1;}}
  for (int i=0;i<n_age;i++) { if (i+1<k_age) {k_lev[i]=1.0;} else {k_lev[i]=0.0;} }
  float k_seq = arma::as_scalar(k_lev.t()*v_inf)*n_comp.size() + (i_comp-1)*v_inf[k_age-1] + k_inf;
  return k_seq; // return i_comp;
}

// fcn_ind_seq(k_age=1,k_comp="I",k_inf=1,n_age=4,n_comp =c("S","I","R"),v_inf=c(2,2,1,1))

// fcn duplicates rows of contact matrix according to the input 'inf_ind' (inf levels per age groups)
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat fcn_matr_ind(arma::mat matr,arma::vec inf_ind) { // IntegerVector
  IntegerVector inds(sum(inf_ind)); int loop_size=inf_ind.size();
  arma::vec start_vals(inf_ind.size()); arma::vec end_vals(inf_ind.size());
  arma::vec out_vals(sum(inf_ind)); arma::mat out_matr(sum(inf_ind),inf_ind.size());
  for (int i=0;i<loop_size;i++) {
    if (i==0) {start_vals[i]=0;} else {start_vals[i]=start_vals[i-1]+inf_ind[i-1];} 
    end_vals[i]=start_vals[i]+inf_ind[i]-1;
    for (int j=start_vals[i];j<end_vals[i]+1;j++) {
      out_vals[j]=i; 
      for (int k_col=0;k_col<out_matr.n_cols;k_col++) {out_matr(j,k_col)=matr(i,k_col);} 
    }
  }
  return out_matr; // arma::mat out_matr=arma::join_rows(start_vals,end_vals);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat fcn_waning_matrix(int n_var, int n_age, arma::vec vec_inf_byage, StringVector comp_list){
  arma::mat matr_waning(n_var,n_var);
  for (int i_age=0;i_age<n_age;i_age++) {
    for (int i_inf=0;i_inf<vec_inf_byage[i_age];i_inf++) {
      int S_ind=fcn_ind_seq((i_age+1),"S",i_inf+1,n_age,comp_list,vec_inf_byage)-1;
      int R_ind=fcn_ind_seq((i_age+1),"R",i_inf+1,n_age,comp_list,vec_inf_byage)-1;
      matr_waning(S_ind,R_ind)=1; matr_waning(R_ind,R_ind)=-1;
    }
  }
  return matr_waning;
}



// this fcn builds the matrix of aging and death terms
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat fcn_aging_matrix(int n_var, int n_age,
                           arma::vec vec_inf_byage, arma::vec agegr_dur, arma::vec death_rates,
                           StringVector comp_list){
  arma::vec vect_age_out_par(n_var); arma::vec vect_death(n_var); 
  arma::mat matr_aging_death_coeffs(n_var,n_var);
  for (int i_age=0;i_age<n_age;i_age++) {
    for (int i_comp=0;i_comp<comp_list.size();i_comp++) {
      for (int i_inf=0;i_inf<vec_inf_byage[i_age];i_inf++) {
        // fcn_ind_seq(k_age=,k_comp=,k_inf=,n_age=,n_comp=,v_inf=)
        int n_seq=fcn_ind_seq(i_age+1,comp_list[i_comp],i_inf+1,n_age,comp_list,vec_inf_byage)-1;
        if (i_age<n_age-1) {vect_age_out_par[n_seq]=1/agegr_dur[i_age];} else {vect_age_out_par[n_seq]=0;}
        for (int i_int=0;i_int<n_age;i_int++) {matr_aging_death_coeffs(n_seq,i_int)=0.0;}
        vect_death(n_seq)=death_rates[i_age];
        if (i_age>0) {
          // only one level of infection in given age group
          if (vec_inf_byage[i_age]==1) {
            // 2 levels of infection in preceding age group
            if (vec_inf_byage[i_age-1]==2) {
              arma::vec nonzero_inds(2);
              nonzero_inds(0)=fcn_ind_seq((i_age+1)-1,comp_list[i_comp],1,n_age,comp_list,vec_inf_byage)-1;
              nonzero_inds(1)=fcn_ind_seq((i_age+1)-1,comp_list[i_comp],2,n_age,comp_list,vec_inf_byage)-1;
              matr_aging_death_coeffs(n_seq,nonzero_inds(0))=vect_age_out_par(nonzero_inds(0));
              matr_aging_death_coeffs(n_seq,nonzero_inds(1))=vect_age_out_par(nonzero_inds(1));
            } else {
              // 1 level of infection in preceding age group
              int nonzero_ind=fcn_ind_seq((i_age+1)-1,comp_list[i_comp],(i_inf+1),
                                          n_age,comp_list,vec_inf_byage)-1;
              matr_aging_death_coeffs(n_seq,nonzero_ind)=vect_age_out_par(nonzero_ind);
            }
          } else {
            // 2 levels of inf in current age group (->also 2 in preceding)
            int nonzero_ind=fcn_ind_seq((i_age+1)-1,comp_list[i_comp],
                                        i_inf+1,n_age,comp_list,vec_inf_byage)-1;
            matr_aging_death_coeffs(n_seq,nonzero_ind)=vect_age_out_par(nonzero_ind);
          }
        } // agegroups>1
        
      } // loop infect levels
    } // loop compartms
  } // loop age groups
  
  // assign 'age out' and death terms to diagonal matrix elements
  for (int k_diag=0;k_diag<n_var;k_diag++) {
    matr_aging_death_coeffs(k_diag,k_diag)=-(vect_age_out_par[k_diag]+vect_death[k_diag]);}
  return matr_aging_death_coeffs;
}

// get multiple sequential indices for a give compartment, given levels of infection per age group
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat fcn_get_seq_inds(arma::vec vec_inf_byage,int n_age,StringVector comp_list, String sel_var){
  arma::mat inf_inds(sum(vec_inf_byage),2); // arma::uvec inf_inds(sum(vec_inf_byage)); 
  int inf_ind_cnt=0;
  for (int i=0;i<n_age;i++){
    for (int j_inf=0;j_inf<vec_inf_byage[i];j_inf++){
      inf_ind_cnt=inf_ind_cnt+1; //Rprintf("inf_ind",inf_ind_cnt);
      inf_inds(inf_ind_cnt-1,0)=i;
      inf_inds(inf_ind_cnt-1,1)=fcn_ind_seq(i+1,sel_var,j_inf+1,n_age,comp_list,vec_inf_byage); // 
    }
  }
  return inf_inds;
}

// build matrix of I->R transitions (needs to be multiplied by coefficient)
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat fcn_recov_matrix(int n_var, int n_age, arma::vec vec_inf_byage, 
                           StringVector comp_list){
  arma::mat matr_recov(n_var,n_var);
  for (int i_age=0;i_age<n_age;i_age++) {
    for (int i_inf=0;i_inf<vec_inf_byage[i_age];i_inf++) {
      int R_ind=fcn_ind_seq((i_age+1),"R",i_inf+1,n_age,comp_list,vec_inf_byage)-1;
      int I_ind=fcn_ind_seq((i_age+1),"I",i_inf+1,n_age,comp_list,vec_inf_byage)-1;
      matr_recov(R_ind,I_ind)=1; matr_recov(I_ind,I_ind)=-1;
    }
  }
  return matr_recov;
}

// generate age groups for a given compartment type, listed sequentially
// eg. there are 2,2,1,1 levels of infection by age groups, 
// then the age groups are: 0,0,1,1,2,3
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec fcn_age_ind_seq_var_ind(arma::vec vec_inf_byage,int n_age){
  arma::vec age_ind_seq_var_ind(sum(vec_inf_byage)); int k_start;
  for (int k=0;k<n_age;k++) { 
    if (k==0) {k_start=0;} else {k_start=k_start+vec_inf_byage[k-1];}
    for (int k_int=k_start;k_int<k_start+vec_inf_byage[k];k_int++) {
      age_ind_seq_var_ind[k_int] =k; }
  }
  return age_ind_seq_var_ind;
}


// build waning terms
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec fcn_build_waning_vect(int i_t,int l_wane,int n_recov, 
                                arma::vec agegr_dur, arma::vec age_ind_seq_var_ind, 
                                arma::vec waning_distr,arma::mat new_recov_hist){
  
  int t_waning_start=i_t-l_wane; if (t_waning_start<0) {t_waning_start=0;}; float age_rate;
  arma::vec waning_vect(i_t-t_waning_start); arma::uvec uvec_inds(i_t-t_waning_start); 
  // Rprintf("t_waning_start: %i",t_waning_start); Rprintf("\ni_t: %i",i_t);
  uvec_inds=linspace<uvec>(0,(i_t-1)-t_waning_start,i_t-t_waning_start); 
  arma::mat exp_vect(uvec_inds.size(),1); // Rprintf("\n exp_vect size: %i",exp_vect.n_rows);
  arma::vec waning_sum(n_recov);
  for (int k_col=0;k_col<n_recov;k_col++) {
    age_rate=1/agegr_dur[age_ind_seq_var_ind[k_col]];
    exp_vect.col(0)=exp(-age_rate*linspace(1,i_t-t_waning_start,i_t-t_waning_start));
    waning_sum(k_col)=sum(
      reverse(new_recov_hist(span(0,(i_t-1)-t_waning_start),k_col)) % exp_vect % waning_distr.elem(uvec_inds));
  }
  return waning_sum;
}

// // // // // // // // // // // // // // // // // // // // // // // // // // // // 
// age-structure SIRS model with memory for waning
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rcpp_age_struct_delay_eq(arma::vec t_span, arma::mat contmatr, 
                                    arma::vec pop_size, arma::vec agegr_dur, arma::vec susc_pars,
                                    arma::vec vec_inf_byage, arma::vec death_rates,
                                    arma::vec init_vals, List params, arma::vec waning_distr, 
                                    StringVector comp_list) {
  
  float daily_births = as<float>(params["daily_births"]);
  float beta_scale = as<float>(params["beta_scale"]); float gamma = as<float>(params["gamma"]);
  float imp_val = as<float>(params["imp_val"]); // float I_init = as<float>(params["I_init"]);
  int l_wane = waning_distr.size(); int t_n = t_span.size(); 
  int n_age=pop_size.size();
  // build infection vector
  arma::mat susc_pars_matr=arma::mat(sum(vec_inf_byage),sum(vec_inf_byage),arma::fill::zeros); 
  for (int k=0;k<susc_pars_matr.n_rows;k++) {susc_pars_matr(k,k)=susc_pars[k];}
  arma::mat cont_matr_adj = fcn_matr_ind(contmatr,vec_inf_byage);
  // matrix product: suscept*contact matrix(adjusted)
  // age group indices by variable index (eg there are 6 I variables, they belong to age groups: c(1,1,2,2,3,4))
  arma::vec age_ind_seq_var_ind=fcn_age_ind_seq_var_ind(vec_inf_byage,n_age); 
  int n_var_per_type(age_ind_seq_var_ind.size());
  // vector of all variables
  int n_var=comp_list.size()*sum(vec_inf_byage); 
  arma::vec all_vars_vect(n_var); // all_vars_vect=arma::linspace(1,n_var,n_var);
  // infection indices
  arma::mat inf_inds=fcn_get_seq_inds(vec_inf_byage,n_age,comp_list,"I");
  arma::mat susc_inds=fcn_get_seq_inds(vec_inf_byage,n_age,comp_list,"S");
  arma::mat recov_inds=fcn_get_seq_inds(vec_inf_byage,n_age,comp_list,"R");
  // arma::vec inf_vect=all_vars_vect;
  arma::uvec inf_ind_uvec(inf_inds.n_rows); arma::uvec susc_inds_uvec=inf_ind_uvec; 
  arma::uvec recov_inds_uvec=inf_ind_uvec; 
  arma::mat susc_vars(inf_inds.n_rows,inf_inds.n_rows);
  for (int i=0;i<inf_ind_uvec.size();i++) {
    inf_ind_uvec(i)=inf_inds(i,1)-1; susc_inds_uvec(i)=susc_inds(i,1)-1; recov_inds_uvec(i)=recov_inds(i,1)-1;
    }
  arma::vec inf_vect_val(n_age);
  // vector of transitions btwn compartms due to aging & death
  arma::mat matr_aging_death_coeffs(n_var,n_var);
  matr_aging_death_coeffs=fcn_aging_matrix(n_var,n_age,vec_inf_byage,agegr_dur,death_rates,comp_list);
  // matrix for waning process (R->S)
  arma::mat matr_waning(n_var,n_var); matr_waning=fcn_waning_matrix(n_var,n_age,vec_inf_byage,comp_list);
  // matrix for recovery process (R->S)
  arma::mat matr_recov(n_var,n_var); matr_recov=fcn_recov_matrix(n_var,n_age,vec_inf_byage,comp_list);
  // create output matrix
  arma::mat x_out(t_span.size(),n_var);
  arma::mat new_inf_matr=x_out; 
  x_out.row(0)=init_vals.t(); arma::uvec i_t_uvec(1);
  arma::vec new_inf_ext(n_var); arma::vec lambda_vect(inf_ind_uvec.n_rows);
  arma::vec new_recov_vect(n_var); arma::vec aging_death_vect(n_var); 
  arma::vec waning_vect(n_var); 
  int t_waning_start; float age_rate;
  arma::vec birth_vector(n_var); birth_vector(0)=daily_births; // int l_wane=l_wane*2;
  arma::mat new_recov_hist(l_wane,sum(vec_inf_byage));
  arma::vec waning_terms(n_var_per_type);
  // // // // // // // // // // // // // // // // // // // // // // // // 
  // LOOP forward in TIME
for (int i_t=1;i_t<t_span.size();i_t++) {
    // infections
    inf_vect_val=fcn_matr_subset(inf_inds,x_out.row(i_t-1).t(),arma::linspace(0,n_age-1,n_age))/pop_size;
    lambda_vect=susc_pars_matr*cont_matr_adj*inf_vect_val;
    for (int k_diag=0;k_diag<inf_ind_uvec.n_rows;k_diag++) {
      susc_vars(k_diag,k_diag)=x_out(i_t-1,susc_inds_uvec[k_diag]);}
    // I variables
    new_inf_ext.elem(inf_ind_uvec) = susc_vars*lambda_vect;
    // S variables
    new_inf_ext.elem(susc_inds_uvec) = -susc_vars*lambda_vect;
    // new recoveries
    new_recov_vect = gamma*matr_recov*x_out.row(i_t-1).t();
    // aging
    aging_death_vect=matr_aging_death_coeffs*x_out.row(i_t-1).t();
    // // //
    // keep in memory recoveries of the last l_wane time steps
    if (i_t-l_wane < -1) {
      new_recov_hist.row(i_t) = new_recov_vect.elem(recov_inds_uvec).t();} else {
        new_recov_hist=join_vert(new_recov_hist.rows(1,l_wane-1),
                                 new_recov_vect.elem(recov_inds_uvec).t());
      }
    // waning 
    waning_terms=fcn_build_waning_vect(i_t, l_wane, n_var_per_type, agegr_dur,
                           age_ind_seq_var_ind,waning_distr,new_recov_hist);
    // int matr_size=new_recov_hist.n_rows; Rprintf("\n t: %i",i_t); Rprintf(", matr_size: %i",matr_size);
    waning_vect.elem(susc_inds_uvec)=waning_terms;
    waning_vect.elem(recov_inds_uvec)=-waning_terms;
    // // importation
    // div_t divresult; divresult=std::div(i_t,30); if (divresult.rem==0) {t_imp=imp_val;} else {t_imp=0;};
    // // if (k_t %% 30==0) {import_inf=10} else {import_inf=0}
    
    // add up vectors to get change in variables
    x_out.row(i_t)=x_out.row(i_t-1) + new_inf_ext.t() + new_recov_vect.t() + 
      aging_death_vect.t() + birth_vector.t() + waning_vect.t();
    
  }
  return x_out; // new_inf_matr
}

// rcpp_age_struct_delay_eq(t_span=1:1e3,contmatr=randn(n=4,m=4),pop_size=c(1.48,2.38,10.27,52.67),
//                          agegr_dur=c(2,3,13,80),susc_pars=rep(1/10,6),
//                          vec_inf_byage=c(2,2,1,1),death_rates=c(1/1e3,1/1e4,1/1e5,1/1e5),
//                          params=params_dde,init_vals=,
//                          waning_distr=waning_prob,comp_list=c("S","I","R"))

