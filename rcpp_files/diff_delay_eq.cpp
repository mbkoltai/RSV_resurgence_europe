#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
//#include <Rcpp.h>

//#include <numeric>
//#include <cmath>
//#include <RcppArmadillo.h>

// [[Rcpp::export]]
NumericMatrix rcpp_delay_eq(NumericVector t_span, List params, 
                            float I_init, float imp_val, NumericVector waning_distr){
  // parameters
  float N = as<float>(params["N"]);
  float daily_births = as<float>(params["daily_births"]);
  float beta_scale = as<float>(params["beta_scale"]); float gamma = as<float>(params["gamma"]);
  float aging_rate = as<float>(params["aging_rate"]); float death_rate = as<float>(params["death_rate"]);
  int n_wane = waning_distr.size(); int t_n = t_span.size(); 
  int n_var_disp=6;
  NumericMatrix out_delay(t_n,n_var_disp);
  NumericVector init_v(n_var_disp,0.0); init_v[1]=N-I_init; init_v[2]=I_init;
  out_delay(0,_)=init_v;
  NumericVector exp_vect(n_wane,0.0); // NumericVector new_state(n_var_disp);
  // set up vars
  float inf_term=0.0; float new_recov=0.0; int n_R_start=0; float sum_exp=0; float t_imp=0.0; // float d_waning=0;
  // LOOP
  for(int i = 1; i < t_n; ++i) {
    // infect term
    inf_term=beta_scale*out_delay(i-1,1)*out_delay(i-1,2)/N;
    // new recoveries
    new_recov=gamma*out_delay(i-1,2);
    n_R_start=i-n_wane; 
    if (n_R_start<0) n_R_start=0;
    // waning of immunity
    NumericMatrix recov_all = out_delay(Range(n_R_start,i-1),Range(5,5));
    int range_R_length=seq(n_R_start,i-1).size();
    for (int k=0; k<range_R_length-1; k++) {exp_vect[k]=exp(-aging_rate*(k+1));}; 
    // IntegerVector x_int=Range(1,range_R_length); NumericVector t_exp = as<NumericVector>(x_int); // slower 
    float d_waning=sum(rev(recov_all)*exp_vect[Range(0,range_R_length-1)]*waning_distr[Range(0,range_R_length-1)]);
    // importation
    div_t divresult; divresult=std::div(i,30); if (divresult.rem==0) {t_imp=imp_val;} else {t_imp=0;};
    // if (k_t %% 30==0) {import_inf=10} else {import_inf=0}
    // increments
    float dS=daily_births - inf_term - (aging_rate+death_rate)*out_delay(i-1,1) + d_waning;
    float dI=inf_term - new_recov - (aging_rate+death_rate)*out_delay(i-1,2) + t_imp;
    float dR=new_recov - (aging_rate+death_rate)*out_delay(i-1,3) - d_waning;
    // update variables
    out_delay(i,_)=NumericVector::create(1.0*i, // std::max(out_delay(i-1,1)+dS,0.0)
              out_delay(i-1,1)+dS, 
              out_delay(i-1,2)+dI,
              out_delay(i-1,3)+dR,
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

// this fcn duplicates rows of contact matrix according to the input 'inf_ind'
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
              int nonzero_ind=fcn_ind_seq((i_age+1)-1,comp_list[i_comp],(i_inf+1),n_age,comp_list,vec_inf_byage)-1;
              matr_aging_death_coeffs(n_seq,nonzero_ind)=vect_age_out_par(nonzero_ind);
            }
          } else {
            // 2 levels of inf in current age group (->also 2 in preceding)
            int nonzero_ind=fcn_ind_seq((i_age+1)-1,comp_list[i_comp],i_inf+1,n_age,comp_list,vec_inf_byage)-1;
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
arma::mat fcn_recov_matrix(int n_var, int n_age, arma::vec vec_inf_byage, StringVector comp_list){
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

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rcpp_age_struct_delay_eq(arma::vec t_span, arma::mat contmatr, 
                                    arma::vec pop_size, arma::vec agegr_dur, arma::vec susc_pars,
                                    arma::vec vec_inf_byage, arma::vec death_rates,
                                    List params, NumericVector waning_distr, 
                                    StringVector comp_list) {
  
  float daily_births = as<float>(params["daily_births"]);
  float beta_scale = as<float>(params["beta_scale"]); float gamma = as<float>(params["gamma"]);
  // float aging_rate = as<float>(params["aging_rate"]); // float death_rate = as<float>(params["death_rate"]);
  float I_init = as<float>(params["I_init"]); float imp_val = as<float>(params["imp_val"]);
  int n_wane = waning_distr.size(); int t_n = t_span.size(); 
  int n_age=pop_size.size();
  // build infection vector
  arma::mat susc_matr=arma::mat(sum(vec_inf_byage),sum(vec_inf_byage),arma::fill::zeros); 
  for (int k=0;k<susc_matr.n_rows;k++) {susc_matr(k,k)=susc_pars[k];}
  arma::mat cont_matr_adj = fcn_matr_ind(contmatr,vec_inf_byage);
  // matrix product: suscept*contact matrix(adjusted)
  // vector of all variables
  int n_var=comp_list.size()*sum(vec_inf_byage); 
  arma::vec all_vars_vect(n_var); // all_vars_vect=arma::linspace(1,n_var,n_var);
  // infection indices
  arma::mat inf_inds=fcn_get_seq_inds(vec_inf_byage,n_age,comp_list,"I");
  // arma::vec inf_vect=all_vars_vect;
  arma::uvec inf_ind_uvec(inf_inds.n_rows); for (int i=0;i<inf_ind_uvec.size();i++) {inf_ind_uvec(i)=inf_inds(i,1)-1;}
  arma::vec inf_vect_val=fcn_matr_subset(inf_inds,all_vars_vect,arma::linspace(0,n_age-1,n_age))/pop_size;
  // arma::vec lambda_vect=susc_matr*cont_matr_adj*inf_vect_val;
  // extended, so it has same dimension as vector of all variables
  arma::vec lambda_vect_ext = all_vars_vect; lambda_vect_ext.elem(inf_ind_uvec)=susc_matr*cont_matr_adj*inf_vect_val;
  // vector of transitions btwn compartms due to aging & death
  arma::mat matr_aging_death_coeffs(n_var,n_var);
  matr_aging_death_coeffs=fcn_aging_matrix(n_var,n_age,vec_inf_byage,agegr_dur,death_rates,comp_list);
  // matrix for waning process (R->S)
  arma::mat matr_waning(n_var,n_var); matr_waning=fcn_waning_matrix(n_var,n_age,vec_inf_byage,comp_list);
  // matrix for recovery process (R->S)
  arma::mat matr_recov(n_var,n_var); matr_recov=fcn_recov_matrix(n_var,n_age,vec_inf_byage,comp_list);
  return matr_recov;
  
  // loop forward in time
  
}

// rcpp_age_struct_delay_eq(t_span=1:1e3,contmatr=randn(n=4,m=4),pop_size=c(1.48,2.38,10.27,52.67),
//                          agegr_dur=c(2,3,13,80),susc_pars=rep(1/10,6),
//                          vec_inf_byage=c(2,2,1,1),death_rates=c(1/1e3,1/1e4,1/1e5,1/1e5),
//                          params=params_dde,waning_distr=waning_prob,comp_list=c("S","I","R"))