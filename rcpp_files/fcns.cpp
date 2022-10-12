#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// #include <Rcpp.h>

//#include "f_mean.h"
//#include "f_l2_norm.h"
//#include <numeric>
//#include <ranges>
//#include <iostream>
//#include <algorithm>

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// /*** R
// library(microbenchmark)
// x <- runif(1e5)
// microbenchmark(
//  mean(x),
//  meanC(x)
// )
// */

// // // // // // // // // // // // // // // // // // // // // // // //
// // // // // // // // // // // // // // // // // // // // // // // //

// [[Rcpp::export]]
NumericVector f2(NumericVector x) {
  int n = x.size(); 
  NumericVector out(n);
  out[0] = x[0];
  for(int i = 1; i < n; ++i) {
    out[i] = out[i - 1] + x[i];
  }
  return out;
}

// [[Rcpp::export]]
NumericVector f3(NumericVector x) {
  int n = x.size(); 
  NumericVector out(n);
  double mean_x=0;
  mean_x = meanC(x);
  for(int i = 1; i < n; ++i) {
    out[i] = x[i]/mean_x;
  }
  return out;
}

// NumericVector f3_h(NumericVector x) {
//   int n = x.size(); 
//   NumericVector out(n);
//   double mean_x=0;
//   mean_x = f_mean(x);
//   for(int i = 1; i < n; ++i) {
//     out[i] = x[i]/mean_x;
//   }
//   return out;
// }

// double f_err(NumericVector x_data, NumericVector x_pred) {
//   int n = x_data.size(); 
//   NumericVector out(n);
//   for(int i = 1; i < n; ++i) {
//     out[i] = x_data[i] - x_pred[i];
//   }
//   double err=0;
//   err = f_l2_norm(out);
//   return err;
// }

// [[Rcpp::export]]
NumericVector attribs() {
  NumericVector out = NumericVector::create(1, 2, 3);
  out.names() = CharacterVector::create("a", "b", "c");
  out.attr("my-attr") = "my-value";
  out.attr("class") = "my-class";
  return out;
}


// [[Rcpp::export]]
List lapply_rcpp(List input, Function f) {
  int n = input.size();
  List out(n);
  
  for(int i = 0; i < n; i++) {
    out[i] = f(input[i]);
  }
  
  return out;
}

// [[Rcpp::export]]
LogicalVector is_naC(NumericVector x) {
  int n = x.size();
  LogicalVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = NumericVector::is_na(x[i]);
  }
  return out;
}

//most arithm fcns vectorised 
// [[Rcpp::export]]
NumericVector pdistC2(double x, NumericVector ys) {
  return sqrt(pow((x - ys), 2));
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double sum3(NumericVector x) {
  double total = 0;
  
  NumericVector::iterator it;
  for(it = x.begin(); it != x.end(); ++it) {
    total += *it;
  }
  return total;
}

// [[Rcpp::export]]
double sum4(NumericVector x) {
  return std::accumulate(x.begin(), x.end(), 0.0);
}

// [[Rcpp::export]]
NumericVector exp_vect(double x, NumericVector ys, double v_max) {
  int y_l=ys.size()-1;
  // NumericVector x_vec=exp(-x*rev(ys[seq(0,y_l)]))*ms(_,0);
  // NumericVector x_vec=exp(-x*rev(ys[seq(0,y_l)]))*ms(_,0);
  // NumericVector x_vec=rev(ys)*Rcpp::exp(-x*Range(1,v_max));
  // NumericVector x_vec(v_max-1); for (int k=0; k<v_max-1; k++) {x_vec[k]=exp(-k*x);}; 
  // std::vector<double> y(v_max,1.0);
  IntegerVector x_int=Range(1,v_max);
  NumericVector xx = as<NumericVector>(x_int);
  NumericVector x_vec=exp(-x*xx);
  return x_vec;
}

// [[Rcpp::export]]
bool bool_vect(NumericVector x) {
bool b = all( x < 3.0 ).is_true();
return b;
}

// int mod(int a, int b) {
//   // int c = a % b;
//   div_t divresult; divresult=std::div(a,b);
//   return(divresult.rem);
// }

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec matr_vec_prod(arma::mat matr,arma::vec vect) {
  arma::vec v_prod = matr*vect;
  return v_prod;
}

// [[Rcpp::export]]
int fcn_vect_sum(IntegerVector vec_int, NumericVector vect_num){
  int v_sum = sum(vec_int); float v_sum_num = sum(vect_num); 
  return v_sum_num;
}

// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::export]]
// arma::mat matr_ind(arma::mat matr,IntegerVector inf_ind) {
//   IntegerVector inds(sum(inf_ind)); int loop_size=inf_ind.size();
//   arma::vec start_vals(inf_ind.size()); arma::vec end_vals(inf_ind.size());
//   arma::vec out_vals(sum(inf_ind)); arma::mat out_matr(sum(inf_ind),inf_ind.size());
//   for (int i=0;i<loop_size;i++) {
//     if (i==0) {start_vals[i]=0;} else {start_vals[i]=start_vals[i-1]+inf_ind[i-1];} 
//     end_vals[i]=start_vals[i]+inf_ind[i]-1;
//   for (int j=start_vals[i];j<end_vals[i]+1;j++) {
//     out_vals[j]=i; 
//     for (int k_col=0;k_col<out_matr.n_cols;k_col++) {out_matr(j,k_col)=matr(i,k_col);} 
//     }
//   }
//   // arma::mat out_matr=arma::join_rows(start_vals,end_vals);
//   return out_matr;
// }
// 

// subset armadillo vector, sum selected elements
// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::export]]
// arma::vec fcn_matr_subset(arma::mat index_matr,arma::vec target_vect,arma::vec inds) {
//   arma::vec out_v(inds.size());
//   for (int i_v=0;i_v<inds.size();i_v++){
//   double sum_v=0;
//   for (int j_sum=0;j_sum<index_matr.n_rows;j_sum++) {
//     if (index_matr(j_sum,0)==inds[i_v]) {sum_v+=target_vect(index_matr(j_sum,1));}   }
//   out_v[i_v]=sum_v;
//   }
//   return out_v;
// }


// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::export]]
// arma::vec fcn_assign_vect_mult(arma::vec vect_fill,arma::uvec vect_inds,arma::vec vect_vals) {
//   arma::vec vect_out=vect_fill;
//   vect_out.elem(vect_inds)=vect_vals;
//   return vect_out;
// }

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


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec fcn_aging_matrix(int n_var, int n_age,arma::vec vec_inf_byage, StringVector comp_list, arma::vec agegr_dur){
arma::vec vect_age_out_par(n_var); 
arma::mat vect_age_in_par_matr(n_var,n_var);
for (int i_age=0;i_age<n_age;i_age++) {
  for (int i_comp=0;i_comp<comp_list.size();i_comp++) {
    for (int i_inf=0;i_inf<vec_inf_byage[i_age];i_inf++) {
      int n_seq=fcn_ind_seq(i_age,comp_list[i_comp],i_inf,n_age,comp_list,vec_inf_byage);
      if (i_age<n_age-1) {vect_age_out_par[n_seq]=1/agegr_dur[i_age];} else {vect_age_out_par[n_seq]=0;}
      for (int i_int=0;i_int<n_age;i_int++) {vect_age_in_par_matr(n_seq-1,i_int)=0.0;}
      if (i_age>1) {
        // only one level of infection in given age group
        if (vec_inf_byage[i_age]==1) {
          // 2 levels of infection in preceding age group
          if (vec_inf_byage[i_age-1]==2) { 
            arma::vec nonzero_inds(2);
            nonzero_inds(0)=fcn_ind_seq(i_age-1,comp_list[i_comp],1,n_age,comp_list,vec_inf_byage);
            nonzero_inds(1)=fcn_ind_seq(i_age-1,comp_list[i_comp],2,n_age,comp_list,vec_inf_byage);
            vect_age_in_par_matr(n_seq,nonzero_inds(0))=vect_age_out_par(nonzero_inds(0));
            vect_age_in_par_matr(n_seq,nonzero_inds(1))=vect_age_out_par(nonzero_inds(1));
          } else {
            // 1 level of infection in preceding age group
            int nonzero_ind=fcn_ind_seq(i_age-1,comp_list[i_comp],i_inf,n_age,comp_list,vec_inf_byage);
            vect_age_in_par_matr(n_seq,nonzero_ind)=vect_age_out_par(nonzero_ind);
          }
        } else {
          // 2 levels of inf in current age group (->also 2 in preceding)
          int nonzero_ind=fcn_ind_seq(i_age-1,comp_list[i_comp],i_inf,n_age,comp_list,vec_inf_byage);
          vect_age_in_par_matr(n_seq,nonzero_ind)=vect_age_out_par(nonzero_ind);
        }
      } // agegroups>1
    } // loop infect levels
  } // loop compartms
} // loop age groups

return vect_age_out_par;
}