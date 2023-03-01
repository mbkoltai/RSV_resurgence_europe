## RSV transmission model with an adjustable delay distribution for immunity waning

The C++ code for the transmission model is in the file [diff_delay_eq.cpp](https://github.com/mbkoltai/RSV_resurgence_europe/blob/main/rcpp_files/diff_delay_eq.cpp).  
In contrast to ODE models, this is the numerical implementation of an integro-differential model, where immunity waning occurs after a given time, defined by a delay distribution, which I implemented as a discretised gamma-distribution.  
It was shown [in the literature](https://link.springer.com/article/10.1140/epjb/e2011-20054-9) that if the delay distribution is unimodal and has a sharply defined peak, this can lead to autonomous oscillations in SIRS models, even without any external forcing in the model.  
We included this mechanism in our RSV model, because we believe that it plays a role in the regular oscillations shown by RSV.

### Files and folders:

-  [**rcpp_files** folder](https://github.com/mbkoltai/RSV_resurgence_europe/blob/main/rcpp_files/):  
&nbsp;&nbsp;&nbsp;&nbsp;  - [diff_delay_eq.cpp](https://github.com/mbkoltai/RSV_resurgence_europe/blob/main/rcpp_files/diff_delay_eq.cpp): transmission model functions  
&nbsp;&nbsp;&nbsp;&nbsp;  - [fcns.cpp](https://github.com/mbkoltai/RSV_resurgence_europe/blob/main/rcpp_files/fcns.cpp): auxiliary functions to build the transmission model (generating indices for matrix/vector subsetting and multiplication)

- [ecdc_data.R](https://github.com/mbkoltai/RSV_resurgence_europe/blob/main/ecdc_data.R): R script to import and visualise RSV data (mainly from ECDC dashboard)
- [ode_dde_models.R](https://github.com/mbkoltai/RSV_resurgence_europe/blob/main/ode_dde_models.R): R script to run Rcpp transmission model and compare it to ODE models written in R


