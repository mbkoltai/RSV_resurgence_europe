#ifndef f_l2_norm_H
#define f_l2_norm_H

double f_l2_norm(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += pow(x[i],2);
  }
  return sqrt(total);
}

#endif