#ifndef f_mean_H
#define f_mean_H

double f_mean(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

#endif
