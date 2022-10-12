cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

# add by loop
sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

# sum vector
cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total; }')

# euclidean dist btwn scalar and vector
cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size(); NumericVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out
;}')


