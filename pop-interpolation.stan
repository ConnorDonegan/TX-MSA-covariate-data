data {
  vector[3] x;
  vector[3] s;
  int idx[3, 5];
  real d;
  int n;
}

parameters {
  vector<lower=0>[n-1] pop_params;
  real<lower=0> tau;
}

transformed parameters {
  vector[n] pop;
  pop[1] = pop_params[1];
  pop[2] = d;
  pop[3:n] = pop_params[2:(n-1)];
}

model {
  real mu;
  for (i in 1:3) {
  mu = mean(pop[idx[i]]);
  x[i] ~ normal(mu, s[i]);
}
 for (i in 2:n) pop[i] ~ normal(pop[i-1], tau);
 // pop[1] ~ student_t(5, 15000, 10000);
 tau ~ student_t(10, 5000, 10000);
}

