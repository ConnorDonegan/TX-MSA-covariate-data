functions {
/**
 * Log probability density of the conditional autoregressive (CAR) model: WCAR specifications only
 *
 * @param y Process to model
 * @param mu Mean vector
 * @param tau Scale parameter
 * @param rho Spatial dependence parameter
 * @param A_w Sparse representation of the symmetric connectivity matrix, A
 * @param A_v Column indices for values in A_w
 * @param A_u Row starting indices for values in A_u
 * @param D_inv The row sums of A; i.e., the diagonal elements from the inverse of Delta, where M = Delta * tau^2 is a diagonal matrix containing the conditional variances.
 * @param log_det_D_inv Log determinant of Delta inverse.
 * @param lambda Eigenvalues of C (or of the symmetric, scaled matrix Delta^{-1/2}*C*Delta^{1/2}); for the WCAR specification, C is the row-standardized version of W.
 * @param n Length of y
 *
 * @return Log probability density of CAR prior up to additive constant
 */
real wcar_normal_lpdf(vector y, vector mu,
		      real tau, real rho,
		      vector A_w, int[] A_v, int[] A_u,
		      vector D_inv, real log_det_D_inv,
		      vector lambda,
		      int n) {
  vector[n] z = y - mu;
  real ztDz; // z transpose * D * z
  real ztAz; // z transpose * A * z
  vector[n] ldet_ImrhoC;
  ztDz = (z .* D_inv)' * z;
  ztAz = z' * csr_matrix_times_vector(n, n, A_w, A_v, A_u, z);
  for (i in 1:n) ldet_ImrhoC[i] = log1m(rho * lambda[i]);
  return 0.5 * (
		-n * log( 2 * pi() )
		-2 * n * log(tau)
		+ log_det_D_inv
		+ sum(ldet_ImrhoC)
		- (1 / tau^2) * (ztDz - rho * ztAz));
}
}

data {
  int N; // sites
  int TT; // time periods
  int nAx_w;
  int nC;
  vector[nAx_w] Ax_w; 
  int Ax_v[nAx_w];
  int Ax_u[N + 1];
  int Cidx[nC];
  vector[N] Delta_inv;
  real log_det_Delta_inv;
  vector[N] lambda;
  int y[TT, N]; // stacked observations over time
  vector[N] log_pop[TT];  
}

transformed data {
  vector[N] ones = rep_vector(1, N);
}
parameters {
  real alpha;
  vector[N] phi[TT];
  real<lower=0> tau[TT];
  real<lower=-1, upper=1> beta_ar[TT-1];
  real<lower=1/min(lambda), upper=1/max(lambda)> rho[TT];
}

transformed parameters {
} 

model {
  vector[TT] mu[N];
  vector[N] phi_mu[TT];
  phi_mu[1] = alpha * ones;
  for (tt in 1:TT) {
    if (tt > 1) {
      phi_mu[tt] = beta_ar[tt-1] * phi[tt-1];
      beta_ar[tt-1] ~ normal(.9, .5);
    }
    phi[tt] ~ wcar_normal(			 
			  phi_mu[tt], tau[tt], rho[tt],
			 Ax_w, Ax_v, Ax_u,
			 Delta_inv, log_det_Delta_inv,
			 lambda, N
						 );
    tau[tt] ~ std_normal();
    mu[tt] = log_pop[tt] + phi[tt];
    y[tt] ~ poisson_log(mu[tt]);
  }
  alpha ~ normal(-5, 5);
}

generated quantities {
  vector[N] rates[TT];
  for (tt in 1:TT) rates[tt] = exp(phi[tt]);
}





