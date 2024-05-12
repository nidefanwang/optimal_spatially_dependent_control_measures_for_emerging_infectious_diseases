
distr_post <- function(x){
  
  data  =   data2;
  
  # range of parameters
  con1  =   { min(x) >= 0 };
  con2  =   { x['p_fn']<1 } & { x['p_fn']>0 };
  con3  =   { max(par_est(x)$epsilon)<1 };
  con4  =   { x['lambda_E'] < x['lambda_I'] };
  con5  =   { min(par_est(x)$beta/gamma) > 1 };
  con6  =   { x['p_fn'] < 0.5 } & { x['D'] < 5 } & { min(par_est(x)$epsilon) > 0.5 };
  
  condition  =   con1 & con2 & con3 & con4 & con5 & con6;
  
  if( ! condition ) { return(log(0)) }
  
  # run epidemic model under the value of unknown parameters x (Time, control and some known parameters are taken in main.R)
  out  =   sol(Time = Time, pars = c(par_est(x), par_fix), control = Control);
  
  # new cases in regions
  new            =   matrix(0, length(Time)-1, n);
  colnames(new)  =   district_all;
  rownames(new)  =   Time[-1];
  temp_Q         =   get.K_i(K = 'Q', out = out);
  temp           =   temp_Q[,-1];
  time           =   temp_Q[, 1];
  
  t.temp  =   length(unique(time)) - 1;
  
  new[1,]  =   temp[2,] - temp[1,]; # corresponding to new isolated cases in 0~24h, 2021-12-05  
  for(t in 2:t.temp){ # This calculation requires the out matrix to have 1 row at time 0, and 2 rows at each integer time thereafter; no non-integer time; The ith fitting value is the value before impulse at time i minus the value before impulse at time i-1.
    temp.n   =   temp[0:2+(t-1)*2,];
    new[t,]  =   (temp.n[3,] - temp.n[2,]) + (temp.n[2,] - temp.n[1,]);
  }
  
  fit_model  <-  new;
  
  if(dim(fit_model)[1] < te){
    temp       =   matrix(0, te-dim(fit_model)[1], dim(fit_model)[2]);
    fit_model  =   rbind(temp, fit_model);
  }
  
  colnames(fit_model)  =   colnames(new);
  rownames(fit_model)  =   1:te;
  
  # likelihood
  LL   <-  matrix(0, dim(data)[1], dim(data)[2]);
  phi  =   x['phi'];
  LL   =   dnbinom(data, mu = fit_model + 0.01, size = phi, log = TRUE);
  
  # prior distribution
  # R0
  R0      <-  (par_est(x)$beta/gamma);
  R_temp  <-  c(rep(6.5,9),rep(2,4),6.5);
  SD      <-  c(rep(1/3,5), rep(2/3,8), 1/3);
  prior1  <-  sum( dnorm(R0[c(1:8,14)], mean = R_temp[c(1:8,14)], sd = SD[c(1:8,14)], log = TRUE) );
  
  # false negative rate
  prior2  <-  dnorm(x['p_fn'], mean = 0.35, sd = 0.35/3*0.3, log = TRUE);
  
  # reduction in contact rate after lockdown
  prior3  <-  sum( dnorm(par_est(x)$epsilon, mean = 0.75,sd = 0.15/3, log = TRUE) );
  
  # contact tracing
  prior4.1  <-  dnorm(x['lambda_E'], mean = 0.19,  sd = 0.019,  log = TRUE);
  prior4.2  <-  dnorm(x['lambda_I'], mean = 0.34,  sd = 0.034,  log = TRUE);
  prior4.3  <-  dnorm(x['Lo'],       mean = 440,   sd = 5,      log = TRUE);
  prior4.4  <-  dnorm(x['r'],        mean = 0.002,  sd = 0.001/3, log = TRUE);  
  prior4    <-  prior4.1 + prior4.2 + prior4.3;
  
  # parameter C in matrix tau
  prior5  <-  dnorm(x['D'], mean = 0.8, sd = 0.2, log = TRUE);
  
  # other
  con6.1  <-  {sum(get.K_i('Cum_infec',out=out)[dim(out)[1],-1]) < 5*sum(data)};
  con6.2  <-  {sum(fit_model[,1]) < 1550};
  prior6  <-  ifelse(con6.1 & con6.2, 0, -Inf);
  
  # delta
  mean_delta  =   c(0.20,0.15,0.15,0.20,0.15, 0.15,0.10,0.14,0.15,0.15, 0.10,0.10,0.10,0.15)/5;
  sd_delta    =   c(0.05,0.05,0.05,0.05,0.05, 0.05,0.05,0.05,0.05,0.05, 0.05,0.05,0.05,0.05)/5;
  prior7      <-  sum( dnorm(par_est(x)$delta, mean = mean_delta, sd = sd_delta, log = TRUE) ); 
  
  # phi
  prior8  <-  dnorm(x['phi'], mean = 1, sd = 0.1266, log = TRUE);
  
  # total
  prior  =   prior1 + prior2 + prior3 + prior4 + prior5 + prior6 + prior7 + prior8;
  
  return(sum(LL) + prior);
}
