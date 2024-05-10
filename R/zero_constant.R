
# 24h of 2021-12-04 is time t=0, row name of data2 indicates new isolated cases number in 0-24h of the day (e.g. row name 1 is t in period (1,2]).

data  <-  data2;
rownames(data)  =   0:46;

# new cases in each region
Temp    <-  apply(data, 2, cumsum);
t_zero  <-  apply(Temp, 2, function(x){y = rownames(data)[which.max(x)+1]; return(as.numeric(y))} );

# input zero constant alpha to return zeroing time in each region under control scheme Control and baseline parameters.
f_temp <- function(alpha){
  
  out_temp  <-  sol_ocp(Time = Time, pars = pars_base, control = Control, Index_SA = Index_SA, alpha = alpha, fun_zero = fun_zero);
  
  y  <-  c();
  
  for(i in district_all){
    temp = paste0(c('E.m','E.s','I.m','I.s'), i);
    temp = apply(out_temp[,temp],1,sum);
    time_temp = out_temp[which(temp == 0), 1];
    y[i] = min(time_temp[time_temp>1]);
  }
  
  z  =   (y-t_zero);
  
  return( z );
}

alpha_cand  <-  seq(0.01, 0.7, 0.01);
fit_val     <-  matrix(0, nrow = length(alpha_cand), ncol = n, dimnames = list(alpha_cand, 1:n));
for(i in 1:length(alpha_cand)){
  fit_val[i, ]  =   f_temp(alpha = alpha_cand[i]);
  print(i);
}

fit_val_s  <-  fit_val^2;

resid_s <- function(remove_col = NULL){
  if(is.null(remove_col)){
    fit_ch  <-  fit_val_s;
  }else{
    fit_ch  <-  fit_val_s[,-remove_col];
  }
  resid  <-  apply(fit_ch, 1, sum);
  y      <-  smooth.spline(x = alpha_cand, y = resid, df = length(resid)/10)$y;
  return(list(resid, y));
}

temp  =   c(6:13);
y     =   resid_s(temp);
plot(alpha_cand, y[[1]], ylim=c(50,150));
lines(alpha_cand, y[[2]]);
alpha_cand[which.min(y[[2]])];
