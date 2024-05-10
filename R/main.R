
############################ Input data and functions ############################

rm(list=ls()); memory.limit(30e4);

setwd('R');

library('deSolve');
library('adaptMCMC');
library('ggplot2');
library('patchwork');
library('pheatmap'); 
library('grid');
library('RColorBrewer'); 
library("geosphere");

# model for parameter estimation and optimal control problem
source('model.R'); 

# solver for epidemic model, on time interval [Time[t],Time[t+1]), lockdown and border closure take effect on the whole interval and screening takes effect at time Time[t+1]
source('sol.R');
source('sol_ocp.R');

# load data
M_comp = read.csv('../data/data.csv');

# districts and counties in Xi'an City, Shaanxi Province, China
district_all <- c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanlaing', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian');

# distance between regions (dist), population (Nr), GDP, functions to calculate mobility rate matrix 
source('cal_popflow.R');

# some functions used
source('somefunctions.R');

# posterior of unknown parameters
source('distr_post.R');

# stochastic model and solving function
source('Events.R');
source('ctmc_function_stop.R');
source('sol_sto_interval.R');
source('sol_sto.R');

# simulated annealing algorithm
source('SA_det.R');
############################ Input data and functions ############################



########################## data and preparation for the following program ##########################

# latent period and infectious period
sigma_base  =   1/(4.4-2);
gamma_base  =   0.4318215;
sigma       =   sigma_base;
gamma       =   gamma_base;

# some model parameters
n        =   14;
Nr       =   Nr;
p_s      =   1.5/100;
gamma_s  =   1/2;
gamma_H  =   1/7;
H_max    =   round(10020.39*8.2/100)*50/100;

# system variables
sys_names  <-  c('S','E.m','E.s','I.m','I.s','CE.m','CE.s', 'CI.m','CI.s', 'H','R', 'Q', 'Cum_infec', 'Cum_confi');

# initial value of epidemic model
S0     =   Nr;
E.m0   =   rep(0,n);
E.s0   =   rep(0,n);
I.m0   =   rep(0,n);
I.s0   =   rep(0,n);
CE.m0  =   rep(0,n);
CE.s0  =   rep(0,n);
CI.m0  =   rep(0,n);
CI.s0  =   rep(0,n);
H0     =   rep(0,n);
R0     =   rep(0,n);

# other output
Q0           =   rep(0,n);
Cum_infec_0  =   rep(0,n);
Cum_confi_0  =   rep(0,n)

# initial value of epidemic model
S0[1]    =   S0[1] - 1;
E.m0[1]  =   E.m0[1] + 1;
y_ini    <-  c(S0, E.m0, E.s0, I.m0,I.s0, CE.m0,CE.s0, CI.m0,CI.s0, H0, R0,  Q0, Cum_infec_0, Cum_confi_0);
temp1    =   sys_names;
temp2    =   district_all;
temp3    =   matrix(0,length(temp1),length(temp2));
for(i in 1:dim(temp3)[1]){
  for(j in 1:dim(temp3)[2]){
    temp3[i,j]  =   paste0(temp1[i],temp2[j]);
  }
}
names(y_ini)  =   c(t(temp3));

# starting time ts of outbreak (time 0 is 24h of 2021.12.4, time te is 24h of 2022.1.20)
t_start  =   as.Date('2021-12-04');
t_end    =   as.Date('2022-01-20');
ts       =   0;
te       =   as.numeric(t_end-t_start);
period   <-  as.Date(ts:te,origin=t_start); # study period

# Time
if(ceiling(ts)==ts){
  Time  <-  ts:te;
}else{
  Time  <-  c(ts, ceiling(ts):te);
}
names(Time)  =   period[(1+floor(ts)):length(period)];

# 24h of 2021.12.09, discovered the infection and start to control
Ud            =   matrix(0,length(period)-1,n);
rownames(Ud)  =   1:te - 1; 
colnames(Ud)  =   district_all;
temp          =   which(period=='2021-12-09'): dim(Ud)[1];
Ud[temp,]     =   1;

# known parameters
par_fix  <-  list(n = n, Nr = Nr, p_s = p_s, gamma_s = gamma_s, gamma_H = gamma_H, H_max = H_max, y_ini = y_ini);

# for control except Un, row name t of control matrix indicates control on (t,t+1]; for Un, t indicates control at time t
# t = 47 is 24h of 2022.1.20
Up             =   matrix(0,length(period)-1,n);
rownames(Up)   =   1:te - 1; 
colnames(Up)   =   district_all;
temp           =   which(period=='2021-12-09'): dim(Up)[1];
Up[temp,]      =   1;

Un             =   matrix(0,length(period)-1,n);
rownames(Un)   =   1:te - 1; 
colnames(Un)   =   district_all;
temp           =   seq(which(period=='2021-12-19'), length(period)-1, by = 2);
Un[temp,]      =   1;
temp2          =   seq(which(period=='2021-12-27'), length(period)-1, by = 1);
Un[temp2,1:3]  =   1;
temp3          =   which(period=='2021-12-17');
Un[temp3,1]    =   1;

Ub             =   matrix(0,length(period)-1,n); 
rownames(Ub)   =   1:te - 1; 
colnames(Ub)   =   district_all;
temp           =   which(period=='2021-12-22'): dim(Ub)[1];
Ub[temp,]      =   1;
temp1          =   which(period=='2021-12-22'): dim(Ub)[1];
Ub[temp1,1]    =   1;

Ul             =   matrix(0,length(period)-1,n);
rownames(Ul)   =   1:te - 1; 
colnames(Ul)   =   district_all;
temp           =   which(period=='2021-12-22'): dim(Ul)[1];
Ul[temp,]      =   1;

# controls
Control        =   list(Up = Up, Ud = Ud, Un = Un, Ub = Ub, Ul = Ul);

district_plot  <-  c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanliang', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian');
########################## data and preparation for the following program ##########################



########################## conduct parameter estimation and save the result ##########################
# initial value of parameters
beta0     =   c(2.6038, 3.0073, 3.0121, 2.8992, 2.7216, 2.6615, 2.6596, 2.8615, 0.8931, 0.8301, 0.8479, 0.8635, 0.8052, 3.0160);
epsilon0  =   c(6.9157, 6.8449, 7.4598, 7.6800, 7.8900, 8.1428, 7.6103, 7.3455, 7.8935, 7.5026, 7.5139, 7.5027, 7.4889, 6.9002)/10;
delta0    =   c(4.0524, 2.8489, 2.8773, 4.0522, 3.1405, 4.1709, 2.1315, 2.8106, 2.0668, 3.0606, 2.0704, 1.9240, 2.0142, 2.8295)/100;
x0        =   c( p_fn =  0.3889, 
                 lambda_E =  0.1968,   lambda_I =  0.3538,     r    =  2.0925e-3,  Lo    =  440.6,
                 D        =  0.8345,   epsilon  =  epsilon0,   beta =  beta0,      delta = delta0, 
                 phi      =  1.00);

# new isolated cases (data2) and new confirmed cases (data1)
data2  =   get.data(x2 = period, type=2)[-1,];
data1  =   get.data(x2 = period, type=1)[-1,];

# run MCMC and save the result
set.seed(1);
# samp  <-  MCMC(distr_post, n = 20e4, init = x0, scale = x0/20, adapt = TRUE, acc.rate = 0.234, list = TRUE);
# save(samp, file = "samp.RData");

# load posterior sample
load('../output/samp.RData');
samp.post  <-  samp$samples[seq(10e4+1,20e4,by=100),];

# posterior median
x_med  <-  apply(samp.post, 2, median); 

# model parameters
pars_base  =   c(par_est(x_med), par_fix);

# solution under actual control
out_actual =   sol(Time = Time, pars = pars_base, control = Control);

# over dispersion parameter
phi  =   x_med['phi'];
########################## conduct parameter estimation and save the result ##########################



########################## preparation for OCP, OCP is conducted in parallel_SA.R ##########################

# three matrices used in ocp

# time period
T_ocp           <-  50;

temp            =   matrix(0, length(0:T_ocp), n);
rownames(temp)  =   0:T_ocp; 
colnames(temp)  =   district_all;

# 1. zero matrix
U_SA            <-  list(Up = temp, Ud = temp, Un = temp, Ub = temp, Ul = temp);

# 2. index matrix
Index_matrix <- function(Up_start, Ud_start, Un_start, Ub_start, Ul_start){
  
  temp            =   matrix(1, length(0:T_ocp), n);
  rownames(temp)  =   0:T_ocp; 
  colnames(temp)  =   district_all;
  
  names_List  <-  c('Up', 'Ud', 'Un', 'Ub', 'Ul');
  
  List  <-  list();
  for(i in 1:length(names_List)){
    List[[i]] = temp;
  }
  names(List)  =  names_List;
  
  A  <-  rbind(Up_start, Ud_start, Un_start, Ub_start, Ul_start);
  rownames(A)  =   names_List;
  
  for(i in 1:n){
    for(j in 1:length(List)){
      temp  =   which( rownames(List[[j]]) == A[names(List)[j],i] );
      List[[j]][1:(temp-1),i]  =   0;
    }
  }
  
  return(List);
}

# control starting time baseline
temp_t0  <-  which(period=='2021-12-04');
temp_t1  <-  which(period=='2021-12-09');
temp_t2  <-  which(period=='2021-12-17');
temp_t3  <-  which(period=='2021-12-19');
temp_t4  <-  which(period=='2021-12-22');

Up_start  <-  rep(temp_t1 - temp_t0 + 0, 14);
Ud_start  <-  rep(temp_t1 - temp_t0 + 0, 14);
Un_start  <-  c(  temp_t2 - temp_t0 + 0, rep(temp_t3 - temp_t0 + 0, 13) );
Ub_start  <-  rep(temp_t4 - temp_t0 + 0, 14);
Ul_start  <-  rep(temp_t4 - temp_t0 + 0, 14);

Index_SA  <-  Index_matrix(Up_start = Up_start, Ud_start = Ud_start, Un_start = Un_start, Ub_start = Ub_start, Ul_start = Ul_start);

# 3. Control_base
max_time  =   max(as.numeric(rownames(Control[[1]])));
sup_dim1  =   300 - max_time;
sup_dim2  =   dim(Control[[1]])[2];
Temp1     =   matrix(1, sup_dim1, sup_dim2);

Control_base  =   Control;

for(i in c('Up', 'Ud', 'Ub', 'Ul')){
  Control_base[[i]]  =   rbind(Control[[i]], Temp1);
  rownames(Control_base[[i]])  =   0:300;
}

Temp2  =   Temp1;
temp1  =   Control$Un[as.character(max_time-1),];
temp2  =   Control$Un[as.character(max_time),];
for(i in 1:dim(Temp2)[1]){
  if((i %% 2) == 1){
    Temp2[i,]  =   temp1;
  }else{
    Temp2[i,]  =   temp2;
  }
}

Control_base$Un  =   rbind(Control_base$Un, Temp2);
rownames(Control_base$Un)  =   0:300;


# baseline parameters and cost calculating function 

Control_base_ocp  <-  U_SA;

for(i in 1:length(Control_base)){
  Control_base_ocp[[i]]  =   Control_base[[i]][1:which(rownames(Control_base[[i]])==T_ocp),];
}

cost_list_base   <-  list(Ul = GDP_all/365, Ub = GDP_all/365*0.05, Un = Nr*10);
alpha_base       <-  0.33; 
config_det_base  <-  list( U_SA = U_SA, Index_SA = Index_SA, cost_list = cost_list_base, alpha = alpha_base, control0 = Control_base_ocp, SA_number = 'base');

f_cost <- function(U, cost_list = cost_list_baseline){ 
  
  tol  =   t(U$Ul)*cost_list$Ul + t(U$Ub)*cost_list$Ub + t(U$Un)*cost_list$Un;
  
  return( sum(tol) );
}


# discuss choices of border closure cost
cost_list1  <-  list(Ul = GDP_all/365, Ub = GDP_all/365*0.20, Un = Nr*10);
cost_list2  <-  list(Ul = GDP_all/365, Ub = GDP_all/365*0.10, Un = Nr*10);
cost_list3  <-  list(Ul = GDP_all/365, Ub = GDP_all/365*0.02, Un = Nr*10);
config1.1  <-  list( U_SA = U_SA, Index_SA = Index_SA, cost_list = cost_list1, alpha = alpha_base, SA_number = '1.1');
config1.2  <-  list( U_SA = U_SA, Index_SA = Index_SA, cost_list = cost_list2, alpha = alpha_base, SA_number = '1.2');
config1.3  <-  list( U_SA = U_SA, Index_SA = Index_SA, cost_list = cost_list3, alpha = alpha_base, SA_number = '1.3');

config_parallel1  <-  list(config_det_base, config1.1, config1.2, config1.3);


# functions used in ocp

# complete two matrices according to the final time
compl <- function(T_ocp, U_SA, Index_SA){
  temp    =   max(as.numeric(rownames(Index_SA[[1]])));
  if(temp == T_ocp){
    for(i in 1:length(Index_SA)){
      Index_SA_temp  =   Index_SA;
      U_SA_temp  =   U_SA;
    }
  }else{
    temp_n  =   (temp+1):T_ocp;
    temp_M  =   matrix(1, length(temp_n), n, dimnames = list(temp_n, c()));
    
    Index_SA_temp  =   Index_SA;
    U_SA_temp      =   U_SA;
    for(i in 1:length(Index_SA)){
      Index_SA_temp[[i]]  =   rbind(Index_SA[[i]], temp_M);
      U_SA_temp[[i]]      =   rbind(U_SA[[i]], 0*temp_M);
    }
  }
  
  return(list(U_SA = U_SA_temp, Index_SA = Index_SA_temp));
}

# zeroing mechanism
fun_zero <- function(Index_SA, out, y_t, alpha, t_now, control){
  
  U_zero    <-  Index_SA[[1]] + Index_SA[[2]] + Index_SA[[3]] + Index_SA[[4]] + Index_SA[[5]];
  temp      =   sapply(colnames(out), function(x){substr(x,start=1,stop=3)});
  temp1     =   which(temp == 'E.m');
  temp2     =   which(temp == 'E.s'); 
  temp3     =   which(temp == 'I.m');
  temp4     =   which(temp == 'I.s'); 
  out_E     <-  out[,temp1] + out[,temp2];
  out_I     <-  out[,temp3] + out[,temp4];
  infec     <-  out_E + out_I;
  infec_now <-  infec[dim(infec)[1], ];
  
  # zeroing condition 1
  condition1  <-  {dpois(0, lambda = infec_now) > alpha};
  
  # zeroing condition 2
  condition2  <-  {U_zero[as.character(t_now), ]};
  
  # zeroing condition 3
  condition3  <-  rep({max(infec[,1]) > (infec)[1,1]} & {dpois(0, lambda = max(infec[,1])) < alpha}, n);
  
  # zeroing condition 4
  temp1  <-  rep(0, n);
  Ub     <-  as.numeric(control$Ub[as.character(t_now),] == 0); 
  temp2  =   Ub * as.numeric({dpois(0, lambda = infec_now) < alpha}); 
  for(i in 1:length(temp1)){
    temp1[i]  =   Ub[i] * sum(temp2[-i]); 
  }
  condition4  <-  {temp1 == 0};
  
  # row and columns to zero
  rn        =   which(substr(rownames(y_t), 1,1) %in% c('E','I')); 
  cn        =   which(condition1 & condition2 & condition3 & condition4); 
  
  if(length(cn) == 1){
    val_zero = sum(y_t[rn, cn]);
  }else if(length(cn) >= 2){
    val_zero = apply(y_t[rn, cn], 2, sum);
  }else{
    val_zero = 0;
  }
  y_t['R', cn]  =   y_t['R', cn] + val_zero;
  y_t[rn, cn]   =   0;
  return(y_t);
}
############################ preparation for OCP, OCP is conducted in parallel_SA.R ############################

# 
setwd('../');
