
############################ distance between regions ############################

# number of patches
n  =   length(district_all);

# longitude and latitude of local governments
axs            <-  matrix(0,n,2);
rownames(axs)  =   district_all;
axs[1,]        =   c(108.954816,34.228829);
axs[2,]        =   c(108.913221,34.164631);
axs[3,]        =   c(108.95039,34.271537);
axs[4,]        =   c(108.946972,34.263032);
axs[5,]        =   c(108.953122,34.299165);
axs[6,]        =   c(109.070984,34.279599);
axs[7,]        =   c(108.967219,34.272934);
axs[8,]        =   c(109.232576,34.668381);
axs[9,]        =   c(108.611425,34.115018);
axs[10,]       =   c(109.220701,34.372988);
axs[11,]       =   c(109.094683,34.541315);
axs[12,]       =   c(108.228573,34.169839);
axs[13,]       =   c(109.330006,34.157866);
axs[14,]       =   c(108.787556,34.339317);

# distance matrix (km)
dist            <-  matrix(0,n,n);
colnames(dist)  =   district_all;
rownames(dist)  =   district_all;
for(i in 1:n){
  for(j in 1:n){
    dist[i,j] = geosphere::distm(axs[i,],axs[j,]);
  }
}
dist  =   dist/1000;
############################ distance between regions ############################


############################ mobility rate tau_ik ############################

# GDP data
GDP_all         <-  c(2510.72, 1136.25, 814.95, 1060.47, 1319.43, 542.06, 579.05, 249.10, 233.61, 252.53, 374.49, 139.44, 142.96, 612.50)*100*100e4;
GDP_per         <-  c(126772, 73830, 82194, 142632, 87933, 55200, 93925, 82757, 40770, 37440, 86388, 25034, 28969, 48236);
names(GDP_all)  =   district_all;
names(GDP_per)  =   district_all;

# population data
Nr  =   GDP_all/GDP_per;

# function to get w_ij
gm <- function(Ni,Nj,dij,C){
  alpha  =   0.46;
  gamma  =   0.64; 
  r      =   82;
  y      =   C*Ni^alpha*Nj^gamma/exp(dij/r)
  return(y);
}

# function to get tau
cal_tau <- function(x){
  
  W            <-  matrix(0, length(district_all), length(district_all));
  colnames(W)  =   district_all; 
  rownames(W)  =   district_all;
  
  importance  =   GDP_all/100/100e4; 
  for(i in 1:dim(W)[1]){
    for(j in 1:dim(W)[2]){
      W[i,j] = gm(importance[i], importance[j], dist[i,j], C=x);
    }
  }
  
  sigma_ij        <-  W/Nr; 
  diag(sigma_ij)  =   0;
  r_ij            <-  3;
  
  mu_ij        <-  sigma_ij/r_ij;
  diag(mu_ij)  =   1;
  mu_i         <-  apply(mu_ij,1,sum);
  
  N_ii_bar        <-    Nr/mu_i;
  N_ij_bar        <-    mu_ij*N_ii_bar;
  diag(N_ij_bar)  =     N_ii_bar; 
  
  N_i_bar           <-  apply(N_ij_bar,2,sum);
  tau_ij            <-  matrix(0,length(Nr),length(Nr));
  colnames(tau_ij)  =   district_all;
  rownames(tau_ij)  =   district_all;
  
  for(i in 1:length(Nr)){
    for(j in 1:length(Nr)){
      tau_ij[i,j] = (r_ij*N_ij_bar[j,i] + sigma_ij[i,j]*N_ii_bar[i]) / N_i_bar[i]; 
    }
  }
  diag(tau_ij)  =   0;
  
  return(tau_ij);
}
############################ mobility rate tau_ik ############################
