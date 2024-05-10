
########################## preparation ##########################
# parallel computing package
library(parallel); 

# number of CPU cores
detectCores(); 

source('R/main.R'); 
########################## preparation ##########################


########################## conduct SA to solve OCP ##########################

config_parallel  <-  config_parallel1;
cl  <-  makeCluster(getOption("cl.cores", length(config_parallel))); 

SA_parallel <- function(i){
  
  source('R/main.R'); 
  config_parallel  <-  config_parallel1; # this line should be the same to the first line in this section 
  
  SA_det(config = config_parallel[[i]]);
}

clusterApply(cl, x = 1:length(config_parallel), fun = SA_parallel);
stopCluster(cl);

########################## conduct SA to solve OCP ##########################
