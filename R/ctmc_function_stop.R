
# stop function 1: stop simulation when 1). number of infected people reaches zero; 2). infected people out of isolation area are more than a threshold.
f_stop1 <- function(out){ # first row of 'out' matrix can be either time or system state
  
  # remove the first row if it is time
  if(colnames(out)[1] == 'time'){
    out  =   out[,-1];
  }else{
    out  =   out;
  }
  
  n_col1  <-  which(substr(colnames(out),start=1,stop=1) %in% c('E', 'I'));
  n_col2  <-  which(substr(colnames(out),start=1,stop=1) %in% c('E', 'I', 'C', 'H'));
  
  # get the number of infected individuals from the system state
  cal_infec <- function(x){
    if(1){
      y = sum(x[n_col1]);
    }else{
      y = sum(x[n_col2]);
    } 
  }
  
  infection  <-  cal_infec(out[dim(out)[1],]);
  
  if({infection==0}|{infection>Inf}){ # threshold to stop the simulation
    y  =   1; # stop
  }else{
    y  =   0; # continue
  }
  
  return(y);
}


# stop function 2: stop simulation when all regions have been imported infections.
f_stop2 <- function(out){ # first row of 'out' matrix can be either time or system state
  
  # remove the first row if it is time
  if(colnames(out)[1] == 'time'){
    out  =   out[,-1];
  }else{
    out  =   out;
  }
  
  n_col1  <-  which(substr(colnames(out),start=1,stop=3) == 'E.m');
  n_col2  <-  which(substr(colnames(out),start=1,stop=3) == 'E.s');
  n_col3  <-  which(substr(colnames(out),start=1,stop=3) == 'I.m');
  n_col4  <-  which(substr(colnames(out),start=1,stop=3) == 'I.s');
  
  Temp  <-  out[,n_col1] + out[,n_col1] + out[,n_col1] + out[,n_col1];
  Temp  =   rbind(rep(0,dim(Temp)[2]), Temp);
  
  if(min(apply(Temp, 2, max) > 0) == 1){
    y  =   1; # stop
  }else{
    y  =   0; # continue
  }
  
  return(y);
}


# stop function 3: same as function 3, to set a different stop threshold
f_stop3 <- function(out){ # first row of 'out' matrix can be either time or system state
  
  # remove the first row if it is time
  if(colnames(out)[1] == 'time'){
    out  =   out[,-1];
  }else{
    out  =   out;
  }
  
  n_col1  <-  which(substr(colnames(out),start=1,stop=1) %in% c('E', 'I'));
  n_col2  <-  which(substr(colnames(out),start=1,stop=1) %in% c('E', 'I', 'C', 'H'));
  
  # get the number of infected individuals from the system state
  cal_infec <- function(x){
    if(1){
      y = sum(x[n_col1]);
    }else{
      y = sum(x[n_col2]);
    } 
  }
  
  infection  <-  cal_infec(out[dim(out)[1],]);
  
  if({infection==0}|{infection>1e4}){ # threshold to stop the simulation
    y  =   1; # stop
  }else{
    y  =   0; # continue
  }
  
  return(y);
}
