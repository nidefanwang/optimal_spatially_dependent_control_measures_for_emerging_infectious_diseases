
########################## stochastic model: parallel simulation 1 ##########################

# obtain the time of infection importation in each region before ub is implemented 

# parallel computing package
library(parallel); 

# number of CPU cores
detectCores(); 

# set the number of cores being used 
cl  <-  makeCluster(getOption("cl.cores", 14)); 

# solving function for ctmc model
ctmc <- function(i){
  
  source('R/main.R'); 
  
  # this function is copied from 'Z Fig 3.1.R'
  move <- function(Control, d){
    
    time_Control  <-  as.numeric( rownames(Control[[1]]) ); # rownames of matrix Control[[i]]
    
    for(i in names(d)){
      if(d[i] == 0){
        Control[[i]]  =   Control[[i]];
      }else if(d[i] < 0){
        Temp  =   Control[[i]][1:(-d[i]),];
        if(sum(Temp) > 0){print('too large d'); break;} 
        Control[[i]]  =   Control[[i]][-(1:(-d[i])),];
        Control[[i]]  =   rbind(Control[[i]], Temp);
        rownames(Control[[i]])  =   time_Control;
      }else{
        D  =   dim(Control[[i]])[1];
        Temp  =   matrix(0, d[i], dim(Control[[i]])[2]);
        Control[[i]]  =   Control[[i]][-((D-d[i]+1):D),];
        Control[[i]]  =   rbind(Temp, Control[[i]]);
        rownames(Control[[i]])  =   time_Control;
      }
    }
    
    return(Control);  
  }
  
  # control_temp  <-  U_SA; control_temp$Up  =   Index_SA$Up; control_temp$Ud  =   Index_SA$Ud; # ctmc_0
  # control_temp  <-  Control_base; # ctmc_1
  control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -3, Ul = 0)); # ctmc_n3
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -4, Ul = 0)); # ctmc_n4
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -5, Ul = 0)); # ctmc_n5
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -6, Ul = 0)); # ctmc_n6
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -7, Ul = 0)); # ctmc_n7
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -8, Ul = 0)); # ctmc_n8
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -9, Ul = 0)); # ctmc_n9
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -10, Ul = 0)); # ctmc_n10
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -11, Ul = 0)); # ctmc_n11
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -12, Ul = 0)); # ctmc_n12 
  
  te  =   60;
  te  =   as.numeric( rownames(control_temp$Ub)[min( which(apply(control_temp$Ub, 1, prod) > 0) )] ); # stop the simulation when ub has been implemented in all regions
  
  out  =   sol_sto(k = i, Time = 0:te, pars = pars_base, control = control_temp, f_stop1, f_stop2);
  return(list(out = out, control = control_temp));
}

# results  <-  clusterApply(cl, x = 1:500, fun = ctmc );
stopCluster(cl);
# save(results, file = "output/ctmc_n3.RData");

########################## stochastic model: parallel simulation 1 ##########################



########################## stochastic model: parallel simulation 2 ##########################

# count the number of imported cases in each region before ub is implemented 

# parallel computing package
library(parallel); 

# number of CPU cores
detectCores(); 

# set the number of cores being used 
cl  <-  makeCluster(getOption("cl.cores", 14)); 

# solving function for ctmc model
ctmc <- function(i){
  
  source('R/main.R'); 
  
  # this function is copied from 'Z Fig 3.1.R'
  move <- function(Control, d){
    
    time_Control  <-  as.numeric( rownames(Control[[1]]) ); # rownames of matrix Control[[i]]
    
    for(i in names(d)){
      if(d[i] == 0){
        Control[[i]]  =   Control[[i]];
      }else if(d[i] < 0){
        Temp  =   Control[[i]][1:(-d[i]),];
        if(sum(Temp) > 0){print('too large d'); break;} 
        Control[[i]]  =   Control[[i]][-(1:(-d[i])),];
        Control[[i]]  =   rbind(Control[[i]], Temp);
        rownames(Control[[i]])  =   time_Control;
      }else{
        D  =   dim(Control[[i]])[1];
        Temp  =   matrix(0, d[i], dim(Control[[i]])[2]);
        Control[[i]]  =   Control[[i]][-((D-d[i]+1):D),];
        Control[[i]]  =   rbind(Temp, Control[[i]]);
        rownames(Control[[i]])  =   time_Control;
      }
    }
    
    return(Control);  
  }
  
  # control_temp  <-  Control_base; # ctmc_num_1
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -4, Ul = 0)); # ctmc_num_n4
  # control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -8, Ul = 0)); # ctmc_num_n8
  control_temp  <-  move(Control_base, d = c(Un = 0, Ub = -12, Ul = 0)); # ctmc_num_n12 
  
  te  =   60;
  te  =   as.numeric( rownames(control_temp$Ub)[min( which(apply(control_temp$Ub, 1, prod) > 0) )] ); # stop the simulation when ub has been implemented in all regions
  
  out  =   sol_sto(k = i, Time = 0:te, pars = pars_base, control = control_temp, f_stop3, f_stop3);
  return(list(out = out, control = control_temp));
}

# results  <-  clusterApply(cl, x = 1:500, fun = ctmc );
stopCluster(cl);
# save(results, file = "output/ctmc_num_n12.RData");
########################## stochastic model: parallel simulation 2 ##########################



########################## function to manage results of simulation 1 ##########################

dec_events <- function(out){
  
  # y0 the place where the first imported case comes from, y1 the importation time of the first imported case, y2 the infection time of the first local case
  y0  <-  c();
  y1  <-  c();
  y2  <-  c();
  
  # difference between row i and row i-1 of out matrix 
  Dif_out  <-  out[-1,-1] - out[-dim(out)[1],-1];
  Time     <-  out[-1,1];
  
  # we get the first importation time and first local transmission base on the fact that case importation or local transmission can make one more local infection
  f_temp <- function(i){ # region i
    
    # importation time of the first imported case 
    compart  <-  c('E.m', 'E.s', 'I.m', 'I.s');
    temp     =   rep(Inf, length(compart));
    for(j in 1:length(compart)){
      r_num  <-  which( Dif_out[, paste0(compart[j], district_all[i])] > 0 );
      if(length(r_num)==0){next}
      Temp   =   Dif_out[r_num, which(substr(colnames(Dif_out), 1, 3) == compart[j])];
      Temp[Temp==1]  =   0;
      Sum  <-  ifelse(!is.null(dim(Temp)), apply(Temp, 1, sum), sum(Temp));
      temp[j]  =   Time[r_num][which(Sum == -1)][1];
    }
    temp[is.na(temp)]  =   Inf;
    temp1  =   min(temp);
    temp0  =   which( Dif_out[which(Time == temp1), ] == -1 ) %% n;
    if(length(temp0)==0){temp0 = Inf}
    
    # infection time of the first local transmission 
    r_num  <-  which( apply(Dif_out[, paste0(c('E.m', 'E.s'), district_all[i])], 1, sum) > 0 );
    temp2  =   Time[r_num][which(Dif_out[r_num, paste0('S', district_all[i])] == -1)][1];
    temp2[is.na(temp2)]  =   Inf;
    
    return(c(temp0, temp1, temp2));
  }
  
  for(i in 1:n){
    temp   =   f_temp(i);
    y0[i]  =   temp[1]; 
    y1[i]  =   temp[2]; 
    y2[i]  =   temp[3]; 
  }
  
  return(list(source = y0, t_import = y1, t_trans = y2));
}


# filenames corresponding to different Ub control schemes
names_ctmc     <-  c();
names_ctmc[1]  =   'ctmc_0';
names_ctmc[2]  =   'ctmc_1';
for(i in 3:12){ names_ctmc[i]  =   paste0('ctmc_n',i) }

# probability of no infection importation under different Ub control schemes
Y  <-  list();
for(i in 1:length(names_ctmc)){
  
  load(paste0('output/', names_ctmc[i], '.RData'));
  Y[[i]]  <-  matrix(0, length(results), n);
  
  for(j in 1:length(results)){
    Y[[i]][j,]  =   dec_events(results[[j]][[1]][[1]])[[2]];
    print(j);
  }
}

names(Y)  =   names_ctmc;

# save(Y, file = "output/ctmc_import_time.RData");
########################## function to manage results of simulation 1 ##########################



########################## function to manage results of simulation 2 ##########################

dec_events <- function(out){
  
  # difference between row i and row i-1 of out matrix 
  Dif_out  <-  out[-1,-1] - out[-dim(out)[1],-1];
  Time     <-  out[-1,1];
  
  # count imported cases in some region i
  f_temp <- function(i){ 
    
    compart  <-  c('E.m', 'E.s', 'I.m', 'I.s');
    
    c1  =   which( colnames(Dif_out) %in% paste0(compart, district_all[i]) );
    c2  =   which( substr(colnames(Dif_out),1,3) %in% compart );
    c2  =   c2[!(c2 %in% c1)];
    
    condition1  <-  {apply(Dif_out[, c1], 1, sum) == 1};
    condition2  <-  {apply(Dif_out[, c2], 1, sum) == -1}
    
    r_num  <-  which( condition1 & condition2 );
    
    return( length(r_num) );
  }
  
  y  <-  rep(0,n);
  
  for(i in 1:n){
    y[i]  =   f_temp(i);
  }
  
  return(y);
}


# filenames corresponding to different Ub control schemes
names_ctmc     <-  c();
names_ctmc[1]  =   'ctmc_num_1';
for(i in 2:4){ names_ctmc[i]  =   paste0('ctmc_num_n',i*4-4) }

# number of imported cases under different Ub control schemes
Y  <-  list();
for(i in 1:length(names_ctmc)){
  
  load(paste0(getwd(), '/Rdata/', names_ctmc[i], '.RData'));
  Y[[i]]  <-  matrix(0, length(results), n);
  
  for(j in 1:length(results)){
    Y[[i]][j,]  =   dec_events(results[[j]][[1]][[1]]);
    print(j);
  }
}

names(Y)  =   names_ctmc;

# save(Y, file = "output/ctmc_import_number.RData");
########################## function to manage results of simulation 2 ##########################
