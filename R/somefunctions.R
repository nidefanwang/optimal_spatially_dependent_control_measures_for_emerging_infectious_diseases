
# Convert unknown parameters x to a list par_est (name of par_est should correspond to the order of elements in x; tau in par_est is D in x)
par_est <- function(x){
  
  par_est         =     list();
  names_temp      =     c('p_fn', 'lambda_E', 'lambda_I', 'r', 'Lo', 'tau', 'epsilon', 'beta', 'delta');
  number_temp     =     c(1,      1,           1,         1,   1,    1,     n,         n,      n);
  x_unname        =     unname(x);
  
  for(k in 1:length(names_temp)){
    if(k==1){
      temp        =     1; 
      par_est[[k]]=     x_unname[temp];
    }else if(k==which(names_temp=='tau')){
      temp        =     1:number_temp[k] + sum(number_temp[1:(k-1)]);
      par_est[[k]]=     cal_tau(x_unname[temp]);
    }else{
      temp        =     1:number_temp[k] + sum(number_temp[1:(k-1)]);
      par_est[[k]]=     x_unname[temp];
    }
  }
  
  names(par_est)  =     names_temp;
  
  return(par_est);
}


# imput compartment K and region i to get model output from out
get.K_i <- function(K, i = district_all, out, temp_K = sys_names){
  
  temp_i  =   match(i, district_all);
  temp1   =   out[,1];
  temp2   =   out[,-1];
  temp3   =   temp2[,1:n + n*(which(temp_K==K)-1)];
  temp4   =   temp3[,temp_i];
  
  return(cbind(temp1,temp4));
}


# input region x1 and time x2 to get data on new isolated cases number, return all all regions is x1 is NA, type indicate isolated cases (2) or confirmed cases (1)
get.data <- function(x1 = district_all, x2, type){
  
  data      =   matrix(0,length(x2),length(x1)); 
  colnames(data) = x1;
  rownames(data) = as.character(x2);
  
  for(i in 1:length(x1)){
    
    temp_n  =   which(M_comp[,'region']==x1[i]);
    temp    =   M_comp[temp_n, ];
    
    for(j in 1:length(x2)){
      data[j,i] = length(which(temp[,type]==x2[j]));
    }
  }
  
  return(data);  
}


# function used in plot, input data vactor (x, y) and ylim
f_ggplot_comp <- function(x, y, ylim){
  
  Z   <-  cbind(x = x, y = y);
  x1  <-  x*Inf;
  y1  <-  y*Inf;
  
  temp      <-  min(which({y >= ylim[1]} & {y <= ylim[2]}));
  if(temp == Inf){
    return(cbind(Z, x1 = x, y1 = y));
  }
  x1[temp]  =   x[temp];
  y1[temp]  =   y[temp];
  
  if(temp==1){
    
    for(i in (temp+1):length(x)){

      k  <-  (y[i] - y[i-1])/(x[i]-x[i-1]);
      b  <-  y[i-1] - k*x[i-1];

      if(y[i] > ylim[2]){
        y1[i]  =   ylim[2];
        if(y[i-1]>ylim[2]){
          x1[i]  =   x[i];
        }else{
          x1[i]  =   (y1[i]-b)/k;
        }
      }else if(y[i] < ylim[1]){
        y1[i]  =   ylim[1];
        if(y[i-1]<ylim[1]){
          x1[i]  =   x[i];
        }else{
          x1[i]  =   (y1[i]-b)/k;
        }
      }else{
        y1[i]  =   y[i];
        x1[i]  =   (y1[i]-b)/k;
      }
    }
    
  }else if(temp==length(x)){
    
    for(i in (temp-1):1){

      k  <-  (y[i] - y[i+1])/(x[i]-x[i+1]);
      b  <-  y[i+1] - k*x[i+1];

      if(y[i] > ylim[2]){
        y1[i]  =   ylim[2];
        x1[i]  =   (y1[i]-b)/k;
      }else if(y[i] < ylim[1]){
        y1[i]  =   ylim[1];
        x1[i]  =   (y1[i]-b)/k;
      }else{
        y1[i]  =   y[i];
        x1[i]  =   (y1[i]-b)/k;
      }
    }
    
  }else{
    
    for(i in (temp-1):1){

      k  <-  (y[i] - y[i+1])/(x[i]-x[i+1]);
      b  <-  y[i+1] - k*x[i+1];

      if(y[i] > ylim[2]){
        y1[i]  =   ylim[2];
        if(y[i+1]>ylim[2]){
          x1[i]  =   x[i];
        }else{
          x1[i]  =   (y1[i]-b)/k;
        }
      }else if(y[i] < ylim[1]){
        y1[i]  =   ylim[1];
        if(y[i+1]<ylim[1]){
          x1[i]  =   x[i];
        }else{
          x1[i]  =   (y1[i]-b)/k;
        }
      }else{
        y1[i]  =   y[i];
        x1[i]  =   (y1[i]-b)/k;
      }
    }
    
    for(i in (temp+1):length(x)){

      k  <-  (y[i] - y[i-1])/(x[i]-x[i-1]);
      b  <-  y[i-1] - k*x[i-1];

      if(y[i] > ylim[2]){
        y1[i]  =   ylim[2];
        if(y[i-1]>ylim[2]){
          x1[i]  =   x[i];
        }else{
          x1[i]  =   (y1[i]-b)/k;
        }
      }else if(y[i] < ylim[1]){
        y1[i]  =   ylim[1];
        if(y[i-1]<ylim[1]){
          x1[i]  =   x[i];
        }else{
          x1[i]  =   (y1[i]-b)/k;
        }
      }else{
        y1[i]  =   y[i];
        x1[i]  =   (y1[i]-b)/k;
      }
    }
    
  }
  
  return(cbind(Z,x1=x1,y1=y1));
}
