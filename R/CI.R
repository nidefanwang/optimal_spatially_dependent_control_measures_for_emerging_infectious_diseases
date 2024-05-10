
# input a posterior sample of parameters to get the solution of epidemic model
cal_mu <- function(samp.post){
  
  Out <- list();
  
  for(i in 1:dim(samp.post)[1]){
    x = samp.post[i,];
    Out[[i]] = sol(Time = Time, pars = c(par_est(x), par_fix), control = Control);
  }
  
  return(Out);
}

# Out = cal_mu(samp.post); save(Out, file = "Out.RData");
load(paste0('output/Out.RData'));

# posterior sample of isolated cases, type=1 for deterministic result, type=2 for stochastic result
samp_pp <- function(samp.post, Out, type = 2){
  
  # mean of negative binomial distribution
  temp1 <- list();
  for(i in 1:dim(samp.post)[1]){
    temp_Q          =     get.K_i(K = 'Q', out = Out[[i]]);
    temp            =     temp_Q[,-1];
    temp = cbind(temp, Total = apply(temp,1,sum));
    time            =     temp_Q[, 1];
    t.temp          =     length(unique(time)) - 1;
    new             =     matrix(0, length(Time)-1, n + 1);
    new[1,]         =     temp[2,] - temp[1,];
    for(t in 2:t.temp){
      temp.n  = temp[0:2+(t-1)*2,];
      new[t,] = (temp.n[3,] - temp.n[1,]);
    }
    temp1[[i]] = new;
  }
  
  # sample
  temp2 = list();
  for(k in 1:dim(samp.post)[1]){
    temp3 = temp1[[k]];
    mu    = temp1[[k]];
    for(i in 1:dim(temp3)[1]){
      for(j in 1:dim(temp3)[2]){
        temp3[i,j] = rnbinom(n = 1, size = samp.post[k,'phi'], mu=mu[i,j]);
      }
    }
    temp2[[k]] = temp3;
  }
  
  if(type == 1){
    temp = temp1;
  }else{
    temp = temp2;
  }
  
  return(temp);
}

Out_fit1 = samp_pp(samp.post, Out, type = 1);
Out_fit2 = samp_pp(samp.post, Out, type = 2);

# smooth the result
sm_pp <- function(Out_fit){
  
  jj = n + 1;
  
  w = c(rep(1,16), rep(1,4), rep(1,27));
  
  temp.t = 1:47;
  
  sm_fit = list();
  
  for(i in 1:length(Out_fit)){
    
    temp = matrix(0, dim(Out_fit[[1]])[1], dim(Out_fit[[1]])[2]);
    
    for(j in 1:jj){
      temp[,j] = smooth.spline(temp.t, Out_fit[[i]][temp.t,j], w = w, df = 12)$y;
    }
    
    temp[temp<0] = 0;
    
    sm_fit[[i]] = temp;
  }
  
  return(sm_fit)
}

sm_Out_fit1 = sm_pp(Out_fit1);
sm_Out_fit2 = sm_pp(Out_fit2);

# credible interval
CI <- function(Out_fit){
  
  f <- function(M){
    y = M[,1:2];
    for(i in 1:dim(M)[1]){
      y[i,] = quantile(M[i,],c(0.025,0.975));
    }
    return(y);
  }
  
  temp1 = list();
  
  for(i in 1:15){
    temp2 = matrix(0,dim(Out_fit[[1]])[1],length(Out_fit));
    for(j in 1:length(Out_fit)){
      temp2[,j] = Out_fit[[j]][,i];
    }
    temp1[[i]] = f(temp2);
  }
  
  return(temp1);
}

sm_CI1 = CI(sm_Out_fit1);
sm_CI2 = CI(sm_Out_fit2);
