
sol_ocp <- function(Time, pars, control, Index_SA, alpha, fun_zero){
  
  # initial value
  y_ini  =   pars$y_ini;
  
  # control variables
  Up  =   control$Up;
  Ub  =   control$Ub;
  Ul  =   control$Ul;
  Un  =   control$Un;
  Ud  =   control$Ud;  
  
  # initial value on each time interval
  y0  =   y_ini;
  
  # used for impulsive transform and zeroing
  y_t            =   t( matrix(y0, nrow = n) );
  rownames(y_t)  =   sys_names; 
  
  # output
  out  =   c();
  
  # solve the model
  for(t in 1:(length(Time)-1)){
    
    # Time[t]
    t_now  =   Time[t];
    
    # locate the row of control variable corresponding to interval Time[t:(t+1)]
    tu  =   which(rownames(Ub) == t_now);
    
    # whether to impulse on time Time[t]
    un            <-  Un[tu,];
    temp.m        =   (1 - pars$p_fn) * un * y_t['I.m',];
    temp.s        =   (1 - pars$p_fn) * un * y_t['I.s',];
    y_t['I.m',]   =   y_t['I.m',]  - temp.m;
    y_t['I.s',]   =   y_t['I.s',]  - temp.s;
    y_t['CI.m',]  =   y_t['CI.m',] + temp.m;
    y_t['CI.s',]  =   y_t['CI.s',] + temp.s;
    y_t['Q',]     =   y_t['Q',]  + temp.m + temp.s; 
    y_t['Cum_confi',]  =   y_t['Cum_confi',]  + temp.m + temp.s;
    
    # initial value on this time interval
    y0         =   c(t(y_t));
    names(y0)  =   names(y_ini);
    
    # control variables and solution on [Time[t],Time[t+1])
    control_t  <-  list( ub = Ub[tu,], up = Up[tu,], ul = Ul[tu,], ud = Ud[tu,] );
    out_temp   <-  ode(y = y0, times = Time[t:(t+1)], fun = model, parms = c(pars, control_t) );
    out        =   rbind(out,out_temp);
    
    # state on the end of this interval
    y_t            =   t( matrix(out_temp[2,-1], nrow = n) );
    rownames(y_t)  =   sys_names;
    
    # zeroing
    y_t  =   fun_zero(Index_SA = Index_SA, out = out, y_t = y_t, alpha = alpha, t_now = t_now, control = control);
  }
  
  return(out);
}
