
# f_stop1: stop function in the interval; f_stop2: stop function at the end of the interval
sol_sto <- function(k, Time, pars, control, f_stop1, f_stop2){
  
  # random number seed
  set.seed(k);
  
  # control variables
  Ub  =   control$Ub;
  Up  =   control$Up;
  Ul  =   control$Ul;
  Un  =   control$Un;
  Ud  =   control$Ud;
  
  # initial value of system
  y_ini  =   pars$y_ini[1:dim(Events)[2]];
  
  # initial value over a interval 
  y0  =   round(y_ini);
  
  # for impulse and clearance
  y_t            =   t( matrix(y0, nrow = n) );
  rownames(y_t)  =   sys_names[1:dim(y_t)[1]];
  
  # The final output of this program (note that the first line of 'out' matrix is redundant so it is removed) 
  out           =    matrix(c(Time[1], y0), 1, length(y0)+1);
  colnames(out) =    c('time', names(y0));
  
  # solve for the model
  for(t in 1:(length(Time)-1)){
    
    # current time Time[t]
    t_now  =   Time[t];
    
    # locate the row of the control variable over time interval Time[t:(t+1)]. 定位时间区间Time[t:(t+1)]内控制变量的取值所在的行.
    tu  <-  which(rownames(Ub) == t_now);
    
    # whether impulse at time Time[t] 
    un            <-  Un[tu,];
    temp.m        =   rbinom(n, size = y_t['I.m',], prob = (1 - pars$p_fn) * un);
    temp.s        =   rbinom(n, size = y_t['I.s',], prob = (1 - pars$p_fn) * un);
    y_t['I.m',]   =   y_t['I.m',]  - temp.m;
    y_t['I.s',]   =   y_t['I.s',]  - temp.s;
    y_t['CI.m',]  =   y_t['CI.m',] + temp.m;
    y_t['CI.s',]  =   y_t['CI.s',] + temp.s;
    
    # initial value of the current interval
    y0         =   c(t(y_t));
    names(y0)  =   names(y_ini);   
    
    # control variable and system solution on interval [Time[t],Time[t+1])
    control_t      <-  list( ub = Ub[tu,], up = Up[tu,], ul = Ul[tu,], ud = Ud[tu,] );
    out_temp       <-  sol_sto_interval(Events = Events, y0 = y0, pars = pars, control = control_t, T = Time[t:(t+1)], f_stop = f_stop1);
    out            =   rbind(out,out_temp[[1]]);
    temp           =   out_temp[[1]];
    y_t            =   t(matrix(temp[dim(temp)[1],-1], nrow = n));
    rownames(y_t)  =   sys_names[1:dim(y_t)[1]];
    
    index_stop1  =   out_temp[[2]];
    index_stop2  =   f_stop2(out);
    if(index_stop1+index_stop2){break}
  }
  
  return(list(out[-1,], index_stop1, index_stop2));
}
