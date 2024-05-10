
sol_sto_interval <- function(Events, y0, pars, control, T, f_stop){
  
  # generate sample path
  with( as.list(c(y0, pars, control)), {
    
    # ocp parameters
    delta.m  =   delta;
    delta.s  =   delta;
    gamma.m  =   gamma;
    gamma.s  =   gamma_s;
    gamma.H  =   gamma_H;
    
    # stochastical solution of model (notice the order of elements of y0' name)
    y0             =   y0[1:dim(Events)[2]];
    out            <-  matrix(y0, 1, length(y0));
    colnames(out)  =   names(y0);
    
    # happening time of events, initial time is T[1]
    times  <-  T[1];
    
    # mobility matrix
    bb     <-  matrix(1-ub,n,1) * matrix(1-ul,n,1);
    ub_ij  <-  bb %*% t(bb); 
    
    # happening time of the next event
    time_next  <-  T[1];
    
    # whether to stop earlier because of reaching stop condition
    index_stop  <-  0;
    
    while( {time_next < T[2]} ){
      
      # current system state
      state  <-  out[dim(out)[1],]; 
      
      # input system state x and compartment compart to return corresponding vector
      compart_num  <-  function(x, compart){
        temp  =   substr( names(x), start=1, stop=nchar(compart) );
        y     =   x[which( temp == compart )];
        return(y);
      }
      
      # system variables
      S     =   compart_num(x = state, compart = 'S');
      E.m   =   compart_num(x = state, compart = 'E.m');
      E.s   =   compart_num(x = state, compart = 'E.s');
      I.m   =   compart_num(x = state, compart = 'I.m');
      I.s   =   compart_num(x = state, compart = 'I.s');
      CE.m  =   compart_num(x = state, compart = 'CE.m');
      CE.s  =   compart_num(x = state, compart = 'CE.s');
      CI.m  =   compart_num(x = state, compart = 'CI.m');
      CI.s  =   compart_num(x = state, compart = 'CI.s');
      H     =   compart_num(x = state, compart = 'H');
      R     =   compart_num(x = state, compart = 'R');
      
      # infectives
      E  =   E.m + E.s;
      I  =   I.m + I.s;
      
      # population moving freely   
      N  =   S + E + I + R;
      
      # happening rate of events
      
      # 1. mobility
      M_S       <-  t(S   * ub_ij * tau) * 0;
      M_E.m     <-  t(E.m * ub_ij * tau);
      M_E.s     <-  t(E.s * ub_ij * tau);
      M_I.m     <-  t(I.m * ub_ij * tau);      
      M_I.s     <-  t(I.s * ub_ij * tau);
      M_R       <-  t(R   * ub_ij * tau) * 0;
      
      rate_Mig  <-  c(M_S, M_E.m, M_E.s, M_I.m, M_I.s, M_R);
      
      # 2. new infections
      New_infec   <-  (1-epsilon*ul)*beta*S*I/N; 
      rate_Infec  <-  c( (1-p_s)*New_infec, p_s*New_infec );   
      
      # 3. transition from latent period to infectious period
      rate_Lat  <-  c(sigma*E.m, sigma*E.s, sigma*CE.m, sigma*CE.s);
      
      # 4. contact tracing isolation (scale.I is an approximate treatment for speeding up the program)
      Logis     <-  ( 1+exp(r*(0-Lo)) ) / (1+exp(r*( sum(up*(E+I)) - Lo )));
      rate_Tra  <-  c(lambda_E*Logis*up*E.m, lambda_E*Logis*up*E.s, lambda_I*Logis*up*I.m, lambda_I*Logis*up*I.s);
      
      # 5. isolation of other way
      rate_Del  <-  c(ud*delta.m*I.m, ud*delta.s*I.s);
      
      # 6. hospitalization of severe cases
      rate_Hos  <-  c(gamma.s*I.s, gamma.s*CI.s);
      
      # 7. recovery
      rate_Rec  <-  c(gamma.m*I.m, gamma.m*CI.m, gamma.H*H);     
      
      # merge all transition rate (length of dim(Events)[1])
      Rate  <-  c(rate_Mig, rate_Infec, rate_Lat, rate_Tra, rate_Del, rate_Hos, rate_Rec);
      
      # happening time of the next event
      rand1      <-  rexp(1, rate = sum(Rate));     
      time_next  =   times[length(times)] + rand1;
      
      # happening event (serial number is number.event in Events matrix)
      rand2         <-  runif(1, 0, 1);
      number.event  <-  min( which( cumsum(Rate)/sum(Rate) >= rand2 ) );
      state_next    <-  state + as.numeric(time_next<=T[2]) * Events[number.event, ];
      
      # record new result
      out    =   rbind(out, state_next);
      times  =   c(times, min(time_next,T[2]));
      
      # if meet the stopping condition
      if(f_stop(out)){
        index_stop  =   1;
        break;
      }
    }
    
    # result
    temp  =   cbind(times, out);
    if(times[length(times)]<T[2]){ 
      temp  =   rbind( temp, c(T[2],out[dim(out)[1],]) ); 
    }else{
      temp  =   temp;
    }
    
    return(list(out = temp, index_stop = index_stop));
  } )
  
}
