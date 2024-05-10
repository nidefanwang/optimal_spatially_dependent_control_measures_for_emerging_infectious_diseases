
model <- function(time, state, pars){
  
  with( as.list(c(state,pars)), {
    
    # extra parameters in ocp
    delta.m   =   delta;
    delta.s   =   delta;
    gamma.m   =   gamma;
    gamma.s   =   gamma_s;
    gamma.H   =   gamma_H;
    
    # input system state x and name of compartment compart, return compartment vector
    compart_num  <-  function(x, compart){
      temp  =   substr( names(x), start=1, stop=nchar(compart) );
      y     =   x[which( temp == compart )];
      return(y);
    }
    
    # compartments
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
    E  <-  E.m + E.s;
    I  <-  I.m + I.s;
    
    # population moving freely
    N  =   S + E + I + R;
    
    # mobility matrix
    bb        <-  matrix(1-ub,n,1) * matrix(1-ul,n,1);
    ub_ij     =   bb %*% t(bb); 
    
    # mobility term
    mob_S     =   - S   * apply(ub_ij*tau,1,sum) + apply(ub_ij*tau* S,   2,sum);
    mob_E.m   =   - E.m * apply(ub_ij*tau,1,sum) + apply(ub_ij*tau* E.m, 2,sum);
    mob_I.m   =   - I.m * apply(ub_ij*tau,1,sum) + apply(ub_ij*tau* I.m, 2,sum);
    mob_CE.m  =   0;
    mob_CI.m  =   0;
    mob_E.s   =   - E.s * apply(ub_ij*tau,1,sum) + apply(ub_ij*tau* E.s, 2,sum);
    mob_I.s   =   - I.s * apply(ub_ij*tau,1,sum) + apply(ub_ij*tau* I.s, 2,sum);
    mob_CE.s  =   0;
    mob_CI.s  =   0;
    mob_H     =   0;
    mob_R     =   - R   * apply(ub_ij*tau,1,sum) + apply(ub_ij*tau* R,   2,sum);
    
    # new infections
    New_infec =   (1-epsilon*ul)*beta*S*I/N;
    
    # contact tracing
    Logis     =   ( 1+exp(r*(0-Lo)) ) / (1+exp(r*( sum(up*(E+I)) - Lo )));
    
    # system equations
    dS     =   mob_S    - New_infec;
    dE.m   =   mob_E.m  + (1-p_s)*New_infec - sigma*E.m - lambda_E*Logis*up*E.m;
    dE.s   =   mob_E.s  +     p_s*New_infec - sigma*E.s - lambda_E*Logis*up*E.s;
    dI.m   =   mob_I.m  + sigma*E.m - ud*delta.m*I.m - gamma.m*I.m - lambda_I*Logis*up*I.m;
    dI.s   =   mob_I.s  + sigma*E.s - ud*delta.s*I.s - gamma.s*I.s - lambda_I*Logis*up*I.s;
    dCE.m  =   mob_CE.m + lambda_E*Logis*up*E.m - sigma*CE.m;
    dCE.s  =   mob_CE.s + lambda_E*Logis*up*E.s - sigma*CE.s;
    dCI.m  =   mob_CI.m + lambda_I*Logis*up*I.m + ud*delta.m*I.m + sigma*CE.m - gamma.m*CI.m;
    dCI.s  =   mob_CI.s + lambda_I*Logis*up*I.s + ud*delta.s*I.s + sigma*CE.s - gamma.s*CI.s;
    dH     =   mob_H    + gamma.s*I.s + gamma.s*CI.s - gamma.H*H;
    dR     =   mob_R    + gamma.m*I.m + gamma.m*CI.m + gamma.H*H;
    
    # cumulative isolated cases and infections    
    dQ           =   lambda_E*Logis*up*(E.m+E.s) + lambda_I*Logis*up*(I.m+I.s) + ud*delta.m*I.m + ud*delta.s*I.s + gamma.s*I.s;
    dCum_infec   =   New_infec; 
    dCum_confi   =   lambda_I*Logis*up*(I.m+I.s) + ud*delta.m*I.m + ud*delta.s*I.s + gamma.s*I.s;
    
    all_vector  = c( dS, dE.m,dE.s, dI.m,dI.s, dCE.m,dCE.s, dCI.m,dCI.s, dH, dR,  dQ, dCum_infec, dCum_confi);
    
    return(list(all_vector));
  })
  
}
