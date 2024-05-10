
############################ simulated annealing algorithm ############################  

# input: 1. two matrices U_SA, Index_SA, 2. number SA_number
SA_det <- function(config){
  
  # configure
  U_SA       <-  config$U_SA;
  Index_SA   <-  config$Index_SA;
  cost_list  <-  config$cost_list;
  alpha      <-  config$alpha;
  control0   <-  config$control0;
  SA_number  <-  config$SA_number;
  
  set.seed(1);
  
  file_name  =   paste0('SA_det_results_', SA_number, '.RData');
  
  ############################ functions used in SA ############################
  
  # function 1: generate a new control variable in feasible region
  gen_stat <- function(U_old, prob = 0.5, seed, pr = T){ # whether to print details (pr = T)
    
    set.seed(seed);
    
    U_new       =   U_old;
    
    i           =   ifelse(length(temp02)==1, temp02, sample(temp02, 1));
    j           =   sample(1:n, 1); 
    
    temp_index  =   Index_SA[[i]][,j];
    temp_U      =   U_old[[i]][,j];
    temp_num1   =   which({temp_index==1}&{temp_U==1});  
    temp_num2   =   which({temp_index==1}&{temp_U==0});
    
    tran_num1   =   temp_num1[which(temp_num1<max(temp_num2))];
    tran_num2   =   temp_num2[which(temp_num2<max(temp_num1))];
    
    if( {runif(1,0,1)<prob}|{length(temp_num1)==0}|{length(temp_num2)==0} ){ 
      
      temp0                =   which(temp_index==1);
      temp                 =   ifelse(length(temp0)==1, temp0, sample(temp0, 1));
      U_new[[i]][temp,j]   =   1 - U_old[[i]][temp,j]; 
      if(pr){print( paste0('i = ', i, '; ', 'j = ', j, ' change') )}
      
    }else{
      
      ran_num  <-  runif(1,0,1);
      
      if({{ran_num<0.5} & {length(tran_num1)>0}} | {{ran_num>=0.5} & {length(tran_num2)==0}}){
        temp1  =   ifelse(length(tran_num1)==1, tran_num1, sample(tran_num1, 1)); 
        temp   =   which(temp_num2>temp1); 
        temp2  =   ifelse(length(temp)==1, temp_num2[temp], temp_num2[sample(temp, 1)]);
        U_new[[i]][temp1,j]  =    1 - U_old[[i]][temp1,j];
        U_new[[i]][temp2,j]  =    1 - U_old[[i]][temp2,j];
      }else{
        temp1  =   ifelse(length(tran_num2)==1, tran_num2, sample(tran_num2, 1));
        temp   =   which(temp_num1>temp1);
        temp2  =   ifelse(length(temp)==1, temp_num1[temp], temp_num1[sample(temp, 1)]);
        U_new[[i]][temp1,j]  =    1 - U_old[[i]][temp1,j];
        U_new[[i]][temp2,j]  =    1 - U_old[[i]][temp2,j];
      }
    }
    
    U_new$Ub[which(U_new$Ul==1)] = 1;
    
    return(U_new);
  }
  
  # function 2: whether control matrix is in the feasible region     
  fea_stat <- function(U){
    
    T_ocp      =   max(as.numeric(rownames(U[[1]])));
    Out_test   =   sol_ocp(Time = 0:T_ocp, pars = pars_base, control = U, Index_SA = Index_SA, alpha = alpha, fun_zero = fun_zero);
    
    Infec_t    <-  Out_test[, which(substr(colnames(Out_test),start=1,stop=3) %in% c('E.m','E.s','I.m','I.s'))];
    Infec_t    =   apply(Infec_t, 1, sum);
    infection  =   rev(Infec_t)[1];
    
    if(infection == 0){
      t_zero  <-  Out_test[min(which(Infec_t == 0)),1];
    }else{
      t_zero  <-  Inf;
    }
    
    H          =   apply(Out_test[,which(substr(colnames(Out_test),start=1,stop=1) %in% c('H'))], 1, sum);
    
    con1       =   { infection == 0 }
    con2       =   { max(H) <= H_max }  
    
    index      =   ifelse(con1 & con2, 1, 0);
    
    return(list(index = index, out = Out_test, t_zero = t_zero));
  }
  
  # function 3: calculate cost
  cal_cost <- function(U, out, cost_list){ # cost_list是一个3元列表, 每个元素是一个n元向量表示每个区域的成本
    
    T_ocp         =    max(as.numeric(rownames(U[[1]])));
    
    cost1        <-   matrix(0, 3, n, dimnames = list(c('Un','Ub','Ul'),c()));
    cost1['Ul',] =    cost_list$Ul; 
    cost1['Ub',] =    cost_list$Ub;  
    cost1['Un',] =    cost_list$Un;  
    
    cost2       <-   matrix(rep(apply(cost1,1,max)*1.2,n), nrow = 3, dimnames = list(c('Un','Ub','Ul'),c()));
    
    tt          <-   out[,'time'];
    
    i1          =    which(tt==0);
    i20         =    which(tt%in%(1:(T_ocp-1)));
    i2          =    i20[seq(1,length(i20),by=2)];
    i3          =    which(tt==T_ocp);
    index_out   <-   out[c(i1,i2,i3),substr(colnames(out),1,3)=='I.m'] + 
      out[c(i1,i2,i3),substr(colnames(out),1,3)=='I.s'] + 
      out[c(i1,i2,i3),substr(colnames(out),1,3)=='E.m'] + 
      out[c(i1,i2,i3),substr(colnames(out),1,3)=='E.s'];
    index_tt    <-   out[c(i1,i2,i3),1];
    if(max(index_tt)<T_ocp){
      Sup = t( matrix(index_out[dim(index_out)[1],],dim(index_out)[2],T_ocp-max(index_tt)) );
      index_out = rbind(index_out, Sup);
    }
    
    index_out[index_out>0] = 1;
    rownames(index_out) = 0:T_ocp;
    colnames(index_out) = district_all;
    
    # total cost type2
    tol1        =    t(U$Ul*index_out)*cost1['Ul',] + t(U$Ub*index_out)*cost1['Ub',] + t(U$Un*index_out)*cost1['Un',];
    tol2        =    t(U$Ul*(1-index_out))*cost2['Ul',] + t(U$Ub*(1-index_out))*cost2['Ub',] + t(U$Un*(1-index_out))*cost2['Un',];
    
    # total cost type1
    tol         =    t(U$Ul)*cost1['Ul',] + t(U$Ub)*cost1['Ub',] + t(U$Un)*cost1['Un',];
    
    state       =    out[dim(out)[1],];
    cum_infec   =    sum(state[which(substr(names(state),start=1,stop=9) %in% c('Cum_infec'))]);
    
    type1       =    sum(tol);   
    type2       =    sum(tol1+tol2) + 1*cum_infec;
    
    return( list(type1 = type1, type2 = type2) );
    
  }
  
  # function 4: calculate next temperature
  cal_temp <- function(c_k){
    coef_te   =    0.9; 
    return(coef_te*c_k);
  }
  
  # function 5
  zero_control <- function(U, t_zero){
    for(i in 1:length(U)){
      temp  =   which(rownames(U[[i]]) == t_zero);
      U[[i]][temp:dim(U[[i]])[1], ]  =   0;
    }
    return(U);
  }
  ############################ functions used in SA ############################
  
  
  ############################ SA ############################
  t1 = proc.time();
  
  # initial control matrix
  U              =    U_SA;
  temp01         =    which( names(U) %in% c('Ud','Up') );
  temp02         =    (1:length(U))[-temp01]; # 控制矩阵中可以操作的部分
  for(i in temp02){
    temp1          =    Index_SA[[i]];
    temp2          =    which(temp1==1);
    temp1[temp2]   =    rbinom(length(temp2), size=1, prob=0.85); # 此处随机抽取的话即使0.8都不能保证及时清零
    U[[i]]         =    U_SA[[i]] + temp1;   
  }
  for(i in temp01){
    U[[i]]         =    Index_SA[[i]];
  }
  
  # 
  if(0){
    control_ini  <-  control0;
  }else{
    control_ini  <-  U;
  }
  
  I         =   control_ini;
  temp_fea  =   fea_stat(I);
  index     =   temp_fea$index;
  I         =   zero_control(U = I, t_zero = temp_fea$t_zero);
  if(!index){stop('infeasible initial control')}
  
  temp_f_I  =   cal_cost(I, out = fea_stat(I)$out, cost_list);
  f_I       =   temp_f_I$type2;
  f_I_real  =   temp_f_I$type1;
  
  # initial and final temperature
  c_ini     =   1e4; 
  c_end     =   0.1; 
  c_k       =   c_ini;
  
  # record objective function values
  Obj       <-  c();
  Obj_real  <-  c();
  
  # record control
  Contr_all  <-   list();
  num_Contr  <-   1;
  
  # seed
  seed_num   =    0;
  
  while(c_k >= c_end){
    
    L_k  =   500;
    
    for(l in 1:L_k){ 
      
      seed_num    =   seed_num + 1;
      
      # new control
      J           =   gen_stat(U_old = I, prob = 0.5, seed = seed_num);
      
      temp_fea    =   fea_stat(J);
      temp_index  =   temp_fea$index;
      temp_out    =   temp_fea$out;
      
      # whether to transit
      if(temp_index == 1){
        
        J         =   zero_control(U = J, t_zero = temp_fea$t_zero);
        
        temp_f_J  =   cal_cost(J, out = temp_out, cost_list);
        f_J       =   temp_f_J$type2;
        f_J_real  =   temp_f_J$type1;
        
        if(f_J <= f_I){
          I         =   J; 
          f_I       =   f_J; 
          f_I_real  =   f_J_real;
        }else{
          P  =   exp((f_I-f_J)/(c_k*3.5e5));
          if(runif(1, 0, 1) <= P){
            I         =   J; 
            f_I       =   f_J;
            f_I_real  =   f_J_real;
          } 
        }
      }
      
      Obj        =   c(Obj, f_I);
      Obj_real   =   c(Obj_real, f_I_real);
      Contr_all[[num_Contr]] = I; 
      num_Contr  =   num_Contr + 1;
      
      # dev.off; pheatmap(I[[5]], cluster_rows=F, cluster_cols=F, legend=F);
      
      print( paste0('cost = ', f_I, '; ', 'cost change = ', f_I-Obj[length(Obj)-1], '; ', 'temperature = ', c_k) );
      
    }
    
    # save
    results  <-  list(Obj = Obj, Obj_real = Obj_real, Contr_all = Contr_all, I = I, config = config, state = 'unfinished', temperature = c_k);
    save(results, file = file_name);
    
    c_k  =   cal_temp(c_k);
    
  }
  
  t2 = proc.time();
  
  # save
  results  <-  list(Obj = Obj, Obj_real = Obj_real, Contr_all = Contr_all, I = I, config = config, t = t2 - t1);
  save(results, file = paste0('output/', file_name));
  
  ############################ simulated annealing algorithm ############################ 
}
