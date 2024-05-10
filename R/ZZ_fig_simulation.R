
####################### plot parameters used #######################

# run 'main.R' first
source('R/main.R');

#
temp      <-  compl(T_ocp = 300, U_SA = U_SA, Index_SA = Index_SA);
U_SA      =   temp$U_SA;
Index_SA  =   temp$Index_SA;

#
display.brewer.all();
brewer.pal.info;
col1  =   brewer.pal(9,"OrRd")[6]; 
col2  =   brewer.pal(9,"YlOrRd")[5]; 
col3  =   brewer.pal(9,"YlGn")[5]; 
col4  =   brewer.pal(9,"YlGnBu")[5];
col5  =   brewer.pal(9,"Set3")[4];
col6  =   brewer.pal(9,"Set3")[1];
col7  =   brewer.pal(11,"PiYG")[8];
col8  =   brewer.pal(10,"Set3")[10];
col   =   c(col2, col3, col4, col1, col5, col6, col7, col8);
plot(1:8,col=col);points(1:8,cex=4,pch=19,col=col);

# extend matrix Control_base
Control_base_compl  <-  Control_base;
for(i in 1:length(Control_base)){
  f_temp <- function(x){
    temp  =   min(which(x>0));
    x[temp:length(x)] = 1;
    return(x);
  }
  Control_base_compl[[i]] = apply(Control_base[[i]], 2, f_temp);
}

# plot configure
size_legend  =   20;
size_title   =   30;
size_axis    =   16;
size_axis_y  <-  20;
size_axis_x  <-  18;

mars  <-  c(t=2, r=10, b=8, l=10);
####################### plot parameters used #######################



####################### function used #######################

# for control matrix Control, move control Uj for dj days, dj>0 backward, dj<0 forward. d is a vector, and the name of its elements are control measures moved.
move <- function(Control, d){
  
  time_Control  <-  as.numeric( rownames(Control[[1]]) ); # row name of Control[[i]]
  
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

# combine mild and severe infections
merge <- function(out){
  
  temp      =   sapply(colnames(out), function(x){substr(x,start=1,stop=3)});
  temp1     =   which(temp == 'E.m');
  temp2     =   which(temp == 'E.s'); 
  temp3     =   which(temp == 'I.m');
  temp4     =   which(temp == 'I.s'); 
  out_E     <-  out[,temp1] + out[,temp2];
  out_I     <-  out[,temp3] + out[,temp4];
  
  return( list(E = out_E, I = out_I) );
}

# input Un to get impulsive time
f_impul <- function(Un){
  y  <-  as.numeric(rownames(Un));
  temp  <-  apply(Un, 1, sum);
  return(y[which(temp > 0)]);
}

# input time and impulsive time to get group
group_out <- function(time, t_impul){
  if(length(t_impul)==0){return(time*0)}
  y  <-  time;
  t_impul  =   t_impul[t_impul <= max(time)];
  for(i in 1:length(t_impul)){
    if(i == 1){
      temp  =   1;
    }else{
      temp  =   max(which(time==t_impul[i-1])); 
    }
    y[temp:min(which(time==t_impul[i]))]  =   i;
  }
  temp  =   min(which(time==t_impul[length(t_impul)]))+1;
  if(temp <= length(y)){y[temp:length(y)]  =   max(y) + 1}
  return(y);
}

# input matirces X, Y, Z, scalars ylim, xlab, ylab, title. Shape of color matrix is the same as Y, with each column composed of the same element.
f_plot <- function(x, Y, Z, ylim, xlim, xlab, ylab, title = '', col, label, seq_y = 50, leg_pos = c(0.60, 0.80)){
  
  temp  =   which(x<=xlim);
  x     =   x[temp];
  Y     =   Y[temp,];
  Z     =   Z[temp,];
  
  ff_temp <- function(y){
    xx = rev(x)[max(which(cumsum(rev(y))==0))];
  }
  zero_time  <-  apply(Y, 2, ff_temp);
  
  Col   =   c();
  Label =   c();
  for(i in 1:dim(Y)[2]){
    Col = c(Col, rep(col[i],dim(Y)[1]));
    Label = c(Label, rep(label[i],dim(Y)[1]));
  }
  names(Col)  =   Col;
  
  data_plot  <-  data.frame(x = x, y = c(Y), group = c(Z), col = Col);
  
  temp  =   ggplot(data = data_plot, aes(x = x, y = y, group = group, color = col)) + 
    scale_y_continuous( breaks = c(seq(0, ylim, seq_y), ylim), labels = c(seq(0, ylim, seq_y), ylim), limits = c(0,ylim) ) + 
    labs(title = 'title') + 
    theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 24,face = "plain"),
          panel.background = element_rect(fill = "grey98"),
          panel.grid.major = element_line(colour = "grey100", size = 0.1)) + 
    scale_x_continuous( breaks = c(seq(0, xlim, 5), xlim), labels = c(seq(0, xlim, 5), xlim) ) + 
    theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + 
    theme( axis.text.y = element_text(size=size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
    xlab(xlab) + theme(axis.title.x=element_text(vjust = 1.8, size = size_axis_x,face = "plain")) + 
    ylab(ylab) + theme(axis.title.y=element_text(vjust = 2, size = size_axis_y,face = "plain")) + 
    geom_line(size = 0.6, aes(col = col)) +
    scale_linetype_manual( values = rep('solid',dim(Y)[2]) ) + 
    scale_color_manual( values = Col, labels = Label ) + # need to name the Col vector, if not, the color is out of order
    theme( legend.position = leg_pos, legend.title=element_blank(), legend.text = element_text(size = size_legend) ) + 
    theme( plot.margin = margin(mars) );
  
  ff_temp <- function(temp, i){
    p = temp + geom_point( x = zero_time[i] - 0.08*i, y = 0, color = col[i], size = 2.36 );
    return(p);
  }
  p  <-  temp;
  for(i in 1:length(zero_time)){
    p = ff_temp(p, i);
  }
  
  return(p);
}
####################### function used #######################



####################### simulation 1 #######################

# In the simulation for n-day advance or delay of control, get the lim of the maximum number of days that control can be advanced. Need to input baseline conrrol matrix.

cal_lim <- function(Control){
  
  Temp_all  <-  apply(Control$Un + Control$Ub + Control$Ul, 1, sum);
  Temp_Un   <-  apply(Control$Un, 1, sum);
  Temp_Ub   <-  apply(Control$Ub, 1, sum);
  Temp_Ul   <-  apply(Control$Ul, 1, sum);
  
  rn  <-  rownames(Control$Up);
  tp  <-  min(which(apply(Control$Up, 1, sum)>0));
  
  lim_all  <-  as.numeric( rn[min(which(Temp_all>0))] ) - as.numeric( rn[tp] ); # 总的
  lim_Un   <-  as.numeric( rn[min(which(Temp_Un>0))] ) - as.numeric( rn[tp] ); # 总的
  lim_Ub   <-  as.numeric( rn[min(which(Temp_Ub>0))] ) - as.numeric( rn[tp] ); # 总的
  lim_Ul   <-  as.numeric( rn[min(which(Temp_Ul>0))] ) - as.numeric( rn[tp] ); # 总的
  
  lim  <-  list(all = lim_all, Un = lim_Un, Ub = lim_Ub, Ul = lim_Ul);
  return(lim);
}


# matrix to saving results
def_mat <- function(index_d, index_factors){
  Results1       <-  matrix(0, length(index_d), length(index_factors));
  colnames(Results1)  =   index_factors;
  rownames(Results1)  =   index_d;
  return(Results1);
}

# input number of days d that Control is changed, to get matrix out and E, I.
simu1 <- function(Control, d){
  
  Control1    =   move(Control = Control, d = d);
  out         =   sol_ocp(Time = 0:100, pars = pars_base, control = Control1, Index_SA = Index_SA, alpha = config_det_base$alpha, fun_zero = fun_zero);
  E.and.I     =   merge(out); 
  
  return(list(out = out, E.and.I = E.and.I, Control = Control1, t_impul = f_impul(Control1$Un)));
}

# input list composed of d and return x, Y, Z
f1_temp <- function(Control, list_d){
  
  results  <-  list();
  for(i in 1:length(list_d)){
    results[[i]]  =   simu1(Control = Control,d = list_d[[i]]);
  }
  
  x  <-  results[[1]]$out[,1];
  Y  <-  matrix(0, length(x), length(results));
  Z  <-  Y;
  for(i in 1:length(results)){
    Y[, i]  =   apply(results[[i]]$E.and.I$I, 1, sum);
    Z[, i]  =   group_out(time = x, t_impul = results[[i]]$t_impul) + ifelse(i==1, 0, max(Z[,i-1]));
  }
  
  return( list(x = x, Y = Y, Z = Z) );
}

# input range of advanced days, return many results
f1_results <- function(Control, index_d, Uj = 'all'){
  
  Results1 <-  def_mat(index_factors = c('Cum', 'peak value', 'peak time', 'zero time'), index_d = index_d);
  
  for(i in 1:length(index_d)){
    k = index_d[i];
    if(Uj=='Un'){
      dj  =   c(Un = k, Ul = 0, Ub = 0);
    }else if(Uj=='Ub'){
      dj  =   c(Un = 0, Ul = 0, Ub = k);
    }else if(Uj=='Ul'){
      dj  =   c(Un = 0, Ul = k, Ub = 0);
    }else{
      dj  =   c(Un = k, Ul = k, Ub = k);
    }
    results  =   simu1(Control = Control, d = dj);
    temp     =   which( substr(colnames(results$out),1,9) %in% 'Cum_infec' );
    cum_vec  <-  apply( results$out[,temp], 1, sum);
    I        <-  apply(results$E.and.I$I, 1, sum);
    Results1[i,1]  <-  rev( cum_vec )[1]; # total infections
    Results1[i,2]  <-  max( I ); # peak of I(t)
    Results1[i,3]  <-  (results$out[,1])[which.max(I)]; # peak time of I(t)
    Results1[i,4]  <-  rev(results$out[,1])[max(which(cumsum(rev(I))==0))]; # zeroing time of infections out of isolation area
  }
  
  return(cbind(index_d, Results1));
}

# get d
f_Uj <- function(Uj, num){
  
  d = list();
  
  for(i in 1:length(num)){
    d[[i]]  =   c(Un = 0, Ub = 0, Ul = 0);
    if(Uj=='all'){
      d[[i]][1:length(d[[i]])]  =   num[i];
    }else{
      d[[i]][Uj]  =   num[i];
    }  
  }
  
  return(d);
}


# range of starting time of 3 control measures
temp     <-  cal_lim(Control_base);

Control  <-  Control_base;

# change 3 control measures simultaneously
temp1  <-  f1_temp(Control = Control, list_d = f_Uj(Uj = 'all', num=c(0,-1,-3,-5)) );
label  <-  c(expression( italic(baseline) ), expression(paste('(', d^n, ", ", d^b, ", ", d^l, ')', '-1')), expression(paste('(', d^n, ", ", d^b, ", ", d^l, ')', '-3')), expression(paste('(', d^n, ", ", d^b, ", ", d^l, ')', '-5')));
p11    <-  f_plot(x = temp1$x, Y = temp1$Y, Z = temp1$Z, ylim = 350, xlim = 50, xlab = '', ylab = '', title = '', col, label) + labs(title = '(A1)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题
# r11    <-  f1_results(Control = Control, index_d = -temp$all:7, Uj = 'all');
# r11    =   cbind(r11, group = rep(1,dim(r11)[1]));
# save(r11, file = "r11.RData");
load('output/r11.RData');

# change Un
temp2  <-  f1_temp(Control = Control, list_d = f_Uj(Uj = 'Un', num=c(0,-2,-5)) );
label  <-  c('baseline', expression(paste(italic(d^n), '=-2   ')), expression(paste(italic(d^n), '=-5   ')) );
p12    <-  f_plot(x = temp2$x, Y = temp2$Y, Z = temp2$Z, ylim = 350, xlim = 50, xlab = 'Time', ylab = 'I(t)', title = '', col, label, leg_pos = c(0.75, 0.80)) + labs(title = '(A1)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题
# r12    <-  f1_results(Control = Control, index_d = -temp$Un:7, Uj = 'Un');
# r12    =   cbind(r12, group = rep(2,dim(r12)[1]));
# save(r12, file = "r12.RData");
load('output/r12.RData');

# change Ub
temp3  <-  f1_temp(Control = Control, list_d = f_Uj(Uj = 'Ub', num=c(0,-2,-5,-11)) );
label  <-  c('baseline', expression( paste(italic(d^b), '=-2   ')), expression(paste(italic(d^b), '=-5   ')), expression(paste(italic(d^b), '=-11 ')));
col1  =   col; col1[4]  =   brewer.pal(10,"Paired")[10];
p13    <-  f_plot(x = temp3$x, Y = temp3$Y, Z = temp3$Z, ylim = 350, xlim = 50, xlab = 'Time', ylab = 'I(t)', title = '', col1, label, leg_pos = c(0.75, 0.8)) + labs(title = '(A2)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题
# r13    <-  f1_results(Control = Control, index_d = -temp$Ub:7, Uj = 'Ub');
# r13    =   cbind(r13, group = rep(3,dim(r13)[1]));
# save(r13, file = "r13.RData");
load('output/r13.RData');

# change Ul
temp4  <-  f1_temp(Control = Control, list_d = f_Uj(Uj = 'Ul', num=c(0,-2,-5)) );
label  <-  c('baseline', expression( paste(italic(d^l), '=-2   ')), expression(paste(italic(d^l), '=-5   ')) );
p14    <-  f_plot(x = temp4$x, Y = temp4$Y, Z = temp4$Z, ylim = 350, xlim = 50, xlab = 'Time', ylab = 'I(t)', title = '', col, label, leg_pos = c(0.75, 0.8)) + labs(title = '(A3)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题
# r14    <-  f1_results(Control = Control, index_d = -temp$Ul:7, Uj = 'Ul');
# r14    =   cbind(r14, group = rep(4,dim(r14)[1]));
# save(r14, file = "r14.RData");
load('output/r14.RData');

# infection size plot
label  <-  c('all', expression( italic(d^n) ), expression(italic(d^b)), expression(paste(italic(d^l), ' ')) );
ylim   =   c(0,1e4);
xlim   =   seq(-13, 7, 2);
r_list1    <-  list(r11 = r11, r12 = r12, r13 = r13, r14 = r14);
data_temp  <-  r_list1;
r_mat1 = c();
for(i in 1:length(r_list1)){
  temp  <-  f_ggplot_comp(x = r_list1[[i]][,1], y = r_list1[[i]][,2], ylim = ylim);
  data_temp[[i]]  =   cbind(x1 = temp[,'x1'], y1 = temp[,'y1'], group = r_list1[[i]][,'group']); 
  r_mat1 = rbind(r_mat1, data_temp[[i]]);
}

Col   =   c();
Label =   c();
for(i in 1:4){
  temp   =   sum(r_mat1[,'group']==i);
  Col    =   c(Col, rep(col[i+4], temp) );
  Label  =   c(Label, rep(label[i],temp));
}
names(Col)  =   Col;
data  <-  cbind(r_mat1, col = Col);
data_plot  <-  data.frame(data);
data_plot[,2]  =   as.numeric(data_plot[,2]);
data_plot[,1]  =   as.numeric(data_plot[,1]);

p15  <-  ggplot(data = data_plot, aes(x = x1, y = y1, group = group, color = col)) +
  scale_y_continuous( breaks = c(seq(ylim[1], ylim[2], 2000), ylim[2]), labels = c(seq(ylim[1], ylim[2], 2000), expression(''>=10000) ), limits = ylim ) + # 设置y轴刻度和标签
  labs(title = '(A4)') + 
  theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"),
        panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1)) + 
  scale_x_continuous( breaks = xlim, labels = xlim ) + 
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + 
  theme( axis.text.y = element_text(size=size_axis,color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
  xlab('Days of change') + theme(axis.title.x=element_text(vjust = 1.8, size = size_axis_x,face = "plain")) + 
  ylab('Cum') + theme(axis.title.y=element_text(vjust = -1, size = size_axis_y,face = "plain")) + 
  geom_point(size = 3, aes(col = col)) +
  geom_line(size = 0.8, aes(col = col), linetype = 3) +
  scale_color_manual( values = Col, labels = Label ) +
  theme( legend.position = c(0.14, 0.8), legend.title=element_blank(), legend.text = element_text(size = size_legend) ) + 
  theme( plot.margin = margin(mars));
####################### simulation 1 #######################



####################### simulation 2 #######################

# 4 parameters to discuss, change one of them to generate new model parameters, factor is vector with 4 elements which represent change in beta, sigma, gamma, p_fn.
get_pars <- function(factor){ # notice that only one parameter can be changed each time
  
  sigma      =   sigma_base;
  gamma      =   gamma_base;
  
  pars       =   pars_base; # parameters of epidemic model
  pars$beta  =   pars$beta + factor['R0']*gamma;
  sigma      =   1/(1/sigma_base + factor['sigma']);
  gamma      =   1/(1/gamma_base + factor['gamma']); 
  
  pars$beta  =   pars$beta*(gamma/gamma_base); # this is to avoid the change in gamma cause a change in R0
  pars$p_fn  =   pars$p_fn + factor['p_fn'];
  pars$sigma =   sigma;
  pars$gamma =   gamma;
  
  return(pars);
}


# cumulative number of infections or confirmed cases at several important time points
cal_infec_number <- function(alpha, Control){
  
  U_SA        <-   Control;
  Index_SA    <-   Control;
  for(i in 1:length(U_SA)){
    U_SA[[i]]  =   U_SA[[i]]*0;
  }
  
  T_end       <-   100;
  
  # when epidemic was found under baseline, there were cumI infectives in infectious period. In the follow-up simulation under different parameters, we assume that control begins when cumI infectives occurrs (the Index_SA matrix of Ud and Up is then obtained).
  # notice that impulse distribution is wrong when model != model_ocp_confirm, but can be used before impulse
  # run without control to get cumulative number of infectives when outbreak was found
  out_temp  =   sol_ocp(Time = 0:T_end, pars = pars_base, control = U_SA, Index_SA = Index_SA, alpha = alpha, fun_zero = fun_zero);
  temp1     =   sapply(colnames(out_temp), function(x){substr(x,start=1,stop=9)});
  temp2     =   out_temp[, which(temp1 == 'Cum_infec')]; # cumulative new infections
  temp      =   temp2;
  t_up      =   as.numeric( rownames(Index_SA$Up) [ min(which(apply(Index_SA$Up,1,sum) > 0)) ] );
  temp3     =   which(out_temp[, 1] == t_up)[1];
  cumI_up   =   sum(temp[temp3, ]);
  
  # run with Ud, Up only, to get cumulative confirmed cases when Un started
  out_temp  =   sol_ocp(Time = 0:T_end, pars = pars_base, control = Index_SA, Index_SA = Index_SA, alpha = alpha, fun_zero = fun_zero);
  temp1     =   sapply(colnames(out_temp), function(x){substr(x,start=1,stop=9)});
  temp2     =   out_temp[, which(temp1 == 'Cum_confi')]; # cumulative new confirmed cases
  temp      =   temp2;
  t_un      =   as.numeric( rownames(Index_SA$Un) [ min(which(apply(Index_SA$Un,1,sum) > 0)) ] );
  temp3     =   which(out_temp[,1] == t_un)[1]; 
  cumC_un   =   sum(temp[temp3, ]);
  
  # run with Ud, Up, Un, to get cumulative confirmed cases when Ul, Ud started
  out_temp  =   sol_ocp(Time = 0:T_end, pars = pars_base, control = Control,  Index_SA = Index_SA, alpha = alpha, fun_zero = fun_zero);
  temp1     =   sapply(colnames(out_temp), function(x){substr(x,start=1,stop=9)});
  temp2     =   out_temp[, which(temp1 == 'Cum_confi')]; # cumulative new confirmed cases
  temp      =   temp2;
  t_ul      =   as.numeric( rownames(Index_SA$Ul) [ min(which(apply(Index_SA$Ul,1,sum) > 0)) ] );
  temp3     =   max(which(out_temp[,1] == t_ul)); 
  cumC_ul   =   sum(temp[temp3, ]);
  
  return( list(cumI_up = cumI_up, cumC_un = cumC_un, cumC_ul = cumC_ul) ); 
}


# input parameters to get starting time of each control measure, Control is baseline control matrix
cal_time <- function(pars, Num, alpha, Control){
  
  # U matrix
  U  =   list();
  for(i in 1:length(Control)){
    U[[i]]  =   Control[[i]]*0;
  }
  names(U)  =   names(Control);
  
  T_end       <-   100;
  
  # cumulative infections reaches cumI_up then contact tracing and Spontaneous go to hospital
  out_temp  =   sol_ocp(Time = 0:T_end, pars = pars, control = U, Index_SA = U, alpha = alpha, fun_zero = fun_zero);
  temp1     =   sapply(colnames(out_temp), function(x){substr(x,start=1,stop=9)});
  temp2     =   out_temp[, which(temp1 == 'Cum_infec')]; # cumulative infections
  temp3     =   apply(temp2, 1, sum);
  temp      =   temp3;
  time_p    =   out_temp[which(temp >= Num$cumI_up), 1] [1];
  
  t1        <-  floor(time_p);
  t2        <-  t1 - 1;
  t3        <-  t1 + 1;
  nt1       <-  which(out_temp[,1]==t1)[1];
  nt2       <-  which(out_temp[,1]==t2)[1];
  nt3       <-  which(out_temp[,1]==t3)[1];
  temp_nt   <-  c(abs(temp[nt1]-Num$cumI_up), abs(temp[nt2]-Num$cumI_up), abs(temp[nt3]-Num$cumI_up));
  time_p    =   c(t1,t2,t3)[which.min(temp_nt)];
  
  temp  =   which(rownames(U$Up) == time_p);
  U$Up[temp:dim(U$Up)[1],]  =   1;
  U$Ud[temp:dim(U$Ud)[1],]  =   1;
  
  # starting time of Un set to be the time when cumulative confirmed cases out of isolation area reaches cumC_un under control Up, Ud.
  out_temp  =   sol_ocp(Time = 0:T_end, pars = pars, control = U, Index_SA = U, alpha = alpha, fun_zero = fun_zero);
  temp1     =   sapply(colnames(out_temp), function(x){substr(x,start=1,stop=9)});
  temp2     =   out_temp[, which(temp1 == 'Cum_confi')]; # cumulative new confirmed cases
  temp3     =   apply(temp2, 1, sum);
  temp      =   temp3;
  time_n    =   out_temp[which(temp >= Num$cumC_un), 1] [1];
  
  t1        <-  floor(time_n);
  t2        <-  t1 - 1;
  t3        <-  t1 + 1;
  nt1       <-  which(out_temp[,1]==t1)[1];
  nt2       <-  which(out_temp[,1]==t2)[1];
  nt3       <-  which(out_temp[,1]==t3)[1];
  temp_nt   <-  c(abs(temp[nt1]-Num$cumC_un), abs(temp[nt2]-Num$cumC_un), abs(temp[nt3]-Num$cumC_un));
  time_n    =   c(t1,t2,t3)[which.min(temp_nt)];
  
  # starting time of Ul set to be the time when cumulative confirmed cases out of isolation area reaches cumC_ul under control Up, Ud, Un.
  U$Un      =   Control$Un;
  t_un      =   as.numeric( rownames(Control_base$Un) [ min(which(apply(Control_base$Un,1,sum) > 0)) ] );
  U         =   move(Control = U, d = c(Un = time_n - t_un));
  out_temp  =   sol_ocp(Time = 0:T_end, pars = pars, control = U, Index_SA = U, alpha = alpha, fun_zero = fun_zero);
  temp1     =   sapply(colnames(out_temp), function(x){substr(x,start=1,stop=9)});
  temp2     =   out_temp[, which(temp1 == 'Cum_confi')]; # cumulative new confirmed cases
  temp3     =   apply(temp2, 1, sum);
  temp      =   temp3;
  time_l    =   out_temp[which(temp >= Num$cumC_ul), 1] [1];
  
  t1        <-  floor(time_l);
  t2        <-  t1 - 1;
  t3        <-  t1 + 1;
  nt1       <-  max(which(out_temp[,1]==t1));
  nt2       <-  max(which(out_temp[,1]==t2));
  nt3       <-  max(which(out_temp[,1]==t3));
  temp_nt   <-  c(abs(temp[nt1]-Num$cumC_ul), abs(temp[nt2]-Num$cumC_ul), abs(temp[nt3]-Num$cumC_ul));
  time_l    =   c(t1,t2,t3)[which.min(temp_nt)];
  
  return(c(Up = time_p, Ud = time_p, Un = time_n, Ul = time_l, Ub = time_l));
}


# construct control matrix for changed parameters, pars is the current parameters, Control is baseline control 
cal_Control <- function(pars, Num, alpha, Control, Time_0){
  
  Time  =   cal_time(pars, Num, alpha, Control);
  y     =   move(Control = Control, d = Time - Time_0);
  
  names(y)  =   names(Control);
  
  return(y);
}


# input vector of parameter changes factor, return out
simu2 <- function(factor, Num){
  
  pars     =   get_pars(factor);
  Control  =   cal_Control(pars = pars, Num = Num, alpha = config_det_base$alpha, Control = Control_base, Time_0 = Time_0);
  out      =   sol_ocp(Time = 0:100, pars = pars, control = Control, Index_SA = Control, alpha = config_det_base$alpha, fun_zero = fun_zero);
  
  return(list(out = out, Control = Control));
}

Num        =   cal_infec_number(alpha = config_det_base$alpha, Control = Control_base);

# input out and Control, return x, Y, Z
f2_temp <- function(out, Control){
  
  x  <-  out[,1];
  Y  <-  apply(merge(out)$I, 1, sum);
  Z  <-  group_out(time = x, t_impul = f_impul(Control$Un));
  
  return( list(x = x, Y = Y, Z = Z) );
}

Time_0  =   cal_time(pars_base, Num, alpha = config_det_base$alpha, Control = Control_base);

# discuss R0
factor1  <-  c(R0 =  0, sigma = 0, gamma = 0, p_fn = 0);
factor2  <-  c(R0 =  1, sigma = 0, gamma = 0, p_fn = 0);
factor3  <-  c(R0 = -1, sigma = 0, gamma = 0, p_fn = 0);
Factor   <-  list(factor1 = factor1, factor2 = factor2, factor3 = factor3);

x = c();
Y = c();
Z = c();
Results2 <- list();
for(i in 1:length(Factor)){
  temp      =   simu2(factor = Factor[[i]], Num);
  Results2  =   list(Results2, temp);
  temp      =   f2_temp(temp$out, temp$Control);
  x  =   temp$x;
  Y  =   cbind(Y, temp$Y);
  Z  =   cbind(Z, temp$Z + ifelse(i==1,0,max(Z)));
}

results21  <-  list(x = x, Y = Y, Z = Z);

label  <-  c('baseline', expression( paste(italic(R[0]), ' + 1  ') ), expression( paste(italic(R[0]), ' - 1  ') ) );
p21    <-  f_plot(x = results21$x, Y = results21$Y, Z = results21$Z, ylim = 400, xlim = 50, seq_y = 100, xlab = 'Time', ylab = 'I(t)', title = '', col, label, leg_pos = c(0.75, 0.8)) + labs(title = '(B1)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题


# discuss sigma
factor1  <-  c(R0 = 0, sigma = 0, gamma = 0, p_fn = 0);
factor2  <-  c(R0 = 0, sigma = +0.5, gamma = 0, p_fn = 0);
factor3  <-  c(R0 = 0, sigma = -0.5, gamma = 0, p_fn = 0);
Factor   <-  list(factor1 = factor1, factor2 = factor2, factor3 = factor3);

x = c();
Y = c();
Z = c();
Results2 <- list();
for(i in 1:length(Factor)){
  temp      =   simu2(factor = Factor[[i]], Num);
  Results2  =   list(Results2, temp);
  temp      =   f2_temp(temp$out, temp$Control);
  x  =   temp$x;
  Y  =   cbind(Y, temp$Y);
  Z  =   cbind(Z, temp$Z + ifelse(i==1,0,max(Z)));
}

results22  <-  list(x = x, Y = Y, Z = Z);

label  <-  c(expression( 'baseline  ' ), expression( paste('1/', sigma, ' + 0.5  ') ), expression( paste('1/', sigma, ' - 0.5  ') ) );
p22    <-  f_plot(x = results22$x, Y = results22$Y, Z = results22$Z, ylim = 400, xlim = 50, seq_y = 100, xlab = 'Time', ylab = 'I(t)', title = '', col, label, leg_pos = c(0.76, 0.8)) + labs(title = '(B2)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题


# discuss gamma
factor1  <-  c(R0 = 0, sigma = 0, gamma = 0, p_fn = 0);
factor2  <-  c(R0 = 0, sigma = 0, gamma = +0.5, p_fn = 0);
factor3  <-  c(R0 = 0, sigma = 0, gamma = -0.5, p_fn = 0);
Factor   <-  list(factor1 = factor1, factor2 = factor2, factor3 = factor3);

x = c();
Y = c();
Z = c();
Results2 <- list();
for(i in 1:length(Factor)){
  temp      =   simu2(factor = Factor[[i]], Num);
  Results2  =   list(Results2, temp);
  temp      =   f2_temp(temp$out, temp$Control);
  x  =   temp$x;
  Y  =   cbind(Y, temp$Y);
  Z  =   cbind(Z, temp$Z + ifelse(i==1,0,max(Z)));
}

results23  <-  list(x = x, Y = Y, Z = Z);

label  <-  c(expression( 'baseline  ' ), expression( paste('1/', gamma, ' + 0.5  ') ), expression( paste('1/', gamma, ' - 0.5  ') ) );
p23    <-  f_plot(x = results23$x, Y = results23$Y, Z = results23$Z, ylim = 400, xlim = 50, seq_y = 100, xlab = 'Time', ylab = 'I(t)', title = '', col, label, leg_pos = c(0.76, 0.8)) + labs(title = '(B3)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain")); # 设置标题


# input changes in parameters, return results
f2_results <- function(factor){
  
  results  =   simu2(factor = factor, Num = Num)$out;
  temp     =   which( substr(colnames(results),1,9) %in% 'Cum_infec' );
  cum_vec  <-  apply( results[,temp], 1, sum);
  I        <-  apply( merge(results)$I, 1, sum);
  y1  <-  rev( cum_vec )[1]; # total infections
  y2  <-  max( I ); # peak of I(t)
  y3  <-  (results[,1])[which.max(I)]; # peak time of I(t)
  y4  <-  rev(results[,1])[max(which(cumsum(rev(I))==0))]; # zeroing time of infectives out of isolation time
  
  y   <-  c(y1, y2, y3, y4);
  names(y)  =   c('Cum', 'peak value', 'peak time', 'zero time');
  
  return( y );
}

f_simu2_Mat <- function(){
  # discuss R0
  factor_R0   <-  seq(-1.5, 1.5, 0.1);
  Mat_R0      <-  matrix(0, length(factor_R0), 5);
  Mat_R0[,1]  =   factor_R0;
  for(i in 1:dim(Mat_R0)[1]){
    factor    =   c(R0 = factor_R0[i], sigma = 0, gamma = 0, p_fn = 0);
    Mat_R0[i,-1]  =   f2_results(factor);
  }
  colnames(Mat_R0)  =   c('x', 'Cum', 'peak value', 'peak time', 'zero time');
  
  # discuss sigma
  factor_sigma   <-  seq(-1.5, 1.5, 0.1);
  Mat_sigma      <-  matrix(0, length(factor_sigma), 5);
  Mat_sigma[,1]  =   factor_sigma;
  for(i in 1:dim(Mat_sigma)[1]){
    y         <-  factor_sigma[i];
    factor    =   c(R0 = 0, sigma = y, gamma = 0, p_fn = 0);
    Mat_sigma[i,-1]  =   f2_results(factor);
  }
  colnames(Mat_sigma)  =   c('x', 'Cum', 'peak value', 'peak time', 'zero time');
  
  # discuss gamma
  factor_gamma   <-  seq(-1.5, 1.5, 0.1);
  Mat_gamma      <-  matrix(0, length(factor_gamma), 5);
  Mat_gamma[,1]  =   factor_gamma;
  for(i in 1:dim(Mat_gamma)[1]){
    y         <-  factor_gamma[i];
    factor    =   c(R0 = 0, sigma = 0, gamma = y, p_fn = 0);
    Mat_gamma[i,-1]  =   f2_results(factor);
  }
  colnames(Mat_gamma)  =   c('x', 'Cum', 'peak value', 'peak time', 'zero time');
  
  return(list(Mat_R0, Mat_sigma, Mat_gamma));
}

# temp       <-  f_simu2_Mat();
# Mat_R0     <-  temp[[1]];
# Mat_sigma  <-  temp[[2]];
# Mat_gamma  <-  temp[[3]];

# save(Mat_R0, file = "Mat_R0.RData");
load('output/Mat_R0.RData');

# save(Mat_sigma, file = "Mat_sigma.RData");
load('output/Mat_sigma.RData');

# save(Mat_gamma, file = "Mat_gamma.RData");
load('output/Mat_gamma.RData');


# infection size plot
label  <-  c(expression( paste(italic(R[0]) ) ), expression( paste('1/', sigma) ), expression( paste('1/', gamma, ' ') ) );
ylim   =   c(1000,7000);
xlim   =   seq(-1.5, 1.5, 0.3);
r_list2    <-  list(r21 = Mat_R0, r22 = Mat_sigma, r23 = Mat_gamma);
data_temp  <-  r_list2;
r_mat2 = c();
for(i in 1:length(r_list2)){
  temp  <-  f_ggplot_comp(x = r_list2[[i]][,1], y = r_list2[[i]][,2], ylim = ylim);
  data_temp[[i]]  =   cbind(x1 = temp[,'x1'], y1 = temp[,'y1'], group = rep(i, dim(r_list2[[i]])[1])); 
  r_mat2 = rbind(r_mat2, data_temp[[i]]);
}

Col   =   c();
Label =   c();
for(i in 1:3){
  temp   =   sum(r_mat2[,'group']==i);
  Col    =   c(Col, rep(col[i+4], temp) );
  Label  =   c(Label, rep(label[i],temp));
}
names(Col)  =   Col;
data  <-  cbind(r_mat2, col = Col);
data_plot  <-  data.frame(data);
data_plot[,2]  =   as.numeric(data_plot[,2]);
data_plot[,1]  =   as.numeric(data_plot[,1]);

p24  <-  ggplot(data = data_plot, aes(x = x1, y = y1, group = group, color = col)) +
  scale_y_continuous( breaks = c(seq(ylim[1], ylim[2], 2000), ylim[2]), labels = c(seq(ylim[1], ylim[2], 2000), expression(''>=7000) ), limits = ylim ) + # 设置y轴刻度和标签
  labs(title = '(B4)') + 
  theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"),
        panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1)) + 
  scale_x_continuous( breaks = xlim, labels = round(xlim,2) ) + 
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + 
  theme( axis.text.y = element_text(size=size_axis,color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
  xlab('Change in parameters') + theme(axis.title.x=element_text(vjust = 1.8, size = size_axis_x,face = "plain")) + 
  ylab('Cum') + theme(axis.title.y=element_text(vjust = -1, size = size_axis_y, face = "plain")) + 
  geom_point(size = 3, aes(col = col)) +
  geom_line(size = 0.8, aes(col = col), linetype = 3) +
  scale_color_manual( values = Col, labels = Label ) + 
  theme( legend.position = c(0.12, 0.8), legend.title=element_blank(), legend.text = element_text(size = size_legend) ) + 
  theme( plot.margin = margin(mars)); 
####################### simulation 2 #######################



########################## functions and data used in the following ##########################

# daily number of people imported from Yanta into each region
pop_imp  <-  cal_tau(x_med['D'])[1,] * Nr[1];

# simulated results of importation time (stored in list Y)
load('output/ctmc_import_time.RData');
Y1  <-  Y;

# simulated results of number of imported cases (stored in list Y)
load('output/ctmc_import_number.RData');
Y2  <-  Y;

# function used in drawing the second y axis, input interval1 which is the original range and interval2 which is the transformed range, return the transformed data
trans_plot <- function(x, interval1, interval2){
  y  =   (interval2[2]-interval2[1])/(interval1[2]-interval1[1])*(x-interval1[1]) + interval2[1];
  return(y);
}

# quantiles of importation time when no control measures implemented
X0  <-  Y1$ctmc_0;
t_imp  <-  matrix(0, 3, n-1);
for(i in 1:dim(t_imp)[2]){
  temp       =   X0[,i+1];
  temp       =   temp[temp<Inf];
  t_imp[,i]  =   quantile(temp, c(0.1, 0.5, 0.9));
}

# probability of no infection importation under different Ub schemes 
p_noimp  <-  matrix(0, length(Y1), n-1, dimnames = list(names(Y1), 2:n) );
for(i in 1:dim(p_noimp)[1]){
  for(j in 1:dim(p_noimp)[2]){
    temp          <-  Y1[[i]][,j+1];
    p_noimp[i,j]  =   sum(temp==Inf)/length(temp);
  }
}

# median importation time if there is infection importation under different Ub schemes
T_imp  <-  matrix(0, length(Y1), n-1, dimnames = list(names(Y1), 2:n) );
for(i in 1:dim(T_imp)[1]){
  for(j in 1:dim(T_imp)[2]){
    temp        <-  Y1[[i]][,j+1];
    temp        =   temp[temp<Inf];
    T_imp[i,j]  =   ifelse(length(temp)==0, Inf, median(temp));
  }
}

# median number of imported cases under different Ub schemes
names2  <-  names(Y2);
N_imp  <-  matrix(0, length(Y2), n, dimnames = list(names2, district_all));
for(i in 1:length(Y2)){
  for(j in 1:n){
    temp  =   Y2[[i]][,j];
    N_imp[i,j]  =   mean(temp);
  }
}
N_imp  =   N_imp[,-1];

# plot parameters
col_deepblue   <-  rgb(3,57,108 , maxColorValue=255);  
col_midblue2   <-  brewer.pal(9,"Blues")[7]; 
col_midblue    <-  brewer.pal(9,"Blues")[6]; 
col_lightblue  <-  brewer.pal(9,"Blues")[5]; 
Col  <-  rev( c('black', col_deepblue, col_midblue2, col_midblue, col_lightblue) );
names(Col)     =   Col;

size_axis    <-  16;
size_axis_y  <-  18;
size_axis_x  <-  18;
size_legend  =   20;
size_title   =   30;

mars  <-  c(t=2, r=10, b=2, l=10);

########################## functions and data used in the following ##########################



########################## stochastic results ##########################
breaks1  <-  seq(0, 6000, by = 1000);
labels2  <-  c('0.20','0.25','0.30','0.35','0.40');
breaks2  <-  as.numeric(labels2);
breaks3  <-  trans_plot(breaks2, c(0.2,0.4), c(0,6000));

A1    <-  data.frame(x = district_plot[-1], y1 = pop_imp[-1], y2 = trans_plot(p_noimp['ctmc_0',], c(0.2,0.4), c(0,6000)), z = rep(1,n-1) );
A1$x  =   factor(A1$x, level = district_plot[-1]);
A1$z  =   factor(A1$z, level = 1 ); 

p_C1  <-  ggplot( data = A1 ) + 
  geom_bar( stat="identity", aes(x=x, y=y1, fill = z), position = position_dodge(0.7), width=0.6, show.legend = F) + 
  scale_fill_manual(values = col_midblue ) + 
  coord_cartesian(clip = 'off') + annotation_custom(grob = textGrob( expression(x10^3) ),  xmin = -12, ymin =6600 ) +
  scale_y_continuous( limits = c(0,6000), breaks = breaks1, labels = breaks1/1000, name = 'Daily population inflow'  ) + 
  theme( axis.title.x=element_blank(), axis.text.x = element_text(size = size_axis_x, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + 
  theme( axis.title.y.right = element_text(size = size_axis_y, color = 'black',vjust = 1, hjust = 0.5, angle = 270) ) + 
  theme( axis.title.y.left = element_text(size = size_axis_y, color = 'black',vjust = 1, hjust = 0.5, angle = 90) ) + 
  theme( axis.text.y = element_text(size = size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) +
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + 
  labs(title = '(C1)') + 
  theme( plot.title=element_text(size = size_title, vjust = 1.6, hjust = 0.5, face = "plain"),
         panel.background = element_rect(fill = "grey98"),
         panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
  theme( plot.margin = margin(mars));

A2   <-   data.frame(x = district_plot[-1], y = t_imp[2,], group1 = rep(1, n-1) );
A2$x =    factor(A2$x, level = district_plot[-1]);  
A22  <-  data.frame(x = c(district_plot[-1], district_plot[-1]), y = c(t_imp[1,], t_imp[3,]), group = c(1:(n-1),1:(n-1)));
A22$x =    factor(A22$x, level = district_plot[-1]);   

breaks  <-  c(seq(5, 22, 5), 22);

p_C2  <-  ggplot( ) + 
  geom_line(data = A22, aes(x=x, y=y , group = group), size = 0.8, col = col_midblue) +
  geom_point(data = A2, aes(x = x, y = y), col = col_deepblue, cex = 4 ) + 
  scale_y_continuous(breaks = breaks, labels = breaks, name = 'Importation time' ) +
  labs(title = '(C2)') + 
  theme( plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"),
         panel.background = element_rect(fill = "grey98"),
         panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
  theme( axis.title.x = element_blank(), axis.text.x = element_text(size = size_axis_x, color = 'black',vjust = 1, hjust = 1, angle = 45) ) +
  theme( axis.title.y = element_text(size = size_axis_y, color = 'black',vjust = 0.3, hjust = 0.5, angle = 90) ) + 
  theme( axis.text.y = element_text(size = size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) +
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + 
  theme( plot.margin = margin(c(0,25,0,10)));  

temp    <-  c(p_noimp['ctmc_0',], p_noimp['ctmc_1',], p_noimp['ctmc_n4',], p_noimp['ctmc_n8',], p_noimp['ctmc_n12',]);
A3      <-  data.frame(x = rep(pop_imp[-1], 5), y = 1-temp, col = rep(Col, each=n-1) );
labels  <-  c('no control', 'baseline  ', expression( paste(italic(d^b), '=-4     ')), expression(paste(italic(d^b), '=-8     ')), expression(paste(italic(d^b), '=-12   ')));

p_C3  <-  ggplot(data = A3 ) +
  geom_point( aes(x = x, y = y, col = col ), cex = 4) + 
  scale_color_manual( values = Col, labels = labels ) + 
  labs(title = '(C3)') + 
  theme( plot.title = element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"),
         panel.background = element_rect(fill = "grey98"),
         panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
  scale_y_continuous( breaks = seq(0, 1, 0.1), labels = c('0.0',seq(0.1, 0.9, 0.1),'1.0'), limits = c(0, 1.02), name = 'Probability of infection importation') + 
  scale_x_continuous( breaks = seq(0, 6000, 1000), labels = seq(0, 6000, 1000), name = 'Daily population inflow' ) +
  theme( axis.text.x = element_text(size=size_axis, color = 'black', vjust = 0.8, hjust = 1, angle = 45) ) +
  theme( axis.text.y = element_text(size=size_axis, color = 'black', vjust = 0.5, hjust = 1, angle = 0) ) +
  theme( axis.title.y = element_text(size = size_axis_y, color = 'black',vjust = 1, hjust = 0.5, angle = 90) ) +  
  theme( axis.title.x = element_text(vjust = 5, size = size_axis_x,face = "plain")) +     
  theme( legend.position = c(0.22, 0.84), legend.title = element_blank(), legend.text = element_text(size = size_legend), legend.background = element_rect(fill = rgb(1,1,1,alpha=0.1)) ) + 
  theme( plot.margin = margin(mars));  

temp  <-  c(N_imp['ctmc_num_1',], N_imp['ctmc_num_n4',], N_imp['ctmc_num_n8',], N_imp['ctmc_num_n12',]);
A4    <-  data.frame(x = rep(pop_imp[-1], 4), y = temp, col = rep(Col[2:5], each=n-1) );
labels  <-  c('baseline  ', expression( paste(italic(d^b), '=-4     ')), expression(paste(italic(d^b), '=-8     ')), expression(paste(italic(d^b), '=-12   ')));

p_C4  <-  ggplot(data = A4 ) + 
  geom_point( aes(x = x, y = y, col = col ), cex = 4) + 
  scale_color_manual( values = Col[2:5], labels = labels ) +
  labs(title = '(C4)') + 
  theme( plot.title = element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"),
         panel.background = element_rect(fill = "grey98"),
         panel.grid.major = element_line(colour = "grey100", size = 0.1)) + 
  scale_y_continuous( breaks = seq(0, 12, 3), labels = seq(0, 12, 3), name = 'Mean number of imported cases') +
  scale_x_continuous( breaks = seq(0, 6000, 1000), labels = seq(0, 6000, 1000), name = 'Daily population inflow' ) +
  theme( axis.text.x = element_text(size=size_axis, color = 'black', vjust = 0.8, hjust = 1, angle = 45) ) +
  theme( axis.text.y = element_text(size=size_axis, color = 'black', vjust = 0.5, hjust = 1, angle = 0) ) +
  theme( axis.title.y = element_text(size = size_axis_y, color = 'black',vjust = -7, hjust = 0.5, angle = 90) ) +
  theme( axis.title.x = element_text(size = size_axis_x, vjust = 5, face = "plain")) +
  theme( legend.position = c(0.22, 0.82), legend.title = element_blank(), legend.text = element_text(size = size_legend), legend.background = element_rect(fill = rgb(1,1,1,alpha=0.5)) ) + 
  theme( plot.margin = margin(mars));

########################## stochastic results ##########################



####################### integrated plot #######################

# 21 × 20
p.all1  <-  list();

p.all1[[1]]  =   p12;
p.all1[[2]]  =   p13;
p.all1[[3]]  =   p14;
p.all1[[4]]  =   p15;
p.all1[[5]]  =   p21;
p.all1[[6]]  =   p22;
p.all1[[7]]  =   p23;
p.all1[[8]]  =   p24;

p.all2  <-  list();

p.all2[[1]]  =   p_C1;
p.all2[[2]]  =   p_C2;
p.all2[[3]]  =   p_C3;
p.all2[[4]]  =   p_C4;

p.all  <-  c(p.all1, p.all2);

wrap_plots(p.all, ncol=4);

####################### integrated plot #######################



####################### control Ub plot in appendix #######################
out_0     <-  simu1(Control = Control_base, d = c(Ub = 0, Un = 0, Ul = 0));
out_n5    <-  simu1(Control = Control_base, d = c(Ub = -6, Un = 0, Ul = 0));
out_n12   <-  simu1(Control = Control_base, d = c(Ub = -12, Un = 0, Ul = 0));
out_list  <-  list(out_0, out_n5, out_n12);

col_temp  <-  c(brewer.pal(9,"YlOrRd")[5], brewer.pal(9,"YlGn")[4], brewer.pal(9,"YlGnBu")[5]);

# input region, return I(t) data frame of all above out matrixes
g1_temp <- function(region, out_list){
  
  y  =   c();
  x  =   c();
  z  =   c();
  l  =   c();
  
  for(i in 1:length(out_list)){
    temp1  =   out_list[[i]]$out;
    temp2  =   out_list[[i]]$Control$Un[,region];
    t_impul  <-  as.numeric(rownames(out_list[[i]]$Control$Un))[which(temp2>0)];
    y     =   c(y, apply(temp1[,paste0(c('I.m','I.s'),region)], 1, sum) );
    x     =   c(x, temp1[,'time']);
    z     =   c(z, group_out(time = temp1[,'time'], t_impul = t_impul) + ifelse(length(z)==0, 0, max(z)) );
    l     =   c(l, rep(i, dim(temp1)[1]));
  }
  
  return(data.frame(time = x, I = y, group = z, num = l));
}

# input dataframe composed of states in out_list, return figure, length of col should be the same as that of out_list
g2_temp <- function(dataframe, ylim, xlim, xlab, ylab, title = '', col, label, seq_y = 50, leg_pos = c(0.60, 0.80)){
  
  temp  =   which(dataframe[,'time'] <= xlim);
  dataframe  =   dataframe[temp,];
  
  len  =   length(unique(dataframe[,'num']));
  Col   =   c();
  Label =   c();
  for(i in 1:len){
    Col = c(Col, rep(col[i], sum(dataframe[,'num']==i)));
    Label = c(Label, rep(label[i],sum(dataframe[,'num']==i)));
  }
  names(Col)  =   Col;
  
  p  =   ggplot(data = dataframe, aes(x = time, y = I, group = group, color = Col)) + 
    labs(title = title) + 
    theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 24,face = "plain"),
          panel.background = element_rect(fill = "grey98"),
          panel.grid.major = element_line(colour = "grey100", size = 0.1)) +  
    scale_y_continuous( breaks = c(seq(0, ylim, seq_y), ylim), labels = c(seq(0, ylim, seq_y), ylim), limits = c(0,ylim) ) + 
    scale_x_continuous( breaks = c(seq(0, xlim, 5), xlim), labels = c(seq(0, xlim, 5), xlim) ) + 
    theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) +  
    theme( axis.text.y = element_text(size=size_axis,color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
    xlab(xlab) + theme(axis.title.x=element_text(vjust = 1.8, size = size_axis_x,face = "plain")) + 
    ylab(ylab) + theme(axis.title.y=element_text(vjust = 2, size = size_axis_y,face = "plain")) +  
    geom_line(size = 0.6, aes(col = Col)) +
    scale_linetype_manual( values = rep('solid',len) ) + 
    scale_color_manual( values = Col, labels = Label ) +
    theme( legend.position = leg_pos, legend.title=element_blank(), legend.text = element_text(size = size_legend) );
  
  return(p);
}


#
mt                  =    20;
mr                  =    30;
ml                  =    10;
mb                  =    0;
mrow                =    0;
mcol                =    6;

# top left figure
mar1                =    c(mt,mrow,0,ml);
# middle figure of the first row
mar2                =    c(mt*1,mrow,0,0);
# top right figure
mar3                =    c(mt,mr,0,0);
# bottom left figure
mar4                =    c(mcol,mrow,mb,ml);
# middle figure of the second row
mar5                =    c(mcol,mrow,mb,0);
# bottom right figure
mar6                =    c(mcol,mr,mb,0);

get_mar <- function(rn){
  if(rn==2){
    # figure with 2 rows
    mar = matrix(0,14,4);
    for(i in 1:14){
      if(i==1){
        temp = mar1;
      }else if(i==8){
        temp = mar4;
      }else if(i==7){
        temp = mar3;
      }else if(i==14){
        temp = mar6;
      }else if(i<7){
        temp = mar2;
      }else{
        temp = mar5;
      }
      mar[i,] = temp;
    }
  }else{
    # figure with 3 rows
    mar = matrix(0,15,4);
    for(i in 1:14){
      if(i==1){
        temp = mar1;
      }else if(i==11){
        temp = mar4;
      }else if(i==5){
        temp = mar3;
      }else if(i==15){
        temp = mar6;
      }else if(i<5){
        temp = mar2;
      }else{
        temp = mar5;
      }
      mar[i,] = temp;
    }
  }
  return(mar);
}

mar = get_mar(3);

p.fit  <-  list();
temp   =   c(250, rep(50,13), 400);
ff_temp <- function(i){
  seq_y  =   ifelse(temp[i]>50, 50, 10);
  dataframe  <-  g1_temp(region = district_all[i], out_list);
  lab  =   c('baseline', expression(paste(italic(d^b), '=-5   ')), expression(paste(italic(d^b), '=-12 ')));
  p    =   g2_temp(dataframe = dataframe, ylim = temp[i], xlim = 50, xlab = '', ylab = '', title = district_plot[i], col = col_temp, label = lab, seq_y = seq_y, leg_pos = c(0.70, 0.80));
  p    =   p + theme(plot.margin = unit(mar[i,], "pt"));
  return(p);
}

for(i in 1:14){
  p.fit[[i]] = ff_temp(i);
}

#
temp3  <-  f1_temp(Control = Control_base, list_d = f_Uj(Uj = 'Ub', num=c(0,-6,-12)) );
label  <-  c('baseline', expression(paste(italic(d^b), '=-6   ')), expression(paste(italic(d^b), '=-12 ')));
p.fit[[15]]  <-  f_plot(x = temp3$x, Y = temp3$Y, Z = temp3$Z, ylim = 350, xlim = 50, xlab = ' ', ylab = ' ', title = '', col = col_temp, label, leg_pos = c(0.75, 0.8)) + labs(title = 'Total') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"));

wrap_plots(p.fit, ncol=5);

####################### control Ub plot in appendix #######################
