
# run 'main.R' first
source('R/main.R'); 

# get the optimal control scheme
load('output/SA_det_results_base.RData'); 

U_ocp  <-  results$I;
out    =   sol_ocp(Time = 0:max(as.numeric(rownames(U_ocp[[1]]))), pars = pars_base, control = U_ocp, Index_SA = Index_SA, alpha = config_det_base$alpha, fun_zero = fun_zero);

c1     =   which(substr(colnames(out),1,3) %in% 'I.m');
c2     =   which(substr(colnames(out),1,3) %in% 'I.s');
It     <-  out[,c1] + out[,c2];

# time when the number of I(t) approaches zero
zero_time1  <-  rep(-1,dim(It)[2]);
for(i in 1:dim(It)[2]){
  temp  =   out[which(It[,i] == 0), 1];
  zero_time1[i]  =   min(temp[temp>5]);
}
zero_time1  =   c(zero_time1, max(zero_time1));

# time when the number of new confirmed cases approaches zero
alpha  =   alpha_base;
c3     =   which(substr(colnames(out),1,9) %in% 'Cum_confi');
Ct     <-  out[,c3];
Ct     =   Ct;
C_new  <-  rbind(Ct[-1,] - Ct[-dim(Ct)[1],], rep(0,dim(Ct)[2]));
zero_time2  <-  rep(-1,dim(C_new)[2]);
for(i in 1:dim(C_new)[2]){
  temp1  =   which(dpois(0, lambda = C_new[,i]) < alpha);
  if(length(temp1) == 0){temp1 = 0}
  temp2  =   max(temp1) + 1;
  zero_time2[i]  =   out[temp2,1];
}
zero_time2  =   c(zero_time2, max(zero_time2));
zero_time2[zero_time2 == 0]  =   NA;

# plot parameters
size_title   =   20;
size_axis    =   16;
size_axis_y  <-  16;
size_axis_x  <-  8;
mars  <-  c(t=2, r=10, b=8, l=10);

t_start  =   as.Date('2021-12-04');
t_end    =   as.Date('2023-01-20');
ts       =   0;
te       =   as.numeric(t_end-t_start);
temp_t   <-  as.Date(ts:te,origin=t_start); 
temp_t   =   substr(temp_t, 6, 10);

# copy from 'ZZ_fig_simulation.R', input time of out matrix and impulsive time
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
  y[(min(which(time==t_impul[length(t_impul)]))+1):length(y)]  =   max(y) + 1;
  return(y);
}

# plot function, input region and plot parameters return ggplot variable
f_temp <- function(i, col, ylim = 150, xlim = 50, by){
  
  if(i < 15){
    data  <-  data.frame(x = out[,1], y = It[,i], group = group_out(time = out[,1], t_impul = as.numeric(rownames(U_ocp$Un)[which(U_ocp$Un[,i]>0)])) );
  }else{
    data  <-  data.frame(x = out[,1], y = apply(It, 1, sum), group = group_out(time = out[,1], t_impul = as.numeric(rownames(U_ocp$Un)[which(apply(U_ocp$Un,1,sum)>0)])) );
  }
  
  title  =   ifelse(i==15, 'Total', district_plot[i]);
  
  temp =  ggplot(data) + labs(title = title) + 
    geom_line(aes(x = x, y = y, group = group ), col = 1, size = 0.5) + 
    theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"), 
          panel.background = element_rect(fill = "grey98"),
          panel.grid.major = element_line(colour = "grey100", size = 0.1)) + # title
    scale_x_continuous( breaks = seq(0, xlim, by=5), labels = temp_t[1+seq(0, xlim, by=5)] ) + # scale and label of x-axis 
    scale_y_continuous( limits = c(0,ylim), breaks = c(seq(0, ylim, by = by),ylim), labels = c(seq(0, ylim, by = by),ylim) ) +
    theme( axis.text.x = element_text(size=size_axis_x, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + # size and location of label of x-axis
    theme( axis.text.y = element_text(size=size_axis_y, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + # size and location of label of y-axis
    xlab('') + theme(axis.title.x=element_text(vjust = 0, size = size_axis_x,face = "plain")) + # title of x-axis
    ylab('') + theme(axis.title.y=element_text(vjust = 0, size = size_axis_y,face = "plain")) + # title of y-axis
    geom_vline( xintercept = zero_time1[i], col = 1, linetype = 2, cex = 0.4) +
    geom_vline( xintercept = zero_time2[i], col = 3, linetype = 2, cex = 0.4);
  # geom_point( aes(x = unique(X), y = temp_C), shape = 16, color = col[2], size = 1.7) + 
  
  return(temp);
}

# margin of figure
mt                  =    20;
mr                  =    30;
ml                  =    10;
mb                  =    0;
mrow                =    0;
mcol                =    6;

mar1                =    c(mt,mrow,0,ml);
mar2                =    c(mt*2,mrow,0,0);
mar3                =    c(mt,mr,0,0);
mar4                =    c(mcol,mrow,mb,ml);
mar5                =    c(mcol,mrow,mb,0);
mar6                =    c(mcol,mr,mb,0);

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

district_plot  <-  c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanliang', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian');
temp = c(75, rep(75,13), 150);
p.fit = list();
i = 1;
p = f_temp(i, col = 1, ylim = 80, xlim = 50, by = 20) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 2;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 3;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"))# + labs(tag='(A)') + coord_cartesian(clip = 'off') + theme( plot.tag = element_text(size=28), plot.tag.position=c(0.62, 1.16) );
p.fit[[i]] = p;
i = 4;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 5;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 6;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 7;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 8;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 9;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 10;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 11;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 12;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 13;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 14;
p = f_temp(i, col = 1, ylim = 15, xlim = 50, by = 3) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;
i = 15;
p = f_temp(i, col = 1, ylim = 150, xlim = 50, by = 30) + theme(plot.margin = unit(mar[i,], "pt"));
p.fit[[i]] = p;

# saved in the size of 10 * 9
wrap_plots(p.fit, ncol = 5);
