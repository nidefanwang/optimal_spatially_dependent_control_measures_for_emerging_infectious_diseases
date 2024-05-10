
########################## Drawing parameters ##########################
source('R/main.R');

library(ggplot2);
library(patchwork);
library(grid);

size_axis    <-  16;
size_axis_y  <-  18;
size_axis_x  <-  18;
size_legend  =   20;
size_title   =   30;

display.brewer.all(); brewer.pal.info;
col0  =   brewer.pal(3,"Set2")[1];

mars  <-  c(t=3, r=10, b=3, l=10);

# set colors
display.brewer.all();
brewer.pal.info;
col0  =   brewer.pal(8,"Set2")[6];
col1  =   brewer.pal(9,"OrRd")[6]; 
col2  =   brewer.pal(9,"YlOrRd")[5]; 
col3  =   brewer.pal(9,"YlGn")[5]; 
col4  =   brewer.pal(9,"YlGnBu")[5];
col5  =   brewer.pal(9,"Set3")[4];
col6  =   brewer.pal(9,"Set3")[1];
col7  =   brewer.pal(11,"PiYG")[8];
col8  =   brewer.pal(10,"Set3")[10];
col   =   c(col0, col1, col2, col3, col4, col1, col5, col6, col7, col8);
plot(1:length(col),col=col); 
points(1:length(col),cex=4,pch=19,col=col);

district_plot  <-  c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanliang', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian');

########################## Drawing parameters ##########################



########################## picture 1: optimal and actual cost ##########################

# function: calculate cost by control scheme matrix
cal_cost_temp <- function(U){
  
  cost1         <-  matrix(0, 3, n, dimnames = list(c('Un','Ub','Ul'),c()));
  cost1['Ul',]  =   GDP_all/365;
  cost1['Ub',]  =   cost1['Ul',]/20;  
  cost1['Un',]  =   Nr*10;
  
  y  =   t(U$Ul)*cost1['Ul',] + t(U$Ub)*cost1['Ub',] + t(U$Un)*cost1['Un',];
  y  =   apply(y,1,sum);
  
  return( y );
}

# optimal control cost for regions
load('output/SA_det_results_base.RData'); 
cost_opt  <-  cal_cost_temp(results$I);
cost_act  <-  cal_cost_temp(Control);
reduc     <-  (cost_act - cost_opt) / cost_act;

# data for bar graph
y     <-  c(cost_opt, cost_act)/1e8;
ylab  <-  y / (GDP_all/1e8)*100; 
A1    <-  data.frame(x = rep(district_plot,2), z = c(rep('cost (optimal)',n), rep('cost (actual)',n)), y = y );
A1$x  =   factor(A1$x, level = district_plot);
A1$z  =   factor(A1$z, level = c('cost (optimal)','cost (actual)') );
A1    <-  data.frame(A1, label = round(ylab,2), reduc = rep(reduc,2), group = rep(1, n*2));

# subfigure 1 
p1  <-  ggplot(A1) +
  geom_bar( stat="identity", aes(x=x, y=y, fill=z), position = position_dodge(0.7), width=0.6) + 
  geom_text( aes(x=x, y=y, label = label ), position = position_dodge(0.9), vjust = -0.8) + 
  scale_fill_manual(values=c( brewer.pal(9,"Blues")[6], brewer.pal(9,"Oranges")[5] ) ) + 
  # geom_point( aes(x = factor(x), y = reduc*220), col = col1, cex = 3 ) + 
  # geom_line( aes(x = factor(x), y = reduc*220, group = group ), col = col1, cex = 0.7, linetype = 2 ) + 
  theme( axis.title.x=element_blank(), axis.text.x = element_text(size=size_axis, color = 'black',vjust = 1, hjust = 1, angle = 45) ) +  
  theme( axis.text.y = element_text(size=size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0), axis.title.y = element_text(vjust = 2, size=size_axis_y) ) + 
  theme( legend.position = c(0.80, 0.85), legend.title=element_blank(), legend.text=element_text(size=size_legend), legend.background = element_rect(fill = rgb(1,1,1,alpha=0.5)) ) + 
  theme( plot.margin = margin(mars)) + 
  labs(title = '(A)') + 
  theme( plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title,face = "plain"), 
         panel.background = element_rect(fill = "grey98"),
         panel.grid.major = element_line(colour = "grey100", size = 0.1)) +  
  coord_cartesian(clip = 'off') + 
  annotation_custom(grob = textGrob( expression(x10^8~~yuan) ),  xmin = -12,ymin =245) +  
  scale_y_continuous( limits = c(0,220), breaks = seq(0,220,by=30), labels = seq(0,220,by=30), name = 'Cost');
  # scale_y_continuous( limits = c(0,220), breaks = seq(0,220,by=30), labels = seq(0,220,by=30), name = 'Cost', sec.axis = sec_axis(~./220, name = 'Reduction in cost', breaks = seq(0,1,by = 0.2) ) ) ;  

########################## picture 1: optimal and actual cost ##########################



########################## picture 2: correlation factors of cost ##########################

x   <-  cost_opt/GDP_all*100;
y1  <-  GDP_all/1e10;
y2  <-  pars_base$beta/gamma_base;

X    <-  cbind(x, y1, y2);
X    =   X[-1,];
ord  <-  order(X[,1]); 
X    =   X[ord,];
X    =   X[-c(1:4),];

Col  <-  rep(c(col4, brewer.pal(4,"Set3")[4]), each = 9); 
names(Col) = Col;

lab  =   c('cost (optimal)', 'GDP', 'R0');
colnames(X) = lab;

X[,'R0'] = (X[,'R0'] - 5.5)/1.5*15;

x_plot  =   factor(rep(X[,1], dim(X)[2]-1), sort(X[,1])); 
A_p2  <-  data.frame(x = rep(X[,1], 2), y = c(X[,-1]), col = c(rep('GDP',9), rep('R0',9)) );

# subfigure 2 
p2  <-  ggplot(data = A_p2) + 
  geom_point( aes(x = x, y = y, col = col, shape = col), cex = 6 ) +  
  scale_x_continuous( limits = c(1.5, 4.1), breaks = seq(1.5, 4, 0.5), labels = seq(1.5, 4, 0.5) ) + 
  scale_y_continuous( limits = c(0,15), breaks = seq(0, 15, by = 3), labels = seq(0, 15, by = 3), name = 'GDP', sec.axis = sec_axis(~., name = 'R0', breaks = (seq(5.5,7,by = 0.3) -5.5)*10, labels = seq(5.5,7,by = 0.3) ) ) +  
  theme( axis.text.x = element_text(size = size_axis, color = 'black',vjust = 0, hjust = 0.65, angle = 0) ) + 
  theme( axis.text.y = element_text(size = size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
  xlab('Optimal cost (%)') + theme(axis.title.x=element_text(vjust = 10, size = size_axis_x, face = "plain")) +  
  theme(axis.title.y=element_text(vjust = -1, size = size_axis_y,face = "plain")) +  
  theme( legend.position = c(0.14, 0.85), legend.title=element_blank(), legend.text=element_text(size=size_legend), legend.background = element_rect(fill = rgb(1,1,1,alpha=0.5)) ) + 
  labs(title = '(B)') + 
  theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title, face = "plain"), 
        panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1)) + 
  coord_cartesian(clip = 'off') + 
  annotation_custom(grob = textGrob( expression(x10^8~~yuan) ),  xmin = -1.1, ymin =16.8);

########################## picture 2: correlation factors of cost ##########################



########################## picture 3 - 4: regression of optimal cost ##########################

# data for broken line graph
tau   <-  cal_tau(x_med['D']);
x1    =   GDP_per;
x2    =   GDP_all;
x3    =   Nr;
x4    =   dist[,1];
x5    =   apply(Nr*tau, 2, sum);
x6    =   pars_base$beta/gamma_base;
x7    =   tau[1,] * Nr[1];
A2    <-  data.frame(region = district_plot, cost = cost_opt/GDP_all*100, GDP_per = x1, GDP = x2, Nr = x3, dist_Yanta = x4, pop_inflow = x5, R0 = x6, inflow_Yanta = x7);

# remove part of sample
rm_num  <-  c(1, 10:13);

# regression 1: cost ~ R0
temp1   <-  A2[-rm_num,'R0'];
order1  <-  order(temp1);
var1    <-  temp1[order1];
y1      <-  (A2[-rm_num,'cost'])[order1];
lm1     <-  lm( y1 ~ var1 );
newdat1 <-  data.frame(var1 = seq(5.5,7,0.1));
fit1    <-  predict( lm1, newdata = newdat1, interval = c("confidence") );

# regression 2: cost ~ GDP
temp2   <-  A2[-rm_num,'GDP'];
order2  <-  order(temp2);
var2    <-  temp2[order2];
y2      <-  (A2[-rm_num,'cost'])[order2];
lm2     <-  lm( y2 ~ var2 );
newdat2 <-  data.frame(var2 = seq(100,1400,100)*1e8);
fit2    <-  predict( lm2, newdata = newdat2, interval = c("confidence") );

# regression 3: R0 ~ GDP
temp3   <-  A2[-rm_num,'GDP'];
order2  <-  order(temp3);
var3    <-  temp2[order3];
y3      <-  (A2[-rm_num,'R0'])[order3];
lm3     <-  lm( y3 ~ var3 );
newdat3 <-  data.frame(var3 = seq(100,1400,100)*1e8);
fit3    <-  predict( lm3, newdata = newdat3, interval = c("confidence") );

# subfigure 3 
p3  <-  ggplot() +  
  geom_point( aes(x = var2, y = y2), shape = 16, color = col[4], size = 4) + 
  geom_point( aes(x = A2[10:13, 'GDP'], y = A2[10:13, 'cost']), shape = 17, color = col[5], size = 4) + 
  geom_path( aes(x = newdat2[,1], y = fit2[,1]), color = col[4], size = 1.1) + # fitting
  geom_path( aes(x = newdat2[,1], y = fit2[,2]), color = col[4], size = 0.9, linetype = "dashed") + # CI_low
  geom_path( aes(x = newdat2[,1], y = fit2[,3]), color = col[4], size = 0.9, linetype = "dashed") + # CI_upp
  scale_x_continuous( limits = c(100,1400)*1e8, breaks = seq(100, 1400, by = 100)*1e8, labels = seq(100, 1400, by = 100) ) + 
  scale_y_continuous( limits = c(0,5), breaks = seq(0, 5, by = 1), labels = seq(0, 5, by = 1) ) + 
  theme( axis.text.x = element_text(size = size_axis, color = 'black',vjust = 0.9, hjust = 1, angle = 45) ) + 
  theme( axis.text.y = element_text(size = size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
  xlab('GDP (100 million yuan)') + theme(axis.title.x=element_text(vjust = 0.2, size = size_axis_x, face = "plain")) +  
  ylab('Relative Cost (%)') + theme(axis.title.y=element_text(vjust = 3, size = size_axis_y,face = "plain")) +  
  labs(title = '(C)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title, face = "plain"), 
                              panel.background = element_rect(fill = "grey98"),
                              panel.grid.major = element_line(colour = "grey100", size = 0.1));

# add region names
y_plot  <-  (cost_opt/GDP_all*100)[-1] + 0.06;
y_plot[12]  =   y_plot[12] + 0.18;
y_plot[11]  =   y_plot[11] - 0.01;
x_plot  =   x2[-1];
lab1    =   district_plot[-1];
x_plot[c('Zhouzhi')]  =  x_plot[c('Zhouzhi')] - 15e8;
x_plot[c('Lantian')]  =  x_plot[c('Lantian')] + 5e8;
x_plot[c('Lintong')]  =   x_plot[c('Lintong')] + 30e8;
x_plot[c('Gaoling')]  =   x_plot[c('Gaoling')] + 70e8;
x_plot1  =   x_plot;
y_plot1  =   y_plot;
p3  =   p3 + theme( plot.margin = margin(mars)) + geom_text(aes(x = x_plot1, y = y_plot1, label = lab1), vjust = -0.5, size = 5);


########################## picture 3 - 4: regression of optimal cost ##########################



########################## picture 5: regression of cumulative inections ##########################

# calculate number of infected people from out matrix
merge <- function(out){
  
  temp      =   sapply(colnames(out), function(x){substr(x,start=1,stop=3)}); 
  temp1     =   which(temp == 'E.m');
  temp2     =   which(temp == 'E.s'); 
  temp3     =   which(temp == 'I.m');
  temp4     =   which(temp == 'I.s'); 
  out_E     <-  out[,temp1] + out[,temp2]; 
  out_I     <-  out[,temp3] + out[,temp4]; 
  
  return( out_E + out_I );
}

# solution under optimal control
out  <-  sol_ocp(Time = 0:50, pars = pars_base, control = results$I, Index_SA = Index_SA, alpha = config_det_base$alpha, fun_zero = fun_zero);

# regions without infection importation
temp      <-  apply(merge(out), 2, max);
index_im  <-  {dpois(0, lambda = temp) < config_det_base$alpha};

# regions with Rt less than 1 when only precise control in place
p_s      <-  pars_base$p_s;
p_m      <-  1 - p_s;
beta     <-  pars_base$beta;
sigma    <-  sigma_base;
lam_E    <-  pars_base$lambda_E;
lam_I    <-  pars_base$lambda_I;
delta    <-  pars_base$delta;
gamma_m  <-  gamma_base;
gamma_s  <-  pars_base$gamma_s;
Rt       <-  p_m*beta*sigma/((sigma + lam_E)*(gamma_m+delta+lam_I)) + p_s*beta*sigma/((sigma+lam_E)*(gamma_s+delta+lam_I));

# final size
nn   <-  which(substr(colnames(out), 1, 9) == 'Cum_infec');
cum  <-  out[dim(out)[1],nn];

# remove part of sample
rm_num  <-  c(1, 10:13);

# regression 3
temp3   <-  A2[-rm_num,'GDP'];
order3  <-  order(temp3);
var3    <-  temp3[order3];
y3      <-  cum[-rm_num][order3];
lm3     <-  lm( y3 ~ var3 );
newdat3 <-  data.frame( var3 = seq(100,1400,100)*1e8 );
fit3    <-  predict( lm3, newdata = newdat3, interval = c("confidence") );

# subfigure 5 
p4  <-  ggplot() +  
  geom_point( aes(x = var3, y = y3), shape = 16, color = col[4], size = 4) + 
  geom_point( aes(x = A2[10:13, 'GDP'], y = cum[10:13]), shape = 17, color = col[5], size = 4) + 
  geom_path( aes(x = newdat3[,1], y = fit3[,1]), color = col[4], size = 1.1) + # fitting
  geom_path( aes(x = newdat3[,1], y = fit3[,2]), color = col[4], size = 0.9, linetype = "dashed") + # CI_low
  geom_path( aes(x = newdat3[,1], y = fit3[,3]), color = col[4], size = 0.9, linetype = "dashed") + # CI_upp
  scale_x_continuous( limits = c(100,1400)*1e8, breaks = seq(100, 1400, by = 100)*1e8, labels = seq(100, 1400, by = 100) ) + 
  scale_y_continuous( limits = c(0,200), breaks = seq(0, 200, by = 50), labels = seq(0, 200, by = 50) ) + 
  theme( axis.text.x = element_text(size = size_axis, color = 'black',vjust = 0.9, hjust = 1, angle = 45) ) + 
  theme( axis.text.y = element_text(size = size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
  xlab('GDP (100 million yuan)') + theme(axis.title.x=element_text(vjust = 0.2, size = size_axis_x, face = "plain")) +  
  ylab('Final size') + theme(axis.title.y=element_text(vjust = 3, size = size_axis_y,face = "plain")) +  
  labs(title = '(D)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = size_title, face = "plain"), 
                              panel.background = element_rect(fill = "grey98"),
                              panel.grid.major = element_line(colour = "grey100", size = 0.1));

# add region names
y_plot3  <-  cum + 2;
x_plot3  <-  A2[,'GDP'];
y_plot3[which(district_all %in% c('Lantian','Xincheng'))]  =  y_plot3[which(district_all %in% c('Lantian','Xincheng'))] + 5;
x_plot3[which(district_all == 'Lantian')]  =  x_plot3[which(district_all == 'Lantian')] + 20e8;
x_plot3[which(district_all == 'Gaoling')]  =  x_plot3[which(district_all == 'Gaoling')] + 90e8;
x_plot3[which(district_all == 'Lintong')]  =  x_plot3[which(district_all == 'Lintong')] + 50e8;
x_plot3[which(district_all == 'Zhouzhi')]  =  x_plot3[which(district_all == 'Zhouzhi')] - 0;
lab3  <-  district_plot;
p4  =  p4 + theme( plot.margin = margin(mars)) + geom_text(aes(x = x_plot3, y = y_plot3, label = lab3), vjust = -0.5, size = 5);

########################## picture 5: regression of cumulative inections ##########################


# save in the size of 15*15
pp  <-  list();
pp[[1]]  =   p1;
pp[[2]]  =   p2;
pp[[3]]  =   p3;
pp[[4]]  =   p4;
wrap_plots(pp, ncol = 2);
