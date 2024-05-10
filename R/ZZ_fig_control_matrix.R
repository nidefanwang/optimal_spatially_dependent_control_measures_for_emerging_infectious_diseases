
# run main.R first
source('R/main.R');

########################## some parameters and data ##########################

# actual control matrix in Xi'an epidemic
Ul              =    matrix(0,length(period)-1,n);
rownames(Ul)    =    1:te - 1; 
colnames(Ul)    =    district_all;
temp0           =    which(period=='2021-12-22'): dim(Ul)[1];
Ul[temp0, c('Yanta', 'Changan', 'Beilin', 'Xixian')] = 1;
temp1           =    which(period=='2021-12-22'): which(period=='2022-01-13');
Ul[temp1, c('Lianhu')] = 1;
temp2           =    which(period=='2021-12-22'): which(period=='2022-01-14');
Ul[temp2, c('Weiyang')] = 1;
temp3           =    which(period=='2021-12-22'): which(period=='2022-01-17');
Ul[temp3, c('Baqiao', 'Xincheng')] = 1;
temp4           =    which(period=='2021-12-22'): which(period=='2022-01-12');
Ul[temp4, c('Yanlaing', 'Huyi')] = 1;
temp5           =    which(period=='2021-12-22'): which(period=='2022-01-11');
Ul[temp5, c('Lintong', 'Gaoling', 'Zhouzhi', 'Lantian')] = 1;

Up              =    Ul
temp            =    which(period=='2021-12-09'): which(period=='2021-12-22');
Up[temp,]       =    1;

Un              =    matrix(0,length(period)-1,n);
rownames(Un)    =    1:te - 1; 
colnames(Un)    =    district_all;
temp            =    seq(which(period=='2021-12-19'), length(period)-1, by = 2);
Un[temp,]       =    1;
temp2           =    seq(which(period=='2021-12-27'), length(period)-1, by = 1);
Un[temp2,1:3]   =    1;
temp3           =    which(period=='2021-12-17');
Un[temp3,1]     =    1;
temp1           =    which(period=='2022-01-14'): dim(Un)[1];
Un[temp1, c('Lianhu')] = 0;
temp2           =    which(period=='2022-01-15'): dim(Un)[1];
Un[temp2, c('Weiyang')] = 0;
temp3           =    which(period=='2022-01-18'): dim(Un)[1];
Un[temp3, c('Baqiao', 'Xincheng')] = 0;
temp4           =    which(period=='2022-01-13'): dim(Un)[1];
Un[temp4, c('Yanlaing', 'Huyi')] = 0;
temp5           =    which(period=='2022-01-12'): dim(Un)[1];
Un[temp5, c('Lintong', 'Gaoling', 'Zhouzhi', 'Lantian')] = 0;

Ub              =    Ul;

Ud              =    Up;

# control variables
Control0        =    list(Ub = Ub, Ul = Ul, Up = Up, Un = Un, Ud = Ud);

# function: calculate cost by control scheme matrix, input the list of schemes of three control measures
cal_cost_temp <- function(U){
  
  cost1         <-  matrix(0, 3, n, dimnames = list(c('Un','Ub','Ul'),c()));
  cost1['Ul',]  =   GDP_all/365;
  cost1['Ub',]  =   cost1['Ul',]/20;  
  cost1['Un',]  =   Nr*10;
  
  y  =   t(U$Ul)*cost1['Ul',] + t(U$Ub)*cost1['Ub',] + t(U$Un)*cost1['Un',];
  y  =   apply(y,1,sum);
  
  return( y );
}

########################## some parameters and data ##########################



########################## plot function ##########################

# input list Control and matrix to plot, return figure
f_temp <- function(A,title){
  
  col0 = rgb(213,212,253, maxColorValue=255);
  col1 = rgb(234,178,175, maxColorValue=255);
  
  B  <-  data.frame(district = rep(colnames(A),times=rep(dim(A)[1],dim(A)[2])), date = rep(rownames(A),dim(A)[2]), value = c(A) );
  B$district = factor(B$district, level = rev(colnames(A)) );
  B$date = factor(B$date, level = rownames(A) );
  
  temp = unique(as.character(B$date));
  temp[-c(seq(1, length(temp), 3), length(temp))] = '';
  x_labels <- temp;
  
  p  <-  ggplot(B) +  
    geom_tile(aes(x = date, y = district, fill=value), colour = "white") + 
    scale_fill_gradient( low = 'grey98', high = col1) + # color
    labs( title = title) + theme( plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 18,face = "plain")) + # title
    theme( axis.ticks = element_blank(), panel.background = element_blank()) + # axis
    theme( axis.text.x = element_text(size = 8,color = 'black',angle=36, vjust = 1.2, hjust = 1.00), axis.text.y = element_text(size = 10,color = 'black',hjust = 1) ) + # axis
    theme( legend.position = "none" ) + # remove legend
    scale_x_discrete( labels = x_labels, name = NULL ) + scale_y_discrete( name = NULL ); # remove axis label
  
  return(p);
}

########################## plot function ##########################



########################## actual control plot ##########################

# control matrix
district_plot       <-   c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanliang', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian');

# date, row names indicates 0-24h of the day
temp_ts  <-  as.Date('2021-12-05');
temp_te  <-  as.Date('2022-01-24');
temp_ns  <-  0;
temp_ne  <-  as.numeric(temp_te-temp_ts);
temp_p   <-  as.character( as.Date(temp_ns:temp_ne,origin=temp_ts) );

A1   <-   Control0$Un;
colnames(A1) = district_plot;
rownames(A1) = as.character(temp_p[1:dim(A1)[1]]);
A2   <-   Control0$Ub;
colnames(A2) = district_plot;
rownames(A2) = as.character(temp_p[1:dim(A2)[1]]);
A3   <-   Control0$Ul;
colnames(A3) = district_plot;
rownames(A3) = as.character(temp_p[1:dim(A3)[1]]);
A4   <-   Control0$Up;
colnames(A4) = district_plot;
rownames(A4) = as.character(temp_p[1:dim(A4)[1]]);

# complete
temp3  <-  matrix(0, 4, n, dimnames = list(c('2022-01-21', '2022-01-22', '2022-01-23', '2022-01-24'), colnames(A1)));
temp3[1:3,'Yanta']  =   1;
temp3[1:2,'Changan']  =   1;
temp3[1:4,'Xixian']  =   1;
temp2  <-  temp3;
temp1  <-  temp3*0;
temp1[,'Yanta']    =   temp3[,'Yanta'];
temp1[,'Changan']  =   temp3[,'Changan'];
temp1[,'Xixian']   =   c(1,0,1,0);

A1  =   rbind(A1, temp1);
A2  =   rbind(A2, temp2);
A3  =   rbind(A3, temp3);

# transmission was discovered at 24h of 12-09 24h 
A1  =   A1[-(1:5),];
A2  =   A2[-(1:5),];
A3  =   A3[-(1:5),];

# 
pp1  <-  list();
pp1[[1]]  =   f_temp(A3,'lockdown'); 
pp1[[2]]  =   f_temp(A2,'border closure'); 
pp1[[3]]  =   f_temp(A1,'screening'); 
wrap_plots(pp1, ncol = 3);

########################## actual control plot ##########################



########################## optmial control plot ##########################

load('output/SA_det_results_base.RData'); 
temp  <-  results$I;
for(i in 1:length(temp)){
  colnames(temp[[i]]) = district_plot;
}
obj  <-  results$Obj_real;

temp_n   <-  (1:5);
temp_Ul  <-  temp$Ul[-temp_n,];  rownames(temp_Ul)  =   temp_p[-temp_n];
temp_Ub  <-  temp$Ub[-temp_n,];  rownames(temp_Ub)  =   temp_p[-temp_n];
temp_Un  <-  temp$Un[-temp_n,];  rownames(temp_Un)  =   temp_p[-temp_n];
U_ocp    <-  list(Ul = temp_Ul, Ub = temp_Ub, Un = temp_Un);

pp2  <-  list();
pp2[[1]]  =   f_temp(U_ocp$Ul, 'lockdown');
pp2[[2]]  =   f_temp(U_ocp$Ub, 'border closure');
pp2[[3]]  =   f_temp(U_ocp$Un, 'screening');
wrap_plots(pp2, ncol = 3);

# 9 × 12
pp  <-  c(pp1, pp2);
wrap_plots(pp, ncol = 3);

########################## optimal control plot ##########################
