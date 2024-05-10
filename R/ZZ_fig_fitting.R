
# horizontal-to-vertical proportion: 10 * 9 for 3 rows graph, 12 * 9 for 2 rows
source('R/main.R')
source('R/CI.R');
source('R/zero_constant.R');

# plot parameters
data_plot           <-   rbind(rep(0,14), unname(data2));
data_plot = cbind(data_plot, apply(data_plot,1,sum));
district_plot       <-   c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanliang', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian', 'Total');
colnames(data_plot) =    district_plot;

# data
df_data             <-   data.frame( cbind(time = 0:47, data_plot) );
df_data             =    cbind( df_data, Total = apply(df_data[,-1],1,sum) ); # whole city data

# fitting value
temp_Q          =     get.K_i(K = 'Q', out = out_actual);
temp            =     temp_Q[,-1];
time            =     temp_Q[, 1];
t.temp          =     length(unique(time)) - 1;
new             =     matrix(0, length(Time)-1, n);
colnames(new)   =     district_all;
rownames(new)   =     Time[-1];
new[1,]         =     temp[2,] - temp[1,];
for(t in 2:t.temp){
  temp.n  = temp[0:2+(t-1)*2,];
  new[t,] = (temp.n[3,] - temp.n[1,]);
}
df_fitt             <-   data.frame(date = period, time = 0:47);
sm_fitt             <-   data.frame(date = period, time = 0:47);
for(i in 1:15){
  weight = c(rep(2,17), rep(1,4), rep(1,27));
  if(i<15){
    df_fitt[,i+2] = c(0, new[,i] );
    sm_fitt[,i+2] = smooth.spline(0:47, df_fitt[,i+2], w = weight, df = 12)$y;
  }else{
    df_fitt[,i+2] = apply(df_fitt[,-c(1,2)], 1, sum);
    sm_fitt[,i+2] = smooth.spline(0:47, df_fitt[,i+2], w = weight, df = 12)$y;
  } 
}
names(df_fitt)[-(1:2)]  =   c(district_plot);
names(sm_fitt)[-(1:2)]  =   c(district_plot);

# choose color
display.brewer.all();
brewer.pal.info;

# color
col1  =   rgb(3,57,108 , maxColorValue=255);     # deep blue
col2  =   brewer.pal(9,"Blues")[7]; 
col3  =   brewer.pal(9,"Blues")[5]; 
col4  =   brewer.pal(9,"Blues")[3]; 
# col2 = rgb(30,130,180, maxColorValue=255);  # mid blue
# col3 = rgb(63,141,168, maxColorValue=255);  # light blue
# col4 = rgb(140,187,203, maxColorValue=255); # lighter blue
# col5 = rgb(150,150,150,maxColorValue=255);
# col6 = rgb(157,77,70,maxColorValue=255);

# region i
f_temp <- function(i, ylim, col.CI2 = col4, col.CI1 = col3, col.sm = col2, col.dot = col1){
  
  df.polygon <- data.frame(x=c(1:47,47:1, 1:47,47:1), y=c(sm_CI1[[i]][,1],rev(sm_CI1[[i]][,2]),sm_CI2[[i]][,1],rev(sm_CI2[[i]][,2])), 
                           group = c(rep('CI1',47*2),rep('CI2',47*2)) );
  df.polygon2 <- data.frame(x=c(1:47,47:1), y=c(sm_CI1[[i]][,1],rev(sm_CI1[[i]][,2])), 
                            group = c(rep('CI1',47*2)) );
  
  col.CI = c(col.CI1, col.CI2);
  
  temp =  ggplot() + ylim(c(0,ylim)) + 
    labs(title = district_plot[i]) + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 18,face = "plain"), 
                                           panel.background = element_rect(fill = "grey98"),
                                           panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
    scale_x_continuous( breaks = c(seq(0,47,by=6), 47), labels = substr(period[1+c(seq(0,47,by=6), 47)],6,10) ) +
    theme( axis.text.x = element_text(size=8, color = 'black',vjust = 1, hjust = 1, angle = 45) ) +
    theme( axis.text.y = element_text(size=12,color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) +
    xlab('') + theme(axis.title.x=element_text(vjust = 0, size = 0.1,face = "plain")) +
    ylab('') + theme(axis.title.y=element_text(vjust = 0, size = 0.1,face = "plain")) +
    geom_polygon(data = df.polygon, aes(x=x, y=y, fill = factor(group)), alpha = 1, show.legend = F) +
    geom_polygon(data = df.polygon2, aes(x=x, y=y, fill = factor(group)), alpha = 1, show.legend = F) +
    scale_fill_manual(values = col.CI) +
    geom_path(aes(x = sm_fitt$time, y = sm_fitt[,district_plot[i]]), color = col.sm, size = 0.5) +
    geom_point(data=NULL, aes(x = df_data$time, y = df_data[,district_plot[i]]), shape = 16, color = col.dot, size = 1.5);
  
  t_zero[15]  =   max(t_zero);
  if(t_zero[i] > 3){
    temp =  temp + geom_vline( xintercept = t_zero[i], col = brewer.pal(9,"Set3")[1], linetype = 2, cex = 0.4);
  }
  
  return(temp);
}

f_temp(1,ylim=250)

# margin of figures (top, bottom, left, right, internal column spacing)
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


# figure in the main text
p.fit  <-  list();
temp   =   c(rep(250,14), 400);
ff_temp <- function(i){
  p = f_temp(i,ylim=temp[i]) + theme(plot.margin = unit(mar[i,], "pt"));
  return(p);
}
for(i in 1:15){
  p.fit[[i]] = ff_temp(i);
}
wrap_plots(p.fit, ncol=5);


# figure in the appendix
p.fit  <-  list();
temp   =   c(250, 50, 50, 30, 30,  rep(15,4), rep(15,4), 50, 400);
ff_temp <- function(i){
  p = f_temp(i,ylim=temp[i]) + theme(plot.margin = unit(mar[i,], "pt"));
  return(p);
}
for(i in 1:15){
  p.fit[[i]] = ff_temp(i);
}
wrap_plots(p.fit, ncol=5);
