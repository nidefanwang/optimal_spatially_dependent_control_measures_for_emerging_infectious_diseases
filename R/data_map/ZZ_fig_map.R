

rm(list = ls())
library(ggplot2)
library('RColorBrewer'); 


# function to add a subplot for a region by inputting dataframe 'fun_data' with 1st, 2nd cols be lon, lat
f_map <- function(fun_fig, fun_data, fun_col="lightblue", fun_alpha=0.5, fun_size=2.8){
  fun_resul = fun_fig + geom_polygon(data = fun_data, aes(x = lon, y = lat), fill = fun_col, alpha = fun_alpha) + 
              geom_path(data = fun_data, aes(x = lon, y = lat), color = "white", size = fun_size);
  return(fun_resul);
}


# function to get dataframe for the above function
f_dat <- function(fun_region){
  fun_raw_dat = read.csv(paste0(fun_region, '.csv'), header=FALSE);
  fun_dat <- data.frame(lon=fun_raw_dat[,1], lat=fun_raw_dat[,2]);
  fun_resul <- rbind(fun_dat, fun_dat[1, ]);
  return(fun_resul);
}


# coordinates of regions
dat_yanta = f_dat('yanta');
dat_beilin = f_dat('beilin');
dat_lianhu = f_dat('lianhu');
dat_xincheng = f_dat('xincheng');
dat_weiyang = f_dat('weiyang');
dat_baqiao = f_dat('baqiao');
dat_gaoling = f_dat('gaoling');
dat_yanliang = f_dat('yanliang');
dat_lintong = f_dat('lintong');
dat_lantian = f_dat('lantian');
dat_huyi = f_dat('huyi');
dat_zhouzhi = f_dat('zhouzhi');
dat_changan = f_dat('changan');


# process data of xixian
xixian_raw_dat = read.csv('xixian_raw_trim.csv', header=FALSE);
xixian_raw_dat[,1] = xixian_raw_dat[,1];
xixian_raw_dat[,2] = -xixian_raw_dat[,2];
x1 = 608; y1 = -590;
x2 = 591; y2 = -348;
x1_p = 108.865956; y1_p = 34.275611;
x2_p = 108.854989; y2_p = 34.397467;
k1 = (x1-x2)/(x1_p-x2_p); k2 = x1_p-x1/k1;
l1 = (y1-y2)/(y1_p-y2_p); l2 = y1_p-y1/l1;
xixian_dat = xixian_raw_dat
for(i in 1:dim(xixian_raw_dat)[1]){
  xixian_dat[i,1] = xixian_raw_dat[i,1]/k1+k2;
  xixian_dat[i,2] = xixian_raw_dat[i,2]/l1+l2;
}
dat_xixian <- data.frame(lon=xixian_dat[,1], lat=xixian_dat[,2]);


# Xi'an map
fig_map = ggplot() + theme_minimal();

fig_map = f_map(fun_fig=fig_map, fun_data=dat_yanta, fun_col="lightblue", fun_alpha=0.8);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_beilin, fun_col="lightblue", fun_alpha=0.6);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_lianhu, fun_col="lightblue", fun_alpha=0.7);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_xincheng, fun_col="lightblue", fun_alpha=0.96);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_weiyang, fun_col="lightblue", fun_alpha=0.8);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_baqiao, fun_col="lightblue", fun_alpha=0.96);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_gaoling, fun_col="lightblue", fun_alpha=0.8);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_yanliang, fun_col="lightblue", fun_alpha=0.6);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_lintong, fun_col="lightblue", fun_alpha=0.8);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_lantian, fun_col="lightblue", fun_alpha=0.8);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_huyi, fun_col="lightblue", fun_alpha=0.6);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_changan, fun_col="lightblue", fun_alpha=0.7,fun_size=2.5);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_zhouzhi, fun_col="lightblue", fun_alpha=0.9);
fig_map = f_map(fun_fig=fig_map, fun_data=dat_xixian, fun_col="lightblue", fun_alpha=1);


# regional governments
gov_loc = matrix(0, 14, 2);
gov_loc[1,] = c(108.954816,34.228829-0.012);  # yanta
gov_loc[2,] = c(108.913221,34.164631-0.02);  # changan
gov_loc[3,] = c(108.95039-0.026,34.271537+0.005);   # lianhu
gov_loc[4,] = c(108.946972,34.263032-0.015);  # beilin
gov_loc[5,] = c(108.953122,34.299165+0.02);  # weiyang
gov_loc[6,] = c(109.070984,34.279599);  # baqiao
gov_loc[7,] = c(108.967219,34.272934+0.011);  # xincheng
gov_loc[8,] = c(109.232576,34.668381);  # yanliang
gov_loc[9,] = c(108.611425,34.115018);  # huyi
gov_loc[10,] = c(109.220701,34.372988); # lintong
gov_loc[11,] = c(109.094683,34.541315-0.02); # gaoling
gov_loc[12,] = c(108.228573,34.169839); # zhouzhi
gov_loc[13,] = c(109.330006,34.157866); # lantian
gov_loc[14,] = c(108.787556+0.05,34.339317+0.01); # xixian

f_gov <- function(fun_fig, fun_i){
  fun_resul = fun_fig + geom_point(aes(x=gov_loc[fun_i,1], y=gov_loc[fun_i,2]), color = rgb(205,75,63, maxColorValue=255), alpha = 0.7, size = 7, pch=20) +
    geom_point(aes(x=gov_loc[fun_i,1], y=gov_loc[fun_i,2]), color = rgb(205,75,63, maxColorValue=255), alpha = 0.7, size = 8, stroke=3, pch=1);
  return(fun_resul);
}

f_tex <- function(fun_fig, fun_tex, fun_pos){
  fun_resul = fun_fig + annotate("text", x=fun_pos[1], y=fun_pos[2], label=fun_tex, colour = "grey6", size=10, hjust=0.5, vjust=0.5)
  return(fun_resul);  
}

f_adj <- function(fun_pos, fun_h, fun_v){
  fun_resul = fun_pos;
  fun_resul[1] = fun_pos[1] + fun_h;
  fun_resul[2] = fun_pos[2] + fun_v;
  return(fun_resul);
}

pos1 = f_adj(gov_loc[1,], 0, -0.018)
fig_map = f_tex(f_gov(fig_map, 1), 'Yanta', pos1);
pos2 = f_adj(gov_loc[2,], 0, -0.04)
fig_map = f_tex(f_gov(fig_map, 2), 'Changan', pos2);
pos3 = f_adj(gov_loc[3,], -0.05, -0.01)
fig_map = f_tex(f_gov(fig_map, 3), 'Lianhu', pos3);
pos4 = f_adj(gov_loc[4,], 0.048, -0.008)
fig_map = f_tex(f_gov(fig_map, 4), 'Beilin', pos4);
pos5 = f_adj(gov_loc[5,], 0, +0.03)
fig_map = f_tex(f_gov(fig_map, 5), 'Weiyang', pos5);
pos6 = f_adj(gov_loc[6,], 0.026, -0.026)
fig_map = f_tex(f_gov(fig_map, 6), 'Baqiao', pos6);
pos7 = f_adj(gov_loc[7,], 0.06, 0.02)
fig_map = f_tex(f_gov(fig_map, 7), 'Xincheng', pos7);
pos8 = f_adj(gov_loc[8,], 0.07, 0.001)
fig_map = f_tex(f_gov(fig_map, 8), 'Yanliang', pos8);
pos9 = f_adj(gov_loc[9,], 0, -0.04)
fig_map = f_tex(f_gov(fig_map, 9), 'Huyi', pos9);
pos10 = f_adj(gov_loc[10,], 0.07, 0.001)
fig_map = f_tex(f_gov(fig_map, 10), 'Lintong', pos10);
pos11 = f_adj(gov_loc[11,], 0, -0.03)
fig_map = f_tex(f_gov(fig_map, 11), 'Gaoling', pos11);
pos12 = f_adj(gov_loc[12,], 0, -0.04)
fig_map = f_tex(f_gov(fig_map, 12), 'Zhouzhi', pos12);
pos13 = f_adj(gov_loc[13,], 0.07, 0.001)
fig_map = f_tex(f_gov(fig_map, 13), 'Lantian', pos13);
pos14 = f_adj(gov_loc[14,], -0.03, -0.03)
fig_map = f_tex(f_gov(fig_map, 14), 'Xixian', pos14);


# save in 696, 660 × 5
size_axis = 36;
xlim1 = 107.70;
xlim2 = 109.70;
ylim1 = 33.72;
ylim2 = 34.74;
fig_map + coord_quickmap(xlim = c(xlim1, xlim2), ylim = c(ylim1, ylim2)) +
  geom_point(aes(x=109.037807, y=34.270926), color = "#FFC0CB", size = 1.8, pch="♥") + 
  geom_point(aes(x=108.658516, y=34.26256), color = "#FFC0CB", size = 1.8, pch="♥") + 
  theme( axis.text.x = element_text(size=size_axis, color = 'grey76',vjust = 0, hjust = 0.5, angle = 0) ) +  
  theme( axis.text.y = element_text(size=size_axis, color = 'grey76',vjust = 0.5, hjust = 1, angle = 0)) +
  labs(x='',y='');

