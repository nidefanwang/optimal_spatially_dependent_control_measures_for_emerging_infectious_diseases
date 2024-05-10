
# run main.R first
source('R/main.R');

library(ggplot2);
library(patchwork);
library(grid);

district_plot = c('Yanta', 'Changan', 'Lianhu', 'Beilin', 'Weiyang', 'Baqiao', 'Xincheng', 'Yanliang', 'Huyi', 'Lintong', 'Gaoling', 'Zhouzhi', 'Lantian', 'Xixian');

# Fig 1: GDP and population    
A1   <-   data.frame(x = rep(district_plot,2), z = c(rep('Per capita GDP',n), rep('Population',n)), y = c(GDP_per*max(Nr)/max(GDP_per), Nr ));
A1$x =    factor(A1$x, level = district_plot);

p1   <-   ggplot(A1, aes(x=x, y=y, fill=z)) +
  scale_y_continuous( limits = c(0,210e4), breaks = seq(0,150,by=30)*1e3*max(Nr)/max(GDP_per), labels = seq(0,150,by=30), name = 'Per capita GDP',  
                      sec.axis = dup_axis(breaks = c(seq(0,200,by=40)*1e4), labels = seq(0,200,by=40), name = 'Population') ) +
  scale_x_discrete( name = NULL ) + 
  labs(title = '(C)') + 
  theme( plot.title = element_text(vjust = 1.6, hjust = 0.5, size = 18,face = "plain"), 
         panel.background = element_rect(fill = "grey98"),
         panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
  theme( axis.text.x = element_text(size=12, color = 'black',vjust = 1, hjust = 1, angle = 45) ) +
  theme( axis.text.y = element_text(size=12, color = 'black',vjust = 0.5, hjust = 1, angle = 0), axis.title.y = element_text(size=12) ) +
  theme( legend.position = c(0.75, 0.9), legend.title=element_blank(), legend.text=element_text(size=12) ) +
  coord_cartesian(clip = 'off') +
  annotation_custom(grob = textGrob(expression(x10^3~~yuan)),  xmin = -11,ymin =232e4) +
  annotation_custom(grob = textGrob(expression(x10^4)),  xmin = +13.8,ymin =232e4) +
  geom_bar(stat="identity", position = position_dodge(0.7), width=0.6) + 
  scale_fill_manual(values=c( rgb(233,182,100, maxColorValue=255), rgb(192,222,114, maxColorValue=255)) );


# Fig 2: population mobility matrix 
A2   <-   data.frame(departure = rep(district_plot, n), destination = rep(district_plot, times=rep(n,n)), value = c( t(cal_tau(x_med['D']))) );
A2$departure = factor(A2$departure, level =  district_plot  );
A2$destination = factor(A2$destination, level = rev(district_plot) );

p2   <-   ggplot( A2, aes(x=departure,y=destination)) +  
  geom_tile( aes(fill=value),colour = "white") + 
  scale_fill_gradient( low = "white",high = "deepskyblue") + # 设置颜色
  labs( title = '(D)') + theme( plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 18,face = "plain")) + # 设置图片标题
  theme( axis.ticks = element_blank(), panel.background = element_blank()) + # 设置坐标轴及省略坐标轴刻度
  theme( legend.position = "right", legend.key.width = unit(0.26,'cm'), legend.key.height = unit(2.3,'cm') ) + # 设置图例   
  theme( axis.text.x = element_text(size = 12,color = 'black',angle=36,hjust = 0.9), axis.text.y = element_text(size = 12,color = 'black',hjust = 1) ) + # 设置坐标轴文字
  scale_x_discrete( name = NULL ) + scale_y_discrete( name = NULL ); # 省略坐标轴标题 


# Fig 3: GDP
A1   <-   data.frame(x = district_plot, z = rep('GDP',n), y = GDP_all/1e8);
A1$x =    factor(A1$x, level = district_plot);

p3   <-   ggplot(A1, aes(x=x, y=y, fill=z)) +
  scale_y_continuous( limits = c(0,2800), breaks = seq(0,2800,by=400), labels = seq(0,2800,by=400), name = 'GDP') + # 设置y轴
  scale_x_discrete( name = NULL ) + 
  labs(title = '(A)') + 
  theme( plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 18,face = "plain"),
                               panel.background = element_rect(fill = "grey98"),
                               panel.grid.major = element_line(colour = "grey100", size = 0.1)) + # 设置图片标题
  theme( axis.text.x = element_text(size=12, color = 'black',vjust = 1, hjust = 1, angle = 45) ) + # 设置x轴标签
  theme( axis.text.y = element_text(size=12, color = 'black',vjust = 0.5, hjust = 1, angle = 0), axis.title.y = element_text(size=12) ) + # 设置y轴标签大小位置等
  theme(legend.position="none") + # 设置图例
  coord_cartesian(clip = 'off') + # 可以在图片外加文字
  annotation_custom(grob = textGrob('100 million yuan'),  xmin = -10.6,ymin =3100) + # 加文字
  geom_bar(stat="identity", position = position_dodge(0.7), width=0.6) + 
  scale_fill_manual(values=brewer.pal(9,"YlOrBr")[4] ); # 画出柱子


# Fig 4: distance
A2  <-   data.frame(departure = rep(district_plot, n), destination = rep(district_plot, times=rep(n,n)), value = c( t(dist) ) );
A2$departure = factor(A2$departure, level =  district_plot  );
A2$destination = factor(A2$destination, level = rev(district_plot) );

p4  <-   ggplot( A2, aes(x=departure,y=destination)) +  
  geom_tile( aes(fill=value),colour = "white") + 
  scale_fill_gradient( low = "white",high = 'darkorange') + # 设置颜色
  labs( title = '(B)') + theme( plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 18,face = "plain")) + # 设置图片标题
  theme( axis.ticks = element_blank(), panel.background = element_blank()) + # 设置坐标轴及省略坐标轴刻度
  theme( legend.position = "right", legend.key.width = unit(0.26,'cm'), legend.key.height = unit(2.3,'cm') ) + # 设置图例 
  labs(fill='value (km)') + # 修改标签标题
  theme( axis.text.x = element_text(size = 12,color = 'black',angle=36,hjust = 0.9), axis.text.y = element_text(size = 12,color = 'black',hjust = 1) ) + # 设置坐标轴文字
  scale_x_discrete( name = NULL ) + scale_y_discrete( name = NULL ); # 省略坐标轴标题 


p.all  <-  list(p3, p4, p1, p2);

# save in the size of 12 × 12 
wrap_plots(p.all, ncol=2);
