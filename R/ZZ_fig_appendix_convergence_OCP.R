
# results of OCP is saved in 'SA_det_results_base.RData'
load('output/SA_det_results_base.RData'); 
Obj    <-  results$Obj_real; 
data1  <-  data.frame(x = 2:length(Obj), y = Obj[-1] - Obj[-length(Obj)]);
data2  <-  data.frame(x = 1:length(Obj), y = Obj);

# plot of convergence and objective function
size_axis    <-  12;
size_axis_y  <-  14;
size_axis_x  <-  14;

p1  <-  ggplot(data1) + 
  geom_line(aes(x=x, y=y), cex = 0.5) + 
  theme(panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 0.4, hjust = 0.4, angle = 45) ) +  
  theme( axis.text.y = element_text(size=size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0), axis.title.y = element_text(vjust = 2, size=size_axis_y) ) + 
  scale_y_continuous( limits = c(-1e9, 1e9), breaks = seq(-1e9, 1e9, by=2e8), labels = seq(-10,10,by=2), name = expression(paste('Difference (', 10^8,' yuan', ')')) ) + 
  scale_x_continuous( limits = c(2, 55000),   breaks = c(2, seq(5000,55000, by =5000)), labels = c(2, seq(5000,55000, by =5000)) ) + 
  xlab('Itaretions') + theme(axis.title.x=element_text(vjust = -1, size = size_axis_x, face = "plain")) + 
  labs(title = '(A)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 20,face = "plain"));

p2  <-  ggplot(data2) + 
  geom_line(aes(x=x, y=y), cex = 0.5) + 
  theme(panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1)) +
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 0.4, hjust = 0.4, angle = 45) ) +  
  theme( axis.text.y = element_text(size=size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0), axis.title.y = element_text(vjust = 2, size=size_axis_y) ) + 
  scale_y_continuous( limits = c(300e8, 750e8), breaks = seq(300e8, 750e8, by=50e8), labels = seq(300,750,by=50), name = expression(paste('Cost (', 10^8,' yuan', ')')) ) + 
  scale_x_continuous( limits = c(1, 55000),   breaks = c(1, seq(5000,55000, by =5000)), labels = c(1, seq(5000,55000, by =5000)) ) + 
  xlab('Itaretions') + theme(axis.title.x=element_text(vjust = -1, size = size_axis_x, face = "plain")) + 
  labs(title = '(B)') + theme(plot.title=element_text(vjust = 1.6, hjust = 0.5, size = 20,face = "plain"));

# 10 × 7
p1 + p2;

