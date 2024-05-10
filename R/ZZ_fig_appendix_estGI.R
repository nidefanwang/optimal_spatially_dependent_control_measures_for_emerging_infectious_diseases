
# parameters of GI
E = 4.7; V = 3.3^2;
b = V/E; a = E/b;

# model latent period parameter
sigma = 1/2.4; 

# loss function
obj <- function(par){
  
  gamma = par[1];
  
  dfit <- function(z){ 
    f = function(x){
      temp = dexp(x, rate = sigma)*dexp(z-x, rate = gamma);
      return(temp);
    }
    p_z = integrate(f,0,z)$value
    return(p_z);
  }
  
  xx = seq(0,10,0.1);
  temp = xx;
  for(i in 1:length(xx)){
    temp[i] = dfit(xx[i]);
  }
  y1 = temp - dgamma(xx,shape=a,scale=b);
  
  return(1000*sum(y1^2));
}

# solve
optim(c(1), obj,  method = "L-BFGS-B",lower =c(0.01), upper = c(10));

# fitting
dfit <- function(z){
  f = function(x){
    temp = dexp(x, rate = sigma) * dexp(z-x, rate = 0.4318);
    return(temp);
  }
  p_z = integrate(f,0,z)$value
  return(p_z);
}

xx = seq(0,50,0.01);
yy = xx;
for(i in 1:length(xx)){
  yy[i] = dfit(xx[i]);
}

zz  <-  dgamma(xx,shape=a,scale=b);
zz[-(seq(1,length(zz),20))]  =   -1000;
A1  <-  data.frame(t = xx, y1 = yy, y2 = zz);

size_axis    <-  16;
size_axis_y  <-  18;
size_axis_x  <-  18;
size_legend  =   20;
size_title   =   30;

# ѡȡ??ɫ
display.brewer.all();
brewer.pal.info;

col1  =   brewer.pal(9,"Blues")[3];
col2  =   rgb(3,57,108 , maxColorValue=255);

# save in the size of 6 * 8 
ggplot(A1) +
  geom_line( aes(x = t, y = y1 ), col = col1, cex = 1.6, linetype = 1 ) + 
  geom_point( aes(x = t, y = y2 ), col = col2, cex = 2.4, shape = 16) + 
  theme( axis.text.x = element_text(size=size_axis, color = 'black',vjust = 0, hjust = 0.5, angle = 0) ) +  
  theme( axis.text.y = element_text(size=size_axis, color = 'black',vjust = 0.5, hjust = 1, angle = 0), axis.title.y = element_text(vjust = 2, size=size_axis_y) ) + 
  scale_y_continuous( limits = c(0, 0.16), breaks = seq(0,0.16,by=0.04), labels = seq(0,0.16,by=0.04), name = '' ) + 
  scale_x_continuous( limits = c(0, 15),   breaks = seq(0,15,by=3), labels = seq(0,15,by=3) ) + 
  xlab('Generation time (days)') + 
  theme(axis.title.x=element_text(vjust = -1, size = size_axis_x, face = "plain"), 
        panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1));
