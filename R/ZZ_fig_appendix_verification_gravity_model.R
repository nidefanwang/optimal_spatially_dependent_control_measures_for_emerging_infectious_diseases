
# verification of the uniformity of gravity model and population mobility model
# run 'main.R' first
source('R/main.R');

# matrix of population mobility rate
tau_ij  <-  cal_tau(x_med['D']);

# coefficient matrix of population mobility model
Tau_ij  <-  tau_ij*0;
for(i in 1:dim(Tau_ij)[1]){
  for(j in 1:dim(Tau_ij)[2]){
    if(i == j){
      Tau_ij[i, j]  =   -sum(tau_ij[i,-j]);
    }else{
      Tau_ij[i, j]  =   tau_ij[j, i];  
    }
  }
}

# equations of N_i s' equilibrium of reduced model
A  <-  rbind( rep(1,n), Tau_ij[-1,] );
b  <-  c(sum(Nr),rep(0,n-1));
Ni_equil  <-  solve(A, b);
max(abs(Ni_equil - Nr));

# figure of Ni_equil and N_i^r, save in the size of 6 * 8 
ggplot() + 
  geom_line(aes(x = seq(0, 220e4, 1e4), y = seq(0, 220e4, 1e4)), size = 1, col = rgb(111,146,199,alpha=100,maxColorValue=255) ) +
  geom_point(aes(x = Nr, y = Ni_equil), size = 4, col = rgb(3,57,108 ,alpha=160,maxColorValue=255) ) +
  scale_y_continuous( breaks = c( seq(0,220e4,50e4)), labels = c( seq(0,220,50)), limits = c(0,220e4) ) + 
  scale_x_continuous( breaks = c( seq(0,220e4,50e4)), labels = c( seq(0,220,50)), limits = c(0,220e4) ) + 
  theme( axis.text.x = element_text(size = 14, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) +  
  theme( axis.text.y = element_text(size = 14, color = 'black',vjust = 0.5, hjust = 1, angle = 0) ) + 
  ylab( expression(paste('Fitted value (', 10^4, ')')) ) + 
  xlab( expression(paste('Population data (', 10^4, ')')) ) + 
  theme(axis.title.y=element_text(vjust = 2, size = 16, face = "plain"),
        axis.title.x=element_text(vjust = 2, size = 16, face = "plain"),
        panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_line(colour = "grey100", size = 0.1));
