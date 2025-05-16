
# sum of two norm pdf, a/b/c is maximums/width/position of pdfs.
omega_type1 <- function(t, a, b, c){
  t  =  time %% 365 
  beta1  =  a[1] * dnorm(t, mean = c[1], sd = b[1]) * sqrt(2*pi)*b[1]
  beta2  =  a[2] * dnorm(t, mean = c[2], sd = b[2]) * sqrt(2*pi)*b[2]
  return(beta1+beta2)
}

# sum of two norm pdf, a/b/c is maximums/width/position of pdfs, c[1]<c[2].
omega_type2 <- function(t, a, b1, b2, c){
  
  t  =  t %% 365 
  
  if(t <= c[1]){
    b = b1[1] 
  }else{
    b = b1[2]
  }
  beta1 = a[1] * dnorm(t, mean = c[1], sd = b) * sqrt(2*pi)*b
  
  if(t <= c[2]){
    b = b2[1]
  }else{
    b = b2[2]
  }
  beta2 = a[2] * dnorm(t, mean = c[2], sd = b) * sqrt(2*pi)*b
  
  return(beta1+beta2)
}

# omega plot
omega <- sapply( 1:365, function(t) omega_t(t, a=c(1,1), b=c(19,20), c=c(120,260)) )
plot(omega)

# mean cases
a1=Cases[1:365]
a2=Cases[366:(365*2)]
a3=Cases[(365*2+1):(365*3)]
a4=Cases[(365*3+1):(365*4)]
a5=Cases[(365*4+1):(365*5)]
a6=Cases[(365*5+1):(365*6)]
aa = (a1+a2+a3+a4+a5+a6)/6

plot(aa)
aa_sm <- stats::smooth.spline(x=1:365-0.5, y=aa, w = rep(1, 365), df=30)$y
aa_sm[aa_sm<0] = 0
lines(aa_sm,col=2)
 
plot(Cases)
lines(rep(aa_sm,6), col=2)


# 先按每年都是aa_sm来试, 即omega_type2只需一组参数












# 按Cases的光滑化来试, omega_type2每年一组参数

a=c(1,1)
b=c(15,20)
c=c(120,120)
beta1  =  a[1] * dnorm(1:365, mean = c[1], sd = b[1]) * sqrt(2*pi)*b[1]
beta2  =  a[2] * dnorm(1:365, mean = c[2], sd = b[2]) * sqrt(2*pi)*b[2]
plot(beta1,pch='')
lines(beta1)
lines(beta2,col=2)

