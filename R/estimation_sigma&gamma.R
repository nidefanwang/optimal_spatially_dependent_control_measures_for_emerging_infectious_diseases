
# reference: Generation time of the alpha and delta SARS-CoV-2 variants: an epidemiological analysis
# Transmission Dynamics of an Outbreak of the COVID-19 Delta Variant B.1.617.2 — Guangdong Province, China, May–June 2021

# mean and variance of GI
E = 4.7; V = 3.3^2;
b = V/E; a = E/b;

l1 = 1/(4.4-2);

obj <- function(par){
  
  sigma = l1;
  gamma = par[1];
  
  dfit <- function(z){ # GI in SEIR model
    f = function(x){
      temp = dexp(x,rate=sigma)*dexp(z-x,rate=gamma);
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
  
  return(sum(y1^2));
}

l2 = optim(c(1), obj,  method = "L-BFGS-B",lower =c(0.01), upper = c(10))$par;

dfit <- function(z){
  f = function(x){
    temp = dexp(x,rate=l1)*dexp(z-x,rate=l2);
    return(temp);
  }
  p_z = integrate(f,0,z)$value
  return(p_z);
}

xx = seq(0,50,0.01);
temp = xx;
for(i in 1:length(xx)){
  temp[i] = dfit(xx[i]);
}

# pdf file was saved with a 8:6 aspect ratio
col1  <- rgb(68,114,196, alpha=160,maxColorValue=255); 
col2  <- rgb(218,122,107, alpha=255,maxColorValue=255); 
par(mar=c(5,6,3,4));
plot(xx,temp,xlim=c(0,15),ylim=c(0,0.2),pch='',xaxs = "i",yaxs = "i",xaxt = 'n',yaxt = 'n',xlab='',ylab='');
lines(xx,temp, lwd=6,lty = 1, col=col1);
lines(xx,dgamma(xx,shape=a,scale=b),lty = 2, lwd=4, col=col2);
axis(1, at=seq(0,15,by=1), labels = seq(0,15,by=1)); 
axis(2, at=seq(0,0.2,by=0.05), labels = seq(0,0.2,by=0.05)); 
mtext('Time (days)',side=1, line=3, cex=1.4); 
mtext('Generation interval',side=2, line=3, cex=1.4); 
