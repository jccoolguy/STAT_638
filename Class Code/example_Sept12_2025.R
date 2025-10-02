n=60;

mydata=c(9,  3,  4,  5,  9,  5,  7,  5,  5,  6,
         2, 11,  6,  4,  8,  6,  5,  4,  3,  6,
         0,  3,  3,  5,  8,  4,  7,  3,  7,  5,
         5,  3, 8,  4,  4,  5,  3,  2,  9,  1,
         4,  4,  4,  4,  2,  4,  6,  6,  3,  9, 
         6,  5,  8,  6,  4,  2,  6,  5,  8,  6)
####### Analysis of the data
# prior Gamma(1, 1); 
# posterior of theta propto theta^{n \bar{X}+1-1} \exp( -(n+1)\theta);
# 
capn=20000
theta.post=rgamma(capn, n*mean(mydata)+1, rate= (n+1));
pred.prob.0= mean(exp(-theta.post)); 
pred.prob.1= mean(exp(-theta.post)*theta.post);
pred.prob.2= mean(exp(-theta.post)*theta.post^2/2);
f1=function(para) dpois((0:2), para)
out=sapply(theta.post,f1)
