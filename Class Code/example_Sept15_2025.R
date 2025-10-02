
mydata=c(2.487, 2.493, 2.495, 2.493, 2.504, 2.485, 2.483, 2.502,
  2.494, 2.490, 2.487, 2.489);
n=length(mydata);
ybar=mean(mydata);
mysd=sd(mydata);
print(ybar)
print(c(ybar-1.96*0.01/sqrt(n), 
ybar+1.96*0.01/sqrt(n)))

### Bayesian method starts from here
sigma=0.01;
sigma2=sigma^2;
### prior mean mu0= 2.5; sigma02=1;
mu0=2.5
sigma02=1;
###
### post dist of the underlying mean 
post.sd= sqrt(1/( (n/sigma2)+1/sigma02   ))
post.mn= (n*ybar/sigma2+ mu0/sigma02)/((n/sigma2)+1/sigma02)
qnorm(c(0.025, 0.975), post.mn, post.sd)

## predictive distribution of a future observation 
pred.mn= post.mn
pred.sd= sqrt(post.sd^2+ 0.01^2)
### prediction interval 
qnorm(c(0.025,0.975), pred.mn, pred.sd)




capn=20000
theta.post=rgamma(capn, n*mean(mydata)+1, rate= (n+1));
pred.prob.0= mean(exp(-theta.post)); 
pred.prob.1= mean(exp(-theta.post)*theta.post);
pred.prob.2= mean(exp(-theta.post)*theta.post^2/2);
f1=function(para) dpois((0:12), para)
out=sapply(theta.post,f1)
pred.prob=apply(out, 1, mean)
hist(mydata, prob=T, nclass=8, ylim=c(0, 0.3), xlim=c(0, 12), 
     xlab="", ylab="")
par(new=T)
plot(c(0:12), as.numeric(pred.prob),
     type="l", lwd=2, ylim=c(0, 0.3), ylab="", xlab="", axes=F)
