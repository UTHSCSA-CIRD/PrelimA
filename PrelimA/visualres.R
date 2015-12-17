visualres <- function(res,...) UseMethod("visualres");

visualres.default <- function(res,fits,ylim=range(res),xlim=range(fits),pch=16,dotcol=alpha('black',.2),nknots=5,
                      hlines=c(-3,0,3),reslim=abs(res)<4,type=c('direct','abs','qq'),main='',xlab=substitute(fits),ylab='',
                      qfun=qnorm,...){
  ## function to plot residuals along with a linear interpolation and smoother, for ease of diagnosi
  ## res, fits = residuals and fits, respectively
  ## ... bunch of arguments passed to plotting functions ...
  ## hlines = vertical location of reference lines
  ## reslim = the subset of res's to use
  ## (to keep extreme outliers from messing up your visualization; for pearson residuals this would be <4)
  ## qfun = function (must take res as the first argument and have any other mandatory parameters pre-specified) for calculating theoretical quantiles
  type<-match.arg(type,several.ok=T); dospline <- length(unique(round(fits,5)))>=5;
  if('direct'%in%type){
    plot(res~fits,ylim=ylim,xlim=xlim,pch=pch,col=dotcol,main=main,xlab=xlab,ylab=paste(ylab,'Residuals'));
    abline(h=hlines,lty=3);
    abline(lm(res[reslim]~fits[reslim]),col='red');
    if(dospline) {
      lines(smooth.spline(fits,res,nknots=nknots),col=alpha('blue',.5),lwd=2,lty=2);
      ## if(class(oo)[1]=='try-error') browser();
    } else abline(lm(res[reslim]~fits[reslim]),col=alpha('blue',.5),lwd=2,lty=2);
  }
  if('abs'%in%type){
    ares<-abs(res);
    plot(ares~fits,ylim=c(0,max(ares)),xlim=xlim,pch=pch,col=dotcol,main=main,xlab=xlab,ylab=paste(ylab,'Absolute Residuals'));
    abline(h=median(ares),lty=3);
    abline(lm(ares[reslim]~fits[reslim]),col='red');
    if(dospline) {
      lines(smooth.spline(fits,ares,nknots=nknots),col=alpha('blue',.5),lwd=2,lty=2);
    } else abline(lm(ares[reslim]~fits[reslim]),col=alpha('blue',.5),lwd=2,lty=2);
  }
  if('qq'%in%type){
    qqdat<-qqplot(qfun(ppoints(res)),res,ylab=paste(ylab,'Residuals'),xlab='Theoretical Quantiles',pch=pch,col=dotcol,main=main);
    qqline(res,dist=qfun,col='red');
  }
}


visualres.cox.zph <- function(res,pch=16,dotcol=alpha('black',.2),...){
  ## very different from visualres.default... for cox.zph it's just a wrapper on the default plot method
  ## might turn out to be useless
  ## res = a subset cox.zph object
  plot(res,pch=16,resid=nrow(res$table)>1,...);
  if(nrow(res$table)==1) points(res$x,res$y,col=dotcol,pch=pch,...);
}
