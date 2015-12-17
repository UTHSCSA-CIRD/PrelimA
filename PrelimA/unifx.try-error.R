`unifx.try-error` <-
function(fit,...,drop=NULL,alpha=.05,FUN=c,killrownames=F) {
  oo<-data.frame(Term='error',Effect=NA,Beta=NA,Error=NA,DF=NA,Statistic=NA,p=NA,Change=NA,Model='error',Call=fit[1],Y=NA);
  if(killrownames) rownames(oo) <- NULL;
  if(is.null(drop)) oo else oo[,!colnames(oo)%in%drop];
}
