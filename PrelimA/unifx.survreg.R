## unifx.survreg <-
## function(fit,...,drop='Call',alpha=.05,FUN=exp,killrownames=T){
##   #summ<-summary(fit,...);
##   oo<-summary(fit,...)$table;
##   ## If robust fit was used, remove the non-robust SE column
##   oo<-oo[,!grepl('Naive SE',colnames(oo))];
##   colnames(oo)<-c('Beta','Error','Statistic','p');
##   oo<-data.frame(Term=rownames(oo),Effect=FUN(oo[,'Beta']),oo[,1:2],
##                  DF={if(length(fit$df)%in%c(1,nrow(oo))) fit$df else rep(c(fit$df,rep(NA,len=nrow(oo))),len=nrow(oo))},
##                  oo[,3:4],Change=ifelse(oo[,'p']<alpha,2*sign(oo[,'Beta']),sign(oo[,'Beta'])),
##                  Model=fit$dist,Call=deparse(fit$call,width.cutoff=500L,nlines=1L),
##                  Y=deparse(fit$terms[[2]],width.cutoff=500L,nlines=1L),stringsAsFactors=F);
##   if(killrownames) rownames(oo) <- NULL;
##   if(is.null(drop)) oo else oo[,!colnames(oo)%in%drop];
## }

unifx.survreg <-
function(fit,...,linfct,uniquelinfct=F,includeorth=T,debug=F,tails=2,dropdef=c('DF','Call'),drop=NULL,alpha=.05,FUN=exp,killrownames=T,padj=NULL){
  ## Should also work with coxph.penal
  ## fit = lme fit
  ## linfct = matrix whose column names are a superset of the model's coefficient names, optional
  ## includeorth = whether to prepend an automatically generated orthogonal contrast matrix to linfct
  ## debug = whether to drop into browser when certain error conditionas encountered
  ## tails = 1 or 2 for p-value calculation
  ## drop = names of columns to drop from final output
  ## alpha = alpha
  ## FUN = transformation of the Beta column
  ## killrownames = whether to erase the rownames, for cleaner output
  drop<-c(dropdef,drop);
  coefs <- names(coef(fit));
  orthcm <- diag(length(coefs)); dimnames(orthcm)<-list(coefs,coefs); 
  if(missing(linfct)) linfct<-orthcm else {
    linfct<-linfct[,coefs];
    if(uniquelinfct) linfct<-unique(linfct);
    if(includeorth) linfct<-rbind(orthcm,linfct);
  }
  cntrlength <- nrow(linfct)-length(coefs);
  oo<-with(adjusted('none')(glht(fit,linfct=linfct)),{
    ## Is DF ever != for fixed terms in a coxph.penal model?
    DF<-rep(NA,length(coefficients));
    data.frame(Term=names(coefficients), Effect=FUN(coefficients),
               Beta=coefficients, Error=sigma, DF=DF,
               Statistic=tstat, p=pvalues,stringsAsFactors=F);
  });
  scale<-summary(fit)$table['Log(scale)',];
  oo<-rbind(oo,data.frame(Term='Log(scale)',Effect=FUN(scale['Value']),
                          Beta=scale[1],Error=scale[2],DF=NA,Statistic=scale[3],p=scale[4]));
  if(!is.null(padj)) {
    padj <- factor(c(rep('x',len=length(coefs)),rep(padj,len=cntrlength),'x'));
    oo$p[padj!='x'] <- unsplit(lapply(split(oo$p,padj),p.adjust),padj)[padj!='x'];    
  }
  oo$Change<-ifelse(oo$p<alpha,2,1)*sign(oo$Beta);
  oo$Model<-'coxph'; oo$Call<-deparse(fit$call,width.cutoff=500L,nlines=1L);
  oo$Y<-deparse(fit$terms[[2]],width.cutoff=500L,nlines=1L);
  if(killrownames) rownames(oo) <- NULL;
  if(is.null(drop)) oo else oo[,!colnames(oo)%in%drop];
}
