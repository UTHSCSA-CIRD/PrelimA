unifx.lm <- 
function(fit,...,linfct,uniquelinfct=F,includeorth=T,debug=F,tails=2,drop='Call',alpha=.05,FUN=c,killrownames=T,DF=NULL,padj=NULL) {
  coefs<-names(coef(fit));
  orthcm <- diag(length(coefs)); dimnames(orthcm)<-list(coefs,coefs);
  ##DF <- rep(summary(fit)$df[1],length(coefs));
  if(missing(linfct)) linfct<-orthcm else {
    linfct<-linfct[,coefs];
    if(uniquelinfct) linfct<-unique(linfct);
    if(includeorth) {
      linfct<-rbind(orthcm,linfct);
    }
  }
  cntrlength <- nrow(linfct)-length(coefs);
  DF <- c(rep(summary(fit)$df[2],length=nrow(linfct)),DF);
  if(length(DF) != nrow(linfct)){
    DF<-rep(DF,length=nrow(linfct));
    warning('Extending length of DF argument to fit the number of contrasts. If this surprises you, there is a problem');
  }

  oo<-with(adjusted('none')(glht(fit,linfct=linfct,df=DF)),{
    data.frame(Term=names(coefficients), Effect=FUN(coefficients),
               Beta=coefficients, Error=sigma, DF=DF,
               Statistic=tstat, p=pvalues)
  });

  ##oo <- summary(fit,...)$coef;
  ##colnames(oo) <- c('Beta','Error','Statistic','p');
  ##oo<-data.frame(Term=rownames(oo),Effect=FUN(oo[,'Beta']),oo[,1:2],DF=NA,oo[,3:4],Change=ifelse(oo[,'p']<alpha,2*sign(oo[,'Beta']),sign(oo[,'Beta'])),
  ##           Model='lm',Call=deparse(fit$call,width.cutoff=500L,nlines=1),
  ##           Y=deparse(fit$call$formula[[2]],width.cutoff=500L,nlines=1),stringsAsFactors=F);
  if(!is.null(padj)) {
    padj <- factor(c(rep('x',len=length(coefs)),rep(padj,len=cntrlength)));
    oo$p[padj!='x'] <- unsplit(lapply(split(oo$p,padj),p.adjust),padj)[padj!='x'];    
  }
  oo$Change<-ifelse(oo$p<alpha,2,1)*sign(oo$Beta);
  oo$Model<-'lm'; oo$Call<-deparse(fit$call,width.cutoff=500L,nlines=1L);
  oo$Y<-deparse(fit$call$formula[[2]],width.cutoff=500L,nlines=1L);
  if(killrownames) rownames(oo) <- NULL;
  if(is.null(drop)) oo else oo[,!colnames(oo)%in%drop];
}

