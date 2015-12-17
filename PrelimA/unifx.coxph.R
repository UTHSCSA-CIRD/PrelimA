unifx.coxph <-
function(fit,...,linfct,uniquelinfct=F,includeorth=T,debug=F,tails=2,dropdef=c('DF','Call'),drop=NULL,alpha=.05,FUN=exp,killrownames=T,padj=NULL){
  ## Should also work with coxph.penal
  ## fit = lme fit
  ## linfct = matrix whose column names are a superset of the model's coefficient names, optional
  ## includeorth = whether to prepend an automatically generated orthogonal contrast matrix to linfct
  ## debug = whether to drop into browser when certain error conditionas encountered
  ## tails = 1 or 2 for p-value calculation
  ## dropdef = names of columns to drop from final output by default for this method
  ## drop = names of columns to drop from final output without changing defaults
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
               Statistic=tstat, p=pvalues);
  });
  if(!is.null(padj)) {
    padj <- factor(c(rep('x',len=length(coefs)),rep(padj,len=cntrlength)));
    oo$p[padj!='x'] <- unsplit(lapply(split(oo$p,padj),p.adjust),padj)[padj!='x'];    
  }
  oo$Change<-ifelse(oo$p<alpha,2,1)*sign(oo$Beta);
  oo$Model<-'coxph'; oo$Call<-deparse(fit$call,width.cutoff=500L,nlines=1L);
  oo$Y<-deparse(fit$terms[[2]],width.cutoff=500L,nlines=1L);
  if(killrownames) rownames(oo) <- NULL;
  if(is.null(drop)) oo else oo[,!colnames(oo)%in%drop];
}
