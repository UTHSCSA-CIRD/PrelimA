unifx.lme <-
function(fit,...,linfct,uniquelinfct=F,includeorth=T,debug=F,tails=2,drop='Call',alpha=.05,FUN=c,killrownames=T,DF=NULL,padj=NULL){
  ## fit = lme fit
  ## linfct = matrix whose column names are a superset of the model's coefficient names, optional
  ## includeorth = whether to prepend an automatically generated orthogonal contrast matrix to linfct
  ## debug = whether to drop into browser when certain error conditionas encountered
  ## tails = 1 or 2 for p-value calculation
  ## drop = names of columns to drop from final output
  ## alpha = alpha
  ## FUN = transformation of the Beta column
  ## killrownames = whether to erase the rownames, for cleaner output
  ## DF = optional vector of degrees of freedom for custom contrasts
  ##      (the orthogonal ones get set automatically) 
  ## padj = optional vector containing any value other than 'x' for grouping p-values for adjustment (Holm)
  ##        for custom contrasts only, as above
  coefs<-names(fixef(fit));
  orthcm <- diag(length(coefs)); dimnames(orthcm)<-list(coefs,coefs); 
  if(missing(linfct)) linfct<-orthcm else {
    ## right here we can implement dynamic DFs
    ## instead of this...
    linfct<-linfct[,coefs];
    ## assume that linfct is a list (still)
    if(uniquelinfct) linfct<-unique(linfct);
    if(includeorth) {
      linfct<-rbind(orthcm,linfct);
    }
  }
  cntrlength <- nrow(linfct)-length(coefs);
  DF <- c(fit$fixDF$X,DF);
  if(length(DF) != nrow(linfct)){
    DF <- c(DF,rep(min(DF),cntrlength));
  }

  oo<-with(adjusted('none')(glht(fit,linfct=linfct,df=DF)),{
    ## DF mismatches happen when custom contrasts are used with LME
    ## Would be a good idea then to add a DF argument same length as linfct
    ## If DF missing, fill with minimum df from fit, maybe
    ## if(length(unique(DF))>1&&length(DF)!=length(fit$fixDF$X)){
    ##   warning('DF Mismatch')
    ##   if(debug) browser();
    ## }
    ## warning('Using model-extracted DFs for the first time. Make sure they match summary(fit).');
    data.frame(Term=names(coefficients), Effect=FUN(coefficients),
               Beta=coefficients, Error=sigma, DF=DF,
               Statistic=tstat, p=pvalues); #p=tails*pt(abs(tstat),DF,lower=F))
  });
  if(!is.null(padj)) {
    padj <- factor(c(rep('x',len=length(coefs)),rep(padj,len=cntrlength)));
    oo$p[padj!='x'] <- unsplit(lapply(split(oo$p,padj),p.adjust),padj)[padj!='x'];    
  }
  oo$Change<-ifelse(oo$p<alpha,2,1)*sign(oo$Beta);
  oo$Model<-'lme'; oo$Call<-deparse(fit$call,width.cutoff=500L,nlines=1L);
  oo$Y<-deparse(fit$call$fixed[[2]],width.cutoff=500L,nlines=1L);
  if(killrownames) rownames(oo) <- NULL;
  if(is.null(drop)) oo else oo[,!colnames(oo)%in%drop];
}
