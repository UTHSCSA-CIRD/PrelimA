## info is a generic function I created, that didn't do much
## until now. This is a building block for automated data validation
## and semi-automated variable identification
## Will return a data.frame with a row for each column in xx
## uglyhack: apparently the generic info method is declared 
## someplace no longer accessible in my environment, so creating
## a duplicate function called `info`
info<-info.data.frame<-function(xx,...){
  ## xx = data.frame
  nr<-nrow(xx);
  oo<-cbind(name=names(xx),
            do.call(rbind,sapply(xx,function(ii){
              numii<-as.numeric(ii);
              chrii<-as.character(ii);
              data.frame(class=class(ii)[1],
                         nunique=length(unique(ii)),
                         ## for detecting typos in characters and factors
                         nchrunique=length(unique(gsub('[ .,:;?]','',tolower(ii)))),
                         n.na=sum(is.na(ii)),
                         n.notna=sum(!is.na(ii)),
                         n.num=sum(!is.na(numii)),
                         n.bool=sum(ii%in%0:1,na.rm=T),
                         n.neg=sum(numii<0,na.rm=T),
                         n.zero=sum(numii==0,na.rm=T),
                         n.ltone=sum(numii<1,na.rm=T),
                         n.one=sum(numii==1,na.rm=T),
                         ## has.n.different=list(apply(aggregate(xx,list(ii),function(kk) length(unique(kk)))[,-1],2,max)),
                         n.int=sum(abs(round(as.numeric(ii))-as.numeric(ii))<.Machine$double.eps,na.rm=T),
                         norm=ks.test(numii,'pnorm')$stat,
                         t=ks.test(numii,'pt',df=nr-1)$stat,
                         lnorm=ks.test(numii,'plnorm')$stat,
                         exp=ks.test(numii,'pexp')$stat,
                         unif=ks.test(numii,'punif')$stat                         
                         );
            },simplify=F)),stringsAsFactors=F);
  ## table of unique values for each variable
  oo$tunique <- sapply(xx,table);
  ## which variables have more than one value for at least one level of the variable represented by this row?
  oo$any.multi<-sapply(oo$name,function(jj)
                       apply(aggregate(xx,xx[,jj,drop=F],function(kk)
                                       length(unique(kk)))[,-1],2,max)>1,simplify=F);
  ## which variables have more than one value for all levels of the variable represented by this row?
  oo$all.multi<-sapply(oo$name,function(jj)
                       apply(aggregate(xx,xx[,jj,drop=F],function(kk)
                                       length(unique(kk)))[,-1],2,min)>1,simplify=F);
  ## how many other variables have multiple values on at least one level of the variable represented by this row?
  oo$n.any.multi<-sapply(oo$any.multi,sum);
  ## how many other variables have multiple values on all levels of the variable represented by this row?
  oo$n.all.multi<-sapply(oo$all.multi,sum);
  ## TODO: number of values that will not be NA if converted to dates
  ## TODO: wrapper function for identifying response vars, censor vars, group vars, random vars, and predictor vars
  ## TODO: benford test
  oo;
}

possiblevars<-function(xx,infxx=info(xx),...){
  ## possible response
  ## yvars<-subset(infxx,n.bool/n.notna<.1&class%in%c('numeric','integer')&nunique/n.notna>.8)$name;
  ## the above rules out too many reasonsable variables... need to think of a different uniqueness criterion
  yvars<-subset(infxx,n.bool/n.notna<.1&class%in%c('numeric','integer'))$name;
  if(length(yvars)==0) yvars<-NULL;
  ## censor vars
  cvars<-subset(infxx,n.bool==n.notna)$name;
  if(length(cvars)==0) cvars<-NULL;
  ## group vars
  gvars<-subset(infxx,n.all.multi>0 & nunique>1)$name;
  if(length(gvars)==0) gvars<-NULL;
  ## random vars
  rvars<-sapply(infxx[gvars,'all.multi'],function(ii) names(ii)[ii],simplify=F);
  ## factorial/categoric vars
  fvars<-subset(infxx,class%in%c('character','factor')&n.all.multi>0 & nunique>1)$name;
  if(length(fvars)==0) fvars<-NULL;
  ## numeric vars
  nvars<-subset(infxx,class%in%c('numeric','integer')&nunique>1)$name;
  if(length(nvars)==0) nvars<-NULL;
  return(list(yvars=yvars,cvars=cvars,gvars=gvars,rvars=rvars,fvars=fvars,nvars=nvars));
}
