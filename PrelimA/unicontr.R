unicontr <-
function(fit,data,compactnames=T,renfrom='',rento='',debug=F){
  ## fit = hopefully any fitted model object will work
  ## data = data frame to use; optional (will get from fit if missing)
  ## compactnames = shorten the row names of output (might break stuff)
  ## renfrom, rento = passed to rgsub
  ##
  ## get the terms of the formula
  t0 <- terms(formula(fit),data=data);
  ## If there are problems later on caused by omitting cluster/frailty/strata
  ## below is where they start. However, otherwise there will be interactions
  ## with those terms which seem to also create problems. Long story short,
  ## if the model already has an interaction with frailty/strata, then
  ## the contrast matrix might be wrong
  t0 <- t0[!grepl('frailty|strata|cluster',labels(t0))];
  ## obtain the full-interaction model
  ord<-max(2,sum(attr(t0,'order')==1));
  t1 <- update(t0,substitute(.~(.)^ord,list(ord=ord)));
  ## obtain the data
  if(missing(data)) data<-eval(fit$call$data);
  if(is.null(data)) {
    warning("No data argument what given when the model was fitted. Will try to extract the data another way, but it may fail.");
    if(class(fit)[1]%in%c('gls','lme','nlme','nls','gnls','lmList')) data <- getData(fit) else {
      data <- model.frame(fit);
    }
  }
  if(!is.null(fit$call$subset)) data <- subset(data,eval(fit$call$subset));
  if(class(fit)[1]=='nlme') stop('nlme models not currently supported');
  ## the following breaks for nlme objects:
  ## First, we make xlv, a named list of all factor levels while discretizing all numerics into 0 (intercept) and 1 (slope)
  xlv <- lapply(data[,unlist(sapply(attr(t0,'variables'),all.names,functions=F))[-1],drop=F],function(ii) if(is.factor(ii)) levels(ii) else 0:1);
  if(attr(t0,'response')!=0){
    resp <- all.names(t0[[2]],functions=F);
    pred <- setdiff(names(xlv),resp);
    xlv[resp]<-0;
  } else { resp <- NULL; pred <- names(xlv); }
  ## Response variable should be constant, since it plays no part in contrasts
  ## Now, which of these pseudo-factors are actually numbers?
  ## nums<-sapply(data[,names(xlv)],is.numeric);
  nums <- sapply(xlv[pred],is.numeric);
  nnames <- try(names(nums)[nums]); if(debug && class(nnames)[1]=='try-error') browser();
  fnames <- names(nums)[!nums];
  ## easy way to clean up row names later
  catcontrlabels <- lapply(xlv[fnames],function(ii) paste(ii[-1],ii[1],sep=' vs '));
  ## Create a dummy data.frame from xlv; add and name a dummy column for the response
  ## (which might break for coxph and survreg, but so far hasn't)
  d0 <- expand.grid(xlv); 
  ## obtain the huge model matrix
  ## The matrix returned has all the group-wise intercepts and all the groupwise numeric interactions
  m1 <- try(model.matrix(update(t1,y~.),cbind(y=1,d0))); if(debug && class(m1)[1] == 'try-error') browser();
  ## get rid of response variables, even multivariate ones
  d1 <- d0; for(ii in resp) d1[[ii]]<-NULL;
  ## d1 is used for matching the human readable names to the column names of the model matrix
  d1[,nnames]<-sapply(nnames,function(ii) factor(d0[[ii]],labels=c('',ii)));
  ## To catch intercept-less models... might screw up survival models, though...
  if('(Intercept)'%in%colnames(m1)){
    d1[,fnames]<-sapply(fnames,function(ii) factor(d0[[ii]],labels=replace(paste0(ii,levels(d0[[ii]])),1,'')));
  } else {
    d1[,fnames]<-sapply(fnames,function(ii) factor(d0[[ii]],labels=paste0(ii,levels(d0[[ii]]))));
  }
  ## d2 contains the human readable names
  d2 <- d1;
  d2[,fnames]<-sapply(fnames,function(ii) factor(d0[[ii]],labels=levels(d0[[ii]])));
  ## If there are no numeric variables the below will be necessary
  ## Possibly no longer necessary. Remove later. 
  if(length(d1)==0) d1 <- matrix(ncol=0,nrow=nrow(d0));
  ren<-data.frame(
         ## changed ii[nnames] to ii[nnames][ii[nnames]!=""] to get rid of extra ampersands
         clean=rgsub(apply(d2,1,function(ii) paste(paste(ii[nnames][ii[nnames]!=""],collapse=' & '),'response of',paste(ii[fnames],collapse=' '))),
           c('^$','^[ ]','[ ]$','[ ]+','[ ]*&[ ]*res','^[ ]*&[ ]*(response of )?',renfrom),c('baseline','','',' ',' res','',rento)),
         raw=rgsub(apply(d1,1,paste0,collapse=':'),c('^:',':$','^:?$',':+'),c('','','\\(Intercept\\)',':')),
         ## this later allows us to easily extract group specific effects for any continuous variable
         ## TODO: refine this to be aware of the order of interaction
         ## TODO: make ' x ' and '-x-' user-specified instead of hardcoded
         what=apply(m1,1,function(ii) if(length(oo<-intersect(names(ii)[ii==1],nnames))>0) paste(gsub(' x ','-x-',oo),collapse=' x ') else 'baseline'),
         stringsAsFactors=F);
  ## terms helps specify the formula for, e.g. minimum scope of AIC to guarantee distinct groups
  ren$term <- gsub(paste0(unlist(xlv[fnames]),collapse="|"),"",ren$raw); ren$order<-str_count(ren$raw,':');
  rownames(m1)<-apply(m1,1,function(ii) tail(colnames(m1)[ii==1],1));
  ## ...because of the missing row possibility, we order the rows as follows, instead of just m1[colnames(m1),]
  unames <- union(union(ren$raw,colnames(m1)),rownames(m1));
  inames <- intersect(intersect(ren$raw,colnames(m1)),rownames(m1));
  if(length(inames)<length(unames)){
    warning('Mismatch between raw names and/or rownames and or colnames');
    if(debug) browser();
    ren<-subset(ren,raw%in%rownames(m1));
  }
  m1 <- m1[ren$raw,]; 
  if(compactnames) rownames(m1) <- ren$clean; #rgsub(ren[,'clean'],renfrom,rento);
  ## marginal slope of each numeric variable in each categorical cell (excluding the ' x ' interactions)
  numfx<-sapply(grep(' x ',setdiff(ren$what,'baseline'),inv=T,val=T),
                function(ii) {
                  iima <- m1[ren$what==ii,,drop=F]; iinra <- nrow(iima);
                  iimb <- m1[ren$what=='baseline',,drop=F]; iinrb <- nrow(iimb);
                  if(iinra<iinrb) {
                    iima <- iima[rep(seq.int(iinra),len=iinrb),];
                  } else if(iinrb<iinra){
                    iimb <- iimb[rep(seq.int(iinra),len=iinrb),,drop=F];
                  }
                  oo <- iima - iimb;
                  ## oo<-m1[ren$what==ii,,drop=F]-m1[ren$what=='baseline',,drop=F];
                  attr(oo,'padj')<-c('x',rep(ii,len=nrow(oo)));
                  oo;
                },simplify=F);
  ## names of columns representing categoric main effects only
  catcols<-subset(ren,order==0&what=='baseline'&raw!='(Intercept)')$raw;
  ## all categoric interactions
  catfx<-sapply(catcols,
                function(ii) {
                  iifac <- gsub(sprintf('(%s).*',paste0(fnames,collapse='|')),'\\1',ii);
                  iiprefix <- catcontrlabels[[iifac]];
                  iima <- m1[ren$what=='baseline'&m1[,ii]!=0,,drop=F]; iinra <- nrow(iima);
                  iimb <- m1[ren$what=='baseline'&m1[,ii]==0,,drop=F]; iinrb <- nrow(iimb);
                  if(iinra<iinrb){
                    iima <- iima[rep(seq.int(iinra),len=iinrb),,drop=F];
                  } else if(iinrb<iinra){
                    iimb <- iimb[rep(seq.int(iinra),len=iinrb),,drop=F];
                  }
                  oo<-try(rbind(NA,iima-iimb));
                  if(class(oo)[1]=='try-error') browser();
                  ## ooerr<-try(rownames(oo)<-gsub(sprintf('(.*)[ ]?%s[ ]?(.*)',xlv[[iifac]][-1]),sprintf('%s, \\1\\2',iiprefix),rownames(oo)));
                  ooerr<-try(rownames(oo)<-gsub(sprintf('(.*)[ ]?%s[ ]?(.*)',
                                                        gsub('([+-])',"\\\\\\1",xlv[[iifac]][-1])),
                                                sprintf('%s, \\1\\2',iiprefix),rownames(oo)));
                  ## gsub("([+-])","\\\\\\1","+/-")
                  if(class(ooerr)[1]=='try-error' && debug) browser();
                  attr(oo,'label')<-iiprefix;
                  attr(oo,'padj')<-c('x',rep(iifac,len=nrow(oo)-1));
                  oo;
                },simplify=F);
  ## trim the level names from the categoric effects to make them user friendly
  names(catfx)<-sapply(catfx,attr,'label');
  catnumfx<-do.call(c,sapply(catcols,function(jj){
    jjfac <- gsub(sprintf('(%s).*',paste0(fnames,collapse='|')),'\\1',jj);
    oo<-lapply(numfx,function(ii){
      iijjma <- ii[m1[ren$what=='baseline',jj]!=0,,drop=F]; iijjnra <- nrow(iijjma);
      iijjmb <- ii[m1[ren$what=='baseline',jj]==0,,drop=F]; iijjnrb <- nrow(iijjmb);
      if(iijjnra<iijjnrb){
        iijjma <- iijjma[rep(seq.int(iijjnra),len=iijjnrb),,drop=F];
      } else if(iijjnrb<iijjnra){
        iijjmb <- iijjmb[rep(seq.int(iijjnrb),len=iijjnra),,drop=F];
      }
      rbind(NA,iijjma-iijjmb);
      ## rbind(NA,ii[m1[ren$what=='baseline',jj]!=0,,drop=F]-ii[m1[ren$what=='baseline',jj]==0,,drop=F])
    });
    jjprefixes<-setNames(sprintf('%s response to %s',catcontrlabels[[jjfac]],names(oo)),names(oo));
    oo<-sapply(names(oo),function(ii) {
      rownames(oo[[ii]])<-gsub(
                            sprintf('%s response of[ ]?(.*)[ ]?%s[ ]?(.*)[ ]?',ii,
                                    paste0(gsub('([+-])',"\\\\\\1",xlv[[jjfac]][-1]),collapse='|')),
                            sprintf('%s, \\1\\2',jjprefixes[[ii]]),
                            rownames(oo[[ii]]));
      attr(oo[[ii]],'padj')<-c('x',rep(paste(ii,jj,sep='.'),len=nrow(oo[[ii]])-1));
      oo[[ii]];
    },simplify=F); names(oo)<-jjprefixes; oo;
    },simplify=F));
  names(catnumfx)<-gsub(sprintf('(%s)\\.',paste0(catcols,collapse='|')),'',names(catnumfx));
  if(length(numfx)>0) numfx<-setNames(lapply(numfx,function(ii) {
    rownames(ii)<-paste(rownames(ii),'group');
    iipadj<-attr(ii,'padj');
    ii<-rbind(NA,ii);
    attr(ii,'padj')<-iipadj;
    ii;
  }),paste(names(numfx),'response in each group')) else {
    numfx <- NULL;
  }
  oo<-c(raw=list(m1),numfx,catfx,catnumfx);
  attr(oo,'contrinfo')<-ren;
  ## (TODO): give the blank rows a descriptive name!
  ## TODO: a numeric indicator for DF
  ## (TODO): each matrix in list has a numeric indicator attribute
  invisible(oo);
}
