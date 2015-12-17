unifx.list <-
function(fit,...,WHATROW=T,.killrownames=T){
  islist <- sapply(fit,function(ii) class(ii)[1])=='list';
  oo <- rbind(
          do.call(rbind,lapply(fit[!islist],function(ii) unifx(ii,...))),
          do.call(rbind,lapply(fit[islist],function(ii) unifx(ii,...,WHATROW=WHATROW)))
          );
  if(WHATROW) oo <- data.frame(What=gsub('\\.$','',mapply(function(aa,bb) gsub(paste(".",aa,sep=''),'',bb,fixed=T),oo$Term,rownames(oo))),oo);
  if(.killrownames) rownames(oo) <- NULL;
  oo;
}
