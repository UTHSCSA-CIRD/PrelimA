## for a unicontr object, find the formula for the smallest marginality-preserving model that supports the contrasts the user selects
minmodel <- function(contr,frm,cm,debug=F){
  ## contr = vector of names for matrices within cm
  ## frm = formula
  ## cm = unicontr object
  if(!all(contr%in%names(cm))){
    cat('\n\nSome contrasts not found in cm\n');
    if(debug) browser() else {
      contr <- intersect(contr,names(cm));
    }
  }
  addterms <- try(unique(unlist(sapply(cm[contr],function(ii){
    apply(ii[-1,,drop=F],1,function(jj)colnames(ii)[jj!=0])
  },simplify=F))));
  if(class(addterms)[1]=='try-error') {
    cat('\n\naddterms errored out with',class(addterms),'\n');
    if(debug) browser();
  }
  ## convert the coef-format terms to raw-format (model-format) terms
  addterms <- subset(attr(cm,'contrinfo'),raw%in%addterms)$term;
  ## swap out ':' to insure marginality and paste together
  addterms <- paste(gsub(':','*',addterms),collapse='+');
  ## wrapping in update should get rid of redundant terms
  frmcon <- try(update(frm,as.formula(paste(".~.+",addterms))));
  if(class(frmcon)[1]=='try-error') {
    cat('frmcon errored out with',class(frmcon),'\n');
    if(debug) browser();
  } else {
    environment(frmcon) <- NULL;
    frmcon;
  }
}
