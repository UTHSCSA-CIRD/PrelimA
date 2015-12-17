rgsub <-
function(xx,from='',to='',ll=NULL,...){
  ## xx = string/s on which to do each replacement
  ## from = regexp to replace
  ## to = what to replace it with
  ## ll = a null value lets rgsub know it's the first iteration
  if(is.null(ll)) {
    ll<-max(length(from),length(to)); from<-rep(from,len=ll); to<-rep(to,len=ll);
  }
  if(ll==1) return(gsub(from,to,xx,...)) else {
    return(gsub(from[1],to[1],rgsub(xx,from[-1],to[-1],ll=ll-1,...)));
  }
}
