#' myboot2 (Bootstrap esimators)
#'
#' @param iter iter
#' @param n n
#'
#'
#' @return histogram for bootstrap sample statistics
#' @export
#'
#' @examples
#' \dontrun{myboot2(iter=10000, sam1, fun="mratio", alpha=0.30)}'
#'
#'
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,...){

  n=length(x)

  y=sample(x,n*iter,replace=TRUE)

  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,main= "Bootstrap sample statistics",...)

  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=1.5)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=1.5)

  text(pte,max(para$density)/2,round(pte,2),cex=1.5)

  return(list(ci=ci, fun=fun, x=x, xstat=xstat))
}
