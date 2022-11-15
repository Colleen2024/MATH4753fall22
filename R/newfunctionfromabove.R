#' mycltu (Uniform central limit function)
#'
#' @param iter iter
#' @param n n
#'
#'
#' @return histogram for a uniform central limit function
#' @export
#'
#' @examples
#' \dontrun{mycltu(1,10000,0,10)}'
#'
newfunctionfromabove=function(n,iter,a=0,b=10){
y=runif(n*iter,a,b)
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
w=apply(data,2,mean)
param=hist(w,plot=FALSE)


ymax=max(param$density)
ymax=1.1*ymax
hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                               "\n", "sample size= ",n,sep=""),xlab="Sample mean")
lines(density(w),col="Blue",lwd=3)
curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
curve(dunif(x,a,b),add=TRUE,lwd=4)

}
