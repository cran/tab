tabmeans <-
function(x,y,xlevels=NULL,yname="Y variable",decimals=1,n=TRUE,se=FALSE) {
  
  # Convert decimals to variable for sprintf
  spf = paste("%0.",decimals,"f",sep="")
  
  # Get unique values of x
  xvals = sort(unique(x))
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels = xvals
  }
  
  # Initialize table
  tbl = matrix("",nrow=1,ncol=3+length(xlevels))
  
  # Get means and SD's or SE's and add variable name and 1st cell entry to table
  means = tapply(X=y,INDEX=x,FUN=mean)
  ns = tapply(X=y,INDEX=x,FUN=length)
  if (se==FALSE) {
    vars = tapply(X=y,INDEX=x,FUN=sd)
    tbl[1,1] = paste(yname,", M (SD)",sep="")
    tbl[1,2] = paste(sprintf(spf,mean(y[!is.na(x)]))," (",sprintf(spf,sd(y[!is.na(x)])),")",sep="")
  } else {
    vars = tapply(X=y,INDEX=x,FUN=function(x) sd(x)/sqrt(length(x)))
    tbl[1,1] = paste(yname,", M (SE)",sep="")
    tbl[1,2] = paste(sprintf(spf,mean(y[!is.na(x)]))," (",sprintf(spf,sd(y[!is.na(x)])/sqrt(length(y[!is.na(x)]))),")",sep="")
  }
  
  # Add mean (SD/SE) values to table
  tbl[1,3:(ncol(tbl)-1)] = paste(sprintf(spf,means)," (",sprintf(spf,vars),")",sep="")
  
  # Add p-value based on ANOVA or t-test depending on number of levels of x
  if (length(xlevels)==2) {
    
    # F test for equal variances then appropriate t-test
    f = var.test(x=y[x==xvals[1]],y=y[x==xvals[2]])
    if (f$p.value<0.05) {
      p = t.test(x=y[x==xvals[1]],y=y[x==xvals[2]],var.equal=FALSE)$p.value
    } else {
      p = t.test(x=y[x==xvals[1]],y=y[x==xvals[2]],var.equal=TRUE)$p.value
    }
    
  } else {
    
    # ANOVA
    p = anova(lm(y~as.factor(x)))$"P"[1]
    
  }
  
  # Add p-value from t-test
  if (p<0.001) {
    tbl[1,ncol(tbl)] = "<0.001"
  } else {
    tbl[1,ncol(tbl)] = sprintf("%0.3f",p)
  }
  
  # If requested, include n's in xlevels labels
  if (n==TRUE) {
    xlevels = paste(xlevels," (n = ",tapply(X=y,INDEX=x,FUN=length),")",sep="")
    overall = paste("Overall (n = ",sum(complete.cases(x) & complete.cases(y)),")",sep="")
  } else {
    overall = "Overall"
  }
  
  # Add column names
  colnames(tbl) = c("Variable",overall,xlevels,"p-value")
  
  # Return table
  return(tbl)
  
}
