tabfreq <-
function(x,y,test="chi",xlevels=NULL,ylevels=NULL,yname="Y variable",decimals=1,n=TRUE,compress=FALSE) {
  
  # Convert decimals to variable for sprintf
  spf = paste("%0.",decimals,"f",sep="")
  
  # Get cell counts and proportions
  counts = table(y,x)
  props = 100*prop.table(table(y,x))
  colprops = 100*prop.table(table(y,x),2)
  
  # If ylevels unspecified, set to actual values
  if (is.null(ylevels)) {
    ylevels = rownames(counts)
  }
  
  # Initialize table
  tbl = matrix("",nrow=nrow(counts)+1,ncol=ncol(counts)+3) 
  
  # Add variable name and levels of Y to first row
  tbl[,1] = c(paste(yname,", n (%)",sep=""),paste("  ",ylevels,sep=""))
  
  # n (%) in each level of y
  tbl[2:nrow(tbl),2] = paste(sprintf("%.0f",rowSums(counts))," (",sprintf("%.1f",rowSums(props)),")", sep="")
  
  # n (%) for each cell
  for (i in 1:nrow(counts)) {
    for (j in 1:ncol(counts)) {
      tbl[i+1,j+2] = paste(sprintf("%.0f",counts[i,j])," (",sprintf("%.1f",colprops[i,j]),")",sep="")
    }
  }
  
  # Statistical test
  if (nrow(counts)==1) {
    pval = "-"
  } else {
    if (test=="chi") {
      pval = chisq.test(x=x,y=y)$p.value
    } else if (test=="fisher") {
      pval = fisher.test(x=x,y=y)$p.value
    }
    if (pval<0.001) {
      pval = "<0.001"
    } else {
      pval = sprintf("%.3f",round(pval,3))
    }
  }
  tbl[1,ncol(tbl)] = pval
  
  # If x binary and compress is TRUE, compress table to a single row
  if (nrow(counts)<=2 & compress==TRUE) {
    tbl = matrix(c(tbl[1,1],tbl[nrow(tbl),2:4],tbl[1,5]),nrow=1)
  }
  
  # If xlevels unspecified, set to actual values
  if (is.null(xlevels)) {
    xlevels = colnames(counts)
  }
  
  # If requested, include n's in xlevels labels
  if (n==TRUE) {
    xlevels = paste(xlevels," (n = ",colSums(counts),")",sep="")
    overall = paste("Overall (n = ",sum(counts),")",sep="")
  } else {
    overall = "Overall"
  }
  
  # Add column names
  colnames(tbl) = c("Variable",overall,xlevels,"p-value")
  
  # Return table
  return(tbl)
  
}
