tablog <-
function(x,y,xlabels=NULL,int=TRUE,decimals=2,n=TRUE,events=TRUE,coef="n") {
  
  # Convert decimals to variable for sprintf
  spf = paste("%0.",decimals,"f",sep="")
  
  # Set x to data frame if not already
  x = as.data.frame(x)
  colx = ncol(x)
  
  # Drop observations with missing values for one or more predictors
  locs = complete.cases(x) & !is.na(y)
  x = as.data.frame(x[locs,])
  y = y[locs]
  
  # Get number of levels in each variable in x
  rows = c()
  for (ii in 1:colx) {
    if (!is.factor(x[,ii]) | (is.factor(x[,ii])&length(unique(x[,ii]))==2)) {
      rows[ii] = 1
    } else {
      rows[ii] = length(unique(x[,ii]))+1
    }
  }
  
  # If xlabels not specified, create generic values
  if (is.null(xlabels)) {
    xlabels = c()
    index = 0
    for (ii in 1:colx) {
      if (rows[ii]==1) {
        index = index + 1
        xlabels[index] = paste("Predictor ",ii,sep="")
      } else {
        index = index + 1
        xlabels[index] = paste("Predictor ",ii,sep="")
        index = index + 1
        xlabels[index] = "Level 1 (ref)"
        for (jj in 2:(rows[ii]-1)) {
          index = index + 1
          xlabels[index] = paste("Level ",jj,sep="")
        }
      }
    }
  }
  
  # Add spaces in front of levels of factor variables for better appearance
  for (ii in 1:length(rows)) {
    if (ii==1 & rows[ii]>1) {
      xlabels[2:rows[ii]] = paste("  ",xlabels[2:rows[ii]],sep="")
      xlabels[2] = paste(xlabels[2]," (ref)",sep="")
    }
    if (ii>1 & rows[ii]>1) {
      xlabels[(sum(rows[1:(ii-1)])+2):sum(rows[1:ii])] = paste("  ",xlabels[(sum(rows[1:(ii-1)])+2):sum(rows[1:ii])],sep="")
      xlabels[(sum(rows[1:(ii-1)])+2)] = paste(xlabels[(sum(rows[1:(ii-1)])+2)]," (ref)",sep="")
    }
  }
  
  # Standardize variables if necessary
  if (coef=="x") {
    for (ii in 1:colx) {
      if (!is.factor(x[,ii]) & length(unique(x[,ii]))>2) {
        x[,ii] = (x[,ii]-mean(x[,ii]))/sd(x[,ii])
      }
    }
  }
  
  # Run logistic regression depending on number of x variables
  if (colx==1) {fit = summary(glm(y~x[,1]),family=binomial)}
  if (colx==2) {fit = summary(glm(y~x[,1] + x[,2]),family=binomial)}
  if (colx==3) {fit = summary(glm(y~x[,1] + x[,2] + x[,3]),family=binomial)}
  if (colx==4) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4]),family=binomial)}
  if (colx==5) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5]),family=binomial)}
  if (colx==6) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6]),family=binomial)}
  if (colx==7) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7]),family=binomial)}
  if (colx==8) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8]),family=binomial)}
  if (colx==9) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8] + x[,9]),family=binomial)}
  if (colx==10) {fit = summary(glm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8] + x[,9] + x[,10]),family=binomial)}
  
  # If intercept requested, adjust rows and xlabels
  if (int==TRUE) {
    rows = c(1,rows)
    xlabels = c("Intercept",xlabels)
    colx = colx + 1
  }
  
  # Initialize table
  tbl = matrix("",nrow=sum(rows),ncol=7)
  tbl[1,2] = sum(locs)
  tbl[1,3] = sum(y)
  
  # Enter values in table
  coef.index = 1
  if (int==TRUE) {coef.index = 0}
  tbl.index = 0
  for (ii in 1:colx) {
    if (rows[ii]==1) {
      coef.index = coef.index+1
      tbl.index = tbl.index+1
      beta = fit$coefficients[coef.index,1]
      se = fit$coefficients[coef.index,2]
      or = exp(beta)
      p = fit$coefficients[coef.index,4]
      tbl[tbl.index,4] = paste(sprintf(spf,beta)," (",sprintf(spf,se),")",sep="")
      tbl[tbl.index,5] = sprintf(spf,or)
      tbl[tbl.index,6] = paste("(",sprintf(spf,exp(beta-1.96*se)),", ",sprintf(spf,exp(beta+1.96*se)),")",sep="")
      if (p<0.001) {
        tbl[tbl.index,7] = "<0.001"
      } else {
        tbl[tbl.index,7] = sprintf("%.3f",p)
      }
    } else {
      tbl[(tbl.index+2),4:7] = "-"
      tbl.index = tbl.index+2
      for (jj in 1:(rows[ii]-2)) {
        coef.index = coef.index+1
        tbl.index = tbl.index+1
        beta = fit$coefficients[coef.index,1]
        se = fit$coefficients[coef.index,2]
        or = exp(beta)
        p = fit$coefficients[coef.index,4]
        tbl[tbl.index,4] = paste(sprintf(spf,beta)," (",sprintf(spf,se),")",sep="")
        tbl[tbl.index,5] = sprintf(spf,or)
        tbl[tbl.index,6] = paste("(",sprintf(spf,exp(beta-1.96*se)),", ",sprintf(spf,exp(beta+1.96*se)),")",sep="")
        if (p<0.001) {
          tbl[tbl.index,7] = "<0.001"
        } else {
          tbl[tbl.index,7] = sprintf("%.3f",p)
        }
      }
    }
  }
  
  # Remove OR information for intercept
  if (int==TRUE) {
    tbl[1,5:6] = "-"
  }
  
  # Add column names
  colnames(tbl) = c("Variable","N","Events","Beta (SE)","OR","95% CI for OR","p-value")
  
  # Add variable names
  tbl[1:nrow(tbl)] = xlabels
  
  # Drop particular columns if requested
  if (n==FALSE) {tbl = tbl[,colnames(tbl)!="N"]}
  if (events==FALSE) {tbl = tbl[,colnames(tbl)!="Events"]}
  
  
  # Return table
  return(tbl)
  
}
