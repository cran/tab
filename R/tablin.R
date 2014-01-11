tablin <-
function(x,y,xlabels=NULL,int=TRUE,decimals=2,n=TRUE,coef="n") {
  
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
  if (coef %in% c("x","xy")) {
    for (ii in 1:colx) {
      if (!is.factor(x[,ii]) & length(unique(x[,ii]))>2) {
        x[,ii] = (x[,ii]-mean(x[,ii]))/sd(x[,ii])
      }
    }
  }
  if (coef=="xy") {
    y = (y-mean(y))/sd(y)
  }
  
  # Run linear regression depending on number of x variables
  if (colx==1) {fit = summary(lm(y~x[,1]))}
  if (colx==2) {fit = summary(lm(y~x[,1] + x[,2]))}
  if (colx==3) {fit = summary(lm(y~x[,1] + x[,2] + x[,3]))}
  if (colx==4) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4]))}
  if (colx==5) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5]))}
  if (colx==6) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6]))}
  if (colx==7) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7]))}
  if (colx==8) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8]))}
  if (colx==9) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8] + x[,9]))}
  if (colx==10) {fit = summary(lm(y~x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8] + x[,9] + x[,10]))}
  
  # If intercept requested, adjust rows and xlabels
  if (int==TRUE) {
    rows = c(1,rows)
    xlabels = c("Intercept",xlabels)
    colx = colx + 1
  }
  
  # Initialize table
  tbl = matrix("",nrow=sum(rows),ncol=5)
  tbl[1,2] = sum(locs)
  
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
      p = fit$coefficients[coef.index,4]
      tbl[tbl.index,3] = paste(sprintf(spf,beta)," (",sprintf(spf,se),")",sep="")
      tbl[tbl.index,4] = paste("(",sprintf(spf,beta-1.96*se),", ",sprintf(spf,beta+1.96*se),")",sep="")
      if (p<0.001) {
        tbl[tbl.index,5] = "<0.001"
      } else {
        tbl[tbl.index,5] = sprintf("%.3f",p)
      }
    } else {
      tbl[(tbl.index+2),3:5] = "-"
      tbl.index = tbl.index+2
      for (jj in 1:(rows[ii]-2)) {
        coef.index = coef.index+1
        tbl.index = tbl.index+1
        beta = fit$coefficients[coef.index,1]
        se = fit$coefficients[coef.index,2]
        p = fit$coefficients[coef.index,4]
        tbl[tbl.index,3] = paste(sprintf(spf,beta)," (",sprintf(spf,se),")",sep="")
        tbl[tbl.index,4] = paste("(",sprintf(spf,beta-1.96*se),", ",sprintf(spf,beta+1.96*se),")",sep="")
        if (p<0.001) {
          tbl[tbl.index,5] = "<0.001"
        } else {
          tbl[tbl.index,5] = sprintf("%.3f",p)
        }
      }
    }
  }
  
  # Add column names
  colnames(tbl) = c("Variable","N","Beta (SE)","95% CI","p-value")
  
  # Add variable names
  tbl[1:nrow(tbl)] = xlabels
  
  # Drop N column if requested
  if (n==FALSE) {
    tbl = tbl[,c(1,3:5)]
  }
  
  # Return table
  return(tbl)
  
}
