## Let's Run Through It
setwd("C:/Users/.../TalkMaterials")

# Generate All Input Files
getwd()
createModels("knitr_template.txt")

# Extract all model information
mplus.Extract <- readModels(recursive = TRUE)

# Compile All Results
# Must create empty matrices to store all information:

#1. Parameter estimates - 13 cols
#2. Standard Errors - 13 cols
#3. Rep & Cond IDs - 2 cols
#4. Model Information - 4 cols

## Matrix Storage
paramE <- matrix(NA,ncol = 13, nrow = 150)
seE <- matrix(NA,ncol = 13, nrow = 150)
simR.C <- matrix(NA,ncol = 2, nrow = 150)
sumStat <- matrix(NA,ncol = 4,nrow = 150)

# Each matrix needs 150 rows, corresponding to each simulation run


for (i in 1:150){
#### Summary Stats
paramE[i,] <- unlist(mplus.Extract[[i]]$parameters$unstandardized[c(1,17:28),3])

seE[i,] <- ifelse(ncol(mplus.Extract[[i]]$parameters$unstandardized) < 7,
				NA,
				unlist(mplus.Extract[[i]]$parameters$unstandardized[c(1,17:28),4]))

#### Summary Stats
	sumStat[i,1] <- ifelse(length(mplus.Extract[[i]]$summaries) < 14,
					NA,
					unlist(mplus.Extract[[i]]$summaries$Parameters))
	sumStat[i,2] <- ifelse(length(mplus.Extract[[i]]$summaries) < 14,
						NA,
						unlist(mplus.Extract[[i]]$summaries$LL))
	sumStat[i,3] <- ifelse(length(mplus.Extract[[i]]$summaries) < 14,
					NA,
					unlist(mplus.Extract[[i]]$summaries$AIC))

	sumStat[i,4] <- ifelse(length(mplus.Extract[[i]]$summaries) < 14,
					NA,
					unlist(mplus.Extract[[i]]$summaries$BIC))

#### Data set identification
repID <- unlist(mplus.Extract[[i]]$summaries$Filename)

splt <- strsplit(repID,c("_",".out"))

# Notice that there is no "i" subscript

# This is okay, as it will be overwritten after each increment and stored in its respective matrix.
simR.C[i,2] <- as.numeric(splt[[1]][2])
simR.C[i,1] <- as.numeric(strsplit(splt[[1]][3],".out")[1])
}

## Post Processing
RESULTS <- cbind(paramE,seE,sumStat,simR.C)

paramID.1 <- unlist(mplus.Extract[[2]]$parameters$unstandardized[c(1,17:28),1])
	paramID.2 <- unlist(mplus.Extract[[2]]$parameters$unstandardized[c(1,17:28),2])

paramName <- paste0(paramID.1,".",paramID.2)

gColNames <- paste0(paramName,".se")

colnames(RESULTS) <- c(goodNames,"n.param","-2LL","AIC","BIC","n.rep","n.cond")

#### All Done!
head(RESULTS)