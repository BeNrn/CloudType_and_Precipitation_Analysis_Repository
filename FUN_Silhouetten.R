#'Rework the Silhouette function in Handl, A., & Kuhlenkasper, T. (2017). Multivariate Analysemethoden. https://doi.org/10.1007/978-3-662-54754-0 (Kapitel 13 - Clusteranalyse)
#'
#'@description The result of a k-means cluster analysis is validated and visualized. 
#'
#'@param cluster The k-means cluster analysis result.
#'@param distMat The distance matrix of the data.
#'@param dat The data as a vector.
#'
#'@return

silhouette_params <- function(cluster, distMat, dat){
  #1.df with basic structure of the data
  #-------------------------------------
  #number of groups
  groupNumber <- cluster$cluster %>% unique() %>% length()
  
  #introduce the df
  df <- data.frame(elementNumber = NULL, index = NULL, groupNum = NULL)
  for(i in 1:groupNumber){
   #data values for class i
   groupElements <- dat[cluster$cluster == i]
   #index of each element 
   distCalc<- which(dat %in% groupElements)
   df_temp <- data.frame(elementNumber = groupElements, index = distCalc, groupNum = i)
   df <- rbind(df,df_temp)
  }
  #sort by index
  df <- df[order(df$index),]
  
  #2.Calculate the mean distance of every element to the other elements in its group
  #---------------------------------------------------------------------------------
  #add a column named "a" (the distance of the element to each other element in the same group)
  df$a <- NA
  
  #distance within group calculation
  for(i in 1:nrow(df)){
    #all indices of the same group
    allDist <- distMat[df$index[i],]
    indexDist <- df$index[df$groupNum == df$groupNum[i]]
    inGroupDist <- allDist[indexDist]
    inGroupDist <- inGroupDist[inGroupDist != 0]
    #calculate a (in group distance)
    df$a[i] <- mean(inGroupDist)
  }
  
  #3. calculate the mean distance to each other group and pick the nearest other group
  #-----------------------------------------------------------------------------------
  #distance to all other groups df
  outGroupDF <- data.frame(index = NULL, ingroup = NULL, outgroup = NULL, b = NULL)
  
  #for every element
  for(i in 1:nrow(df)){
    groupNames <- unique(df$groupNum)
    groupNames <- sort(groupNames)
    #all other group numbers/names
    groupNames <- groupNames[-df$groupNum[i]]
    #for all other groups
    for(j in 1:length(groupNames)){
      allDist <- dm[df$index[i],]
      index_others <- df$index[df$groupNum == groupNames[j]]
      outGroupDist <- allDist[index_others]
      #mean of the out group distance to group j (for more than 2 groups there are
      #multiple other groups)
      b <- mean(outGroupDist)
      #store them in a df
      outGroupDF_temp <- data.frame(index = df$index[i], ingroup = df$groupNum[i],
                                    outgroup = groupNames, meanDist = b)
      outGroupDF <- rbind(outGroupDF, outGroupDF_temp)
    }
  }
  #add the nearest group and its b-value
  df$nearestGroup <- NA
  df$b <- NA
  for(i in 1:nrow(df)){
    distToAllOtherGroups <- outGroupDF[outGroupDF$index == i,]
    nearestGroup <- distToAllOtherGroups$outgroup[distToAllOtherGroups$meanDist == min(distToAllOtherGroups$meanDist)]
    df$nearestGroup[i] <- nearestGroup
    df$b[i] <- min(distToAllOtherGroups$meanDist)
  }
  
  #4. calculate s(i)
  #-----------------
  df$s_i <- NA
  
  for(i in 1:nrow(df)){
    df$s_i[i] <- ((df$b[i] - df$a[i])/pmax(df$a[i], df$b[i]))
  }
  
  #5. mean s(i) over all groups
  #----------------------------
  meanSI <- numeric(0)
  for(i in 1:length(unique(df$groupNum))){
    meanSI_temp <- df$s_i[df$groupNum == i] %>% mean()
    meanSI <- c(meanSI, meanSI_temp)
  }
  
  #6.Silhouette coefficient
  #------------------------
  silcoeff <- mean(df$s_i)
  
  #7. data output
  #---------------
  #sort the data first by group (for loop), than by s_i value (order argument)
  df_total <- subset(df, subset = FALSE)
  for(i in 1:length(unique(df$groupNum))){
    df_temp <- df[df$groupNum == i,]
    df_temp <- df_temp[order(df_temp$s_i, decreasing = TRUE),] %>% print()
    df_total <- rbind(df_total, df_temp)
  }
  df <- df_total
  rm(df_total)
  
  dat <- matrix(0, nrow(df), 3)
  dat[,1] <- df$groupNum
  dat[,2] <- df$nearestGroup
  dat[,3] <- df$s_i
  #adjust to strange plot function
  #name the columns by index
  dimnames(dat) <- list(df$index)

  out <- list(dat, meanSI, silcoeff)
  return(out)
}