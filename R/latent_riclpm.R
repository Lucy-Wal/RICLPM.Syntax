#' Function to generate syntax for multiple indicator RICLPM
#' Inputs:
#'v = name want to give each variable. E.g., c("Var1","Var2", "Var3") 
#'items = item range for each variable. E.g., c(3, 4, 6)  
#'time = numeric number of time waves E.g., 4 
#constraint = boolean - set to T if wish to add constraints to cross-lags
#resid = boolean - set to T if wish to include residual error terms

#' @export
latent_riclpm <- function(v, items, time, constraint, resid){
  
  alphabet <- paste0(letters)  
  start <- 1
  cfa <- c("\n")
  cfa <- c(cfa, "##Factors##")
  
  ##Specifying Factors
  #For each variable
  for(var in 1:length(v)){
    #Create a dataframe of the strings by time (row) and item (column)  
    data <- data.frame(1:time)
    for (item in (1:items[var])){ 
      col <- c()
      for(t in (1:time)){
        col <- c(col, paste0("T", t, "_", v[var], item, " + "))}
      data[[paste0(v[var], item)]] <- col}
    
    #Format data frame
    data <- data[, -c(1)]   #remove time row
    data_Item <- data
    data_Item[, ncol(data_Item)] <- gsub("[ + ]", "", data_Item[, ncol(data_Item)])   #remove + from final col 
    
    for(col in 1:ncol(data_Item)){data_Item[, col] <- paste0("f", alphabet[start + (col - 1)], "*", data_Item[, col])}
    start <- start + ncol(data_Item)
    
    #Write into CFA Syntax
    cfa <- c(cfa, "\n")
    for (row in 1:nrow(data_Item)){
      line <- capture.output(cat(paste0("F", v[var], row, " =~ "), unlist(data_Item[row,])))
      cfa <- c(cfa, "\n", line)}}
  
  ##Specifying Residuals
  cfa <- c(cfa, "\n")
 if(resid == T){
   cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Residuals##")
  
  for(var in 1:length(v)){
    #Create a dataframe of the strings by time (row) and item (column)  
    data <- data.frame(1:time)
    for (item in (1:items[var])){ 
      col <- c()
      for(t in (1:time)){
        col <- c(col, paste0("T", t, "_", v[var], item, " + "))}
      data[[paste0(v[var], item)]] <- col}
    
    #Format data
    data <- data[, -c(1)]   #remove time row
    data_Time <- data
    data_Time[nrow(data_Time),] <- gsub("[ + ]", "", data_Time[nrow(data_Time),])   #remove + from final row
    
    cfa <- c(cfa, "\n")
    for(item in 1:ncol(data)){
      for(t in 1:(nrow(data)-1)){
        first <- gsub("[ + ]", "", data_Time[t,item])
        line <- capture.output(cat(first, paste0(" ~~ "), unlist(data_Time[(t+1):nrow(data),item])))
        cfa <- c(cfa, "\n", line)}
      cfa <- c(cfa, "\n")}}
 } else{cfa <- c(cfa, "\n")}
  
  
  ##Random Intercepts
  cfa <- c(cfa, "\n", "##Random Intercepts##", "\n")
  
  for(var in 1:length(v)){
    #Create a dataframe of the strings by time (row) and item (column)  
    data <- data.frame(1:time)
    for (t in (1:time)){ 
      col <- c(paste0("1*F", v[var], t, " + "))
      data[t,1] <- col}
    
    data[time, 1] <- gsub("[ + ]", "", data[t, 1])   #remove + from final 
    
    line <- capture.output(cat(paste0("RI_", v[var], " =~ "), unlist(data[,1])))
    cfa <- c(cfa, "\n", line)}
  
  #Residual Variances to 0
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##F variance to 0##")
  
  for(var in 1:length(v)){
    cfa <- c(cfa, "\n")
    #Create a dataframe of the strings by time (row) and item (column)  
    data <- data.frame(1:time)
    for (t in (1:time)){ 
      col <- c(paste0("F", v[var], t))
      data[t,1] <- col}
    for (t in (1:time)){ 
      line <- capture.output(cat(paste0(data[t,1], " ~~ 0*", data[t,1])))
      cfa <- c(cfa, "\n", line)}}
  
  
  #Within Person Variables
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Within Person Variables##")
  
  for(var in 1:length(v)){
    cfa <- c(cfa, "\n")
    #Create a dataframe of the strings by time (row) and item (column)  
    data <- data.frame(1:time)
    for (t in (1:time)){ 
      col <- c(paste0(v[var], t))
      data[t,1] <- col}
    for (t in (1:time)){ 
      line <- capture.output(cat(paste0("w", tolower(data[t,1]), " =~ 1*F", data[t,1])))
      cfa <- c(cfa, "\n", line)}}
  
  
  #Cross-lag regressions
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Cross Lag Regressions##")
  cfa <- c(cfa, "\n")

  letter_index <- 1:length(v)
  for (var in 1:length(v)){
    if(var > 1){letter_index <- (letter_index + (length(v)))}
    cfa <- c(cfa, "\n")
  for(t in 2:time){
    if(constraint == T){
      data <- as.data.frame(paste0(alphabet[letter_index], "*w",  tolower(v), t-1, " +"))
    } else {data <- as.data.frame(paste0("w",  tolower(v), t-1, " +"))}
      data[nrow(data),] <- gsub("[ + ]", "", data[nrow(data),])
      dv <- paste0("w", tolower(v[var]), t, " ~")
      iv <- capture.output(cat(unlist(data[,1])))
      line <- capture.output(cat(dv, iv))
      cfa <- c(cfa, line, "\n")
    }
  }
  
  #Residual Covariances in Same Wave
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Residual Covariances in Same Wave##")
  
  for (var in 1:(length(v)-1)){
    for(v2 in (var + 1):length(v)){
      cfa <- c(cfa, "\n")
      for(t in 1:time){
        line <- capture.output(cat(paste0("w", tolower(v[var]), t, " ~~ ", "w", tolower(v[v2]), t)))
        cfa <- c(cfa, "\n", line)}
    }}
  
  
  #Variances and Covariances of Random Intercepts
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Variances and Covariances of Random Intercepts##")
  cfa <- c(cfa, "\n")
  for(var in 1:length(v)){
    line <- capture.output(cat(paste0("RI_", v[var], " ~~ ", "RI_", v[var])))
    cfa <- c(cfa, "\n", line)}
  
  cfa <- c(cfa, "\n")
  
  for (var in 1:(length(v)-1)){
    for(v2 in (var + 1):length(v)){
      line <- capture.output(cat(paste0("RI_", v[var], " ~~ ", "RI_", v[v2])))
      cfa <- c(cfa, "\n", line)}
  }
  
  #Fix correlations between random intercepts and other exogenous variables to 0
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Fix correlations between random intercepts and other exogenous variables to 0##")
  cfa <- c(cfa, "\n")
  
  data <- as.data.frame((paste0("RI_", v, " + ")))
  data$V2 <- paste0("0*w", tolower(v), "1 + ")
  data[nrow(data),] <- gsub("[ + ]", "", data[nrow(data),])   #remove + from final row
  line <- capture.output(cat(unlist(data[,1]), " ~~ ", unlist(data[,2])))
  cfa <- c(cfa, "\n", line)
  
  
  
  return(cfa)
}
