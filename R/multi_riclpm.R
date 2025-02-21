#' Function to generate syntax for multiple indicator RICLPM
#' Inputs:
#'v = name want to give each variable. E.g., c("Var1","Var2", "Var3") 
#'items = item range for each variable. E.g., c(3, 4, 6)  
#'time = numeric number of time waves E.g., 4 
#'constraint = "config" (default), "metric", or "scalar"


#' @export
multiple_riclpm <- function(v, items, time, constraint = F){
  
  alphabet <- paste0(letters)  
  start <- 1
  cfa <- c("\n")
  ##Random Intercepts
  cfa <- c(cfa, "\n", "##Random Intercepts##", "\n")
    
    for(var in 1:length(v)){
      #Create a dataframe of the strings by time (row) and item (column)  
      data <- data.frame(1:time)
      for (item in (1:items[var])){ 
        col <- c(paste0("1*T", 1:time, "_", v[var], item, " + "))
        data[,] <- col
        data[time, 1] <- gsub("[ + ]", "", data[time, 1])   #remove + from final 
      
      line <- capture.output(cat(paste0("RI_", v[var], item, " =~ "), unlist(data[,1])))
      cfa <- c(cfa, "\n", line)}
      cfa <- c(cfa, "\n")}
    
  #Within Person Variables
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Within Person Variables##")
  cfa <- c(cfa, "\n")
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
    
    if(constraint == "metric" | constraint == "scalar"){
      for(col in 1:ncol(data_Item)){data_Item[, col] <- paste0("f", alphabet[start + (col - 1)], "*", data_Item[, col])}}
    start <- start + ncol(data_Item)
    #Write into CFA Syntax
    cfa <- c(cfa, "\n")
    for (row in 1:nrow(data_Item)){
      line <- capture.output(cat(paste0("WF", v[var], row, " =~ "), unlist(data_Item[row,])))
      cfa <- c(cfa, "\n", line)}}
  print(start)
  
  
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
      data <- as.data.frame(paste0("WF",  v, t-1, " +"))
      data[nrow(data),] <- gsub("[ + ]", "", data[nrow(data),])
      dv <- paste0("WF", v[var], t, " ~")
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
        line <- capture.output(cat(paste0("WF", v[var], t, " ~~ ", "WF", v[v2], t)))
        cfa <- c(cfa, "\n", line)}
    }}
  
  
  #Fix correlations between random intercepts and other exogenous variables to 0
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Fix correlations between random intercepts and other exogenous variables to 0##")
  cfa <- c(cfa, "\n")
  
  data <- data.frame(1:max(items))
  data2 <- data.frame(1:length(v))
  data2[,] <- paste0("0*WF", v, "1 + ")
  for (var in 1:length(v)){
    data[1:items[var], (var + 1)] <- paste0("RI_", v[var], 1:items[var], " + ")}
  data[items[var], var + 1] <- gsub("[ + ]", "", data[items[var],var +1])   #remove + from final row
  data2[nrow(data2),] <- gsub("[ + ]", "", data2[nrow(data2),]) 
  data[is.na(data)] <- ""
   line <- capture.output(cat(unlist(data[,-1]), " ~~ ", unlist(data2)))
  cfa <- c(cfa, "\n", line)
  
  
  
  if(constraint == "scalar"){
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "##Constrain intercepts over time##")
    cfa <- c(cfa, "\n")
    flline <- c("\n")
    for(var in 1:length(v)){
      #Create a dataframe of the strings by time (row) and item (column)  
      data <- data.frame(1:time)
      for (item in (1:items[var])){ 
        col <- c(paste0("T", 1:time, "_", v[var], item, " + "))
        data[,] <- col
        data[time, 1] <- gsub("[ + ]", "", data[time, 1])   #remove + from final 
        line <- capture.output(cat( unlist(data[,1]), paste0("~ ", alphabet[start],  "*1")))
        start <- start +1       
        cfa <- c(cfa, "\n", line)}
      cfa <- c(cfa, "\n")
      cfa <- c(cfa, "\n")
      if(var == length(v)){
        flline <- capture.output(cat(flline, paste0("WF", v[var], 2:(time -1), " + ")))
        flline <- capture.output(cat(flline, paste0( "WF", v[var], time, " ~ 1")))
        }  else{flline <- capture.output(cat(flline, paste0("WF", v[var], 2:time, " + ")))}
        }
      cfa <- c(cfa, "Free latent means from T2 onwards")
      cfa <- c(cfa, "\n")
      cfa <- c(cfa, flline)
  }
  
  return(cfa)
}

