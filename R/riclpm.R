#' Function to generate syntax for RICLPM
#' Inputs:
#'v = name want to give each variable. E.g., c("Var1","Var2", "Var3") 
#'items = item range for each variable. E.g., c(3, 4, 6)  
#'time = numeric number of time waves E.g., 4 
#constraint = boolean - set to T if wish to add constraints to cross-lags

#' @export
riclpm <- function(v, time, constraint){
  alphabet <- paste0(letters)  
  start <- 1
  cfa <- c("\n")

  ##Random Intercepts
  cfa <- c(cfa, "\n", "##Random Intercepts##", "\n")
  
  for(var in 1:length(v)){
    #Create a dataframe of the strings by time (row) and item (column)  
    data <- data.frame(1:time)
    for (t in (1:time)){ 
      col <- c(paste0("1*T", t, "_", v[var], " + "))
      data[t,1] <- col}
    
    data[time, 1] <- gsub("[ + ]", "", data[t, 1])   #remove + from final 
    
    line <- capture.output(cat(paste0("RI_", v[var], " =~ "), unlist(data[,1])))
    cfa <- c(cfa, "\n", line)}
  ##Specifying Residuals
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Residuals##")
 
  for(var in 1:length(v)){
    for(t in (1:time)){
      line <- paste0("w", tolower(v[var]), t, " ~~ w", tolower(v[var]), t)
      cfa <- c(cfa, "\n", line)}
      cfa <- c(cfa, "\n")}

  #Within Person Variables
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "\n")
  cfa <- c(cfa, "##Within Person Variables##")
  
  for(var in 1:length(v)){
    cfa <- c(cfa, "\n")
    for (t in (1:time)){ 
      line <- capture.output(cat(paste0("w", tolower(v[var]), t, " =~ 1*T", t, "_", v[var])))
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
      } else{data <- as.data.frame(paste0("w",  tolower(v), t-1, " +"))}
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
  

  
  
  
  return(cfa)
}

##**To Run**##
#v = c("Var1","Var2", "Var3") - name want to give each variable
#items = c(3, 4, 6) - item range for each variable
#time = 4 - e.g., numeric number of time waves
#constraint = boolean - T if wish to add constraints to cross-lags
