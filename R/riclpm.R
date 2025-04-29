#' Function to generate syntax for RICLPM
#'Needed User Inputs:
#'v = name want to give each variable. E.g., c("Var1","Var2", "Var3")
#'time = numeric number of time waves E.g., 4
#'Additional Options (default = F)
#'constraint - set to T if wish to add constraints to cross-lagged effects and residual variances and covariances
#'z1_o - set to T if wish to add a time-invariant predictor z1 of the observed variables
#'z1_RI - set to T if wish to add a time-invariant predictor z1 of the Random Intercepts
#'z2_RI - set to T if wish to set random intercepts predicting a time-invariant outcome z2
#'z2_w - set to T if wish to set within components predicting a time-invariant outcome z2
#' @export
riclpm <- function(v, time, constraint = F, z1_o = F, z1_RI = F, z2_RI = F, z2_w = F){
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
      if(constraint == T & t > 1){
        line <- paste0("w", tolower(v[var]), t, " ~~ v", tolower(v[var]), "*w", tolower(v[var]), t)
      } else{line <- paste0("w", tolower(v[var]), t, " ~~ w", tolower(v[var]), t)}
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
        if(constraint == T & t > 1){
          line <- capture.output(cat(paste0("w", tolower(v[var]), t, " ~~ ", "cov*w", tolower(v[v2]), t)))
        } else {line <- capture.output(cat(paste0("w", tolower(v[var]), t, " ~~ ", "w", tolower(v[v2]), t)))}
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


  #Constrain grand means over time
  if(constraint == T){
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "##Constrain Grand Means Over Time##")
    cfa <- c(cfa, "\n")
    for(var in 1:length(v)){
      line <- capture.output(cat(paste0("T", 1:(time - 1), "_", v[var],  " + ")))
      line <- capture.output(cat(paste0(line, "T", time, "_", v[var], " ~ m", v[var], "*1")))
      cfa <- c(cfa, "\n", line)}}

  if(z1_o | z2_RI | z2_w == T){
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "##Regression of observed variables on z1 (constrained)##")
    cfa <- c(cfa, "\n")
    for(var in 1:length(v)){
      line <- capture.output(cat(paste0(v[var], 1:(time - 1), " + ")))
      line <- capture.output(cat(paste0(line, v[var], time, " ~ s", var,"*z1")))
      cfa <- c(cfa, "\n", line)}}

  if(z1_RI == T ){
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "##Regression of Random Intercepts on z1 Constrained over Time##")
    cfa <- c(cfa, "\n")
    line <- capture.output(cat(paste0("RI_", v[1:length(v) - 1], " + ")))
    line <- capture.output(cat(paste0(line, "RI_", v[length(v)], "~ z1")))
    cfa <- c(cfa, "\n", line)}

  if(z2_RI == T){
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "##Regression of Time Invariant Outcome z2 on random intercepts##")
    cfa <- c(cfa, "\n")
    line <- capture.output(cat(paste0("RI_", v[1:length(v) - 1], " + ")))
    line <- capture.output(cat(paste0("z2 ~ ", line, "RI_", v[length(v)])))
    cfa <- c(cfa, "\n", line, "\n")
    cfa <- c(cfa, "z2 ~~ z2")}

  if(z2_w == T){
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "\n")
    cfa <- c(cfa, "##Regression of Time Invariant Outcome z2 on within components##")
    cfa <- c(cfa, "\n")
    #Create a dataframe of the strings by time (row) and item (column)
    data <- data.frame(1:time)
    for(var in 1:length(v)){
      for (t in (1:time)){
        col <- c(paste0("w", v[var], t, " + "))
        data[t,var] <- col}}
    data[time, length(v)] <- gsub("[ + ]", "", data[time, length(v)])   #remove + from final
    line <- capture.output(cat(paste0("z2 ~ ", unlist(data[,]))))
    cfa <- c(cfa, "\n", line, "\n")
    cfa <- c(cfa, "z2 ~~ z2")}

  return(cfa)
}


