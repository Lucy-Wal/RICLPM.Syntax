% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_riclpm.R
\name{multi_riclpm}
\alias{multi_riclpm}
\title{Function to generate syntax for multiple indicator RICLPM
Inputs:
v = name want to give each variable. E.g., c("Var1","Var2", "Var3")
items = item range for each variable. E.g., c(3, 4, 6)
time = numeric number of time waves E.g., 4
constraint = "config" (default), "metric", or "scalar"}
\usage{
multi_riclpm(v, items, time, constraint = F)
}
\description{
Function to generate syntax for multiple indicator RICLPM
Inputs:
v = name want to give each variable. E.g., c("Var1","Var2", "Var3")
items = item range for each variable. E.g., c(3, 4, 6)
time = numeric number of time waves E.g., 4
constraint = "config" (default), "metric", or "scalar"
}
