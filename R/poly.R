library(tidyverse)
library(stringr)
library(stringi)

#' polynomial_features
#'
#' Generate a new feature matrix consisting of all polynomial
#' combinations of the features with degree less than or equal to the specified
#' degree.
#' @usage polynomial_features(df,degree)
#' @param df Dataframe, containing the features of interest.
#' @param degree Integer, the degree of the polynomial.
#' @examples
#' val1 = rnorm(50)
#' val2 = rnorm(50)
#' df = as.data.frame(cbind(val1,val2))
#' df_poly = polynomial_features(df,2)
#' @export
polynomial_features <- function(df,degree){
  # getting only numeric column
  nums <- unlist(lapply(df, is.numeric))
  df_poly = df[ , nums]
  # building the formula
tmp = sapply(colnames(df_poly),function(i){
        sapply(seq_along(1:degree),function(j){
          paste0('poly(',i,',',degree,',raw=TRUE)[,',j,']')})})
polym = stri_flatten(tmp,collapse="+")
formula <- as.formula(paste(' ~ .^',degree,'+',paste0(polym),collapse="+"))
tmp_pol = model.frame(formula, data=df)
# extract columns powered to degree
tmp_pol_poly = tmp_pol[,grepl("poly",colnames(tmp_pol))]
regex_filter = "poly\\(([a-zA-Z0-9_]*),\\s*\\d*\\s*,\\s*raw\\s*=\\s*TRUE\\)\\[\\s*,\\s*(\\d+)\\]"
# renaming your columns powered to degree
for(i in colnames(tmp_pol_poly)){
  string_of_interest = str_match(string = i, pattern = regex_filter)
  names(tmp_pol_poly)[names(tmp_pol_poly) == i] <- paste0(paste0(string_of_interest[2],"^"),
                                                          string_of_interest[3])
}
tmp_pol = cbind(tmp_pol,tmp_pol_poly)
tmp_pol = dplyr::select(tmp_pol, -c(colnames(tmp_pol[,grepl("poly",colnames(tmp_pol))])))
formula =  as.formula(paste('~ .^',degree))
tmp_inter = model.matrix(formula, data=df)
tmp_inter = as.data.frame(tmp_inter)
tmp =  cbind(tmp_inter,tmp_pol)
drop = colnames(df)
tmp_final = tmp[,!(names(tmp) %in% drop)]
}
