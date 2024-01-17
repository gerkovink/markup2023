#' Compute the Means squared error
#'
#'This package allows you to directly compute the mean squared error,
#'making it easier to evaluate your model performance.
#'
#' @param truth A vector of numbers representing the observed values
#' @param predicted A vector of predicted outcomes
#' @returns An integer representing the Mean squared error
#' @author Marta Sech \email{m.sech@students.uu.nl}
#' @export
#' @examples
#' truth<-c(1,2,3,4,5)
#' predicted<-c(0,0,2,3,4)
#' mse(truth,predicted)
#'
mse<-function(truth,predicted){
     mse<-mean((predicted-truth)^2)
     return(mse)
}
