#' @export
mean.test <- function(x, ...)return(x$mean)

#' @import ggplot2
#' @export
plot.test <- function(x, ...){
  ggplot(data=x$results, aes(x=score, y=x$results$Freq/length(x$raw))) +
    geom_bar(stat="identity") +
    ylab("Proportion") +
    ggtitle("Proportion of simulated test scores") +
    scale_x_discrete(name = "Test Score")
}

#' @export
summary.test <- function(object, ...){
  cat("The probability of passing such a test at the 40% standard pass rate is", score(object), "\n")
  summary(object$raw)
}


#' @export
print.test <- function(x, ...){
  cat(deparse(substitute(x)), "is a test marked out of",  x$maxScore, "with", nrow(x$test), "questions, simulated with", length(x$raw), "runs. \n")
  print(x$test)
  print(x$results)
}

#' Calculate probability of scoring over v percent on a test x
#'
#' @param x the input test
#' @param v the threshold score percentage (defaults to 0.40)
#'
#' @return The proportion of students who would score at least v percent by random guessing.
#' @export
#'
score <- function(x, v = 0.40){
  return(sum(x$results$Freq[(ceiling(v*x$maxScore)+1):length(x$results$Freq)]) / length(x$raw) )
}
