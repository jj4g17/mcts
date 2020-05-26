#library(devtools)
#library(tidyverse)
#library(fs)
#library(shiny)

#' Simulate Multiple Choice Test
#'
#' @importFrom stats rbinom
#' @importFrom stats rhyper
#' @importFrom utils read.csv
#'
#' @description The main function to be used in simulating multiple choice tests. \cr
#'
#' Formatting for input x is imperative. For a test of n questions, mcts expects an n x 3 matrix with each row representing one question. \cr
#' The first column determines the question type; "normal" - one correct answer out of p questions; "neg" - one correct answer out of p possible answers where you lose 1 mark for an incorrect answer;
#' "multi" - q correct answers out of p possible answers with 1 mark gained for each correct answer. The second column determines the number of possible answers p.
#' The third column is reserved for the special case "multi" and determines the number of correct answers q.\cr \cr
#' This test can be inputted either by matrix, file (character) or data.frame.
#'
#' @param x the inputted test. This can be of types: \cr
#' matrix - writing test directly in R \cr
#' character - reading from file name \cr
#' data.frame - if file has already been read
#' @param ... used to pass repetitions and header for file input, as well as truncation boolean.
#'
#' @return A list with class 'test' displaying the format of the test as well as the simulated results in both a raw and tabulated form.
#' @export
#'
mcts <- function(x, ...){
  UseMethod("mcts")
}

#' @export
mcts.matrix = function(x, reps = 1000, truncation = TRUE, ...){
  mcts(as.data.frame(x), reps, truncation)
}

#' @export
mcts.character <- function(x, header = FALSE, reps = 1000, truncation = TRUE, ...){ #mcts for character (file) input
  ###VALIDATE INPUT HERE
  test <- read.csv(x, header = header)

  mcts(as.matrix(test), reps, truncation)
}

#' @export
mcts.data.frame <- function(x, reps = 1000, truncation = TRUE, ...){
  colnames(x) <- c("type", "answers", "misc")
  score <- replicate(reps, sum(apply(x, 1, simq)))
  if(truncation){score <- replace(score, score<0, 0)}

  maxScore <- sum(apply(x, 1, function(x){
    if (x[1] == "multi") return(as.numeric(x[3]))
    else return(1)
  }))

  l <- list(test = x, raw = score, results = as.data.frame(table(factor(score, levels=min(score):maxScore), dnn = "score")), mean = mean(score), maxScore = maxScore)

  return(structure(l, class = "test"))
}

#' @export
mcts.default <- function(x = NULL, reps = 1000, truncation = TRUE, ...){ #mcts for null x/unspecified classes
  if(is.null(x))mcts(cbind(rep("normal", 10), rep(4L, 10), rep(0L, 10)), reps, truncation)
  else warning(paste("test cannot process test of class", class(x), "and can only be used on classes matrix, vector(integer) and character. mcts may be called without an argument for an example case."))
}

#' MCTS Shiny App
#'
#' @description Opens a shiny app in which a test can be configured and analysed with ease.
#' @import shiny
#' @export
#'
mcts_app <- function(){
  #Opens a Shiny page to create a test with
  runApp("R")
}

simq <- function(x){
  #This function also acts as validation for the inputted test
  if (x[1] == "normal"){
    return(rbinom(1, 1, 1/as.numeric(x[2])))
  }
  else if (x[1] == "neg"){
    return(2*rbinom(1, 1, 1/as.numeric(x[2]))-1)
  }
  else if (x[1] == "multi"){
    #x2 = number of answers, #x3 = number to pick.
    return(rhyper(1, as.numeric(x[3]), as.numeric(x[2])-as.numeric(x[3]), as.numeric(x[3])))
  }
}
