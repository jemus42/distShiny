library("shiny")
library("ggplot2")

#### Defining Choices ####

alpha.choices <- c("5%" = 0.05, "1%" = 0.01, "10%" = 0.1)
side.choices <- c("Left sided" = "left", "Right sided" = "right", "Two sided" = "two.sided")

#### Helper functions ####

crit_z <- function(alpha = .05, direction = "left"){
  if (!(direction %in% c("left", "right", "two.sided"))){
    stop("Unknown argument to direction, must be 'left', 'right' or 'two.sided'")
  }
  if (direction == "left"){
    crit <- qnorm(alpha)
  } else if (direction == "right"){
    crit <- qnorm(1 - alpha)
  } else if (direction == "two.sided"){
    alpha <- alpha/2
    crit <- c(qnorm(alpha), qnorm(1 - alpha))
  }
  return(crit)
}

crit_t <- function(alpha = .05, direction = "left", df = 1, ncp = 0){
  if (!(direction %in% c("left", "right", "two.sided"))){
    stop("Unknown argument to direction, must be 'left', 'right' or 'two.sided'")
  }
  if (direction == "left"){
    crit <- qt(alpha, df = df, ncp = ncp)
  } else if (direction == "right"){
    crit <- qt(1 - alpha, df = df, ncp = ncp)
  } else if (direction == "two.sided"){
    alpha <- alpha/2
    crit <- c(qt(alpha, df = df, ncp = ncp), qt(1 - alpha, df = df, ncp = ncp))
  }
  return(crit)
}
