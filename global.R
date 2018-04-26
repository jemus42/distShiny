library("shiny")
library("dplyr")
library("ggplot2")
library("rmarkdown")

theme_set(theme_classic())

#### Defining Choices ####
alpha.choices <- c("5%" = 0.05, "1%" = 0.01, "10%" = 0.1)
side.choices  <- c("Left sided" = "left", "Right sided" = "right", "Two sided" = "two.sided")

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

crit_f <- function(alpha = .05, df1 = 1, df2 = 1){

  crit <- qf(1 - alpha, df1 = df1, df2 = df2)

  return(crit)
}

powervis <- function(mu0 = 0, mu1 = 1, sd = 1, n = 1, alpha = 0.05) {
  sd      <- sd/sqrt(n)
  crit    <- qnorm(1 - alpha, mu0, sd)
  resolution <- 1e-2
  power   <- data.frame(x = seq(crit, 7, resolution),  y = dnorm(seq(crit, 7, resolution), mu1, sd))
  beta    <- data.frame(x = seq(-4, crit, resolution), y = dnorm(seq(-4, crit, resolution), mu1, sd))
  alphadf <- data.frame(x = seq(crit, 7, resolution),  y = dnorm(seq(crit, 7, resolution), mu0, sd))

  h_0 <- data.frame(x = seq(-4, 7, resolution), y = dnorm(seq(-4, 7, resolution), mu0, sd))
  h_1 <- data.frame(x = seq(-4, 7, resolution), y = dnorm(seq(-4, 7, resolution), mu1, sd))

  ymax <- dnorm(0, mean = 0, sd = sd) * .8

  ggplot(data = NULL, aes(x = -4:7)) +
    geom_line(aes(x = h_0$x, y = h_0$y), color = "black") +
    geom_line(aes(x = h_1$x, y = h_1$y), color = "black") +
    geom_area(aes(x = power$x, y = power$y, fill = "power"), alpha = .7) +
    geom_area(aes(x = alphadf$x, y = alphadf$y, fill = "alpha"), alpha = .7) +
    geom_area(aes(x = beta$x, y = beta$y, fill = "beta"), alpha = .7) +
    scale_x_continuous(breaks = seq(-5, 10, 1)) +
    scale_fill_brewer(palette = "Set1",
                      labels = c(expression(alpha), expression(beta), expression(1 - beta))) +
    annotate("label", x = 0, y = ymax, label = "H[0]", parse = TRUE, size = 6, family = "Palatino") +
    annotate("label", x = mu1, y = ymax, label = "H[1]", parse = TRUE, size = 6, family = "Palatino") +
    labs(x = "x", y = expression(P(x)), title = "", fill = "") +
    theme_bw(base_family = "Palatino") +
    theme(panel.grid.major = element_line(size = 0.1),
          panel.grid.minor = element_line(linetype = "blank"),
          legend.position = c(.1, .5),
          legend.text = element_text(size = rel(1.5)))
}
