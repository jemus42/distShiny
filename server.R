shinyServer(function(input, output) {

  #### Normal distribution #####
  output$plot_norm <- renderPlot({
    mean      <- input$norm_mean
    sd        <- input$norm_sd
    alpha     <- as.numeric(input$norm_alpha)
    direction <- input$norm_sides
    ymax      <- dnorm(mean, mean = mean, sd = sd)

    validate(
      need(input$norm_sd != 0, "Standard deviation must be greater than zero!")
    )

    p <- ggplot(data.frame(x = c(mean-4*sd, mean+4*sd)), aes(x))
    p <- p + stat_function(fun = dnorm, args = list(mean = mean, sd = sd))
    p <- p + geom_vline(x = ((crit_z(alpha, direction) * sd) + mean),
                        linetype = "longdash", colour = "red")
    p <- p + ylim(0, ymax) + ylab("P(x)")
    print(p)
  })

  output$data_norm <- renderText({
    mean      <- input$norm_mean
    sd        <- input$norm_sd
    alpha     <- as.numeric(input$norm_alpha)
    direction <- input$norm_sides

    crit.z <- round(crit_z(alpha, direction), 2)
    crit.n <- round((crit_z(alpha, direction) * sd) + mean, 2)

    if (direction != "two.sided"){
      return(paste0("The critical value is ", crit.n, " (zcrit ", crit.z, ")"))
    } else {
      return(paste0("The critical values are ", crit.n[1],
                   " (lower zcrit ", crit.z[1], ") and ",
                   crit.n[2], " (upper zcrit ", crit.z[2], ")"))
    }
  })

  #### t-Distribution ####
  output$plot_t <- renderPlot({
    df        <- input$t_df
    ncp       <- input$t_ncp
    alpha     <- as.numeric(input$t_alpha)
    direction <- input$t_sides

    validate(
      need(input$t_df != 0, "Degrees of freedom must be greater than zero!")
    )

    p <- ggplot(data.frame(x = c(-10, 10)), aes(x))
    p <- p + stat_function(fun = dt, args = list(df = df, ncp = ncp))
    p <- p + geom_vline(x = crit_t(alpha, direction, df, ncp),
                        linetype = "longdash", colour = "red")
    p <- p + ylim(0, .42) + ylab("P(x)")
    print(p)
  })

  output$data_t <- renderText({
    df        <- input$t_df
    ncp       <- input$t_ncp
    alpha     <- as.numeric(input$t_alpha)
    direction <- input$t_sides

    crit.t <- round(crit_t(alpha, direction, df, ncp), 2)

    if (direction != "two.sided"){
      return(paste0("The critical value is ", crit.t))
    } else {
      return(paste0("The critical values are ", crit.t[1], " and ", crit.t[2]))
    }
  })

  #### Chi^2-distribution
  output$plot_chisq <- renderPlot({
    df        <- input$chi_df
    alpha     <- as.numeric(input$chi_alpha)

    validate(
      need(input$chi_df != 0, "Degrees of freedom must be greater than zero!")
    )

    p <- ggplot(data.frame(x = c(0.001, 40)), aes(x))
    p <- p + stat_function(fun = dchisq, args = list(df = df))
    p <- p + geom_vline(x = qchisq(1 - alpha, df = df),
                        linetype = "longdash", colour = "red")
    p <- p + ylim(0, .42) + ylab("P(x)")
    print(p)
  })

  output$data_chi <- renderText({
    df        <- input$chi_df
    alpha     <- as.numeric(input$chi_alpha)

    crit.chi  <- round(qchisq(1 - alpha, df = df), 2)

    return(paste0("The critical value is ", crit.chi))
  })
})
