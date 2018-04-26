shinyServer(function(input, output) {

  #### Normal distribution #####
  output$plot_norm <- renderPlot({
    n         <- ceiling(input$norm_n)
    mean      <- input$norm_mean
    sd        <- input$norm_sd
    se        <- sd/sqrt(n)
    alpha     <- as.numeric(input$norm_alpha)
    direction <- input$norm_sides
    ymax      <- dnorm(mean, mean = mean, sd = se)

    validate(need(input$norm_sd != 0, "Standard deviation must be greater than zero!"))
    validate(need(input$norm_n != 0, "Sample size must be greater than zero!"))
    validate(need(!is.na(input$norm_mean), "Mean must be set!"))

    df <- data.frame(x = c(mean-4*se, mean+4*se))

    p <- ggplot(data = df, aes(x = x))
    p <- p + stat_function(fun = dnorm, args = list(mean = mean, sd = se))
    p <- p + geom_vline(xintercept = ((crit_z(alpha, direction) * se) + mean),
                        linetype = "longdash", colour = "red")
    p <- p + ylim(0, ymax) + ylab("P(x)")
    print(p)
  })

  output$data_norm <- renderText({
    mean      <- input$norm_mean
    sd        <- input$norm_sd
    se        <- sd/sqrt(ceiling(input$norm_n))
    alpha     <- as.numeric(input$norm_alpha)
    direction <- input$norm_sides

    crit.z <- round(crit_z(alpha, direction), 2)
    crit.n <- round((crit_z(alpha, direction) * se) + mean, 2)

    if (direction != "two.sided"){
      return(paste0("The critical value is ", crit.n,
                    " (zcrit ", crit.z, ") — ",
                    "Standard Error = ", round(se, 2)))
    } else {
      return(paste0("The critical values are ", crit.n[1],
                   " (lower zcrit ", crit.z[1], ") and ",
                   crit.n[2], " (upper zcrit ", crit.z[2], ") — ",
                   "Standard Error = ", round(se, 2)))
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
    p <- p + geom_vline(xintercept = crit_t(alpha, direction, df, ncp),
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
    crit      <- qchisq(1 - alpha, df)
    p         <- 1 - as.numeric(input$chi_test)
    test      <- qchisq(p, df)

    chidf <- tibble(
      x = seq(.01, 17.5, .01),
      y = dchisq(x, df)
    )

    if (test >= 8.5){
      lab_x    <- test - .5
      lab_just <- "left"
    } else {
      lab_x    <- test + .5
      lab_just <- "right"
    }

    validate(
      need(input$chi_df != 0, "Degrees of freedom must be greater than zero!")
    )

    p <- ggplot(chidf, aes(x, y))
    p <- p + geom_line()
    p <- p + geom_ribbon(data = subset(chidf, x >= crit),
                         aes(ymin = 0, ymax = y), fill = "#99004d", alpha = .5)
    p <- p + geom_ribbon(data = subset(chidf, x >= test),
                         aes(ymin = 0, ymax = y), fill = "#99004d", alpha = .3)
    p <- p + geom_segment(aes(y = 0, yend = dchisq(crit, df) + .02,
                              x = crit, xend = crit),
                          linetype = "dotted", colour = "#99004d")
    p <- p + geom_segment(aes(y = 0, yend = dchisq(test, df) + .02,
                              x = test, xend = test), size = .5,
                          linetype = "longdash", colour = "#99004d")
    # p <- p + geom_text(aes(label = "empirical value", x = lab_x,
    #                         y = dchisq(test, df) + .02), size = 6,
    #                     vjust = lab_just, color = "#99004d")
    if (df == 1){
      p <- p + scale_y_continuous(labels = NULL, limits = c(0, .2))
      p <- p + labs(y = "Density", x = expression(chi^2),
                    caption = "zoom factor: 500%")
    } else {
      p <- p + scale_y_continuous(labels = NULL)
      p <- p + labs(y = "Density", x = expression(chi^2))
    }
    print(p)
  })

  output$data_chi <- renderText({
    df        <- input$chi_df
    alpha     <- as.numeric(input$chi_alpha)

    crit.chi  <- round(qchisq(1 - alpha, df = df), 2)

    return(paste0("The critical value is ", crit.chi))
  })

  #### F-Distribution ####
  output$plot_f <- renderPlot({
    df1       <- input$f_df1
    df2       <- input$f_df2
    alpha     <- as.numeric(input$f_alpha)

    validate(
      need(input$f_df1 != 0, "Degrees of freedom must be greater than zero!"),
      need(input$f_df2 != 0, "Degrees of freedom must be greater than zero!")
    )

    p <- ggplot(data.frame(x = c(0, 10)), aes(x))
    p <- p + stat_function(fun = df, args = list(df1 = df1, df2 = df2))
    p <- p + geom_vline(xintercept = crit_f(alpha, df1, df2),
                        linetype = "longdash", colour = "red")
    p <- p + ylab("P(x)")
    print(p)
  })

  output$data_f <- renderText({
    df1       <- input$f_df1
    df2       <- input$f_df2
    alpha     <- as.numeric(input$f_alpha)

    crit.f <- round(crit_f(alpha, df1, df2), 2)

    return(paste0("The critical value is ", crit.f))

  })

  #### Plot Errors
  output$plot_errors <- renderPlot({
    n         <- ceiling(input$norm_n2)
    mean      <- input$norm_mean2
    sd        <- input$norm_sd2
    se        <- sd/sqrt(n)
    alpha     <- as.numeric(input$norm_alpha2)

    validate(need(input$norm_sd != 0, "Standard deviation must be greater than zero!"))
    validate(need(input$norm_n != 0, "Sample size must be greater than zero!"))
    validate(need(!is.na(input$norm_mean), "Mean must be set!"))

    p <- powervis(mu0 = 0, mu1 = mean, sd = sd, n = n, alpha = alpha)
    print(p)
  })

  output$error_table <- renderTable({
    n         <- ceiling(input$norm_n2)
    mean      <- input$norm_mean2
    sd        <- input$norm_sd2
    se        <- sd/sqrt(n)
    alpha     <- as.numeric(input$norm_alpha2)
    crit      <- qnorm(1 - alpha, 0, se)

    tab <- data.frame(d     = round((mean / sd), 2),
                      beta  = paste0(round(pnorm(crit, mean = mean, sd = se), 2) * 100, "%"),
                      power = paste0(round(pnorm(crit, mean = mean, sd = se, lower.tail = F), 2) * 100, "%"),
                      row.names = NULL)

    # ret <- sprintf("<a>Effect size: <strong>%s</strong><br />
    #                Beta Error: <strong>%s</strong><br />
    #                Power: <strong>%s</strong></a>",
    #                tab$d, tab$beta, tab$power)

    return(tab)

  })

  #### Law of Large Numbers ####
  output$plot_lln <- renderPlot({
    n <- input$lln_sample_size

    validate(need(input$lln_sample_size < 10000, "Please! Our servers can't take that much coin tossing!!\nPlease select a sample size less than 10.000"))

    s1 <- sample(c(1, 2), n, replace = T)

    s2 <- map_dbl(seq_along(s1), function(x){
      i <- s1[1:x]
      length(i[i == 1]) / length(i)
    })

    s3 <- tibble(
      x = seq_along(s1),
      result = ifelse(s1 == 1, "Heads", "Tails"),
      p_heads = s2,
      p_tails = 1 - s2
    )

    # needs... more thought
    if (n <= 100){
      pt_size <- 1
    } else {
      pt_size <- .5
    }

    ggplot(s3, aes(x = x, y = p_heads)) +
      geom_point(size = .75, alpha = .3) +
      geom_hline(aes(yintercept = 1/2), size = pt_size, color = "red") +
      labs(# title = "Das Gesetz der großen Zahl",
        title = paste('Häufigkeit von "Kopf" bei', n, 'Münzwürfen'),
        y = "rel. Häufigkeit", x = "Münzwürfe") +
      # annotate("label", x = 1000, y = .40, label = "erwartete Häufigkeit", alpha = .8,
      #          fill = "red", color = "white", size = 4, label.padding = unit(.35, "lines")) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(breaks = scales::pretty_breaks(),
                         labels = scales::percent_format())
  })
})
