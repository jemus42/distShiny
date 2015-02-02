shinyServer(function(input, output) {

  #### Plotting Code #####
  output$plot_norm <- renderPlot({
    mean      <- input$norm_mean
    sd        <- input$norm_sd
    alpha     <- as.numeric(input$norm_alpha)
    direction <- input$norm_sides

    ymax      <- dnorm(mean, mean = mean, sd = sd)

    p <- ggplot(data.frame(x = c(mean-4*sd, mean+4*sd)), aes(x)) +
                ylim(0, ymax) +
                stat_function(fun = dnorm, args = list(mean = mean, sd = sd))
    p <- p + geom_vline(x = ((crit_z(alpha, direction) * sd) + mean),
                        linetype = "longdash", colour = "red")
    print(p)
  })
})
