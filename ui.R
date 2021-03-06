shinyUI(
  navbarPage(title = "ShinyDist v2.1.0",
    tabPanel(title = "Distributions", icon = icon("area-chart"),
      navlistPanel(widths = c(3, 9),
        "Choose a distribution",
        tabPanel("Normal Distribution",
          h2("Normal Distribution & Significance Testing"),
          inputPanel(
            numericInput("norm_mean", label = "Mean", value = 100, step = 1),
            numericInput("norm_test", label = "x-test", value = 100, step = 1),
            numericInput("norm_sd",   label = "Standard Deviation", value = 15, min = .1, step = .1),
            numericInput("norm_n",    label = "Sample Size", value = 1, min = 1, step = 1),
            selectInput("norm_alpha", label = "Alpha", choices = alpha.choices),
            selectInput("norm_sides", label = "Direction", choices = side.choices)
          ),
          h3(textOutput("data_norm")),
          plotOutput("plot_norm")
        ),
        tabPanel("t-Distribution",
          h2("t-Distribution & Significance Testing"),
          inputPanel(
            numericInput("t_df",   label = "Degrees of Freedom", min = 1, value = 1, step = 1),
            numericInput("t_ncp",  label = "Non-Centrality Parameter", value = 0, min = .01, step = .1),
            selectInput("t_alpha", label = "Alpha", choices = alpha.choices),
            selectInput("t_sides", label = "Direction", choices = side.choices)
          ),
          h3(textOutput("data_t")),
          plotOutput("plot_t")
        ),
        tabPanel("Chi^2-Distribution",
          h2("Chi^2-Distribution & Significance Testing"),
          inputPanel(
            numericInput("chi_df",   label = "Degrees of Freedom", min = 1, value = 1, step = 1),
            selectInput("chi_alpha", label = "Alpha", choices = alpha.choices),
            sliderInput("chi_test",  label = "p-Value", min = 0, max = 1, step = .01, value = .5)
          ),
          h3(textOutput("data_chi")),
          plotOutput("plot_chisq")
        ),
        tabPanel("F-Distribution",
          h2("F-Distribution & Significance Testing"),
          inputPanel(
            numericInput("f_df1",  label = "Df 1 (numerator)", min = 1, value = 1, step = 1),
            numericInput("f_df2",  label = "Df 2 (denominator)", min = 1, value = 1, step = 1),
            selectInput("f_alpha", label = "Alpha", choices = alpha.choices)
          ),
          h3(textOutput("data_f")),
          plotOutput("plot_f")
        )
      )
    ),
    tabPanel("Error Types", icon = icon("area-chart"),
       tabPanel("Normal Distribution",
                h2("Normal Distribution: Error Types"),
                inputPanel(
                  sliderInput("norm_mean2", label = "Mean of H1", min = 0, max = 8, value = 2, step = .1),
                  sliderInput("norm_sd2",   label = "Standard Deviation", min = .1, max = 4, value = 1, step = .1),
                  sliderInput("norm_n2",    label = "Sample Size", min = 1, max = 50, value = 1, step = 1),
                  selectInput("norm_alpha2", label = "Alpha", choices = alpha.choices)
                  #selectInput("norm_sides", label = "Direction", choices = side.choices)
                ),
                tags$div(align = "center",
                         tableOutput("error_table")),
                plotOutput("plot_errors")
       )
    ),
    tabPanel("Law of Large Numbers", icon = icon("bar-chart"),
             h2("The Law of Large Numbers"),
             conditionalPanel("input.lln_begin == 0",
                              inputPanel(
                                actionButton("lln_begin", "Toss a coin 10 times!")
                              )
             ),
             conditionalPanel("input.lln_begin != 0",
                              inputPanel(
                                actionButton("lln_add_1", "Toss once more"),
                                actionButton("lln_add_10", "Toss 10 times"),
                                actionButton("lln_add_100", "Toss 100 times"),
                                actionButton("lln_reset", "Reset")
                              )
             ),
             plotOutput("plot_lln")
    ),
    tabPanel("Central Limit Theorem", icon = icon("bar-chart"),
             h2("The Central Limit Theorem"),
             inputPanel(
               numericInput("clt_sample_size", label = "Sample Size", min = 2, value = 10, step = 1)
             )
    ),
    tabPanel("About", icon = icon("question"),
      includeMarkdown("about.md")
    )
  )
)
