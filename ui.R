shinyUI(
  navbarPage(title = "ShinyDist v1.2.1",
    tabPanel(title = "Distributions", icon = icon("area-chart"),
      navlistPanel(widths = c(3, 9),
        "Choose a distribution",
        tabPanel("Normal Distribution",
          h2("Normal Distribution & Significance Testing"),
          inputPanel(
            numericInput("norm_mean", label = "Mean", value = 0, step = .1),
            numericInput("norm_sd",   label = "Standard Deviation", value = 1, min = .01, step = .1),
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
            selectInput("chi_alpha", label = "Alpha", choices = alpha.choices)
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
    tabPanel("About", icon = icon("question"),
      includeMarkdown("about.md")
    )
  )
)
