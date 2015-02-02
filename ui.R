shinyUI(
  navbarPage(title = "ShinyDist",
    tabPanel(title = "Distributions",
      navlistPanel(
        "Choose a distribution",
        tabPanel("Normal Distribution",
          inputPanel(
            numericInput("norm_mean", label = "Mean", value = 0, step = .1),
            numericInput("norm_sd", label = "SD", value = 1, min = .01, step = .1),
            selectInput("norm_alpha", label = "Alpha", choices = alpha.choices),
            selectInput("norm_sides", label = "Direction", choices = side.choices)
          ),
          plotOutput("plot_norm")
        ),
        tabPanel("t-Distribution",
          inputPanel(),
          plotOutput("plot-t")
        ),
        tabPanel("Chi^2-Distribution",
          inputPanel(),
          plotOutput("plot-chisq")
        ),
        tabPanel("F-Distribution",
          inputPanel(),
          plotOutput("plot-f")
        )
      )
    )
  )
)
