navbarPage("ARMA models",
           tabPanel("AR(p)",
                    withMathJax(),
                    uiOutput('AR_eqn'),
                    uiOutput("AR_ch_eqn"),
                    uiOutput("AR_roots"),
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("AR_order", "Order",
                                    min=1, max=5, value=1),
                        uiOutput("AR_sliders"),
                        sliderInput("AR_sample_size",
                                    "Number of observations",
                                    min=10, max=500, value=10,
                                    step=10,
                                    animate=animationOptions(interval=300, loop=F))
                        ),
                      mainPanel(
                        h3("Theory"),
                        plotOutput("theoretical_AR_plot"),
                        h3("Simulation"),
                        plotOutput("sample_AR_plot")
                      )
                    )
           ),
           tabPanel("MA(q)",
                    withMathJax(),
                    uiOutput('MA_eqn'),
                    uiOutput("MA_ch_eqn"),
                    uiOutput("MA_roots"),
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("MA_order", "Order",
                                    min=1, max=5, value=1),
                        uiOutput("MA_sliders"),
                        sliderInput("MA_sample_size",
                                    "Number of observations",
                                    min=10, max=500, value=10,
                                    step=10,
                                    animate=animationOptions(interval=300, loop=F))
                      ),
                      mainPanel(
                        h3("Theory"),
                        plotOutput("theoretical_MA_plot"),
                        h3("Simulation"),
                        plotOutput("sample_MA_plot")
                      )
                    )
           ),
           tabPanel("ARMA(p, q)",
                    withMathJax(),
                    uiOutput('ARMA_eqn'),
                    uiOutput('ARMA_AR_ch_eqn'),
                    uiOutput("ARMA_AR_roots"),
                    uiOutput('ARMA_MA_ch_eqn'),
                    uiOutput("ARMA_MA_roots"),
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("ARMA_AR_order", "AR Order",
                                    min=1, max=5, value=1),
                        uiOutput("ARMA_AR_sliders"),
                        hr(),
                        sliderInput("ARMA_MA_order", "MA Order",
                                    min=1, max=5, value=1),
                        uiOutput("ARMA_MA_sliders"),
                        sliderInput("ARMA_sample_size",
                                    "Number of observations",
                                    min=10, max=500, value=10,
                                    step=10,
                                    animate=animationOptions(interval=300, loop=F))
                      ),
                      mainPanel(
                        h3("Theory"),
                        plotOutput("theoretical_ARMA_plot"),
                        h3("Simulation"),
                        plotOutput("sample_ARMA_plot")
                      )
                    )
           )
)