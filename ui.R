navbarPage("Power Implications for Reduced Sample Size",
           theme = shinytheme("cerulean"),
           windowTitle = "Power Implications",
           tabPanel("Disclaimer",
                    fluidPage(
                      fluidRow(column (7, includeMarkdown("disclaimer.md")))
                    )
           ),
           tabPanel("Power Evaluation",
                    # Tab for fixed sample test
                    # Outputs have suffix "20"
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = "sig20",
                          label = "One-sided significance level",
                          value = 0.025,
                          min = 0.001,
                          step = 0.001
                        ),
                        numericInput(
                          inputId = "power20",
                          label = "Power (%)",
                          value = 90,
                          min = 1,
                          step = 0.1
                        ),
                        bsTooltip("power20",
                                  "Power for statistical significance to detect a hypothesized design effect (>0)",
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        br(),
                        sliderInput(
                          inputId = "frac20",
                          label = "Proportion (%) of data available",
                          min = 10, max = 100, value = 50, step = 1
                        ),
                        bsTooltip("frac20",
                                  "The fraction of patients with endpoint assessment",
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        helpText("Note: This may depend on the data structure. For example, for time-to-event outcomes this proportion refers to the number of patients with endpoints, relative to the expectation in the original design."
                        )
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Power Information",
                                   br(),
                                   tableOutput("power_table20"),
                                   helpText("* The fraction of the hypothesized effect for the study design which would need to be observed to achieve statistical significance.",
                                            br(),
                                            "For example, 0.60 indicates that an estimate that is 60% as large as the hypothesized value would reach significance.",
                                            br(),
                                            "Note: This may depend on the data structure. For example, for time-to-event endpoints this would be expressed on the -log(hazard ratio) scale."
                                   )
                          ),
                          tabPanel("Power vs. Sample Size",
                                   br(),
                                   br(),
                                   h4(textOutput("title_power_curve_sample20")),
                                   div(style = "margin-top:-0.5em",
                                       plotlyOutput("power_curve_sample20")
                                   )
                          ),
                          tabPanel("Power vs. Effect Size",
                                   br(),
                                   sliderInput(
                                     inputId = "trt20",
                                     label = "Treatment effect relative to the hypothesized design effect",
                                     min = 0, max = 2, value = c(0.5, 1.5),
                                     step = 0.05
                                   ),
                                   bsTooltip("trt20",
                                             "A value of 1 corresponds to a treatment effect equal to the hypothesized design effect",
                                             "right",
                                             trigger = "hover", options = list(container = "body")
                                   ),
                                   h4(textOutput("title_power_curve20")),
                                   div(style = "margin-top:-0.5em",
                                       plotlyOutput("power_curve20")
                                   )
                          )
                        )
                      )
                    )
           ),
           
           # ***************************************************
           # Group Sequential Designs (start)
           # ***************************************************
           tabPanel("Group Sequential Design",
                    sidebarLayout(
                      sidebarPanel(
                        numericInput(
                          inputId = "alphaGSD",
                          label = "One-sided significance level",
                          value = 0.025,
                          min = 0.001,
                          step = 0.001
                        ),
                        numericInput(
                          inputId = "powerGSD",
                          label = "Power (%)",
                          value = 90,
                          min = 1,
                          step = 0.1
                        ),
                        bsTooltip("powerGSD",
                                  "Power for statistical significance to detect a hypothesized design effect (>0) using the sample size for the fixed design.",
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        br(),
                        sliderInput(
                          inputId = "tauGSD",
                          label = "Proportion (%) of data available",
                          min = 10, max = 100, value = 50, step = 1
                        ),
                        bsTooltip("fracGSD",
                                  "The fraction of patients with endpoint assessment",
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        selectInput(inputId = "designSelect", label="Type of group sequential design", 
                                    choices = c("Pocock" = "P", 
                                                "O'Brien-Fleming"="OF"), 
                                    selected="Pocock"),
                        bsTooltip("designSelect",
                                  "Implement an interim analysis with data available and a final analysis when all data are collected",
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        #helpText("Note: This may depend on the data structure. For example, for time-to-event outcomes this proportion refers to the number of patients with endpoints, relative to the expectation in the original design."),
                        checkboxInput(inputId = "dilutionCheck",
                                      label = strong("No dilution effect for second stage"),
                                      value = TRUE),
                        bsTooltip("dilutionCheck",
                                  HTML("Loss in assumed treatment effect for the second stage. For example, &eta; = 0.25 means that the assumed treatment effect for the second stage is only 75% of the assumed treatment effect of the first stage."),
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        uiOutput("dilutionSettings"),
                        checkboxInput(inputId = "psiCheck",
                                      label = strong("Equal variances for second stage"),
                                      value = TRUE),
                        bsTooltip("psiCheck",
                                  HTML("Inflation factor for the variance. For example, a value of &psi;=2 means that the variance for the second stage is twice the variance of the first stage."),
                                  "right",
                                  trigger = "hover", options = list(container = "body")
                        ),
                        uiOutput("psiSettings"),
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Achieved Power",
                                   br(),
                                   br(),
                                   plotlyOutput('power_plotlyGSD'),
                                   checkboxInput("checkTableGSD", "Show results in table format", value = FALSE),
                                   conditionalPanel(
                                     condition = "input.checkTableGSD == true",
                                     DT::dataTableOutput('power_tableGSD')
                                   ),
                                   bsTooltip("checkTableGSD",
                                             HTML("Show results for the achieved power as a table"),
                                             "right",
                                             trigger = "hover", options = list(container = "body")
                                   ),
                                   br()
                          ),
                          tabPanel("Sample Size Adjustment",
                                   br(),
                                   uiOutput('textadjust'),
                                   br(),
                                   div(style="display:inline-block;vertical-align:top; width: 220px;", 
                                       numericInput("deltaGSD", label = HTML("Treatment effect &delta;"), min = 0, max = 5, value = 0.5, step = 0.01)),
                                   bsTooltip("deltaGSD",
                                             HTML("The originally assumed treatment effect."),
                                             "right",
                                             trigger = "hover", options = list(container = "body")
                                   ),
                                   div(style="display:inline-block;vertical-align:top; width: 220px;", 
                                       numericInput("varGSD", label = HTML(paste("Variance &sigma;", tags$sup(2), sep = "")), min = 0, max = 5, value = 1, step = 0.01)),
                                   bsTooltip("varGSD",
                                             HTML("The originally assumed variance."),
                                             "right",
                                             trigger = "hover", options = list(container = "body")
                                   ),
                                   div(style="display:inline-block;vertical-align:top; width: 220px;", 
                                       numericInput("r", label = HTML("Randomization ratio 1:r (plc:trt)"), min = 0, max = 5, value = 1, step = 0.01)),
                                   bsTooltip("r",
                                             HTML("Randomization ratio for placebo (plc) versus treatment (trt) group."),
                                             "right",
                                             trigger = "hover", options = list(container = "body")
                                   ),
                                   br(),
                                   uiOutput('samplesize'),
                                   br(),
                                   uiOutput('parameters'),
                                   DT::dataTableOutput('adjust_tableGSDeta'),
                                   checkboxInput("checkTableEta", HTML("Show results for range of dilution effect &eta; in table format"), value = FALSE),
                                   #plotlyOutput('power_plotlyETA'), 
                          )
                        )
                        
                      )                    
                    )
           ),
           
           # ***************************************************
           # Group Sequential Design (end)
           # ***************************************************
           tabPanel("Help",
                    fluidPage(fluidRow(column (8, withMathJax(includeMarkdown("help_info.md"))))),
                    br(),
                    tabsetPanel(
                      tabPanel("Overview", fluidPage(fluidRow(column (8, withMathJax(includeMarkdown("help_general.md")))))),
                      tabPanel("Power Evaluation", fluidPage(fluidRow(column (8, withMathJax(includeMarkdown("help_general_eval.md")))))),
                      tabPanel("Group Sequential Design", fluidPage(fluidRow(column (8, withMathJax(includeMarkdown("help_general_gsd.md"))))))
                    )
           )
           
           # tabPanel("Help",
           #          fluidPage(
           #            fluidRow(column (8, withMathJax(includeMarkdown("help.md"))))
           #          )
           # )
)