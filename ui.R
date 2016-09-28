shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("SimulatR - Simuler les situations d'inférence",
             tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                       tags$title("SimulatR - Simuler les situations d'inférence"))
  ),
  
  tabsetPanel(
    
    # Guide ----
    
    tabPanel("Guide d'utilisation", 
             fluidRow(column(2),
                      column(7, includeMarkdown("README_app.md")),
                      column(3))),
    tabPanel("Estimation d'une moyenne",
             fluidRow(column(12,
                             fluidRow(column(3,
                                             sliderInput("varlevmean", label = "Variabilité (écart-type)", min = 0, max = 20, value = 10, step = 1)),
                                      column(3,
                                             sliderInput("sampsizemean", label = "Taille de l'échantillon", min = 10, max = 999, value = 100, step = 1)),
                                      column(1,
                                             actionButton("getonemean", label = "1 tirage")),
                                      column(1, 
                                             actionButton("gettenmean", label = "10 tirages")),
                                      column(1,
                                             actionButton("gethundredmean", label = "100 tirages")),
                                      column(1,
                                             actionButton("resetmean", label = "Remettre à zéro"))))),
             fluidRow(
               column(6,
                      plotOutput("plotpopmean"),
                      dataTableOutput("tabmean")),
               column(6, 
                      plotOutput("plothist"),
                      sliderInput("levalphamean", label = "Choisir le seuil alpha", min = 0.01, max = 0.5, value = 0.05, step = 0.01),
                      plotOutput("meandistrib")))),
    
    tabPanel("Comparaison de moyennes",
             fluidRow(column(12,
                             fluidRow(column(3,
                                             sliderInput("varlevcomp", label = "Variabilité (écart-type)", min = 0, max = 20, value = 10, step = 1)),
                                      column(3,
                                             sliderInput("sampsizecomp", label = "Taille de l'échantillon", min = 10, max = 499, value = 50, step = 1)),
                                      column(1,
                                             actionButton("getonecomp", label = "1 tirage")),
                                      column(1, 
                                             actionButton("gettencomp", label = "10 tirages")),
                                      column(1,
                                             actionButton("gethundredcomp", label = "100 tirages")),
                                      column(1,
                                             actionButton("resetcomp", label = "Remettre à zéro"))))),
             fluidRow(
               column(6,
                      plotOutput("plotpopcomp"),
                      dataTableOutput("tabcomp")),
               column(6, 
                      plotOutput("plothistcomp"),
                      sliderInput("levalphacomp", label = "Choisir le seuil alpha", min = 0.01, max = 0.5, value = 0.05, step = 0.01),
                      plotOutput("tdistrib")))),
    
    tabPanel("Contingence",
             fluidRow(column(12,
                             fluidRow(column(6,
                                             sliderInput("sampsizechi", label = "Taille de l'échantillon", min = 10, max = 999, value = 100, step = 1, width = "80%")),
                                      column(1,
                                             actionButton("getonechi", label = "1 tirage")),
                                      column(1, 
                                             actionButton("gettenchi", label = "10 tirages")),
                                      column(1,
                                             actionButton("gethundredchi", label = "100 tirages")),
                                      column(1,
                                             actionButton("resetchi", label = "Remettre à zéro")))
             )),
             fluidRow(
               column(6,
                      plotOutput("plotpopchi"),
                      dataTableOutput("tabchi")),
               column(6, 
                      plotOutput("plotmosaic"),
                      sliderInput("levalphachi", label = "Choisir le seuil alpha", min = 0.01, max = 0.5, value = 0.05, step = 0.01),
                      plotOutput("chidistrib"))))
    
  )
)
)
