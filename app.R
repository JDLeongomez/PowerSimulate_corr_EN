#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse) 
library(faux)
library(ggExtra)
library(ggpubr)
library(plyr)
library(scales)

input <<- tibble(
  alts = "Any correlation",
  meanx = 172.2,
  meany = 68.2,
  sdx = 6.4,
  sdy = 10.5,
  labelx = "Heigth (cm)",
  labely = "Weight (kg)",
  corrxy = 0.39
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "PowerSimulate: Correlation"),
  HTML("<center><a href='https://shiny.jdl-svr.lat/PowerSimulate'><img src='powersimulate.svg'' width='600'></a></center>"),
  tags$h3(HTML("<center>Correlation</center>")),
  p(HTML("<center>Code available from
      <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate_corr_EN'>GitHub</a>
      - Created by
      <a style=color:#ff5555;  href='https://jdleongomez.info/en/'>Juan David Leongómez</a>, Universidad El Bosque
      · 2023 · <a style=color:#4075de;  href='https://shiny.jdl-svr.lat/PowerSimulate_corr_ES/'>
      Versión en español</a> 
      · List of <a style=color:#ff5555;  href='https://shiny.jdl-svr.lat/PowerSimulate_corr_EN'>PowerSimulate</a> apps.</center>")),
  hr(),
  p(HTML("<center>Power analysis based on the simulation of a population, and the probability of
         obtaining a significant result with a sample of a given size.<br>Although more direct 
         tools for power analysis exist for correlation tests, this application relies on 
         simulations to illustrate the concept of statistical power.</center>")),
  fluidRow(
    column(2,
           tags$h2("Variable parameters"),
           tags$h4("Variable X"),
           textInput(inputId = "labelx",
                     label = "Label for X variable",
                     value = "Heigth (cm)",
                     width = '300px'),
           numericInput(inputId = "meanx",
                        label = "Mean",
                        min = -Inf,
                        max = Inf,
                        value = 172.2,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sdx",
                        label = "Standard deviation",
                        min = -Inf,
                        max = Inf,
                        value = 6.4,
                        step = 0.0001,
                        width = '300px'),
           hr(),
           tags$h4("Variable Y"),
           textInput(inputId = "labely",
                     label = "Label for Y variable",
                     value = "Weight (kg)",
                     width = '300px'),
           numericInput(inputId = "meany",
                        label = "Mean",
                        min = -Inf,
                        max = Inf,
                        value = 68.2,
                        step = 0.0001,
                        width = '300px'),
           numericInput(inputId = "sdy",
                        label = "Standard deviation",
                        min = -Inf,
                        max = Inf,
                        value = 10.5,
                        step = 0.0001,
                        width = '300px')
    ),
    column(4,
           tags$h1("Population effect size"),
           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "corrxy",
                       label = "Correlation coefficient (Pearson)",
                       min = -1,
                       max = 1,
                       value = 0.39,
                       step = 0.01,
                       width = 'auto'),
           tags$h3("If this was correlation in the population"),
           plotOutput("effectPlot") %>% 
             withSpinner(color = "#ff5555")
    ),
    column(2,
           tags$h2("Simulation parameters"),
           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "sample_size",
                       label = "Sample size",
                       min = 5,
                       max = 1000,
                       value = 50,
                       step = 1,
                       width = '300px'),
           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background:#ff5555}")),
           sliderInput(inputId = "alpha",
                       label = HTML("Significance level (tipically &alpha; = 0.05)"),
                       min = 0,
                       max = 1,
                       value = 0.05,
                       step = 0.001,
                       width = '300px'),
           selectInput(inputId = "alts",
                       label = "Hypothesis",
                       choices = c("Any correlation", 
                                   "Positive correlation",
                                   "Negative correlation"
                       )),
           numericInput(inputId = "reps",
                        label = HTML("Number of simulations:
                                     <span style='font-weight:normal'>By default only 100 simulations are run, 
                                     but once you have checked all the parameters, I suggest that you run 1000 
                                     or more simulations to increase the accuracy (the more simulations you run, 
                                     the longer it will take).</span>"),
                        min = 1,
                        max = 1000000,
                        value = 100,
                        step = 1,
                        width = '300px'),
           nextGenShinyApps::submitButton("runSim", text = "All ready? Run the simulation!", 
                                          icon("paper-plane"), bg.type = "danger")
    ),
    column(4,
           tags$h1("Statistical power"),
           tags$h3("This is the statistical power you would reach"),
           plotOutput("powerPlot") %>% 
             withSpinner(color = "#ff5555"),
           htmlOutput("powText")
    )
  )
)

server <- function(input, output, session) {
  
  # Simulate population
  dat <- reactive({
    datos <- rnorm_multi(n = 10000, 
                         mu = c(input$meanx, input$meany),
                         sd = c(input$sdx, input$sdy),
                         r = input$corrxy, 
                         varnames = c("Xvar", "Yvar"),
                         empirical = TRUE)
    return(datos)
  })
  
  # Population distribution plot 
  output$effectPlot <- renderPlot({
    p <- ggplot(dat(), aes(x = Xvar, y = Yvar)) +
      geom_point(alpha = 0.2, color = "#ff555560") +
      geom_smooth(method = "lm") +
      annotate("text", x = -Inf, y = Inf, 
               hjust = -0.2, vjust = 2, size = 6,
               label = paste0("r = ", input$corrxy)) +
      stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~")),
                            label.x = -Inf, label.y = Inf,
                            hjust = -0.1, vjust = 3) +
      labs(x = input$labelx, y = input$labely)
    ggMarginal(p, type = "density", fill = "#ff5555")
  })

  # Create object with selected hypothesis alternative
  altern <<- reactive({
    dplyr::case_when(
      input$alts == "Any correlation" ~ "two.sided",
      input$alts == "Positive correlation" ~ "greater",
      TRUE ~ "less")
  })
  
  sig.lev <<- reactive({
    input$alpha
  })
  
  # Simulate samples and test significance in each
  dat.sim <- reactive({
    req(input$alts)
    dato <- ddply(map_dfr(seq_len(input$reps), ~dat() %>%
                            sample_n(input$sample_size) %>%
                            mutate(sample = as.factor(.x))),
                  .(sample), summarise,
                  p = round(cor.test(x = Xvar, y = Yvar,
                                     alternative = altern())$p.value, 3),
                  "Significance" = ifelse(p <= sig.lev(), "Significant", "Non-significant"))
    return(dato)
  })
  
  # Power simulation plot 
  output$powerPlot <- renderPlot({
    ggplot(dat.sim(), aes(x = p, fill = Significance)) +
      scale_fill_hue(direction = -1) +
      geom_histogram(bins = 1/input$alpha, breaks = seq(0, 1, input$alpha), 
                     alpha = 0.8) +
      scale_fill_manual(values = c("#4075de", "#ff5555")) +
      labs(y = "Count", x = "p-value") +
      scale_x_continuous(breaks = pretty_breaks(n = 20)) +
      annotate("text", x = 0.5, y = Inf, size = 7, vjust = 2,
               label = paste0("Power (1 - β) = ", round(sum(dat.sim()$Significance == "Significant") / input$reps, 2))) +
      annotate("text", x = 0.5, y = Inf, vjust = 5,
               label = paste0("Sample size = ", input$sample_size)) +
      annotate("text", x = 0.5, y = Inf, vjust = 6.5,
               label = paste0("α = ", input$alpha)) +
      theme(legend.position="bottom", 
            legend.title=element_text(size=14),
            legend.text = element_text(size = 12)) +
      guides(fill = guide_legend(reverse=TRUE))
  })
  
  output$powText <- renderText({
    paste("<b style=color:#ff5555;>INTERPRETATION: </b>
          The power is nothing more than the proportion of significant results 
          (<em>p</em> < α). So, if the true correlation in the population was <font color=\'#ff5555\'><b><em>r</em> = ",
          input$corrxy, "</b></font>, with a random sample of <font color=\'#ff5555\'><b><em>n</em> = ", input$sample_size, 
          "</b></font>, you would get a significant result in aproximately <font color=\'#ff5555\'><b>", 
          percent(round(sum(dat.sim()$Significance == "Significant") / input$reps, 2)),
          "</b></font> of the cases.")
  })
}

# Same theme for plots
thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
