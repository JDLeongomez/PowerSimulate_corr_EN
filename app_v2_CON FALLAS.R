# PowerSimulate: Correlation
# Enhanced version with modern UI and improved functionality

# Load required libraries
library(shiny)
library(bslib)
library(tidyverse)
library(faux)
library(ggExtra)
library(ggpubr)
library(scales)
library(shinyWidgets)
library(rmarkdown)
library(knitr)
library(pwr)
library(shinycssloaders)  # Added for withSpinner function

# Define UI
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "darkly",
    primary = "#ff5555",
    "navbar-bg" = "#292b2c"
  ),
  
  # Header
  layout_column_wrap(
    width = 1,
    card(
      card_header(
        h1("PowerSimulate: Correlation", class = "text-center"),
        class = "bg-primary text-white"
      ),
      p(class = "text-center",
        "Power analysis based on the simulation of a population and the probability of obtaining a significant result with a sample of a given size.",
        br(),
        "This application uses simulations to illustrate the concept of statistical power for correlation tests."
      )
    )
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 8),
    
    # Parameters card
    card(
      card_header("Parameters", class = "bg-primary text-white"),
      navset_pill(
        nav_panel(
          "Simulation Settings",
          sliderInput(inputId = "corrxy", 
                      label = "Correlation coefficient (Pearson)", 
                      min = -1, max = 1, value = 0.39, step = 0.01),
          sliderInput(inputId = "sample_size",
                      label = "Sample size",
                      min = 5, max = 1000, value = 50, step = 1),
          sliderInput(inputId = "alpha",
                      label = HTML("Significance level (α)"),
                      min = 0.001, max = 0.1, value = 0.05, step = 0.001),
          selectInput(inputId = "alts",
                      label = "Hypothesis",
                      choices = c("Any correlation" = "two.sided", 
                                  "Positive correlation" = "greater",
                                  "Negative correlation" = "less")),
          numericInput(inputId = "reps",
                       label = "Number of simulations",
                       min = 100, max = 10000, value = 1000, step = 100),
          actionButton("runSim", "Run Simulation", class = "btn-primary w-100", 
                       icon = icon("play"))
        ),
        nav_panel(
          "Variable Parameters",
          layout_column_wrap(
            width = 1,
            card(
              card_header("Variable X", class = "bg-secondary"),
              textInput(inputId = "labelx", label = "Label for X variable", value = "Height (cm)"),
              numericInput(inputId = "meanx", label = "Mean", min = -Inf, max = Inf, value = 172.2, step = 0.1),
              numericInput(inputId = "sdx", label = "Standard deviation", min = 0.0001, max = Inf, value = 6.4, step = 0.1)
            ),
            card(
              card_header("Variable Y", class = "bg-secondary"),
              textInput(inputId = "labely", label = "Label for Y variable", value = "Weight (kg)"),
              numericInput(inputId = "meany", label = "Mean", min = -Inf, max = Inf, value = 68.2, step = 0.1),
              numericInput(inputId = "sdy", label = "Standard deviation", min = 0.0001, max = Inf, value = 10.5, step = 0.1)
            )
          )
        )
      )
    ),
    
    # Results card
    card(
      card_header("Results", class = "bg-primary text-white"),
      navset_pill(
        nav_panel(
          "Population Distribution",
          plotOutput("effectPlot", height = "400px") |> withSpinner(color = "#ff5555"),
          p(class = "text-center", "This plot shows the simulated population with the specified correlation.")
        ),
        nav_panel(
          "Power Analysis",
          plotOutput("powerPlot", height = "400px") |> withSpinner(color = "#ff5555"),
          card(
            htmlOutput("powText")
          ),
          downloadButton("downloadPower", "Download Power Plot", class = "btn-secondary"),
          downloadButton("downloadReport", "Download Report", class = "btn-secondary")
        ),
        nav_panel(
          "Sample Simulator",
          numericInput("single_sample_size", "Draw a single sample of size:", min = 5, max = 1000, value = 50),
          actionButton("drawSample", "Draw Sample", class = "btn-secondary"),
          plotOutput("samplePlot", height = "400px") |> withSpinner(color = "#ff5555"),
          htmlOutput("sampleStats")
        )
      )
    )
  ),
  
  # Footer
  card(
    p(class = "text-center",
      "Enhanced version of PowerSimulate: Correlation",
      br(),
      "Based on the original by Juan David Leongómez"
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Create reactive values
  rv <- reactiveValues(
    population = NULL,
    simulation_results = NULL,
    current_sample = NULL,
    power = NULL
  )
  
  # Generate population when parameters change
  observe({
    rv$population <- rnorm_multi(
      n = 10000, 
      mu = c(input$meanx, input$meany),
      sd = c(input$sdx, input$sdy),
      r = input$corrxy, 
      varnames = c("Xvar", "Yvar"),
      empirical = TRUE
    )
  })
  
  # Population distribution plot 
  output$effectPlot <- renderPlot({
    req(rv$population)
    
    p <- ggplot(rv$population, aes(x = Xvar, y = Yvar)) +
      geom_point(alpha = 0.2, color = "#ff555560") +
      geom_smooth(method = "lm", color = "#4075de") +
      annotate("text", x = -Inf, y = Inf, 
               hjust = -0.2, vjust = 2, size = 6,
               label = paste0("r = ", input$corrxy)) +
      stat_regline_equation(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~")),
                            label.x = -Inf, label.y = Inf,
                            hjust = -0.1, vjust = 3) +
      labs(x = input$labelx, y = input$labely) +
      theme_bw(base_size = 14)
    
    ggMarginal(p, type = "density", fill = "#ff5555")
  })
  
  # Run power simulation when button is clicked
  observeEvent(input$runSim, {
    withProgress(message = "Running simulations...", value = 0, {
      results <- tibble()
      
      for(i in 1:input$reps) {
        # Update progress bar
        incProgress(1/input$reps, detail = paste("Simulation", i, "of", input$reps))
        
        # Sample from population
        sample_data <- rv$population %>% sample_n(input$sample_size)
        
        # Run correlation test
        test_result <- cor.test(
          x = sample_data$Xvar, 
          y = sample_data$Yvar,
          alternative = input$alts
        )
        
        # Store results
        results <- bind_rows(
          results,
          tibble(
            sample = i,
            r = test_result$estimate,
            p = test_result$p.value,
            significant = p <= input$alpha
          )
        )
      }
      
      rv$simulation_results <- results
      rv$power <- mean(results$significant)
    })
  })
  
  # Update single_sample_size when sample_size changes
  observe({
    updateNumericInput(session, "single_sample_size", value = input$sample_size)
  })
  
  # Draw a single sample
  observeEvent(input$drawSample, {
    # Rest of your existing code remains the same
    rv$current_sample <- rv$population %>% sample_n(input$single_sample_size)
    
    # Calculate correlation
    cor_test <- cor.test(
      x = rv$current_sample$Xvar, 
      y = rv$current_sample$Yvar,
      alternative = input$alts
    )
    
    rv$sample_result <- list(
      r = cor_test$estimate,
      p = cor_test$p.value,
      significant = cor_test$p.value <= input$alpha,
      n = input$single_sample_size
    )
  })
  
  # Sample plot
  output$samplePlot <- renderPlot({
    req(rv$current_sample)
    
    ggplot(rv$current_sample, aes(x = Xvar, y = Yvar)) +
      geom_point(size = 3, color = "#ff5555") +
      geom_smooth(method = "lm", color = "#4075de") +
      annotate("text", x = -Inf, y = Inf, 
               hjust = -0.2, vjust = 2, size = 6,
               label = paste0("Sample r = ", round(rv$sample_result$r, 3))) +
      labs(
        x = input$labelx, 
        y = input$labely,
        title = paste("Random Sample (n =", rv$sample_result$n, ")"),
        subtitle = paste0(
          "p = ", round(rv$sample_result$p, 4), 
          " (", ifelse(rv$sample_result$significant, "Significant", "Not significant"), ")"
        )
      ) +
      theme_bw(base_size = 14)
  })
  
  # Sample statistics
  output$sampleStats <- renderUI({
    req(rv$sample_result)
    
    sig_color <- ifelse(rv$sample_result$significant, "#5cb85c", "#d9534f")
    
    HTML(paste0(
      "<div class='card'>",
      "<div class='card-body'>",
      "<h5>Sample Statistics:</h5>",
      "<ul>",
      "<li>Sample correlation (r): <b>", round(rv$sample_result$r, 3), "</b></li>",
      "<li>p-value: <b>", round(rv$sample_result$p, 4), "</b></li>",
      "<li>Significance: <b style='color:", sig_color, "'>", 
      ifelse(rv$sample_result$significant, "Significant", "Not significant"), "</b></li>",
      "<li>True population correlation: <b>", input$corrxy, "</b></li>",
      "</ul>",
      "<p>This is <b>one</b> possible sample from the population. ",
      "Statistical power represents the probability of getting a significant result ",
      "across many such samples.</p>",
      "</div></div>"
    ))
  })
  
  
  # Power simulation plot
  output$powerPlot <- renderPlot({
    req(rv$simulation_results)
    
    ggplot(rv$simulation_results, aes(x = p, fill = significant)) +
      geom_histogram(bins = 30, alpha = 0.8) +
      scale_fill_manual(
        values = c("TRUE" = "#5cb85c", "FALSE" = "#d9534f"),
        labels = c("TRUE" = "Significant", "FALSE" = "Non-significant"),
        name = "Result"
      ) +
      geom_vline(xintercept = input$alpha, linetype = "dashed", color = "white") +
      annotate("text", x = input$alpha + 0.02, y = Inf, 
               label = paste("α =", input$alpha), 
               vjust = 2, hjust = 0, color = "white") +
      annotate("text", x = 0.5, y = Inf, 
               label = paste0("Power = ", round(rv$power, 3)), 
               vjust = 4, hjust = 0.5, color = "white", size = 5) +
      annotate("text", x = 0.5, y = Inf, 
               label = paste0("Sample size = ", input$sample_size), 
               vjust = 6, hjust = 0.5, color = "white") +
      labs(
        title = "Power Analysis Results",
        subtitle = paste0("Based on ", input$reps, " simulations with r = ", input$corrxy),
        x = "p-value",
        y = "Count"
      ) +
      theme_bw(base_size = 14) +
      theme(
        legend.position = "bottom",
        panel.background = element_rect(fill = "#333333"),
        plot.background = element_rect(fill = "#333333"),
        panel.grid = element_line(color = "#444444"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(color = "white"),
        legend.background = element_rect(fill = "#333333"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")
      )
  })
  
  # Power text explanation
  output$powText <- renderUI({
    req(rv$power)
    
    HTML(paste0(
      "<div class='p-3'>",
      "<h5 class='text-primary'>INTERPRETATION:</h5>",
      "<p>The power (", round(rv$power * 100, 1), "%) is the proportion of significant results (<i>p</i> < α).</p>",
      "<p>If the true correlation in the population is <b style='color:#ff5555;'><i>r</i> = ", input$corrxy, 
      "</b>, with random samples of <b style='color:#ff5555;'><i>n</i> = ", input$sample_size,
      "</b>, you would get a significant result in approximately <b style='color:#ff5555;'>", 
      percent(rv$power), "</b> of cases.</p>",
      "</div>"
    ))
  })
  
  # Download handlers for the power plot
  output$downloadPower <- downloadHandler(
    filename = function() {
      paste("power_analysis_r", input$corrxy, "_n", input$sample_size, "_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = {
        ggplot(rv$simulation_results, aes(x = p, fill = significant)) +
          geom_histogram(bins = 30, alpha = 0.8) +
          scale_fill_manual(
            values = c("TRUE" = "#5cb85c", "FALSE" = "#d9534f"),
            labels = c("TRUE" = "Significant", "FALSE" = "Non-significant"),
            name = "Result"
          ) +
          geom_vline(xintercept = input$alpha, linetype = "dashed") +
          annotate("text", x = input$alpha + 0.02, y = Inf, 
                   label = paste("α =", input$alpha), 
                   vjust = 2, hjust = 0) +
          annotate("text", x = 0.5, y = Inf, 
                   label = paste0("Power = ", round(rv$power, 3)), 
                   vjust = 4, hjust = 0.5, size = 5) +
          annotate("text", x = 0.5, y = Inf, 
                   label = paste0("Sample size = ", input$sample_size), 
                   vjust = 6, hjust = 0.5) +
          labs(
            title = "Power Analysis Results",
            subtitle = paste0("Based on ", input$reps, " simulations with r = ", input$corrxy),
            x = "p-value",
            y = "Count"
          ) +
          theme_bw(base_size = 14)
      }, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Download report handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("power_report_", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      # Create a temporary report file
      tmp_report <- tempfile(fileext = ".Rmd")
      
      # Define alternative text based on the selected alternative
      alt_text <- case_when(
        input$alts == "two.sided" ~ "Any correlation",
        input$alts == "greater" ~ "Positive correlation",
        input$alts == "less" ~ "Negative correlation"
      )
      
      # Write the report content
      cat(
        '---
title: "Statistical Power Analysis Report"
date: "`r Sys.Date()`"
output: html_document
params:
  corrxy: !r', input$corrxy, '
  sample_size: !r', input$sample_size, '
  alpha: !r', input$alpha, '
  alt_text: "', alt_text, '"
  alt_type: "', input$alts, '"
  reps: !r', input$reps, '
  power: !r', rv$power, '
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(scales)
library(pwr)
library(dplyr)
```

# Create results data frame
```{r}
results <- data.frame(
  p = c(', paste(rv$simulation_results$p, collapse = ","), '),
  significant = c(', paste(as.character(rv$simulation_results$significant), collapse = ","), ')
)
```

# Convert significant to logical
```{r}
results$significant <- results$significant == "TRUE"
```
# Power Analysis for Correlation
This report summarizes the results of a power analysis for detecting a correlation of r = `r params$corrxy` using a sample size of n = `r params$sample_size`.

## Parameters
* True population correlation: `r params$corrxy`
* Sample size: `r params$sample_size`
* Significance level (α): `r params$alpha`
* Test direction: `r params$alt_text`
* Number of simulations: `r params$reps`

## Results
* Estimated power: `r round(params$power * 100, 1)`%

This means that with the given parameters, you would detect a significant effect in approximately `r round(params$power * 100, 1)`% of studies.

## Power Plot
```{r}
ggplot(results, aes(x = p, fill = significant)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  scale_fill_manual(
    values = c("TRUE" = "#5cb85c", "FALSE" = "#d9534f"),
    labels = c("TRUE" = "Significant", "FALSE" = "Non-significant"),
    name = "Result"
  ) +
  geom_vline(xintercept = params$alpha, linetype = "dashed") +
  annotate("text", x = params$alpha + 0.02, y = Inf, 
           label = paste("α =", params$alpha), 
           vjust = 2, hjust = 0) +
  labs(
    title = "Power Analysis Results",
    subtitle = paste0("Based on ", params$reps, " simulations with r = ", params$corrxy),
    x = "p-value",
    y = "Count"
  ) +
  theme_bw(base_size = 14)
```

## Interpretation
The power of `r round(params$power * 100, 1)`% indicates that if the true correlation in the population is r = `r params$corrxy`, with a random sample of n = `r params$sample_size`, you would obtain a statistically significant result in approximately `r round(params$power * 100, 1)`% of studies.

A conventionally acceptable level of power is 80% or higher. Based on this analysis, the current design is `r ifelse(params$power >= 0.8, "adequately powered", "underpowered")`.

# Create a function to compute required sample size for different power levels
```{r}
compute_sample_sizes <- function(correlation, power_levels, alpha = 0.05, alternative = "two.sided") {
  sample_sizes <- sapply(power_levels, function(pow) {
    pwr::pwr.r.test(r = abs(correlation), power = pow, sig.level = alpha, alternative = alternative)$n
  })
  return(ceiling(sample_sizes))
}
```

# Generate sample size recommendations
```{r}
power_levels <- c(0.7, 0.8, 0.9, 0.95)
sample_sizes <- compute_sample_sizes(params$corrxy, power_levels, params$alpha, params$alt_type)
```

# Create a data frame for plotting
```{r include=FALSE}
recommendations <- data.frame(
  Power = power_levels,
  SampleSize = sample_sizes
)
```

# Plot the recommendations
```{r}
ggplot(recommendations, aes(x = Power, y = SampleSize)) +
  geom_line(size = 1, color = "#4075de") +
  geom_point(size = 3, color = "#ff5555") +
  geom_vline(xintercept = 0.8, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = params$sample_size, linetype = "dashed", color = "#ff5555") +
  annotate("text", x = 0.8, y = max(sample_sizes), label = "Conventional 80% power", 
           vjust = -0.5, hjust = 1.1, color = "gray50") +
  annotate("text", x = min(power_levels), y = params$sample_size, 
           label = paste("Current sample size:", params$sample_size), 
           vjust = -0.5, hjust = 0, color = "#ff5555") +
  scale_x_continuous(labels = scales::percent, limits = c(min(power_levels) - 0.05, max(power_levels) + 0.05)) +
  labs(
    title = "Sample Size Recommendations for Different Power Levels",
    subtitle = paste0("For correlation r = ", params$corrxy, " with α = ", params$alpha),
    x = "Statistical Power",
    y = "Required Sample Size"
  ) +
  theme_bw()  
```
  
## Sample Size Recommendations
For detecting a correlation of r = `r params$corrxy` with different power levels:

* For 70% power: n ≥ `r recommendations$SampleSize[1]`
* For 80% power: n ≥ `r recommendations$SampleSize[2]`
* For 90% power: n ≥ `r recommendations$SampleSize[3]`
* For 95% power: n ≥ `r recommendations$SampleSize[4]`

## Conclusion
```{r results="asis"}
if(params$power < 0.8) {
  cat(paste0("The current sample size of ", params$sample_size, " is insufficient to achieve the conventional 80% power level. Consider increasing the sample size to at least ", recommendations$SampleSize[2], " participants."))
} else {
  cat(paste0("The current sample size of ", params$sample_size, " is sufficient to achieve at least ", round(params$power * 100), "% power, which exceeds the conventional threshold of 80%."))
}
```

Report generated on `r Sys.Date()` using PowerSimulate', file = tmp_report)

# Render the report
rmarkdown::render(input = tmp_report, 
                  output_file = file, 
                  quiet = TRUE)

# Clean up temporary file AFTER rendering is complete
on.exit(unlink(tmp_report))

    } )

# Power text
output$powText <- renderUI({
  req(rv$power)
  
  # Format power value
  power_percent <- round(rv$power * 100, 1)
  
  # Determine power adequacy
  power_class <- ifelse(rv$power >= 0.8, "text-success", "text-danger")
  power_text <- ifelse(rv$power >= 0.8, "sufficient", "insufficient")
  
  # Get the formula for power calculation
  power_formula <- renderFormula(
    input$corrxy, input$sample_size, input$alpha, input$alts
  )
  
  # Create the HTML
  HTML(paste0(
    "<div class='text-center'>",
    "<h3>Estimated Power: <span class='", power_class, "'>", power_percent, "%</span></h3>",
    "<p>Based on ", input$reps, " simulations, this means that ", power_percent, "% of studies would find a significant result.</p>",
    "<p>This is considered <span class='", power_class, "'><b>", power_text, "</b></span> statistical power.</p>",
    "<hr>",
    "<h5>Analytical Power Calculation:</h5>",
    "<p>", power_formula, "</p>",
    "</div>"
  ))
})

# Helper function to generate power formula text
renderFormula <- function(r, n, alpha, alternative) {
  # Calculate analytical power
  pwr_test <- pwr::pwr.r.test(
    r = abs(r), 
    n = n, 
    sig.level = alpha,
    alternative = alternative
  )
  
  analytical_power <- round(pwr_test$power * 100, 1)
  
  # Create formula text
  paste0(
    "Analytical power calculation: ", analytical_power, "%<br>",
    "<small>(Using Cohen's power formula for correlation tests)</small>"
  )
}
}

# Run the application
shinyApp(ui = ui, server = server)