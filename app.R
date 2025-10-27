
check_data <- function(df, threshold = 0.3) {
  if (!is.data.frame(df)) stop("Input must be a data.frame")
  if (nrow(df) == 0) stop("Data is empty")
  if (ncol(df) == 0) stop("No columns in data")

  na_cols <- sapply(df, function(x) all(is.na(x)))
  if (any(na_cols)) {
    stop(paste("Columns with all NA:", paste(names(df)[na_cols], collapse = ", ")))
  }

  message(" Converting categorical variables to factors...")
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
  }

  message(" Removing duplicated rows...")
  df <- df[!duplicated(df), ]

  message(" Handling missing values...")
  na_ratio <- sapply(df, function(x) mean(is.na(x)))
  if (any(na_ratio > threshold)) {
    message("️ Dropping variables with > ", threshold * 100, "% missing values: ",
            paste(names(df)[na_ratio > threshold], collapse = ", "))
    df <- df[, na_ratio <= threshold]
  }

  for (col in names(df)) {
    if (anyNA(df[[col]])) {
      if (is.numeric(df[[col]])) {
        mean_val <- mean(df[[col]], na.rm = TRUE)
        df[[col]][is.na(df[[col]])] <- mean_val
      } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
        mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
        df[[col]][is.na(df[[col]])] <- mode_val
        if (is.factor(df[[col]])) {
          df[[col]] <- droplevels(df[[col]])
        }
      }
    }
  }

  message("Data check & cleaning completed.")
  return(df)
}
library(R6)
library(arules)
library(arulesCBA)
library(discretization)
library(arulesViz)
library(DT)

quick_bin <- function(x, bins = 4) {
  cut(x, breaks = bins, labels = FALSE, include.lowest = TRUE)
}

CustomerSegmenter <- R6Class("CustomerSegmenter",
                             public = list(
                               raw_data = NULL,
                               processed_data = NULL,
                               binned_data = NULL,
                               transactions = NULL,
                               rules = NULL,

                               initialize = function(data) {
                                 self$raw_data <- private$check_data(data)
                                 self$processed_data <- self$raw_data
                               },

                               preprocess = function(factor_cols = NULL) {
                                 message("No additional preprocessing needed as data was standardized during check_data().")
                               },

                               bin_numeric_vars = function(method = c("auto", "manual"), target_var = NULL, bin_counts = list()) {
                                 method <- match.arg(method)
                                 numeric_cols <- sapply(self$processed_data, is.numeric)
                                 df_num <- self$processed_data[, numeric_cols, drop = FALSE]

                                 if (ncol(df_num) == 0) {
                                   warning("No numeric variables to bin")
                                   return(NULL)
                                 }

                                 if (method == "auto") {
                                   if (is.null(target_var) || !(target_var %in% colnames(self$processed_data))) {
                                     stop("For 'auto' method, you must provide a target variable name 'target_var'.")
                                   }
                                   df_supervised <- self$processed_data[, c(target_var, names(df_num))]
                                   formula_text <- as.formula(paste(target_var, "~ ."))
                                   med.caim <- discretizeDF.supervised(formula_text, data = df_supervised, method = "caim")
                                   self$processed_data[, names(med.caim)] <- med.caim
                                   message("Auto binning by CAIM method completed.")
                                   print(summary(med.caim))
                                 } else {
                                   for (col in names(bin_counts)) {
                                     n_bins <- bin_counts[[col]]
                                     self$processed_data[[col]] <- quick_bin(self$processed_data[[col]], n_bins)
                                     self$processed_data[[col]] <- as.factor(self$processed_data[[col]])
                                     message(paste0("Binned variable", col, " into ", n_bins, " groups."))
                                   }
                                 }

                                 self$binned_data <- self$processed_data
                                 message("Binning process completed.")
                               },

                               run_association_rules = function(support = 0.03, confidence = 0.1) {
                                 self$transactions <- as(self$binned_data, "transactions")
                                 self$rules <- apriori(self$transactions, parameter = list(supp = support, conf = confidence))
                                 message(paste(" Generated", length(self$rules), "association rules."))
                               },

                               run_cars_rules = function(target_var, support = 0.03, confidence = 0.1) {
                                 if (is.null(self$binned_data)) stop("Binned data not found. Please run bin_numeric_vars() first.")
                                 formula <- as.formula(paste(target_var, "~ ."))
                                 self$transactions <- transactions(self$binned_data)
                                 self$rules <- mineCARs(formula, self$transactions, support = support, confidence = confidence)
                                 message(paste(" Generated", length(self$rules), "targeted rules."))
                               },

                               clean_rules = function() {
                                 if (is.null(self$rules)) stop("️ No rules to clean.")
                                 self$rules <- self$rules[!is.redundant(self$rules)]
                                 self$rules <- self$rules[is.significant(self$rules, self$transactions)]
                                 self$rules <- self$rules[is.maximal(self$rules)]
                                 message(paste(" Cleaned to", length(self$rules), "rules."))
                               },

                               get_rules_summary = function() {
                                 if (is.null(self$rules)) return(NULL)
                                 summary(self$rules)
                               },

                               inspect_rules_dt = function() {
                                 if (is.null(self$rules)) stop("️ No rules to inspect.")
                                 inspectDT(self$rules)
                               },

                               plot_rules_graph = function() {
                                 if (is.null(self$rules)) stop("️ No rules to plot.")
                                 plot(self$rules, method = "graph", engine = "htmlwidget")
                               },

                               plot_rules_metrics = function() {
                                 if (is.null(self$rules)) stop("️ No rules to plot.")
                                 plot(self$rules, measure = c("support", "lift"), shading = "confidence")
                               }
                             ),

                             private = list(
                               check_data = function(data) {
                                 if (!is.data.frame(data)) stop("Data must be a data.frame.")
                                 data <- na.omit(data)
                                 factor_cols <- sapply(data, function(x) is.character(x) || is.logical(x))
                                 data[, factor_cols] <- lapply(data[, factor_cols, drop = FALSE], as.factor)
                                 return(data)
                               }
                             )
)
library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("🔍 Customer Behavior with Association Rules"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", " Upload CSV File", accept = c(".csv")),
      checkboxGroupInput("selected_vars", " Select variables to KEEP", choices = NULL, selected = NULL),
      radioButtons("bin_method", " Binning Method", choices = c("Auto (CAIM)" = "auto", "Manual" = "manual")),
      conditionalPanel(condition = "input.bin_method == 'auto'", uiOutput("target_selector")),
      conditionalPanel(condition = "input.bin_method == 'manual'", uiOutput("bin_sliders")),
      numericInput("support", "Min Support", 0.03, min = 0.001, max = 1, step = 0.01),
      numericInput("confidence", "Min Confidence", 0.1, min = 0.001, max = 1, step = 0.01),
      actionButton("run", " Run Analysis")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(" Distribution", uiOutput("distPlots")),
        tabPanel(" Rules Summary", verbatimTextOutput("summary")),
        tabPanel(" Rules Table", dataTableOutput("rules_dt")),
        tabPanel(" Graph", uiOutput("graphPlot")),
        tabPanel(" Metrics", plotOutput("metricsPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  data_raw <- reactiveVal(NULL)
  seg <- reactiveVal(NULL)

  observeEvent(input$file1, {
    df <- read.csv(input$file1$datapath)
    df_checked <- CustomerSegmenter$private_methods$check_data(df)
    data_raw(df_checked)

    updateCheckboxGroupInput(session, "selected_vars", choices = names(df_checked), selected = names(df_checked))
  })

  output$target_selector <- renderUI({
    req(data_raw())
    df <- data_raw()
    factor_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    selectInput("target_var", " Select Target Variable", choices = factor_vars)
  })

  output$bin_sliders <- renderUI({
    req(data_raw())
    df <- data_raw()
    num_cols <- names(df)[sapply(df, is.numeric)]
    lapply(num_cols, function(col) {
      sliderInput(inputId = paste0("bin_", col), label = paste("Bins for", col), min = 2, max = 10, value = 4)
    })
  })

  observeEvent(input$run, {
    req(data_raw(), input$selected_vars)
    tryCatch({
      df_filtered <- data_raw()[, input$selected_vars, drop = FALSE]
      cs <- CustomerSegmenter$new(df_filtered)

      if (input$bin_method == "auto") {
        req(input$target_var)
        cs$bin_numeric_vars(method = "auto", target_var = input$target_var)
        cs$run_cars_rules(target_var = input$target_var, support = input$support, confidence = input$confidence)
      } else {
        df <- df_filtered
        bin_list <- list()
        for (col in names(df)[sapply(df, is.numeric)]) {
          bin_input <- input[[paste0("bin_", col)]]
          if (!is.null(bin_input)) bin_list[[col]] <- bin_input
        }
        cs$bin_numeric_vars(method = "manual", bin_counts = bin_list)
        cs$run_association_rules(support = input$support, confidence = input$confidence)
      }

      cs$clean_rules()
      seg(cs)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  output$summary <- renderPrint({ req(seg()); seg()$get_rules_summary() })
  output$rules_dt <- renderDataTable({ req(seg()); inspectDT(seg()$rules) })
  output$graphPlot <- renderUI({ req(seg()); seg()$plot_rules_graph() })
  output$metricsPlot <- renderPlot({ req(seg()); seg()$plot_rules_metrics() })

  output$distPlots <- renderUI({
    req(data_raw())
    df <- data_raw()
    plot_output_list <- lapply(names(df), function(col) {
      plotname <- paste0("plot_", col)
      plotOutput(plotname, height = "300px")
    })
    do.call(tagList, plot_output_list)
  })

  observe({
    req(data_raw())
    df <- data_raw()
    for (col in names(df)) {
      local({
        column <- col
        output[[paste0("plot_", column)]] <- renderPlot({
          if (is.numeric(df[[column]])) {
            ggplot(df, aes_string(x = column)) +
              geom_histogram(bins = 20, fill = "steelblue", color = "white") +
              labs(title = paste("Distribution of", column)) + theme_minimal()
          } else {
            ggplot(df, aes_string(x = column)) +
              geom_bar(fill = "orange") +
              labs(title = paste("Distribution of", column)) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }
        })
      })
    }
  })
}

shinyApp(ui, server)
