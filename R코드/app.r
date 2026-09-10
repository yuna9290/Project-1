library(shiny)
library(bslib)
library(shiny)
library(bslib)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(ggplot2)
source("functions.R")


# UI
ui <- page_sidebar(
  title = tags$span("Diffusion Model Demand Forecasting", style = "color:white; font-weight:600;"),
  theme = bs_theme(version = 5, bootswatch = "cosmo", primary = "#212529"),
  
  tags$head(tags$style(HTML("
    .movie-info-grid {display:grid; grid-template-columns:125px 1px 1fr; column-gap:15px; row-gap:18px; align-items:center;}
    .movie-info-label {font-weight:700; color:#343a40;}
    .movie-info-divider {background-color:#c7c7c7; width:1px; height:100%; min-height:24px;}
    .movie-info-value {color:#343a40;}
  "))),
  
  sidebar = sidebar(
    h5("Data Upload"),
    fileInput("file", "Upload Data", accept = c(".csv", ".xlsx", ".xls")),
    hr(),
    
    h5("Movie"),
    selectInput("movie", NULL, choices = c("왕과 사는 남자", "오디세이")),
    hr(),
    
    h5("Diffusion Model"),
    radioButtons("model", NULL, choices = c("Bass", "Logistic", "Gumbel", "Exponential")),
    hr(),
    
    h5("Estimation Method"),
    radioGroupButtons(inputId = "method", label = NULL, choices = c("OLS", "Q-Q plot", "MLE"), justified = TRUE, status = "dark"),
    hr(),
    
    h5("Weekend / Holiday Adjustment"),
    selectInput("holiday", NULL, choices = c("No Adjustment" = 1, "Weekend/Holiday = 2 Weekdays" = 2, "Weekend/Holiday = 3 Weekdays" = 3)),
    hr(),
    
    numericInput("horizon", "Prediction Horizon", value = 7, min = 1, max = 365)
  ),
  
  layout_columns(
    value_box(title = tags$div("Market Potential (m)", style = "color:#212529; font-weight:bold; border-bottom:2px solid #212529; padding-bottom:7px; margin-bottom:8px;"), value = textOutput("m_value")),
    value_box(title = tags$div("Coefficient of Innovation (p)", style = "color:#212529; font-weight:bold; border-bottom:2px solid #212529; padding-bottom:7px; margin-bottom:8px;"), value = textOutput("p_value")),
    value_box(title = tags$div("Coefficient of Imitation (q)", style = "color:#212529; font-weight:bold; border-bottom:2px solid #212529; padding-bottom:7px; margin-bottom:8px;"), value = textOutput("q_value"))
  ),
  
  layout_columns(
    card(card_header("Time-Series Forecast"), plotOutput("forecast_plot", height = "330px")),
    
    card(
      card_header("Movie Information"),
      card_body(
        div(
          class = "movie-info-grid",
          
          div(class = "movie-info-label", "Title"),
          div(class = "movie-info-divider"),
          div(class = "movie-info-value", textOutput("title_info", inline = TRUE)),
          
          div(class = "movie-info-label", "Director"),
          div(class = "movie-info-divider"),
          div(class = "movie-info-value", textOutput("director_info", inline = TRUE)),
          
          div(class = "movie-info-label", "Cast"),
          div(class = "movie-info-divider"),
          div(class = "movie-info-value", textOutput("cast_info", inline = TRUE)),
          
          div(class = "movie-info-label", "Genre"),
          div(class = "movie-info-divider"),
          div(class = "movie-info-value", textOutput("genre_info", inline = TRUE)),
          
          div(class = "movie-info-label", "Analysis Period"),
          div(class = "movie-info-divider"),
          div(class = "movie-info-value", textOutput("period_info", inline = TRUE)),
          
          div(class = "movie-info-label", "Cumulative Audience"),
          div(class = "movie-info-divider"),
          div(class = "movie-info-value", textOutput("audience_info", inline = TRUE))
        )
      )
    ),
    
    col_widths = c(8, 4)
  ),
  
  card(card_header("Predicted Values"), tableOutput("prediction_table"))
)


# SERVER
server <- function(input, output, session) {
  
  # 업로드 파일
  uploaded_data <- reactive({
    
    req(input$file)
    
    ext <- tolower(
      tools::file_ext(input$file$name)
    )
    
    if (ext == "csv") {
      
      df <- read.csv(
        input$file$datapath,
        skip = 3,
        header = TRUE,
        check.names = FALSE,
        fileEncoding = "UTF-8-BOM"
      )
      
    } else if (ext %in% c("xlsx", "xls")) {
      
      df <- readxl::read_excel(
        input$file$datapath,
        skip = 3
      )
      
    } else {
      
      stop(
        "CSV 또는 Excel 파일만 업로드할 수 있습니다."
      )
    }
    
    validate(
      need(
        all(
          c(
            "날짜",
            "관객수",
            "누적관객수"
          ) %in% names(df)
        ),
        "파일에 날짜, 관객수, 누적관객수 열이 필요합니다."
      )
    )
    
    df %>%
      select(
        날짜,
        관객수,
        누적관객수
      ) %>%
      mutate(
        날짜 = as.Date(
          날짜,
          "%Y.%m.%d"
        ),
        관객수 = as.numeric(
          gsub(",", "", 관객수)
        ),
        누적관객수 = as.numeric(
          gsub(",", "", 누적관객수)
        )
      ) %>%
      filter(
        !is.na(날짜),
        !is.na(관객수),
        !is.na(누적관객수)
      ) %>%
      arrange(날짜)
  })
  
  
  # 영화별 개봉일
  release_date <- reactive({
    
    if (input$movie == "왕과 사는 남자") {
      as.Date("2026-02-04")
    } else {
      as.Date("2026-08-05")
    }
  })
  
  
  # 원본 영화 데이터
  raw_movie_data <- reactive({
    
    df <- uploaded_data() %>%
      filter(
        날짜 >= release_date()
      ) %>%
      arrange(날짜)
    
    validate(
      need(
        nrow(df) > 0,
        "개봉일 이후 데이터가 없습니다."
      )
    )
    
    df
  })
  
  
  # 휴일효과 보정
  movie_data <- reactive({
    
    prepare_data(
      df = raw_movie_data(),
      holiday_factor = input$holiday
    )
  })
  
  
  # 모형 적합
  fit_result <- reactive({
    
    tryCatch(
      fit_diffusion(
        df = movie_data(),
        model = input$model,
        method = input$method
      ),
      error = function(e) {
        list(
          error = conditionMessage(e)
        )
      }
    )
  })
  
  
  # 미래 예측
  forecast_data <- reactive({
    
    result <- fit_result()
    
    validate(
      need(
        is.null(result$error),
        result$error
      )
    )
    
    predict_diffusion(
      df = movie_data(),
      params = result,
      model = input$model,
      horizon = input$horizon,
      holiday_factor = input$holiday
    )
  })
  
  
  # m
  output$m_value <- renderText({
    
    result <- fit_result()
    
    validate(
      need(
        is.null(result$error),
        result$error
      )
    )
    
    if (
      is.null(result$m) ||
      is.na(result$m)
    ) {
      
      "N/A"
      
    } else {
      
      format(
        round(result$m),
        big.mark = ",",
        scientific = FALSE
      )
    }
  })
  
  
  # p
  output$p_value <- renderText({
    
    result <- fit_result()
    
    validate(
      need(
        is.null(result$error),
        result$error
      )
    )
    
    if (
      is.null(result$p) ||
      is.na(result$p)
    ) {
      
      "N/A"
      
    } else {
      
      format(
        round(result$p, 4),
        nsmall = 4
      )
    }
  })
  
  
  # q
  output$q_value <- renderText({
    
    result <- fit_result()
    
    validate(
      need(
        is.null(result$error),
        result$error
      )
    )
    
    if (
      is.null(result$q) ||
      is.na(result$q)
    ) {
      
      "N/A"
      
    } else {
      
      format(
        round(result$q, 4),
        nsmall = 4
      )
    }
  })
  
  
  # 영화 정보
  output$title_info <- renderText({
    input$movie
  })
  
  output$director_info <- renderText({
    
    if (input$movie == "왕과 사는 남자") {
      "장항준"
    } else {
      "Christopher Nolan"
    }
  })
  
  output$cast_info <- renderText({
    
    if (input$movie == "왕과 사는 남자") {
      "유해진, 박지훈, 유지태, 전미도"
    } else {
      "Matt Damon, Tom Holland, Anne Hathaway, Robert Pattinson"
    }
  })
  
  output$genre_info <- renderText({
    
    if (input$movie == "왕과 사는 남자") {
      "Drama, Historical"
    } else {
      "Adventure, Action, Fantasy"
    }
  })
  
  output$period_info <- renderText({
    
    df <- raw_movie_data()
    
    paste(
      format(
        min(df$날짜),
        "%Y-%m-%d"
      ),
      "~",
      format(
        max(df$날짜),
        "%Y-%m-%d"
      )
    )
  })
  
  output$audience_info <- renderText({
    
    df <- raw_movie_data()
    
    format(
      round(
        tail(df$누적관객수, 1)
      ),
      big.mark = ",",
      scientific = FALSE
    )
  })
  
  
  # 시계열 그래프
  output$forecast_plot <- renderPlot({
    
    actual <- raw_movie_data()
    pred <- forecast_data()
    
    ggplot() +
      geom_line(
        data = actual,
        aes(
          x = 날짜,
          y = 관객수,
          color = "Actual"
        ),
        linewidth = 0.8
      ) +
      geom_line(
        data = pred,
        aes(
          x = Date,
          y = Predicted_Audience,
          color = "Prediction"
        ),
        linewidth = 0.9,
        linetype = "dashed"
      ) +
      scale_color_manual(
        values = c(
          "Actual" = "#212529",
          "Prediction" = "#2780E3"
        )
      ) +
      labs(
        x = NULL,
        y = "Daily Audience",
        color = NULL
      ) +
      theme_minimal(
        base_size = 12
      ) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  })
  
  
  # 예측값 표
  output$prediction_table <- renderTable({
    
    pred <- forecast_data()
    
    pred %>%
      transmute(
        Date = format(
          Date,
          "%Y-%m-%d"
        ),
        `Predicted Audience` = format(
          round(Predicted_Audience),
          big.mark = ",",
          scientific = FALSE
        ),
        `Predicted Cumulative` = format(
          round(Predicted_Cumulative),
          big.mark = ",",
          scientific = FALSE
        )
      )
    
  }, striped = TRUE, bordered = FALSE)
}


shinyApp(ui = ui, server = server)


