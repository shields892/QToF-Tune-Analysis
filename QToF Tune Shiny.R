library(shinydashboard)
library(shiny)
source("QToF Tune Analysis.R")

# User Interface
ui <- dashboardPage(
  
  dashboardHeader(title = "QToF Health Check"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Negative Mode",tabName = "N",icon = icon("-")),
      menuItem("Positive Mode",tabName = "P",icon = icon("+"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #Negative Mode tab content
      tabItem(tabName = "N",
              fluidPage(
                fluidRow(
                  column(2,checkboxGroupInput(inputId = "Theoretical_N",
                                              label = "Theoretical m/z",
                                              choices = c(112.985587,301.998139,601.978977,1033.988109,1333.968947,1633.949786),
                                              selected = c(112.985587))),
                  column(6,checkboxGroupInput(inputId = "Parameter_N",
                                              label = "Tune Parameter",
                                              choices = c(
                                                "Actual",
                                                "Time",
                                                "Abundance",
                                                "Calibration Abundance" = "Calibration_Abundance",
                                                "Resolution",
                                                "Primary Residuals" = "Primary_Residuals",
                                                "Corrected Residuals" = "Corrected_Residuals"
                                              ),
                                              selected = c("Abundance",
                                                           "Corrected_Residuals"))),
                  column(10,checkboxGroupInput(inputId = "type_N",
                                               label = "Tune Type",
                                               choices = c(
                                                 "Check Tune" = "check_tune",
                                                 "Tranmission Tune" = "transmission_tune",
                                                 "System Tune" = "system_tune"
                                               ),
                                               selected = c("check_tune",
                                                            "transmission_tune")))),
                box(title = "QToF Tune Result",plotOutput("TuneParameter_N",width = "100%")),
                tableOutput("table_N"),tableOutput("table_recent_N")
              )
      ),
      tabItem(tabName = "P",
              fluidPage(
                fluidRow(
                  column(2,checkboxGroupInput(inputId = "Theoretical_P",
                                              label = "Theoretical m/z",
                                              choices = c(118.086255,322.048121,622.02896,922.009798,1221.990636,1521.971475),
                                              selected = c(118.086255))),
                  column(6,checkboxGroupInput(inputId = "Parameter_P",
                                              label = "Tune Parameter",
                                              choices = c(
                                                "Actual",
                                                "Time",
                                                "Abundance",
                                                "Calibration Abundance" = "Calibration_Abundance",
                                                "Resolution",
                                                "Primary Residuals" = "Primary_Residuals",
                                                "Corrected Residuals" = "Corrected_Residuals"
                                              ),
                                              selected = c("Abundance",
                                                           "Corrected_Residuals"))),
                  column(10,checkboxGroupInput(inputId = "type_P",
                                               label = "Tune Type",
                                               choices = c(
                                                 "Check Tune" = "check_tune",
                                                 "Tranmission Tune" = "transmission_tune",
                                                 "System Tune" = "system_tune"
                                               ),
                                               selected = c("check_tune",
                                                            "transmission_tune")))),
                box(title = "QToF Tune Result",plotOutput("TuneParameter_P")),
                tableOutput("table_P"),tableOutput("table_recent_P")
              )
      )
    )
  )
)




# Server logic
server <- function(input, output) {
  #Negative Mode
  data_N <- reactive( {
    tune_data %>%
      filter(Mode == "N") %>%
      filter(Theoretical %in% input$Theoretical_N) %>%
      gather("variable","value",Actual:Corrected_Residuals) %>%
      filter(variable %in% input$Parameter_N) %>%
      filter(tune_type %in% input$type_N)
  } )
  
  data_sum_N <- reactive( {
    data_N() %>%
      group_by(Theoretical,Mode,variable) %>%
      do(one.samp.bal(.$value,n = 500)) %>%
      select(-p.value) %>%
      full_join(data_N() %>%
                  filter(Date == max(as.Date(Date)) &
                           tune_type == "check_tune") %>%
                  select(-Date,-tune_type) %>%
                  rename("Most Recent Check Tune" = value))
  })
  
  
  output$TuneParameter_N <-
    renderPlot({
      ggplot() +
        geom_rect(data = data_sum_N(),aes(xmin = as.Date(-Inf),
                                          xmax = as.Date(Inf),
                                          ymin = ci2.5,
                                          ymax = ci97.5),
                  fill = "gray75") +
        geom_hline(data = data_sum_N(),aes(yintercept = mean)) +
        geom_point(data = data_N(),aes(x = as.Date(Date),value,color = tune_type)) +
        facet_wrap(Theoretical ~ variable,scales = "free_y") +
        theme(text = element_text(size = 12),
              axis.text.x = element_text(angle = -30)) +
        xlab("Date") +
        ylab("Value")
    })
  
  output$table_N <- renderTable(data_sum_N())
  
  #Positive Mode
  data_P <- reactive( {
    tune_data %>%
      filter(Mode == "P") %>%
      filter(Theoretical %in% input$Theoretical_P) %>%
      gather("variable","value",Actual:Corrected_Residuals) %>%
      filter(variable %in% input$Parameter_P) %>%
      filter(tune_type %in% input$type_P)
  } )
  
  data_sum_P <- reactive( {
    data_P() %>%
      group_by(Theoretical,Mode,variable) %>%
      do(one.samp.bal(.$value,n = 500)) %>%
      select(-p.value) %>%
      full_join(data_P() %>%
                  filter(Date == max(as.Date(Date)) &
                           tune_type == "check_tune") %>%
                  select(-Date,-tune_type) %>%
                  rename("Most Recent Check Tune" = value))
  })
  
  output$TuneParameter_P <-
    renderPlot({
      ggplot() +
        geom_rect(data = data_sum_P(),aes(xmin = as.Date(-Inf),
                                          xmax = as.Date(Inf),
                                          ymin = ci2.5,
                                          ymax = ci97.5),
                  fill = "gray75") +
        geom_hline(data = data_sum_P(),aes(yintercept = mean)) +
        geom_point(data = data_P(),aes(x = as.Date(Date),value,color = tune_type)) +
        facet_wrap(Theoretical ~ variable,scales = "free_y") +
        theme(text = element_text(size = 12),
              axis.text.x = element_text(angle = -30)) +
        xlab("Date") +
        ylab("Value")
    })
  
  output$table_P <- renderTable(data_sum_P())
  
}

# Run app
shinyApp(ui, server)