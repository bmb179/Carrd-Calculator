library(shiny)

ui <- fluidPage(
  tags$link(rel = "stylesheet", href = 'https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap'),
  tags$style('body {background-color: #19181C; color: #FFFFFFDB; font-family: "Inter", sans-serif;} .dropdown-menu{background-color:#19181C;}'),
  div(h3('Business Value Estimate'), p('Note: Visit namebio.com to see domain name comparables')),
  numericInput('assets', 'Asset Value (Inventory, Domains, and Templates in $):', value = 0, min = 0),
  numericInput(inputId = 'arr', label = 'Annual Recurring Revenue ($):', value = 0, min = 0),
  numericInput(inputId = 'sales', label = 'Annual One-Time Sales ($):', value = 0, min = 0),
  numericInput(inputId = 'costs', label = 'Annual Costs ($):', value = 0, min = 0),
  dateInput(inputId = 'years', label = 'Date of First Sale:', value = Sys.Date(), min = Sys.Date() - (365*10), max = Sys.Date() + 1),
  actionButton('calculate', 'Calculate Value'),
  uiOutput('output.result')
)

server <- function(input, output) {
  values <- reactiveValues(valuation = NULL)
  observeEvent(input$calculate, { # Valuation Process
    revenue <- input$arr + input$sales
    margin <- (revenue - input$costs)/revenue
    if (is.nan(margin) | is.infinite(margin) | is.na(margin)) {margin <- 0}
    biz.age <- as.numeric((Sys.Date() - input$years)/365)
    if (biz.age >= 3 ) {
      time.multiple <- 1.50
    } else if (biz.age < 1) {
      time.multiple <- 1.00
    } else {
      time.multiple <- 1.25
    }
    values$valuation <- input$assets + (((input$arr * 3) + input$sales) * time.multiple * margin)
    if (values$valuation < 0) {values$valuation <- 0}
  })
  output$output.result <- renderUI({
    req(values$valuation)
    div(h3('Estimated Value'), h3(paste0('$', round(values$valuation, 0))))
  })
}

shinyApp(ui = ui, server = server)