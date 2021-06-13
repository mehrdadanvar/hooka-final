library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(summarytools)
load(file = "./hooka.Rdata",envir = .GlobalEnv)
this <- db.out
# Define UI for application
ui <-
    dashboardPage(
        header = dashboardHeader(title = "Hooka Project Primary Analysis"),
        sidebar = dashboardSidebar((
            sidebarMenu(
                menuItem("Main", tabName = "main", icon = icon("dashboard")),
                menuItem("Summary", tabName = "summary", icon = icon("table")),
                menuItem("Plots", tabName = "plots", icon = icon("tree"))
            )
        )),
        body = dashboardBody(tabItems(
            tabItem(tabName = "main",
                    fluidRow(
                        box(
                            width = 4,
                            title = "Select One Variable",
                            status = "info",
                            selectInput(
                                inputId = "m",
                                label = "Names",
                                choices = as.list(names(this))
                            )
                        ),
                    box(width = 8,
                        title = "Resulting Data First Six Row",
                        status = "warning",
                        tableOutput(outputId = "d.summary")))
                    ),
            tabItem(tabName = "summary",
                    fluidRow(
                        box(
                            width = 3,
                            title = "Select One Variable",
                            status = "info",
                            selectInput(
                                inputId = "s.i",
                                label = "Names",
                                choices = as.list(names(this))
                            )
                        ),
                        box(width = 9,
                            height = "300",
                            title = "Summary of this Variable",
                            status = "warning",
                            verbatimTextOutput(outputId = "s.s"))))
        ))
    )
# Define server logic
server <- function(input, output) {
    me <- reactive({
        input$m
    })
    output$d.summary <- renderTable({
        head(this[,me()])
    })
    you <- reactive({
        input$s.i
    })
    output$s.s <- renderPrint({
        summarytools::freq(this[,you()])
    })
    output$general <- renderTable({
        data.frame(
            Participants = dim(this)[1],
            Total.Variables = dim(this)[2],
            General = str_starts(names(db.out), pattern = "g") %>% sum(),
            Opinion = str_starts(names(db.out), pattern = "o") %>% sum(),
            Not.User = str_starts(names(this), pattern = "n") %>% sum(),
            Cigarrete.Reason = str_starts(names(this), pattern = "c.reason.use") %>% sum(),
            C.Quit.Reason = str_starts(names(this), pattern = "cq.reason.quit") %>% sum(),
            C.Quit.Negative = str_starts(names(this), pattern = "cq.quit.negative") %>% sum(),
            Hooka.Reason = str_starts(names(this), pattern = "h.main.reason") %>% sum(),
            Hooka.Quit.Reason = str_starts(names(this), pattern = "hq.reason.quit") %>% sum(),
            H.Quit.Negative = str_starts(names(this), pattern = "hq.quit.negative") %>% sum(),
            Surveyor.Variables = str_starts(names(this), pattern = "p.") %>% sum()
        ) %>% t()
    }, rownames = T)
    
    # Show the first "n" observations ----
    # The output$view depends on both the databaseInput reactive
    # expression and input$obs, so it will be re-executed whenever
    # input$dataset or input$obs is changed
    
    # output$summary <- renderPrint({
    #    names(this)
    # })
    # output$view <- renderTable({
    #     head(datasetInput())
    # })
}

# Run the application
shinyApp(ui = ui, server = server)

# fluidRow(box(
#     selectInput(
#         inputId = "col",
#         label = "6 rows",
#         choices = as.list(names(this)),
#         selected = names(this)[2],
#         width = "70%"
#     ),
#     box(tableOutput(outputId = "d.summary"), width = "30%")
# )),