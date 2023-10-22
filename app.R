library(shiny)
library(RSQLite)
library(mongolite)
library(tidyverse)
library(lubridate)
library(emo)
library(shinyjs)
library(DT)
library(bslib)
library(shinycustomloader)
library(shinyWidgets)
library(htmltools)
library(plotly)
library(shinyTime)
library(shinycssloaders)
library(highcharter)
library(cowsay)

# MongoDB Atlas connection string
uri_apps = "mongodb+srv://jasminex21:Lm2bap6wggyWvqaJ@mycluster.mongodb.net/entries?retryWrites=true&w=majority"
uri_statuses = "mongodb+srv://jasminex21:Lm2bap6wggyWvqaJ@mycluster.mongodb.net/statuses?retryWrites=true&w=majority"
uri_merged = "mongodb+srv://jasminex21:Lm2bap6wggyWvqaJ@mycluster.mongodb.net/merged?retryWrites=true&w=majority"
uri_mood = "mongodb+srv://jasminex21:Lm2bap6wggyWvqaJ@mycluster.mongodb.net/mood?retryWrites=true&w=majority"

# Connect to MongoDB Atlas
connection = mongolite::mongo(uri_apps)
statuses = mongolite::mongo(uri_statuses)
merged = mongolite::mongo(uri_merged)
mood = mongolite::mongo(uri_mood)

quotes = read_csv("quotes_revised.csv")

cycles = c("Summer 2024", "Summer 2025", "Other")

allTags = c(paste0(emo::ji("heart"), "Favorite"), 
               paste0(emo::ji("purple heart"), "Hopeful"), 
               paste0(emo::ji("cross mark"), "Long shot"), 
               paste0(emo::ji("woman_technologist"), "Remote"), 
               paste0(emo::ji("superhero"), "Hybrid"), 
               paste0(emo::ji("earth_asia"), "Outside the US"))

moods = c(paste0(emo::ji("laughing"), "Really happy!"), 
          paste0(emo::ji("grinning"), "Content"), 
          paste0(emo::ji("slightly_smiling_face"), "Average"), 
          paste0(emo::ji("neutral_face"), "Really could be better"), 
          paste0(emo::ji("unamused"), "Annoyed"), 
          paste0(emo::ji("cry"), "Having a bad time")
          )

requiredFields = c("positionTitle", 
                   "companyName")
allFields = c(requiredFields,
              "roleDescription",
              "appliedDate",
              "tags", 
              "addlNotes", 
              "cycle")

quoteCard = card(
  full_screen = T, 
  card_header(class = "color:#294053", 
              strong("Random Motivational Quote")), 
  # card body
  card_body(
    uiOutput("motivateQuote"), 
    actionButton("motivate", 
                 label = "Generate!")
  )
)

# card item that shows acceptance/response rates by cycle
cycleCard = card(
  full_screen = TRUE,
  height = "500px",
  card_header(class = "bg-dark", 
              strong("Response and Acceptance Rates by Application Cycle")), 
  card_body(
    selectInput("selectCycle", 
                label = "Application cycle", 
                choices = cycles, 
                selected = cycles[1]), 
    br(),
    layout_column_wrap(
      width = 1/3,
      withLoader(highchartOutput("cycleResponses", 
                              height = "200px"), 
                 type = "html", 
                 loader = "loader1"),
      withLoader(highchartOutput("cycleAcceptance", 
                              height = "200px"), 
                 type = "html", 
                 loader = "loader1"), 
      withLoader(highchartOutput("appsOverTime", 
                                 height = "200px"), 
                 type = "html", 
                 loader = "loader1")
    ), 
    br(), 
    layout_column_wrap(
      width = 1/2, 
      uiOutput("cycleResponsesComm"), 
      uiOutput("cyclesAccComm")
    )
  )
)

timeCard = card(
  full_screen = TRUE,
  height = "500px",
  card_header(class = "bg-dark", 
              strong("Application Rates")), 
  card_body(
    selectInput("selectCycle2", 
                label = "Application cycle", 
                choices = cycles, 
                selected = cycles[1]), 
    br(), 
    highchartOutput("appsOverTime")
    
  )
)


ui = page_navbar(title = strong("Nerdvana: Your Tech Oasis"), 
                 id = "nav", 
                 shinyjs::useShinyjs(),
                 includeCSS("www/styles.css"),
                 nav_panel(title = strong("Home"), 
                           h1(strong("Welcome to Nerdvana!")),
                           h4(strong("I am so glad you're here :)")),
                           fluidRow(
                             column(6, 
                                    markdown("
                                    This is a dynamic, interactive web app made 
                                    for *you* and only you. It's hard being a 
                                    student in computer science, so Nerdvana - 
                                    a home base for students studying computer 
                                    science - is designed to make your life just 
                                    a bit simpler, so you can prioritize what 
                                    matters most to you.
                                    
                                    Internship-hunting is one of the most 
                                    stressful parts of a college student's 
                                    journey to a career in tech - so Nerdvana 
                                    provides a simple interface to track 
                                    internships you've applied to and their 
                                    statuses. The statistics panel shows you 
                                    just how well you're doing in the process.
                                    
                                    Mental health has become increasingly 
                                    important amidst the stress of higher-level 
                                    education. It is sometimes hard to take note
                                    of your moods and emotions, so I encourage 
                                    you to slow down, and reflect on your mood 
                                    every once in a while. For this purpose, 
                                    Nerdvana provides a daily mood tracker that 
                                    records your mood, and displays notable trends. 
                                             
                                             "),
                                    br(),
                                    fluidRow(
                                      column(2), 
                                      column(6, 
                                             quoteCard)
                                      # column(1)
                                    )), 
                             column(6, 
                                    div(img(src = "nerdvana_logo.png", 
                                            height = 550, 
                                            width = 550), 
                                        style = "text-align: center;"))
                           ),
                           # fluidRow(layout_column_wrap(
                           #   width = 1/3,
                           #   quoteCard
                           # )
                           # )
                           ), 
                 nav_panel(title = strong("Internship Tracker"),
                   navset_tab(
                     nav_panel(title = strong("Create Entries"), 
                               layout_sidebar(
                                 sidebar = sidebar(
                                   width = 450,
                                   id = "entriesForm", 
                                   dateInput("appliedDate",
                                             label = "Date applied",
                                             value = today(),
                                             max = today(),
                                             format = "mm-dd-yyyy"),
                                   selectInput("cycle", 
                                               label = "Application Cycle", 
                                               choices = cycles, 
                                               selected = cycles[1]), 
                                   textInput("positionTitle",
                                             label = "Position",
                                             placeholder = "Data Science Intern"),
                                   textInput("companyName",
                                             label = "Company",
                                             placeholder = "MLH"),
                                   textAreaInput("roleDescription",
                                                 label = "Role description",
                                                 placeholder = "Your responsibilities include...",
                                                 resize = "vertical"),
                                   checkboxGroupInput("tags",
                                                      label = "Tags",
                                                      choiceNames = allTags,
                                                      choiceValues = allTags),
                                   textAreaInput("addlNotes",
                                                 label = "Additional notes",
                                                 placeholder = "Any other important things to know about this role",
                                                 resize = "vertical"),
                                   actionButton("submitEntry",
                                                label = "Submit")
                                 ), 
                                 h3(strong("Your Applications")),
                                 fluidRow(DT::dataTableOutput("entriesTable"))
                               )), 
                     nav_panel(title = strong("Track Responses"), 
                               layout_sidebar(
                                 sidebar = sidebar(
                                   width = 450, 
                                   id = "statusForm", 
                                   textInput("ID", 
                                             label = "ID", 
                                             placeholder = "21"),
                                   radioButtons("status",
                                                label = "Status", 
                                                choices = c("Accepted" = "Accepted", 
                                                            "Rejected" = "Rejected", 
                                                            "Interview" = "Interview"), 
                                                selected = "Rejected"), 
                                   textAreaInput("notes", 
                                                 label = "Additional notes", 
                                                 resize = "vertical", 
                                                 placeholder = "Final comments about this position"), 
                                   actionButton("submitUpdate", 
                                                label = "Submit")
                                 ), 
                                 h3(strong("Your Application Statuses")), 
                                 fluidRow(dataTableOutput("statusesTable"))
                               )), 
                     nav_panel(title = strong("Statistics"), 
                               br(),
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectCycle", 
                                                    label = "Application cycle", 
                                                    choices = cycles, 
                                                    selected = cycles[1]))
                               ),
                               fluidRow(
                                 column(width = 6, 
                                        withLoader(highchartOutput("cycleResponses", 
                                                                   height = "280px"), 
                                                   type = "html", 
                                                   loader = "loader1")), 
                                 column(6, 
                                        withLoader(highchartOutput("cycleAcceptance", 
                                                                   height = "280px"), 
                                                   type = "html", 
                                                   loader = "loader1"))), 
                               fluidRow(
                                 withLoader(highchartOutput("appsOverTime", 
                                                            height = "280px"), 
                                            type = "html", 
                                            loader = "loader1")
                                 
                               )
                               
                               )
                   )), 
                 nav_panel(title = strong("Mental Health"), 
                           h1(strong("Mood Tracker")), 
                           layout_sidebar(
                             sidebar = sidebar(
                               width = 350,
                               timeInput("timeOfDay", 
                                         label = "Time", 
                                         value = strptime("12:34:56", "%T"), 
                                         minute.steps = 5),
                               actionButton("toNow", 
                                            label = "Current Time"),
                               radioButtons("chooseMood", 
                                            label = "What's your current mood?", 
                                            choices = moods, 
                                            selected = NULL), 
                               actionButton("submitMood", 
                                            label = "Submit")
                           ), 
                           h3(strong("Your Mood Fluctuations Throughout the Day")),
                           fluidRow(plotlyOutput("moodPlot", height = "500px"), 
                                    br(), br(), 
                                    strong(textOutput("moodStatement")))
                           
                           ))
                 )

server = function(input, output, session) {
  
  # random quote generator: 
  randNum = function(quote_set) {
    
    floor(runif(1, min = 1, max = nrow(quote_set)))
  }
  
  motivateMe = eventReactive(input$motivate, {
    
    num = randNum(quotes)
    # cowsay::say(paste0(strong(quotes$Quote[num]), br(), "-- ", quotes$Author[num]))
    HTML(paste0(strong(quotes$Quote[num]), br(), "-- ", quotes$Author[num]))
    
  })
  
  output$motivateQuote = renderUI({
    toSay = motivateMe()
    toSay
  })
  
  observeEvent(input$submitMood, {
    
    newMood = data.frame(
      Datetime = input$timeOfDay,
      Mood = input$chooseMood
    )
    
    mood$insert(newMood)
    
  })
  
  observeEvent(input$submitMood, {
    showModal(modalDialog(
      title = "Mood submitted!",
      "Your current mood has been submitted :)"
    ))
  })
  
  observeEvent(input$toNow, {
    updateTimeInput(session, "timeOfDay", value = Sys.time())
  })
  
  moodData = reactive({
    
    input$submitMood
    mood$find() %>%
      filter(day(Datetime) == day(today()))
    
  })
  
  output$moodStatement = renderText({ 
    
    data = moodData()
    
    # Map mood categories to numerical values
    data$moodVal = 0  # Initialize a new column for numerical values
    # data$Mood = as.character(data$Mood) 
    data$moodVal[grepl("Really happy", data$Mood)] = 6
    data$moodVal[grepl("Content", data$Mood)] = 5
    data$moodVal[grepl("Average", data$Mood)] = 4
    data$moodVal[grepl("Really could be better", data$Mood)] = 3
    data$moodVal[grepl("Annoyed", data$Mood)] = 2
    data$moodVal[grepl("Having a bad time", data$Mood)] = 1
    
    midpoint = floor(nrow(data) / 2)
    firstHalf = data[1:midpoint, ]
    secondHalf = data[midpoint + 1:nrow(data), ]
    
    meanFirst = mean(firstHalf$moodVal, na.rm = T)
    meanSecond = mean(secondHalf$moodVal, na.rm = T)
    
    if (meanSecond > meanFirst) {
      statement = "It looks like your mood is increasing - that's great! 
      Keep it up!"
    }
    else if (meanFirst > meanSecond) {
      statement = "Your mood appears to be decreasing - take it easy, get some 
      rest, and spend time with your loved ones. Hope you feel better soon!"
    }
    else {
      statement = "Your mood appears to be stable!"
    }
    
  })
  
  output$moodPlot = renderPlotly({
    
    plot_ly(data = moodData(), x = ~Datetime, y = ~Mood, 
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'pink')) %>%
      layout(
        # title = list(text = "Mood Fluctuations Throughout the Day", font = list(size = 24)),
        xaxis = list(title = "Time of Day", tickfont = list(size = 16)),
        yaxis = list(title = "", categoryarray = rev(moods), tickfont = list(size = 16)),
        showlegend = FALSE
      )
    
    
  })
  
  # auto-incrementing ID for the Entries table s.t. each entry has a unique ID
  idTracker = reactiveValues(idValue = 0)
  
  # enabling the Submit button of the Create Entries tab only when required
  # fields are filled
  observe({
    reqFieldsFilled = requiredFields %>%
      sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
      all
    
    shinyjs::toggleState("submitEntry", reqFieldsFilled)
  })
  
  observeEvent(input$submitEntry, {
    
    if (nrow(connection$find()) > 0) {
      idTracker$idValue = connection$find(query = '{}', fields = '{}', 
                                          sort = '{"ID": -1}', limit = 1)$ID + 1
    }
  })
  
  observeEvent(input$submitEntry, {
    
    # disable submit button right after clicked
    shinyjs::disable("submitEntry")
  
    # dataframe (row) to insert into database
    entry = data.frame(
      ID = idTracker$idValue, 
      Date = as.Date(input$appliedDate), 
      Position = input$positionTitle, 
      Company = input$companyName, 
      Description = input$roleDescription, 
      Tags = HTML(paste0(input$tags, collapse="<br/>")),
      `Additional Notes` = input$addlNotes, 
      Cycle = input$cycle
    )
    
    connection$insert(entry)
    
    shinyjs::reset("entriesForm")
    on.exit({
      shinyjs::enable("submitEntry")
    })
  })
  
  # reloading the table of Entries/Applications every time a new entry is made
  entriesData = reactive({
    
    input$submitEntry
    connection$find()
    
  })
  
  output$entriesTable = renderDataTable({
    
    DT::datatable(entriesData(), 
                  rownames = F, 
                  selection = "none", 
                  escape = F)
    
  })
  
  # Statuses
  
  observe({
    validUpdate = !is.null(input$ID) && input$ID != ""
    shinyjs::toggleState("submitUpdate", validUpdate)
  })
  
  observeEvent(input$submitUpdate, {
    
    shinyjs::disable("submitUpdate")
    
    update = data.frame(
      ID = as.numeric(input$ID), 
      Status = input$status, 
      `Additional Notes` = input$notes
    )
    
    statuses$insert(update)
    
    shinyjs::reset("statusForm")
    on.exit({
      shinyjs::enable("submitUpdate")
    })
    
    # merge the tables
    
    insertStatus = connection$find() %>%
      left_join(statuses$find(), by = "ID") %>%
      filter(ID == input$ID) %>%
      select(c(1, 3, 4, 6, 9, 10, 8))
      # select(ID, Position, Company, Tags, Status, Cycle)
    
    merged$insert(insertStatus)
    
  })
  
  statusesData = reactive({
    
    input$submitUpdate
    merged$find()
    
  })
  
  output$statusesTable = renderDataTable({
    
    DT::datatable(statusesData(), 
                  rownames = F, 
                  selection = "none", 
                  escape = F, 
                  colnames = c(
                    "Additional Notes" = "Additional_Notes_y"
                  ))
    
  })
  
  # stats
  
  # all applications for current cycle
  cycleEntries = reactive({
    data = entriesData() %>%
      filter(Cycle == input$selectCycle)
  })
  
  # all results from current cycle (inc. interview)
  cycleResults = reactive({
    
    tibble(merged$find()) %>%
      filter(Cycle == input$selectCycle)
    
  })
  
  output$cycleResponses = renderHighchart({
    palette = c("#B7A5E9", "#E7A08E")
    cyclePData = data.frame(
      name = c("Received Response", "No Response"), 
      y = c(nrow(cycleResults()), 
                nrow(cycleEntries()) - nrow(cycleResults())), 
      color = palette
    )
    highchart() |> 
      hc_chart(type = "pie") |> 
      hc_add_series(data = cyclePData, name = "Count") |>
      hc_title(text = "Response Rate") |>
      hc_plotOptions(pie = list(
        dataLabels = list(
          enabled = TRUE
        )
      ))
  })
  
  output$cycleAcceptance = renderHighchart({
    
    # color_mapping = c("Accepted" = "#98BD8A", 
    #                   "Rejected" = "#DD7D7D")
    palette = c("#98BD8A", "#DD7D7D")
    solidStatus = nrow(cycleResults() %>%
                         filter(Status != "Interview"))
    accepted = nrow(cycleResults() %>%
                      filter(Status == "Accepted"))
    cycleRData = data.frame(
      name = c("Accepted", "Rejected"), 
      y = c(accepted, solidStatus - accepted),
      color = palette
    )
    
    highchart() |> 
      hc_chart(type = "pie") |> 
      hc_add_series(data = cycleRData, name = "Count") |>
      hc_title(text = "Acceptance Rate") |>
      hc_plotOptions(pie = list(
        dataLabels = list(
          enabled = TRUE
        )
      ))
  })
  
  # number of apps per date
  countsByDate = reactive({
    
    data = cycleEntries()
    data$Date = as.Date(data$Date)
    
    data = data %>%
      group_by(Date) %>%
      summarise(Count = n())
    
  })
  
  output$appsOverTime = renderHighchart({
    
    data = countsByDate()
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_add_series(data, "line", hcaes(x = Date, y = Count), color = "red", 
                    name = "Number of Applications") %>%
      hc_title(text = "Number of Applications during Cycle") %>%
      hc_xAxis(labels = list(format = "{value:%Y-%m-%d}"))
    
  })
  
}

shinyApp(ui = ui, server = server)




