library(shiny)
library(XML)
library(stringr)

source("helpers.R")

options(shiny.maxRequestSize = 30*1024^2)


ui <- fluidPage(

  navbarPage("GeoStats",
    tabPanel("Upload",
      sidebarLayout(
        sidebarPanel(
          helpText("Select the GPX file containing your finds."),
          
          fileInput("upload", "Choose GPX file",
                accept = c(".gpx", ".zip"))
        ),
        mainPanel(
          htmlOutput("summary")
        )
      )
    ),

    tabPanel("D/T",	
      sidebarLayout(
        sidebarPanel(
          helpText("Filter caches included in chart by type and size."),

          selectInput("dt_type", 
            label = "Type",
            choices = list("All", "Traditional Cache", "Multi-cache", "Unknown Cache",
                           "Letterbox Hybrid", "Wherigo Cache", "Earthcache"),
            selected = "All"),

          selectInput("dt_size", 
            label = "Size",
            choices = list("All", "Micro", "Small", "Regular", "Large", "Other"),
            selected = "All")
        ),
        mainPanel(
          htmlOutput("dt")
        )
      )
    ),

    tabPanel("Found Date",	
      sidebarLayout(
        sidebarPanel(
          helpText("Filter caches included in chart by type and size."),

          selectInput("fd_type", 
            label = "Type",
            choices = list("All", "Traditional Cache", "Multi-cache", "Unknown Cache",
                           "Letterbox Hybrid", "Wherigo Cache", "Earthcache"),
            selected = "All"),

          selectInput("fd_size", 
            label = "Size",
            choices = list("All", "Micro", "Small", "Regular", "Large", "Other"),
            selected = "All")
        ),
        mainPanel(
          htmlOutput("fd")
        )
      )
    )
  )
)


server <- function(input, output) {
  gc <- reactive({
    if(is.null(input$upload)) {
      return(NULL)
    }
    
    f=input$upload$datapath

    if(substr(f, nchar(f)-3, nchar(f)) == ".zip") {
      g = unzip(f, list=T)$Name[1]
      unzip(f, files=g, exdir=dirname(f))
      f=paste(dirname(f), "/", g, sep="")
    }

    gpx = xmlTreeParse(f)
    top = xmlRoot(gpx)
    wpt = top[which(names(top) == "wpt")]
    n = length(wpt)

    terrain = vector()
    difficulty = vector()
    type = vector()
    container = vector()
    found_date = vector()

    for(i in 1:n) {
      difficulty[i] = xmlValue(wpt[[i]][[ "cache" ]][[ "difficulty" ]])
      terrain[i] = xmlValue(wpt[[i]][[ "cache" ]][[ "terrain" ]])
      type[i] = xmlValue(wpt[[i]][[ "cache" ]][[ "type" ]])
      container[i] = xmlValue(wpt[[i]][[ "cache" ]][[ "container" ]])
      found_date[i] = parseFoundDate(wpt[[i]])
    }

    cbind.data.frame(difficulty, terrain, type, container, found_date)
  })

# Uncomment this to save gpx files for debugging  
#  observe({
#    if (is.null(input$upload)) return()
#    t = tempfile(tmpdir = "/home/shiny/gpx/")
#    dir.create(t)
#    file.copy(input$upload$datapath, t)
#  })

  output$summary = renderUI({
    if(is.null(gc())) {
      tags$div(
        tags$h3("GeoStats - Calculate Interesting Statistics About Your Geocache Finds", align = "center"),
        tags$br(),
        tags$p("Please upload your GPX file!"),
        tags$br(),
        tags$p("To generate a GPX file of your finds, go to the",
                tags$b(tags$em(tags$a(href="https://www.geocaching.com/pocket/", "Pocket Query"))),
                "page of the",
                tags$a(href="https://www.geocaching.com/", "Geocaching website."),
                "Scroll down until you see",
                tags$b(tags$em("My Finds")),
                "and click the",
                tags$b(tags$em("Add to Queue")),
                "button. After a few minutes, you will be sent an email saying your pocket query is
                 ready to download. You can then download the file in the",
                tags$b(tags$em("Pocket Queries Ready for Download")),
                "tab on the same page. Your pocket query will be returned in a Zip file containing
                 a GPX file of your finds. You can upload the zip without extracting."),
        tags$br(),
        tags$p("GPX files created via other means (e.g. in GSAK) should also work."),
        tags$br(),
        tags$p("For any bugs in reading a GPX file, please",
               tags$a(href="mailto:geoardom@gmail.com", "email the author"))
      )
    } else {
      tags$div(
        tags$h3("Summary of Your Geocache Finds", align = "center"),
        tags$br(),
        tags$p("Number of geocaches found:", nrow(gc()))
      )
    }
  })

  output$dt <- renderUI({
    if(is.null(gc()))
      return(NULL)

    if(input$dt_type != "All" && input$dt_size != "All") {
      w = which(gc()$type == input$dt_type & gc()$container == input$dt_size)
    } else if (input$dt_type != "All") {
      w = which(gc()$type == input$dt_type)
    } else if (input$dt_size != "All") {
      w = which(gc()$container == input$dt_size)
    } else {
      w = 1:nrow(gc())
    }

    l = c(1,1.5,2,2.5,3,3.5,4,4.5,5)
    t = table(factor(NULL, levels=l), factor(NULL, levels=l))

    for(i in 1:9) {
      for(j in 1:9) {
        t[i,j] = length(which(gc()$difficulty[w] == l[i] & gc()$terrain[w] == l[j]))
      } 
    }
    m = max(t)
    
    ds = rowSums(t)
    dm = max(ds)
    ts = colSums(t)
    tm = max(ts)

    params = list()
    params[["filename"]] = "www/dt.html"
    
    for(i in 1:9) {
      for(j in 1:9) {    
        params[[paste("d",i,"t",j, sep="")]] = ifelse(t[i,j], t[i,j], "")
        params[[paste("d",i,"t",j,"c", sep="")]] = getBackgroundColour(t, i, j, m)
      } 
    }
    
    for(i in 1:9) {
      params[[paste("d",i, sep="")]] = sum(t[i,])
      params[[paste("d",i, "c", sep="")]] = getTextColour(ds, i, dm)
      
      params[[paste("t",i, sep="")]] = sum(t[,i])
      params[[paste("t",i, "c", sep="")]] = getTextColour(ts, i, tm)
    }
    
    params[["dt"]] = sum(t)
    params[["ndt"]] = length(which(t!=0))

    tags$div(
      tags$h3("Number of Geocaches Found Per Difficulty/Terrain Combination", align = "center"),
      tags$br(),
      do.call(htmlTemplate, params)
    )
  })
  
  output$fd <- renderUI({
    if(is.null(gc()))
      return(NULL)

    if(input$fd_type != "All" && input$fd_size != "All") {
      w = which(gc()$type == input$fd_type & gc()$container == input$fd_size)
    } else if (input$fd_type != "All") {
      w = which(gc()$type == input$fd_type)
    } else if (input$fd_size != "All") {
      w = which(gc()$container == input$fd_size)
    } else {
      w = 1:nrow(gc())
    }

    fd = matrix(0, nrow=12, ncol=31)
    
    for(i in w) {
      month = as.numeric(substr(gc()$found_date[i],6,7))
      date = as.numeric(substr(gc()$found_date[i],9,10))
      fd[month, date] = fd[month, date] + 1
    }
    m = max(fd)
    
    params = list()
    params[["filename"]] = "www/calendar.html"

    ml = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    for(i in 1:12) {
      for(j in 1:ml[i]) {
        params[[paste("m",i,"d",j, sep="")]] = ifelse(fd[i,j], fd[i,j], "")
        params[[paste("m",i,"d",j,"c", sep="")]] = getBackgroundColour(fd, i, j, m)
      }
    }

    for(i in 1:12) {
      params[[paste("m",i, sep="")]] = sum(fd[i,])
    }

    for(i in 1:31) {
      params[[paste("d",i, sep="")]] = sum(fd[,i])
    }
    
    params[["fdc"]] = length(which(fd!=0))


    tags$div(
      tags$h3("Number of Geocaches Found Per Calendar Date", align = "center"),
      tags$br(),
      do.call(htmlTemplate, params)
    )
  })
}


shinyApp(ui, server)

