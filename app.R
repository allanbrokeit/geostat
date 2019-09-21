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
          textOutput("summary")
        )
      )
    ),

    tabPanel("D/T",	
      sidebarLayout(
        sidebarPanel(
          helpText("Filter caches included in plot by type and size."),

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
          helpText("Filter caches included in plot by type and size."),

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

  output$summary = renderText({
    if(is.null(gc())) {
      "Please upload your GPX file!"
    } else {
      paste("# caches: ", nrow(gc()))
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

    do.call(htmlTemplate, params)
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

    do.call(htmlTemplate, params)
  })
}


shinyApp(ui, server)

