
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(taxize)


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2, shiny.sanitize.errors = TRUE)


shinyServer(function(input, output, session) {

 ### Functions
  
  
  getDiff = function(vec){ 
    vect = c(vec,0) - c(0,vec)
    vect = vect[-which(vect == tail(vect, 1))]
    return(vect)
  }
  
  NetScrap2 = function(path, dic){ 
    # Collapse dictionary
    regex = paste(dic[,1], collapse = "|")
    # Scrape scientific names in text with taxize 
    names <- scrapenames(file = path, return_content = T)
    print("tax_name")
    # Identify family and class names
    #  families <- taxize::tax_name(names, get = "family", db = "ncbi",
    #                                 verbose = F,
    #                               ask = F) # Look for families
    # # 
    print("past_taxname")
    # Create object with OCR text
    txt <- names$meta$content
    # Find pattern on OCR text
    pattern <- gregexpr(regex, txt)
    
    # loop to retrieve occurrences. 
    loop <- c()
    for (i in 1:length(pattern[[1]])){
      loop[i] = stringi::stri_sub(txt, pattern[[1]][i], pattern[[1]][i] + attributes(pattern[[1]])$match.length[i]) }
    # Match offsets 
    x <- names$data$offsetstart
    y <- names$data$scientificname 
    z = data.frame(x,y, "z" = rep("red", length(x)))
    t = data.frame("x"= unlist(pattern),"y"= loop, "z" = rep("blue", length(loop)) )
    p = rbind(z,t)
    p = p[order(p$x),]
    x1 = getDiff(p$x)
    p = data.frame(p, "x1" = x1)
    t1 = dic[,2][match(substr(p$y[grep(regex, p$y)],1,3),
                       substr(dic[,1],1,3))]
    p$t1 = rep(NA,length(p$z))
    p$t1[which(p$z == "blue")] = t1
    p$t1[which(is.na(p$t1))] = "a"
    ob <- c(p, "text" = names$meta$content)
    print(str(ob))
    return(ob)
  } 
  
## Function Helpers for shiny databases:
  
#casts from the inputs to a one-row data.frame
  CastData <- function(data) {
    datar <- data.frame(animal = input$name,
                        plant = input$name2,
                        stringsAsFactors = FALSE)
    print(datar)
    #rownames(datar) <- data["id"]
    return (datar)
  }
  
  # This creates an empty record, to be used 
  
  CreateDefaultRecord <- function() {
    mydefault <- CastData(c("name" = input$name, "name2" = input$name2))
    return (mydefault)
  }
 
# takes the data as selected in the DataTable, and updates the inputs with the respective values:

  UpdateInputs <- function(data, session) {
    updateTextInput(session, "animal", value = input$name)
    updateTextInput(session, "plant", value = input$name2)
  }
  
  #finds the next ID of a new record
  
  GetNextId <- function() {
    if (exists("responses") && nrow(responses) > 0) {
      max(as.integer(rownames(responses))) + 1
    } else {
      return (1)
    }
  }

  # Create
  
  CreateData <- function(data) {
    data <- CastData(data)
    rownames(data) <- GetNextId()
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }

  # Read
  
  ReadData <- function() {
    if (exists("responses")) {
      responses
    }
  }
  
  # Update
  
  UpdateData <- function(data) {
    data <- CastData(data)
    responses[row.names(responses) == row.names(data), ] <<- data
  }
 
  # Delete

  DeleteData <- function(data) {
    responses <<- responses[row.names(responses) != unname(data["id"]), ]
  }
 
  # GetTableMetadata 
  
  GetTableMetadata <- function() {
    fields <- c(id = "Id", 
                name = "Name")
    result <- list(fields = fields)
    return (result)
  }

  
  
  
  #### 
  # Server 
  ####
  
  # Get the name of files uploaded and display it as a dropping list
  
  output$renderUI <- renderUI({
    dat <- input$file1
    selectInput('articlePath', 'Select an article to read', dat$name)
  })
  
  
  # Reactive expression after pressing "go" button to explore articles and create a plot
  read <- eventReactive(input$GoButton,{
    path <- input$file1$datapath
    dictio <- read.csv(input$dictionary,header = TRUE, stringsAsFactors = F)
    
    NetScrap2(path, dic = dictio)})
    
  
  
  # Render Article (uselful to copy tables)
  
  articleRender <- eventReactive(input$renderArticle, { 
    path <- input$file1$datapath[match(input$articlePath2,input$file1$name)]
    print(path)
    
    pdftools::pdf_text(path)
  })
  
  
  output$rbokeh <- renderRbokeh({
    figure() %>%
      ly_points(x, log1p(x1), data = read(),
                color = z, glyph = t1,
                hover = y) %>%
      x_range(callback = shiny_callback("x_range"))
  })
  
  
  output$renderUISlider <- renderUI({

    sliderInput("range", "Select range to render text:",
                min = 1, max = max(read()$x),
                value = c(200,500))
  })
  
  
  output$article <- renderText({ 
    substr(read()$text, min(input$range), max(input$range))
    })

  
  ## Database creation with shiny: 
  # code modified from: https://ipub.com/shiny-crud-app/
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
})
