
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rbokeh)

# Set paths of files
dictionary.path <- "dic/"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("dic","/", filenames.dic, sep = "")
names(path2) <- filenames.dic


shinyUI(
  navbarPage("SpIn Miner: Species Interactions Miner", id="vis",
             tabPanel("About", 
                      fluidPage(
                        column(9, 
                      includeMarkdown("About.Rmd")
                               )
                      ),
                      p("This shiny app is currently on development and
                        under MIT License."),
                      p("Contact Person: Gabriel Muñoz"),
                      p(a(shiny::icon('github fa-2x'),href='https://github.com/fgabriel1891/SpeciesInteractionsMiner',target='_blank'))
             ),
             
             tabPanel("Upload Literature",
                      
                      
                          fluidPage(
                            column(5, 
                                   h4("Upload Literature"),
                                   fileInput('file1', 'Upload your article(s) here',
                                             accept=c('.pdf'), multiple = T,
                                             placeholder = "No file(s) selected"),
                                   h4("Important!"),
                                   h5("Files must be in PDF format"), 
                                   p( "It is recomended to name your files appropiately"),
                                   p( "(e.g. SomeAuthor_et_al_2018.pdf)"),
                                   h5("Scanned versions of articles are not yet accepted ")
                                   ),
                             column(5, 
                                    h4("Select a dictionary"),
                                    selectInput(inputId = "dictionary",
                                                label = "From the dropdown list",
                                                path2)),
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("style.css"))
                          )
                      
             ),
             tabPanel("Exploration Tool",
                      fluidRow(
                        column(4, 
                               uiOutput("renderUI")),
                        column(2,
                               actionButton(inputId = "GoButton", 
                                            label = "Explore"))
                        
                      ),
                      fluidPage(
                        column(8,  
                               rbokehOutput("rbokeh", width = "100%", height = "400px")
                               ),
                        column(4,
                              uiOutput("renderUISlider"),
                              h5(textOutput("article")))
                      ),
                      fluidRow(
                        column(4, 
                               textInput("name", "Plant", "")
                               ),
                        column(2, 
                               p("Interacts with")),
                        column(4, 
                               textInput("name2", "Animal", "")
                               ),
                        column(2,
                               actionButton("submit", "Submit")
                               ),
                        fluidRow(
                          column(7,
                                 #data table
                                 DT::dataTableOutput("responses", width = 300)
                        )),
                        
                        
                        #input fields
                        tags$hr()
                        
                      )),
             
             tabPanel("Download", 
                      h2("Download interactions mined"),
                      fluidPage(
                        column(3, 
                               #action buttons  
                               downloadButton("new", "Download Database")
                               )),
                      h2("Visualize the results")
                      )
                      )
             )
  
