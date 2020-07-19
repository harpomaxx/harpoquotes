#

library(shiny)
library(magick)
library(shinyjs) 
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  #shinythemes::themeSelector(),
  tags$head(tags$link(rel="shortcut icon",
                      href="https://raw.githubusercontent.com/harpomaxx/harpoquotes/master/favicon.ico",
                      type="image/vnd.microsoft.icon"
            )),

  #titlePanel(id="big-heading","Harpo Quotes Generator", windowTitle = "Harpo Quotes Generator"),
  h1(id="big-heading", "Harpo's Quotes Generator"),
  tags$style(HTML("#big-heading{background-color: pink;color: brown}")),
  h5("Now, you finally have the oportunity to make your own version of Harpo's quotes to use whenever you want. Please, use it wisely! "),
   
   # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
        div(
        textAreaInput ("text",
                  "Put your quote here",width = 300, height  = 100 ,
                  value = "Something really important about a lot of things..."),style = "color: brown"),
  
        div(
        textInput("year",
                  "circa year",value="2020", width = 100 ),style = "color: brown"),
  
        downloadButton("down","Download")
        
        ),
  
  
    column(4,imageOutput("imageQuote"))
           )
         
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
 
  # Core wrapping function
  wrap.it <- function(x, len)
  { 
    sapply(x, function(y) paste(strwrap(y, len), 
                                collapse = "\n"), 
           USE.NAMES = FALSE)
  }
  
  
  # Call this function with a list or vector
  wrap.labels <- function(x, len)
  {
    if (is.list(x))
    {
      lapply(x, wrap.it, len)
    } else {
      wrap.it(x, len)
    }
  }
  
  
  generate_quote <- function(text,year){
    harpo_quote_base <- image_read("harpo-frases-base.png")
    image_annotate(harpo_quote_base, paste0("''",wrap.labels(text,28),"''"), size = 60, color = "black",
                   degrees = 0, location = "+40+100", style="Italic", weight = 700) %>% 
      image_annotate(input$year, size = 33, color = "black",
                     degrees = 0, location = "+520+305") %>% 
      image_annotate("Harpo's Quotes (R)", location = "+820+500", size=10)
  }
   
#  output$down <- downloadHandler(
#    filename = "harpo-quote.png",
#    content = function(file) { 
#      file.copy('image.png', file)
#      } 
#    contentType = "image/png"
#  )
    data <-mtcars 
    
    output$down <- downloadHandler(
      filename = "harpo-quote.png",
      content = function(file) {
        file.copy("./image.png", file)
        #write.csv(data, file)
        
      },
      contentType = "image/png"
      
    )
  
  output$imageQuote <- renderImage({
    shinyjs::runjs("$('#year').attr('maxlength', 4)")
    shinyjs::runjs("$('#text').attr('maxlength', 75)")
     image <- generate_quote(input$text,input$year)
     
     tmpfile <- image %>% 
       image_write(path=paste0(getwd(),"/image.png"),
                  format = 'png')
     
     list(src = tmpfile,
          alt = input$text,
          width = 926 * 0.3,
          height = 516 * 0.3)
     
   }
   , deleteFile = FALSE)
 
}

# Run the application 
shinyApp(ui = ui, server = server)

