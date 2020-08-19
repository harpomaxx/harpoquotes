#https://stackoverflow.com/questions/63195691/can-i-use-observeevent-with-bsplusbs-accordion

library(shiny)
library(magick)
library(shinyjs) 

library(slickR)

  
chata_character<-  list(
         name="Chata",
         base_image="chata-frases-base.png"
         )

harpo_character<- list(
        name="Harpo",
        base_image="harpo-frases-base2.png"
        )

facha_character<- list(
  name="Facha",
  base_image="facha-frases-base.png"
)

rodralez_character<- list(
  name="Rodralez",
  base_image="rodralez-frases-base.png"
)

flequiboina_character<- list(
  name="Prof. Flequiboina",
  base_image="flequiboina-frases-base.png"
)


character_phrases_base<-list(chata_character,harpo_character,facha_character,rodralez_character,flequiboina_character)
  
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
           
           
           #verbatimTextOutput("dbg"),
           div(
             p("Click on your favorite character: "),style = "font-weight: bold;color: brown"),
           slickROutput("slick_output", width='80%',height='110%'),
          
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
  
  output$slick_output <- renderSlickR({
    x <- slickR(c("chata-small.png",
                  "harpo-small.png",
                  "facha-small.png",
                  "rodralez-small.png",
                  "flequiboina-small.png"
    ),
    slideId = 'harpoquotes',
    height = '1%',
    width = '1%') + 
      settings(slidesToShow=1,centerMode=FALSE,variableWidth = FALSE)
  })
  
  active_slick <- shiny::reactiveValues()
  
  shiny::observeEvent(input$slick_output_current,{
    
    clicked_slide    <- input$slick_output_current$.clicked
    relative_clicked <- input$slick_output_current$.relative_clicked
    center_slide     <- input$slick_output_current$.center
    total_slide      <- input$slick_output_current$.total
    active_slide     <- input$slick_output_current$.slide
    
    if(!is.null(clicked_slide)){
      active_slick$clicked_slide    <- clicked_slide
      active_slick$center_slide     <- center_slide
      active_slick$relative_clicked <- relative_clicked
      active_slick$total_slide      <- total_slide
      active_slick$active_slide     <- active_slide
    }
  })
  
#  output$dbg <- renderText({
#    #paste("hole")
#    l <- shiny::reactiveValuesToList(active_slick)
#    paste(gsub('_',' ',names(l)), unlist(l),sep=' = ',collapse='\n')
#  })
  
 
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
    if(!is.null(active_slick$clicked_slide)){
        char_name<-character_phrases_base[[active_slick$clicked_slide]]$name
        char_base_image<-character_phrases_base[[active_slick$clicked_slide]]$base_image
    }else{
      char_name<-character_phrases_base[[1]]$name
      char_base_image<-character_phrases_base[[1]]$base_image
      
    }  
      
    harpo_quote_base <- image_read(char_base_image)
    image_annotate(harpo_quote_base, paste0("''",wrap.labels(text,28),"''"), size = 60, color = "black",
                   degrees = 0, location = "+40+100", style="Italic", weight = 700) %>% 
      image_annotate(paste0("(",char_name,", circa ", input$year,")"), 
                     size = 40, color = "black",
                     degrees = 0, location = "+180+355") %>% 
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

