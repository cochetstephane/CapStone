# User Interface

ui <- fluidPage(
    
    titlePanel(h1("Enter your sentence and hit the predict button: ")),
    
    titlePanel("When typing a sentence, the algorithm relies successively on 
                4-grams, 3-grams and 2-grams to predict, as soon as it is found,
                the up-coming word. If it is not found with the 2-grams strategy,
                it selects randomly the most used word, among the top-10, so
                there is always a proposal to come"),
    
    # INPUT for entry  
    fluidRow(column(width = 12,
              textInput("Intext",
              label = h1("Enter here after:"),
              value = "",
              width = '600px')),
             ),
   
    fluidRow(column(width = 12,
             br(),
             submitButton(h2("Predict")),
             br())
            ),
    
    textOutput("Intext2"),
    br(),
    tags$head(tags$style(HTML("
                            #Intext2 {
                              font-size: 20px;
                            }
                            ")))
  
)  

library(shiny)
library(DT)
library(sqldf)
library(data.table)
library(glue)
library(quanteda)
library(stringi)
library(stringr)
library(tidytext)

quadri <- readRDS("table_4_gram.rds")
tri   <-  readRDS("table_3_gram.rds")
tri <- data.frame(tri)
tri["unigram"] <- rownames(tri)
bi   <-   readRDS("table_2_gram.rds")
top  <-   readRDS("top_1_gram.rds")
top_df <- data.frame(top)
top_df["unigram"] <- rownames(top_df)

quadri <- data.table(quadri)
tri <- data.table(tri)
bi <- data.table(bi)

# SERVER
server <- function(input, output) {
    
    output$Intext2 <- renderText({
      gc()
      input$Predict
      #input$Intext
      text <- NULL
      text <- input$Intext
      x <- unlist(strsplit(text, " "))
      # Generates too much reduction
      #x <- x[!x %in% stop_words$word]
      text  <- paste(x, collapse = " ")
      text <- tolower(trimws(text))  # trim white space and in lower case
      
      test <- data.table (original = text)
      test[,extracted:=sapply(strsplit(original,'\\s+'),function(v) paste(collapse=' ',tail(v,3L)))]
      input <- test$extracted
      
      len <- sapply(strsplit(input," "), length)

       # LOGIC STRUCTURE 
      ifelse ( {(len == 2) & (len <3)} , {paste("Your entry must have 3 words at a minimum and you have currently 2 words")  },
      
      ifelse ( {(len == 1) & (len <3)} , {paste("Your entry must have 3 words at a minimum and you have currently 1 word")   },
      
      ifelse ( {(len == 0) & (len <3)} , {paste("Your entry must have 3 words at a minimum and you have currently no word entered")  },   
      
      ifelse (  {sen <- glue::glue('select * from quadri where unigram like "', paste(input, "%"), '"' )
                 out_quadri <- sqldf(sen)
                ((len == 3) & (nrow(out_quadri) > 0)) },    
                                              
                                              {a <- c(out_quadri$top.features.quadri == max(out_quadri$top.features.quadri))
                                               DF <- out_quadri[a,]
                                               sortie <- DF[sample(nrow(DF),1),][1,2]
                                               paste("The algorithm on 4-grams proposes :", sortie  )
                                               },
      
      ifelse ( {input <- sub(word(input), '', input) %>% trimws("l")
                sen <- glue::glue('select * from tri where unigram like "', paste(input, "%"), '"' )
                out_tri <- sqldf(sen)
                ( (len == 3) & (nrow(out_tri) > 0) ) }  ,
                                 
                                               {a <- c(out_tri$top.features.tri == max(out_tri$top.features.tri))
                                               DF <- out_tri[a,]
                                               sortie <-  DF[sample(nrow(DF),1),][1,2]
                                               paste("The algorithm on 3-grams proposes :", sortie )
                                               },
      
      ifelse ( {input <- sub(word(input), '', input) %>% trimws("l")
                sen <- glue::glue('select * from bi where unigram like "', paste(input, "%"), '"' )
                out_bi <- sqldf(sen)
                ( (len == 3) & (nrow(out_bi) > 0) )  } ,
               
                                              {a <- c(out_bi$top.features.bi == max(out_bi$top.features.bi))
                                               DF <- out_bi[a,]
                                               sortie <- DF[sample(nrow(DF),1),][1,2]
                                               paste("The algorithm on 2-grams proposes :",  sortie )   },
               
                  {paste("The algorithm proposes based on a random selection:",top_df[sample(nrow(top_df),1),][1,2])}  ))))))    
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
