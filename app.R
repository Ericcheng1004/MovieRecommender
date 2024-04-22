library(shiny)
library(imdbapi)
library(DT)
source("demo_Together.R")
`%notin` <- Negate(`%in%`)

# OMDB API Key
#imdb_key <- imdbapi::omdb_api_key(Sys.getenv("OMDB_API_KEY"))
imdb_key = Sys.getenv("OMDB_API_KEY")

Combined = as.data.frame(read_parquet("Combined.parquet"))
recommend = as.data.frame(read_parquet("recommend.parquet"))
simil = as.data.frame(read_parquet("simil.parquet"))

# remove dupe titles
Combined = Combined %>%
  group_by(primaryTitle) %>% arrange(desc(numVotes)) %>% slice(1) %>% ungroup() %>% select(-numVotes)

#simil = simil %>% dplyr::filter(data2_name %in% recommend$Movie1)
movielist = simil %>% rename("movie"="data2_name") %>% select(movie) %>% arrange(movie) %>% as_tibble() %>%
  inner_join(Combined %>% select(imdbId,primaryTitle,genres.y,startYear) %>% 
              rename("genres"="genres.y","movie"="primaryTitle"),by="movie") %>%
  mutate(movie2=paste0(movie," (",startYear,")"))
#Combined = Combined %>% dplyr::filter(numVotes>=20000)
genres = Combined %>% select(genres.y) %>% separate_longer_delim(genres.y,delim=",") %>% count(genres.y) %>% arrange(genres.y) %>% dplyr::filter(n>100) %>%
  rename("genres"="genres.y") %>%
  bind_rows(tribble(~genres,"None"),.)

hybrid_rec2_wrapper <- function(movie,genre){
  if(length(movie)<1){
    return(data.frame(data=c('No data to display...')))
  } else{
    df = hybrid_rec2(movie) %>% as_tibble()
    print(df)
    df = df %>% left_join(Combined %>% select(primaryTitle,genres.y,imdbId) %>% rename("Genres"="genres.y"),by=c("name"="primaryTitle"))
    if(genre=='None'){
      # do nothing
    } else{
      df = df %>% dplyr::filter(str_detect(Genres,genre))
    }
    df = df %>% head(40)
    df = df %>% rename("Recommended Movie"="name","Score"="Combined_values") %>% select(`Recommended Movie`,`Score`,`Genres`,imdbId) %>%
      rowwise() %>% mutate(Score = round(Score,2)) %>% ungroup()
    df = df %>% mutate(img=paste0("<img src ='https://img.omdbapi.com/?i=tt",stringr::str_pad(imdbId,7,pad="0"),"&h=600&apikey=",imdb_key,"'></img>")) %>%
      select(-imdbId)
    df = df %>% dplyr::filter(`Recommended Movie` %notin% movie)
    df = df %>% as.data.frame()
    return(df)
  }
}

collab_rec2_wrapper <- function(movie,genre){
  if(length(movie)<1){
    return(data.frame(data=c('No data to display...')))
  } else{
    df = collab_rec2(movie)
    df = df %>% left_join(Combined %>% select(primaryTitle,genres.y,imdbId) %>% rename("Genres"="genres.y"),by=c("name"="primaryTitle"))
    if(genre=='None'){
      # do nothing
    } else{
      df = df %>% dplyr::filter(str_detect(Genres,genre))
    }
    df = df %>% head(40)
    df = df %>% rename("Recommended Movie"="name","Score"="number") %>% select(`Recommended Movie`,`Score`,`Genres`,imdbId) %>%
      rowwise() %>% mutate(Score = round(Score,2)) %>% ungroup()
    df = df %>% mutate(img=paste0("<img src ='https://img.omdbapi.com/?i=tt",stringr::str_pad(imdbId,7,pad="0"),"&h=600&apikey=",imdb_key,"'></img>")) %>%
      select(-imdbId)
    df = df %>% dplyr::filter(`Recommended Movie` %notin% movie)
    df = df %>% as.data.frame()
    return(df)
  }
}
content_rec2_wrapper <- function(movie,genre){
  if(length(movie)<1){
    return(data.frame(data=c('No data to display...')))
  } else{
    df = content_rec2(movie)
    df = df %>% left_join(Combined %>% select(primaryTitle,genres.y,imdbId) %>% rename("Genres"="genres.y"),by=c("name"="primaryTitle"))
    if(genre=='None'){
      # do nothing
    } else{
      df = df %>% dplyr::filter(str_detect(Genres,genre))
    }
    df = df %>% head(40)
    df = df %>% rename("Recommended Movie"="name","Score"="values") %>% select(`Recommended Movie`,`Score`,`Genres`,imdbId) %>% mutate(Score=as.numeric(Score)) %>%
      rowwise() %>% mutate(Score = round(Score,2)) %>% ungroup()
    df = df %>% mutate(img=paste0("<img src ='https://img.omdbapi.com/?i=tt",stringr::str_pad(imdbId,7,pad="0"),"&h=600&apikey=",imdb_key,"'></img>")) %>%
      select(-imdbId)
    df = df %>% dplyr::filter(`Recommended Movie` %notin% movie)
    df = df %>% as.data.frame()
    return(df)
  }
}


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("Movie Recommender System"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      selectInput("algo", "Algorithms:",
                  c("Hybrid" = "hybrid",
                    "Collaborative" = "collab",
                    "Content" = "content")),
      
      selectizeInput("movie", "Movies:",
                     choices = NULL,
                     multiple=TRUE),
      
      selectizeInput("genre", "Genres:",
                     choices = NULL),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
    #   sliderInput("n",
    #               "Number of observations:",
    #               value = 500,
    #               min = 1,
    #               max = 1000)
    #   
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput('table'))#tableOutput("table"))
      )
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # server-side R list rendering
  updateSelectizeInput(session, 'movie', choices = movielist$movie, server = TRUE)
  updateSelectizeInput(session, 'genre', choices = genres$genres, server = TRUE)
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    algo <- switch(input$algo,
                   hybrid = hybrid_rec2_wrapper,
                   collab = collab_rec2_wrapper,
                   content = content_rec2_wrapper,
                   hybrid_rec2_wrapper)
    
    algo(input$movie,input$genre)
  })
  
  # Generate an HTML table view of the data ----
  # output$table <- renderTable({
  #   d()
  # })
  # output$table <- DT::renderDataTable({
  #   DT::datatable(d(),escape=FALSE)
  # })
  output$table <- DT::renderDT({
    DT::datatable(d(),escape = FALSE)
  })
  rsconnect::setAccountInfo(name='movierecommender1',
                            token='D76BCD4E83CCBFEC817B4AA730AB8BC2',
                            secret='waUDijctAPgo8+t+BvkOZ5yUzpeGHEGciT2+3MUc')
}

# Create Shiny app ----
shinyApp(ui, server)




