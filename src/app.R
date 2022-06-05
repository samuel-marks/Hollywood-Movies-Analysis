
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(dplyr)
library(plotly)

ui <- fluidPage(
  tags$head(tags$style(HTML('* {font-family: "Comic Sans MS"};'))),
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Moviescopers" , titleWidth = 350),
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='VSKLogo.png' width = '300'>",
          "<br>",
          "<br>" )),
        
        menuItem("Home" , tabName = "home" , icon = icon("home")),
        menuItem("Genre" , tabName = "genre" , icon = icon("film")),
        menuItem("Runtime" , tabName = "runtime" , icon = icon("clock")),
        menuItem("Release Day", tabName = "releaseday" , icon = icon("calendar")),
        menuItem("Movies Table", tabName = "moviestable" , icon = icon("table")),
        menuItem("Compare" , tabName = "compare" , icon = icon("equals")),
        menuItem("About" , tabName = "about" , icon = icon("info"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "home",
                includeMarkdown("www/home.md"),
                selectInput("homeSelector" , choices = c("Purple",
                                                            "Orange"),
                            label = "Select any Graph:" ,
                            selected = "Purple"),
              column(5, align = "center", plotOutput("homeBarGraph")%>%withSpinner(color = "green")),
              column(7, align = "center", plotlyOutput("homeLineGraph" ,width = "100%",height = "480px")%>%withSpinner(color = "green"))
              ),
        
        tabItem(tabName = "genre",
                includeMarkdown("www/genre.md"),
                selectInput("genreSelector" , choices = c("Gross % in US",
                                                          "Gross % Overseas",
                                                          "Budget",
                                                          "Gross in $(millions)"),
                            label = "Genre w.r.t " ,
                            selected = "Gross % in US"),
                
              column(12, align = "center",plotOutput("genreGraph") %>% withSpinner(color = "green"))
                ),
        
        tabItem(tabName = "runtime",
                selectInput("runtimeSelector" , choices = c("Gross % in US",
                                                          "Gross % Overseas",
                                                          "IMDb Rating",
                                                          "Profit %"),
                            label = "Runtime w.r.t :" ,
                            selected = "Gross % in US"),
                
              column(12, align = "center", plotlyOutput("runtimeGraph" ,
                                                        width = "100%",
                                                        height = "720px") %>% withSpinner(color = "green")),
          ),
        tabItem(tabName = "releaseday" ,
                selectInput("releasedaySelector" , choices = c("Gross % in US",
                                                                "Gross % Overseas"),
                                                     label = "Release Day w.r.t :" ,
                                                     selected = "Gross % in US"),
                column(12, align = "center", plotOutput("releasedayGraph") %>% withSpinner(color = "green"))
                ),
        tabItem(tabName = "compare" ,
                includeMarkdown("www/compare.md"),
                column(6,selectInput("compareselector1" , choices = c("Gross % in US",
                                                               "Gross % Overseas"),
                            label = "Release Day w.r.t :" ,
                            selected = "Gross % in US"),
                        ),
                column(6,selectInput("compareselector2" , choices = c("Gross % in US",
                                                                                "Gross % Overseas"),
                                               label = "Release Day w.r.t :" ,
                                               selected = "Gross % in US"),
                        )
        ),tabItem(tabName = "about" ,
                includeMarkdown("www/about.md")
        )
      )
      
    )
  )
)

server <- function(input , output){
  #import dataset
  
  movies <- read.csv("Section6-Homework-Data.csv")
  
  #rename columns
  colnames(movies) <- c("Day.of.Week","Director","Genre","Movie.Title","Release.Date",
                        "Studio","Adjusted.Gross.in.millions","Budget.in.millions",
                        "Gross.in.millions","IMDb.Rating","MovieLens.Rating",
                        "Overseas.in.millions","Overseas.Percentage"
                        ,"Profit.in.millions","Profit.Percentage"
                        ,"Runtime.in.minutes","US.in.millions","Gross.Percentage.US")
  
  #FILTERING THE DATASET
  
  #corrections in the dataset
  c1 <- as.numeric(movies$Gross.in.millions)
  c2 <- as.numeric(movies$Adjusted.Gross.in.millions)
  c3 <- as.numeric(movies$Overseas.in.millions)
  c4 <- as.numeric(movies$Profit.in.millions)
  
  #apply the corrections to the dataset
  movies$Adjusted.Gross.in.millions <- c2
  movies$Gross.in.millions <- c1
  movies$Overseas.in.millions <- c3
  movies$Profit.in.millions <- c4
  
  #replace the NA values with appropriate values
  movies[is.na(movies$Profit.in.millions),"Profit.in.millions"] <- 
    movies[is.na(movies$Profit.in.millions),"Budget.in.millions"] *
    movies[is.na(movies$Profit.in.millions),"Profit.Percentage"] / 100
  
  movies[is.na(movies$Gross.in.millions),"Gross.in.millions"] <- 
    movies[is.na(movies$Gross.in.millions),"Budget.in.millions"] +
    movies[is.na(movies$Gross.in.millions),"Profit.in.millions"]
  
  movies[is.na(movies$Overseas.in.millions),"Overseas.in.millions"] <- 
    movies[is.na(movies$Overseas.in.millions),"Overseas.Percentage"] / 100 *
    movies[is.na(movies$Overseas.in.millions), "Gross.in.millions"]
  
  #adding filters as per requirement
  #and
  #save the filters in a new data set
  
  #filter as per required Genre
  genreFilter <- movies$Genre %in% c("action","adventure","animation",
                                     "comedy","drama")
  
  moviesFiltered <- movies[genreFilter,]
  
  #Categorizing Genre wise
  moviesFiltered$Genre <- factor(moviesFiltered$Genre)
  
  #Filter as per required Studio
  studioFilter <- moviesFiltered$Studio %in% c("Buena Vista Studios" , "Universal" ,
                                               "WB" , "Fox" , "Paramount Pictures" ,
                                               "Sony")
  
  moviesFiltered <- moviesFiltered[studioFilter,]
  
  #categorizing Studio wise
  moviesFiltered$Studio <- factor(moviesFiltered$Studio)
  
  #Converting type of Date from char to date
  moviesFiltered$Release.Date <-  as.Date(moviesFiltered$Release.Date,"%d/%m/%Y")
  
  #visualize data
  library(ggplot2)
  library(plotly)
  library(RColorBrewer)
  library(ggthemes)
  
  #_________________________________________home_______________
  
  
  #Number of movies per Studio
  m <- ggplot(data = moviesFiltered,aes(y=Studio)) +
    geom_bar(fill= "purple") + theme_gdocs()
  
  #Number of movies per Genre
  n <- ggplot(data = moviesFiltered,aes(x=Genre)) + 
    geom_bar(fill = "orange")+ theme_gdocs()
  
  #Comparison of ratings
  
  w <- ggplot(data = moviesFiltered ,  aes(x = IMDb.Rating , y = MovieLens.Rating)) + 
    geom_line(aes(colour = Genre))+
    geom_point(aes(colour = Genre),alpha = 0.5) +
    geom_smooth(alpha = 0.2) + theme_gdocs()
  w2 <- ggplotly(w)
  
  #__________________________________________Genre______________
  
  
  #insight of Gross % in US wrt genre
  p <- ggplot(data = moviesFiltered,aes(x= Genre,y= Gross.Percentage.US))+
    geom_jitter(aes(size = Budget.in.millions , colour = Studio) , alpha = 0.7) +
    geom_boxplot(alpha = 0.5) + theme_gdocs()
  
  #insight of Gross % Overseas wrt genre
  a <- ggplot(data = moviesFiltered,aes(x= Genre,y= Overseas.Percentage))+
    geom_jitter(aes(size = Budget.in.millions , colour = Studio) , alpha = 0.7) +
    geom_boxplot(alpha = 0.5) + theme_gdocs()
  
  #insight of budget wrt genre
  q <- ggplot(data = moviesFiltered,aes(x= Genre,y= Budget.in.millions))+
    geom_jitter(aes(size = Budget.in.millions , colour = Studio) , alpha = 0.7) +
    geom_boxplot(alpha = 0.5) + theme_gdocs()
  
  #insight of gross wrt genre
  
  l <- ggplot(data = moviesFiltered, aes(x=Genre,y=Gross.in.millions )) + 
    geom_jitter(aes(color = Studio, size = Budget.in.millions),alpha = 0.7)+
    geom_boxplot(alpha = 0.5)+
    theme_gdocs()
  
  #__________________________________________Runtime______________
  
  #Observing the effect of Run-time
  
  r <- ggplot(data = moviesFiltered,aes(x= Runtime.in.minutes,y= Gross.Percentage.US))+
    geom_line(alpha = 0.7 , aes(colour = Genre)) +
    geom_point(aes(colour = Genre),alpha = 0.5) +
    geom_smooth(alpha = 0.2) + theme_gdocs()
  
  s <- ggplot(data = moviesFiltered,aes(x= Runtime.in.minutes,y= Overseas.Percentage))+
    geom_line(alpha = 0.7 , aes(colour = Genre)) +
    geom_point(aes(colour = Genre),alpha = 0.5) +
    geom_smooth(alpha = 0.2)+ theme_gdocs()
  
  t <- ggplot(data = moviesFiltered,aes(x= Runtime.in.minutes,y= IMDb.Rating))+
    geom_line(alpha = 0.7 , aes(colour = Genre)) +
    geom_point(aes(colour = Genre),alpha = 0.5) +
    geom_smooth(alpha = 0.2) + theme_gdocs()
  
  v <- ggplot(data = moviesFiltered,aes(x= Runtime.in.minutes,y= Profit.Percentage))+
    geom_line(aes(colour = Genre)) +
    geom_point(aes(colour = Genre),alpha = 0.5) +
    geom_smooth(alpha = 0.2)+ theme_gdocs()
  
  #_________________________________________Release Day_______________
  
  #Gross % insight wrt Day of the week
  x <- ggplot(data = moviesFiltered ,  aes(x = Day.of.Week , y = Gross.Percentage.US)) + 
    geom_jitter(aes(colour = Studio , size = Budget.in.millions),alpha = 0.7)+
    geom_boxplot(alpha = 0.5) + theme_gdocs()
  
  y <- ggplot(data = moviesFiltered ,  aes(x = Day.of.Week , y = Overseas.Percentage)) + 
    geom_jitter(aes(colour = Studio , size = Budget.in.millions),alpha = 0.7)+
    geom_boxplot(alpha = 0.5) + theme_gdocs()
  
  #________________________________________________________
  
  
  
  output$homeLineGraph <- renderPlotly({
    print(w2)
  }
  )
  output$homeBarGraph <- renderPlot({
    homeBarPlot = switch(
      input$homeSelector,
      "Purple" = m ,
      "Orange" = n
    )
    
    print(homeBarPlot)
  },height = 480)
  
  output$genreGraph <- renderPlot({
    genrePlot = switch(
      input$genreSelector,
      "Gross % in US" = p ,
      "Gross % Overseas" = a ,
      "Budget" = q ,
      "Gross in $(millions)" = l
    )
    
    print(genrePlot)
  },
  width = 1216 ,height = 684)
  
  output$runtimeGraph <- renderPlotly({
    runtimePlot = switch(
      input$runtimeSelector,
      "Gross % in US" = r ,
      "Gross % Overseas" = s ,
      "IMDb Rating" = t ,
      "Profit %" = v
    )
    
    print(runtimePlot)
  }
  )
  
  output$releasedayGraph <- renderPlot({
    releasedayPlot = switch(
      input$releasedaySelector,
      "Gross % in US" = x ,
      "Gross % Overseas" = y
    )
    
    print(releasedayPlot)
  },
  width = 1280,height = 720)
  
  
  
}

shinyApp(ui = ui , server = server)