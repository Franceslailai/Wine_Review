if(!require(leaflet)){install.packages("leaflet")}   
if(!require(plotly)){install.packages(c("plotly"))}
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(janitor)
library(dplyr)
library(plotly)
library(mailtoR)
library(sf)  
library(leaflet)
library(tmap)

data(World)                   # The first column is the 3 character name, just like in the Wine dataset
names(World)[2] <- "Country"  # Change the column name; indeed, it's Country in Wine.RData
load("wine.RData")


ui <- dashboardPage(skin = "purple",                            #change the skin
                    dashboardHeader(title = "A Liquid Fits All",  # Title of the dashboard
                                    tags$li(class = "dropdown",
                                            tags$a(href="https://www.linkedin.com/in/yi-lun-frances-lai/", 
                                                   target="_blank", 
                                                   icon("linkedin", "fa-1.5x", lib = "font-awesome")  # fa-1.5x mean 3 times bigger
                                            )),
                                    tags$li(class = "dropdown",
                                            tags$a(href="mailto:yilun.lai@edu.em-lyon.com", 
                                                   target="_blank", 
                                                   icon("envelope", "fa-1.5x", lib = "font-awesome")  # fa-1.5x mean 3 times bigger
                                            ))
                    ),  
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "Introduction", icon = icon("leaf", lib = "font-awesome")),
                        menuItem("Overview", tabName = "Overview", icon = icon("wine-bottle", lib = "font-awesome")),
                        column(12, h4(" Inputs")),
                        sliderInput("points", h5("Points"),                        # Element 1: point selection
                                    min = 80, max = 96, 
                                    value = c(80, 96), step = 4, sep = ""),
                        checkboxGroupInput("price_quartile", h5("Price Quartile"),  # Element 2: price_quartile selection
                                           choices = unique(wine$Price_quartile),
                                           selected = c("Low", "Medium","High","Higher")),  
                        numericInput("max_country", h5("Max number countries (8)",br()," only appliable for overview page"),  # Element 3: nb of countries displayed
                                     min = 2, max = 8, step = 1, value = 4)
                      )),
                    
                    ####### where body shows
                    dashboardBody(
                      tabItems(
                        tabItem(tabName ="Introduction",          #build the info boxes
                                fluidRow(
                                  column(8,h2(textOutput("welcome"),style="color:maroon")),
                                  column(4),
                                  column(12,  box(tags$b(helpText("You recall  your birthday with your loved ones; 
                                                  you never forget the heart breaking feeling of your first love. 
                                                  All these occasions, it is always there. it accompanies you through all the ups and downs. 
                                                  A liquid fits all scenarios, wine.")),
                                                  helpText("Wine is one of the most consumed alcoholic beverages with the greatest impact in the world. 
                                                  It is fermented and brewed from fresh grapes or grape juice with no less than 7% alcohol. 
                                                  Besides bringing relaxed and happy vibe to people, 
                                                  the nutrition of grapes during fermentation process is another terrific added value.",br(),br(),
                                                  "Before studying in France,I was aware of the fame of the wine and its indispensable role in French dinning, 
                                                  but I never really had the chance to truly investigated or experienced it myself. When I arrived in Paris in August in 2021, 
                                                  I found wine is not only everywhere, but it has so many types for different phases during dining, wine for appetizer, 
                                                  wine for digestive, red wine with red meat, and white wine with seafood.I found this special liquid suits for all occasions. 
                                                   Consequently, I decided to explore this wine Reviews to exert some insights and to verify doubts I had.",br(),br(),                                             
                                                  "As now I am a student, I choose to focus on the affordable wine price, under $25.",br(), br(),
                                                  "The source of Wine Reviews dataset is from Kaggle : 
                                           https://www.kaggle.com/zynicide/wine-reviews?select=winemag-data_first150k.csv"),
                                           br(), width = 10)), 
                                
                                  column(10,
                                         infoBoxOutput("number_wine", width = 6),
                                         infoBoxOutput("number_country", width = 6),
                                         infoBoxOutput("number_winery", width = 6),
                                         infoBoxOutput("number_grape", width = 6),
                                br(),br()),
                                column(2),
                                column(12,box(h3(helpText("Correlation Coefficient of Price and Point")),
                                       helpText("The country widget on the left (max 8 countries) is not applicable because this graph depicts 
                                                         the relationship between price and points throughout the entire data set. 
                                                         In general, there is no strong association between price and points, 
                                                         and even in the lowest price quartile, there is no evident correlation between price and points. 
                                                         However, you may still play with the widgets to acquire different insights."),
                                       column(10,plotlyOutput("correlation_coefficient_price_points", height = 300)), width = 10),
                                       column(2,br(),br())))),
                        
                        tabItem(tabName = "Overview", 
                                fluidRow(                                   #everything is the frame
                                  tabBox(
                                    title = "Overview", height = "920px", width = "400px",
                                    tabPanel("Country",        # tab 1: country overview based on price and points
                                             fluidRow(
                                               br(),
                                               column(8,helpText("In total, there are 43 countries in the dataset. You can utilize the left-hand widgets 
                                                        to analyze the result you're interested in. 
                                                        However, the country widget's maximum is set to eight for better graph visualization. 
                                                        More than 60% of the data comes from the United States (36%), France (13%), and Italy (12%). 
                                                        As a result, the top-ranking countries in OVERVIEW have a similar trend."),
                                                      plotlyOutput("country", height = 300),br()),
                                               column(4),br(),
                                               column(8,plotlyOutput("country_nbwine", height = 400),br(),br()),
                                               column(4,DT::dataTableOutput("table_country"),br(),br()), 
                                               column(10,leafletOutput("country_plot", height = 450)),
                                               column(2)  )
                                    
                                  ),
                                  tabPanel("Winery",            # tab 2: Winery overview
                                           fluidRow(
                                             br(),
                                             column(8,tags$b(helpText("A winery is a building or property that produces wine, 
                                                or a business involved in the production of wine, such as a wine company."
                                             )), 
                                             helpText("The following chart shows the similar trend. However, the number of French wineries is larger than that in 
                                             Italy though the number of wines from Italy is larger than that from France."),
                                             column(4)),
                                             column(8,plotlyOutput("winery", height = 300),br(),br()),
                                             column(4,DT::dataTableOutput("table_winery_group_by_country_nb"),br(),br()),       
                                             column(8,DT::dataTableOutput("table_winery")),
                                             column(4))
                                  ),
                                  
                                  tabPanel("Designation",             # tab 3: Designation(Vineyard) overview
                                           fluidRow(
                                             br(),
                                             column(8,tags$b(helpText("Designation is the vineyard where the grapes are grown.")),
                                                    br(),
                                                    plotlyOutput("designation", height = 300),
                                                    br(),
                                                    leafletOutput("designation_plot", height = 450)),
                                             column(4))
                                  ),
                                  tabPanel("Grape Variety",             # tab 4: Grape Variety overview
                                           fluidRow(
                                             br(),
                                             column(8,plotlyOutput("grape", height = 300)),
                                             column(4),
                                             br(),
                                             column(8,tags$b(helpText(br(),"The following table displays the most used grape variety for wine listed in this dataset.")),
                                                    br(),DT::dataTableOutput("table_grape"),br()),
                                             column(4,br(),br()),
                                             column(12,h4(helpText("TOP 3 GRAPE VARIETY"),br(),helpText("Chardonnay, Sauvignon Blanc, and Cabernet Sauvignon are the most 
                                                 frequently used grape varieties to make wine. ")),br())),
                                           fluidRow(
                                             column(4,img(src = 'Chardonnay.jpg', width = "70%",style="display: block; margin-left: auto; margin-right: auto;")),
                                             column(4,img(src = 'Sauvignon.jpg', width ="70%",style="display: block; margin-left: auto; margin-right: auto;")),    
                                             column(4,img(src = 'Cabernet.png', width = "70%",style="display: block; margin-left: auto; margin-right: auto;"),br()),
                                             column(4, helpText( "Chardonnay, a green-skinned grape variety used in the production of white wine, 
                                               is originated in the Burgundy wine region of eastern France, but is now grown wherever wine is produced. 
                                               The Chardonnay grape itself is neutral, but with many of the flavors commonly associated with influences, 
                                               such as soil and oak.")),
                                             column(4,helpText( "Sauvignon Blanc is a green-skinned grape variety that originates from the 
                                               Bordeaux region of France. Its flavor can range from aggressively grassy to 
                                               sweetly tropical depending on the climate.")),
                                             column(4,(helpText("Cabernet Sauvignon is one of the world's most widely recognized red wine grape varieties. 
                                              The classic profile of Cabernet Sauvignon tends to be full-bodied wines with high tannins 
                                              and noticeable acidity that contributes to the wine's aging potential."))))
                                  ),
                                  tabPanel("Data",        # tab 5: ALL of the data exclude designation and comment
                                           fluidRow(
                                             column(12, br(),
                                                    tags$b(helpText(" The following table demonstrates the whole dataset.")),
                                                    helpText("All 43 countries are displayed in the following table." ,br(),
                                                             "Thus, country widget on the left is not applicable, but you can use both points and price widgets to 
                                                             look into the details that you are interested in. 
                                                             Furthermore, review of each wine is also provided for you to imagine the taste")
                                                    ),
                                             column(12, DT::dataTableOutput("table_whole"))))
                                  
                                )   
                        )
                      )
                    )
))

server <- function(input, output){
  
  
  
  ####### for server
  data <- reactive({                # Creates the dynamic data for all to use
    wine= subset(wine, select = - c(Region_1,Designation,Description))
    wine %>%                  # Filter Points & Price_quartile
      filter(Points >= input$points[1], 
             Points <= input$points[2],
             Price_quartile %in% input$price_quartile) 
  })
  
  
  output$welcome <- renderText( " Hi!!   My dear WINE fellows!")
  
  #######  for introduction page-info box
  output$number_wine <- renderInfoBox({            #first info box for wine number
    wine <- data()
    infoBox(
      value = data () %>% count(), 
      title = "Total Number of Wine", 
      icon = icon("glass-martini", lib = "font-awesome"),
      color = "maroon",
      width = 6
    )
  })
  
  output$number_country <- renderInfoBox({            #second info box for country number
    wine <- data()
    infoBox(
      value = data () %>%count(Country) %>% tally(), 
      title = "Total Number of Country",
      icon = icon("globe", lib = "font-awesome"),
      color = "maroon",
      width = 6
    )
  })
  
  output$number_winery <- renderInfoBox({            #third info box for winery number
    wine <- data()
    infoBox(
      value = data () %>%count(Winery) %>% tally(), 
      title = "Total Number of Winery", 
      icon = icon("warehouse", lib = "font-awesome"),
      color = "maroon",
      width = 6
    )
  })
  
  output$number_grape <- renderInfoBox({            #fourth infobox for grape number
    wine <- data()
    infoBox(
      value = data () %>%count(Variety) %>% tally(), 
      title = "Total Number of Grape Variety", 
      icon = icon("seedling", lib = "font-awesome"),
      color = "maroon",
      width = 12
    )
  })
  
  #######  for introduction page- correlation coefficient plot between price and points
  output$correlation_coefficient_price_points <- renderPlotly({
    data() %>% 
      ggplot(aes(x = Price,y = Points )) + geom_point(size = 0.8,color="purple")+  
      geom_smooth(method='lm', formula= y~x, color="darkred")+
      labs(x="Price of Wine", y = "Points of Wine")+ theme_classic() + 
      theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12) )    
  })
  
  ####following for first tab -country
  data <- reactive({                # Creates the dynamic data 
    wine= subset(wine, select = - c(Region_1,Designation,Description))
    wine %>%                  # Filter Points & Price_quartile
      filter(Points >= input$points[1], 
             Points <= input$points[2],
             Price_quartile %in% input$price_quartile) 
  })
  
  filter_country <- reactive({            # Performs a filter to limit countries
    data() %>% 
      group_by(Country) %>%           # Analysis by country
      summarise(nb_price = n()) %>%  # Sum number of price
      arrange(desc(nb_price)) %>%    # Order by the number
      head(input$max_country) %>%     # Keep only max_country
      pull(Country)                   # Keep only the Country column/variable
  }) 
  
  output$country <- renderPlotly({
    data_country <- data() %>%  filter(Country %in% filter_country()) 
    g_country <-  data_country %>% ggplot(aes(x = Points, fill = Country)) + geom_bar()+
      scale_fill_brewer(palette ="Set2") +   
      ylab("Number of Wine")+theme_minimal()+ 
      theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12) ) 
    ggplotly(g_country)
  })
  
  output$country_nbwine <- renderPlotly({
    data() %>% filter(Country %in% filter_country())%>%
      group_by(Country) %>%
      count() %>%
      ggplot(aes(y=reorder(Country,n), x = n, fill =Country)) + geom_col(width = 0.6)+
      geom_text(aes(label=n), vjust=2, size=3)+
      scale_fill_brewer(palette ="BuPu") +
      xlab("Number of Wine") +  ylab("Country")+ theme_minimal()+ 
      theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12) ) 
  })
  
  output$country_plot <- renderLeaflet({
    data_map <- data() %>%
      group_by(Country) %>%
      count()                                                         # Counts nb instances, like n()
    data_map <- left_join(World %>% 
                            select(Country, geometry), data_map)   # Joining the geometry column
    palet <- colorBin("Spectral",                                   # Palette
                          domain = data_map %>% pull(n), bins = c(0,500,1000,3000,5000,16000)                   # Domain of labels: nb medals
    )
    
    
    
    labels <- sprintf(                               # Below we define the labels
      "<strong>%s</strong><br/>%g WINES",                            # Adding text to label
      data_map$Country,                                               # We show the country name...
      data_map$n                                                      # ... and the nb medals
    ) %>% lapply(htmltools::HTML)                                       # Embedded all into html language
    
    data_map %>% 
      data.frame() %>%                                # Turn into dataframe (technical)
      sf::st_sf() %>%                                 # Format in sf
      st_transform("+init=epsg:4326") %>%             # Convert in particular coordinate reference 
      leaflet() %>%                                   # Call leaflet
      setView(lng = 3, lat = 55,zoom = 1.45)%>%
      addPolygons(fillColor = ~palet(n),        # Create the map (colored polygons)
                  weight = 2,                         # Width of separation line
                  opacity = 1,                        # Opacity of separation line
                  color = "white",                    # Color of separation line
                  dashArray = "3",                    # Dash size of separation line
                  fillOpacity = 0.7,                  # Opacity of polygon colors
                  highlight = highlightOptions(       # 5 lines below control the cursor impact
                    weight = 2,                       # Width of line
                    color = "#CBCBCB",                # Color of line
                    dashArray = "",                   # No dash
                    fillOpacity = 0.7,                # Opacity
                    bringToFront = TRUE),
                  label = labels,                     # LABEL! Defined above!
                  labelOptions = labelOptions(        # Label options below...
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      ) %>%
      addLegend(pal = palet,                    # Legend: comes from palet colors defined above
                values = ~n,                    # Values come from lifeExp variable
                opacity = 0.7,                  # Opacity of legend
                title = "Numner of the Wine",           # Title of legend
                position = "bottomright")       # Position of legend
  })
  
  
  country1<- reactive({
    data()%>% 
      group_by(Country, .drop = FALSE) %>%
      summarise(nb=n())%>%
      arrange(desc(nb))
  })
  
  output$table_country <- DT::renderDataTable({ country1()},
                                              options = list(   
                                                lengthMenu = list(c(3, 5, 8), c('3', '5','8')),    
                                              pageLength = 5  ))
  
  ####following for second tab - winery
  
  winery <- reactive({
    data()%>% 
      group_by(Winery,Country, .drop = FALSE) %>%
      summarise(Number=n())%>%
      arrange(desc(Number))
  })
  
  
  output$winery <- renderPlotly({
    winery() %>% filter(Country %in% filter_country())%>%
      group_by(Country) %>%
      count() %>%
      ggplot(aes(y=reorder(Country,n), x = n, fill =Country)) + geom_col(width = 0.6)+
      geom_text(aes(label=n), vjust=2, size=3)+
      scale_fill_brewer(palette ="BuPu") +
      xlab("Number of Winery") +  ylab("Country")+ theme_minimal()+ 
      theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12) ) 
  })
  
  
  output$table_winery <- DT::renderDataTable({winery()}) # Create the output object of country table!
  
  ####following for third tab - Designation(vineyard ) 
  
  data1 <- reactive({                # Creates the dynamic data for all to use
    wine= subset(wine, select = - c(Region_1,Description))
    wine %>%                  # Filter Points & Price_quartile
      filter(Points >= input$points[1], 
             Points <= input$points[2],
             Price_quartile %in% input$price_quartile) 
  })
  
  
  designation <- reactive({
    data1()%>% 
      group_by(Country, .drop = FALSE) %>%
      summarise(n=n_distinct(Designation))%>%
      arrange(desc(n))
  })
  
  
  output$designation <- renderPlotly({
    designation() %>% filter(Country %in% filter_country())%>%
      group_by(Country) %>%
      ggplot(aes(y=reorder(Country,n), x = n, fill =Country)) + geom_col(width = 0.6)+
      scale_fill_brewer(palette ="BuPu") +
      geom_text(aes(label=n), vjust=2, size=3)+
      xlab("Number of Designation") +  ylab("Country")+ theme_minimal()+ 
      theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12) ) 
  })
 
  #### world map for designation 
  
  output$designation_plot <- renderLeaflet({
    data_map <- data1() %>%
      group_by(Country) %>%
      summarise(n=n_distinct(Designation))
      data_map <- left_join(World %>% 
                            select(Country, geometry), data_map)  # Joining the geometry column
       
      palet <- colorBin("Spectral",                                   # Palette
                        domain = data_map %>% pull(n), bins = c(0,500,1000,2000,3000,4000)                   # Domain of labels: nb medals
      )
      
      
      labels <- sprintf(                               # Below we define the labels
        "<strong>%s</strong><br/>%g Designation",                            # Adding text to label
        data_map$Country,                                               # We show the country name...
        data_map$n                                                      # ... and the nb medals
      ) %>% lapply(htmltools::HTML)                                       # Embedded all into html language
      
      data_map %>% 
        data.frame() %>%                                # Turn into dataframe (technical)
        sf::st_sf() %>%                                 # Format in sf
        st_transform("+init=epsg:4326") %>%             # Convert in particular coordinate reference 
        leaflet() %>%                                   # Call leaflet
        setView(lng = 3, lat = 55,zoom = 1.45)%>%                                  # Call leaflet
        addPolygons(fillColor = ~palet(n),        # Create the map (colored polygons)
                    weight = 2,                         # Width of separation line
                    opacity = 1,                        # Opacity of separation line
                    color = "white",                    # Color of separation line
                    dashArray = "3",                    # Dash size of separation line
                    fillOpacity = 0.7,                  # Opacity of polygon colors
                    highlight = highlightOptions(       # 5 lines below control the cursor impact
                      weight = 2,                       # Width of line
                      color = "#CBCBCB",                # Color of line
                      dashArray = "",                   # No dash
                      fillOpacity = 0.7,                # Opacity
                      bringToFront = TRUE),
                    label = labels,                     # LABEL! Defined above!
                    labelOptions = labelOptions(        # Label options below...
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        addLegend(pal = palet,                    # Legend: comes from palet colors defined above
                  values = ~n,                    # Values come from lifeExp variable
                  opacity = 0.7,                  # Opacity of legend
                  title = "Number of Designation",           # Title of legend
                  position = "bottomright")       # Position of legend
  })
  
  
  
  
  ####following for fourth tab - grape variety    
  data1 <- reactive({                # Creates the dynamic data for all to use
    wine= subset(wine, select = - c(Region_1,Description))
    wine %>%                  # Filter Points & Price_quartile
      filter(Points >= input$points[1], 
             Points <= input$points[2],
             Price_quartile %in% input$price_quartile) 
  })
  grape<- reactive({
    data()%>% 
      group_by(Country, .drop = FALSE) %>%
      summarise(n=n_distinct(Variety))%>%
      arrange(desc(n))
  })
  
  output$grape <- renderPlotly({
    grape() %>% filter(Country %in% filter_country())%>%
      group_by(Country) %>%
      # count() %>%
      ggplot(aes(y=reorder(Country,n), x = n, fill =Country)) + geom_col(width = 0.6)+
      scale_fill_brewer(palette ="BuPu") +
      geom_text(aes(label=n), vjust=2, size=3)+
      xlab("Number of Grape Variety") +  ylab("Country")+ theme_minimal()+ 
      theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12) ) 
  })
  
  grape1<- reactive({
    data1()%>% 
      group_by(Variety, .drop = FALSE) %>%
      summarise(nb=n())%>%
      arrange(desc(nb))
  })
  
  output$table_grape <- DT::renderDataTable({grape1()}) # Create the output object of country table!
  
  
  ####following for fifth tab - DATA 
  ####### for server
  data_data <- reactive({                # Creates the dynamic data for all to use
    wine= subset(wine, select = - c(Region_1,Designation))
    wine %>%                  # Filter Points & Price_quartile
      filter(Points >= input$points[1], 
             Points <= input$points[2],
             Price_quartile %in% input$price_quartile) 
  })
  
  output$table_whole <- DT::renderDataTable({ data_data()}) # Create the output object of country table!
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
