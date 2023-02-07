library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(
    title = "Personalized Song Lyric Assistant",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("GET FROM WEB", tabName = "search", icon = icon("search")),
      menuItem("BROWSE COLLECTION", tabName = "browse", icon = icon("music")),
      menuItem("MY ARTISTS", tabName = "profile", icon = icon("user")),
      menuItem("TOP 10", tabName = "top", icon = icon("star")),
      menuItem("WORD CLOUD", tabName = "cloud", icon = icon("fa fa-cloud"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "search",
              fluidRow( 
                box(width=8, align = "center",
                    #Search Bar
                    fluidRow(
                      column(8,
                             textInput(inputId = "txtArtist",
                                       label = h4("Search by Artist"), 
                                       value = ""),
                             tags$style(type='text/css', "#txtArtist { width: 300px; }")
                      ),
                      br(),
                      br(),
                      column(4,
                             
                             #Search Button
                             
                             actionButton(inputId = "btnSearch", 
                                          label = "Search")
                      ))                   
                )
              ),
              
              fluidRow(
                box(width=12,
                    uiOutput("result")
                )),
              
              fluidRow(
                box(width=12,
                    uiOutput("albums"),
                    actionButton(inputId = "btnShowAll", 
                                 label = "Show All Albums")
                )),
              
              fluidRow(
                box(width=12,
                    
                    list(uiOutput("songs"),
                         actionButton(inputId = "btnSelectAll", 
                                      label = "Select All"),
                         actionButton(inputId = "btnDeselectAll", 
                                      label = "Deselect All"),
                         actionButton(inputId = "btnAdd2", 
                                      label = "Add to Collection")
                         
                    )
                )
              ),
              
              #-------------------------------------------------------------------
              
              fluidRow(
                box(width=12,
                    
                    list(uiOutput("showAllAlbums"),
                         uiOutput("ShowSongs"),
                         actionButton(inputId = "btnSelectAll3", 
                                      label = "Select All"),
                         actionButton(inputId = "btnDeselectAll3", 
                                      label = "Deselect All"),
                         actionButton(inputId = "btnAdd3", 
                                      label = "Add to Collection")
                    ))
              )
              
      ),
      
      tabItem(tabName = "browse",
              fluidRow(align = "center",
                       
                       h3("Collection", align = "center"),
                       actionButton(inputId = "btnRefresh2", 
                                    label = "Refresh Collection"),
                       br(),
                       actionButton(inputId = "btnDelete2", 
                                    label = "Delete"),
                       br(),
                       br(),
                       box(width = 12, 
                           DT::dataTableOutput('table')
                       ),
                       box(width = 6,
                           verbatimTextOutput('emotion')
                       ),
                       box(width = 6,
                           actionButton(inputId = "btnSearchRelated",
                                        label = "Search Related Song")
                       ),
                       box(width = 12,
                           verbatimTextOutput('related')
                       ),
                       box(width = 12,
                           verbatimTextOutput('lyrics')
                       )
                       
                       
              )
              
      ),
      tabItem(tabName = "profile",
              fluidRow(align = "center",
                       column(12,
                              h3("Artists' Profile", align = "center"),
                              actionButton(inputId = "btnRefresh1", 
                                           label = "Refresh Artists"),
                              br(),
                              br(),
                              
                              box(width=6,
                                  h4("List of Artists"), 
                                  DT::dataTableOutput('table1')
                                  
                                  
                              ),
                              
                              box(width=6,
                                  imageOutput('artistImage'),
                                  verbatimTextOutput('info')
                                  
                              )
                       )
                       
              )
      ),
      
      
      
      tabItem(tabName = "top",
              fluidRow(
                
                
                
                h3("TOP 10", align = "center"),
                br(),
                
                box(width=12,
                    selectInput("select_from", "From", c("Web" = "web",
                                                         "Collection" = "collection"), multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, selected = "web"),
                    conditionalPanel(
                      condition = "input.select_from == 'collection'"
                      
                    ),
                    conditionalPanel(
                      condition = "input.select_from == 'web'",
                      selectInput("select_genre", "Genre", c("All" = "all",
                                                             "Acoustic" = "acoustic",
                                                             "Adult Contemporary" = "adult-contemporary",
                                                             "African" = "african",
                                                             "Alternative" = "alternative",
                                                             "Avant-Garde" = "avant-garde",
                                                             "Blues" = "blues",
                                                             "Children's Music" = "childrens-music",
                                                             "Christian" = "christian",
                                                             "Classical" = "classical",
                                                             "Comedy" = "comedy",
                                                             "Country" = "country-music",
                                                             "Dance" = "dance",
                                                             "Electronic" = "electronic",
                                                             "Folk" = "folk",
                                                             "Funk" = "funk",
                                                             "Hip Hop/Rap" = "hip-hop-rap",
                                                             "Holiday" = "holiday",
                                                             "Instrumental" = "instrumental",
                                                             "Jazz" = "jazz",
                                                             "Latin" = "latin",
                                                             "Musical" = "musical",
                                                             "New Age" = "new-age",
                                                             "Oldies" = "oldies",
                                                             "Pop" = "pop",
                                                             "R&B" = "r-and-b",
                                                             "Reggae" = "reggae",
                                                             "Rock" = "rock",
                                                             "Ska" = "ska",
                                                             "Soul" = "soul",
                                                             "Soundtrack" = "soundtrack",
                                                             "Vocal" = "vocal",
                                                             "World" = "world"), multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, selected = "all")
                    )
                ),
                
                box(width=6,
                    h4("ARTISTS", align = "center"),
                    
                    uiOutput("topArtists")
                ),
                box(width=6,
                    conditionalPanel(
                      condition = "input.select_from == 'collection'"
                      #                       h4("WORDS", align = "center"),
                      #                       plotOutput("topWords")
                    ),
                    conditionalPanel(
                      condition = "input.select_from == 'web'",
                      h4("SONGS", align = "center"),
                      uiOutput("topSongs")
                    )
                    
                    
                )
                
                
              )
      ),
      tabItem(tabName = "cloud",
              fluidRow(align="center",
                       
                       h3("Word Cloud", align = "center"),
                       actionButton(inputId = "btnShowArtists", 
                                    label = "Show"),
                       br(),
                       br(),
                       
                       box(width=6,
                           h4("List of Artists"), 
                           DT::dataTableOutput('artist_word1')
                           
                       ),
                       
                       box(width=6,
                           h4("List of Artists"), 
                           DT::dataTableOutput('artist_word2')
                           
                       ),
                       
                       br(),
                       box(width =6, plotOutput('word1')
                           
                           
                       ),
                       box(width =6, plotOutput('word2')
                           
                           
                       )
                       
              )
              
              
      )
      
      
      
      
    )
  )
)





