#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinyWidgets)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Projet FAO"),
    wellPanel(
        navbarPage("Menu",
                   tabPanel("Population",
                            h1('Analyse de la population mondiale'),
                            sidebarLayout(
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    plotOutput("LinePlot1")
                                )
                            )),
                   #2.1.1
                   tabPanel("Alimentation - Analyse",
                            h1('Analyse de la disponibilitée alimentaire'),
                           
                            sidebarLayout(
                                
                                sidebarPanel = selectInput("Area", 
                                                           "Area",
                                                           choices = c("Choose",as.vector(countryInit %>% distinct())),
                                                           multiple = FALSE, 
                                                           ),
                                
                                mainPanel(
                                    tableOutput("Table1")
                                         )    
                                
                            )
                            ),
                   #2.1.2
                   tabPanel("Alimentation - Liste de céreal",
                            # h1('Proportion des céréales pour l’alimentation animale'),
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table2")
                            #     )    
                            #     
                            # )
                            
                            fluidRow(
                                column(width = 4,
                                       h2("Proportion des céréales pour l’alimentation animale"),
                                       tableOutput("Table2")),
                                column(width = 8,
                                       h2(""),
                                       plotOutput('PieChart',width = "50%"))
                            )
                            ),
                   #2.2.1
                   # tabPanel("Alimentation - Proportion ",
                   #          h1('Dispo alimentaire par produit et pays'),
                   #          sidebarLayout(
                   #              
                   #              sidebarPanel = NULL,
                   #              mainPanel = mainPanel(
                   #                  tableOutput("Table3")
                   #              )    
                   #              
                   #          )
                   #          
                   #          ),
                   #2.2.2
                   tabPanel("Alimentation - Ration énergie/poids",
                            # h1('Ratio énergie poids par produit en kcal'),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table4")
                            #     )    
                            #     
                            # )
                            fluidRow(
                                column(width = 4,
                                       h2("Ratio énergie poids par produit en kcal"),
                                       tableOutput("Table4")),
                                column(width = 8,
                                       h2(""),
                                       plotOutput('Table20',width = "50%"))
                            )
                   ),
                   #2.2.3
                   tabPanel("Alimentation - Pourcentage proteine de chaque produit",
                            h1("Pourcentage de protèine par produit"),
                            sidebarLayout(

                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table5")
                                )

                            )
                           
                   ),
                   #2.3.1
                   tabPanel("Les plus calorique",
                            # h1("Les 5 aliments parmis les 20 les plus caloriques"),
                            # sidebarLayout(
                            # 
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table6")
                            #     )
                            # 
                            # )
                            fluidRow(
                                column(width = 4,
                                       h2("Ratio énergie poids par produit en kcal"),
                                       tableOutput("Table6")),
                                column(width = 8,
                                       h2(""),
                                       plotOutput('barChartCal',width = "50%"))
                            )
                            
                   ),
                   #2.3.2
                   tabPanel("Les plus protéinés",
                            # h1("Les 5 aliments parmis les 20 les plus protéinés"),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table7")
                            #     )    
                            #     
                            # )
                            
                            fluidRow(
                                column(width = 4,
                                       h2("Les 5 aliments parmis les 20 les plus protéinés"),
                                       tableOutput("Table7")),
                                column(width = 8,
                                       h2(""),
                                       plotOutput('barChartProt',width = "75%"))
                            )
                   ),
                   #2.4.1
                   tabPanel("Végétaux uniquement",
                            # h1("Disponibilité intérieure mondiale des végétaux en kcal et en kg protéine par année"),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table8")
                            #     )    
                            #     
                            # )
                            fluidRow(
                                column(width = 4,
                                       h2("Disponibilité intérieure mondiale des végétaux en kcal et en kg protéine par année"),
                                       tableOutput("Table8")),
                                column(width = 8,
                                       h2("Disponibilité par année en fonction de calories"),
                                       plotOutput('smooth1',width = "75%")),
                            ),
                            fluidRow(
                                column(width = 4,
                                       h2(""),
                                       tableOutput("")),
                                column(width = 8,
                                       h2("Disponibilité par année en fonction de protéine"),
                                       plotOutput('smooth2',width = "75%")),
                            )
                   ),
                   #2.4.2
                   tabPanel("Tous végétarien",
                            # h1("Nombre humain végétarien nourris si toute la disponibilité intérieure mondiale de produits végétaux était utilisée pour de la nourriture"),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table9")
                            #     )    
                            #     
                            # )
                            fluidRow(
                                column(width = 4,
                                       h2("Nombre humain végétarien nourris si toute la disponibilité intérieure mondiale de produits végétaux était utilisée pour de la nourriture"),
                                       tableOutput("Table9")),
                                column(width = 8,
                                       h2(""),
                                       plotOutput('',width = "75%")),
                            )
                   ),
                   #2.4.3
                   tabPanel("Rien se perd tous se transforme",
                            h1("All to food"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table10")
                                )    
                                
                            )
                   ),
                   #3.1
                   tabPanel("Proportion de la population en sous-nutrition " ,
                            h1("Population en sous-nutrition"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table11")
                                )    
                                
                            )
                   ),
                   #3.2
                   tabPanel("Liste des pays en sous-nutrition" ,
                            # h1("Pays :"),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table12")
                            #     )    
                            #     
                            # )
                            
                            fluidRow(
                                column(width = 4,
                                       h2(" "),
                                       tableOutput("Table12")),
                                column(width = 8,
                                       h2(""),
                                       plotOutput("gdp", width = "95%")),
                            )
                   ),
                   #3.3
                   tabPanel("Liste des produits les plus exportés " ,
                            h1(""),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table13")
                                )    
                                
                            )
                   ),
                   #3.4
                   tabPanel("Les plus grandes importations ?" ,
                            h1("Les 200 plus grandes importations de ces produits :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table14")
                                )    
                                
                            )
                   ),
                   #3.5
                   tabPanel("Regrouper les importations par produit" ,
                            # h1("Ratio 1 :"),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table15")
                            #     )    
                            #     
                            # ),
                            # 
                            # h1("Ratio 2:"),  
                            # sidebarLayout(
                            #     
                            #     sidebarPanel = NULL,
                            #     mainPanel = mainPanel(
                            #         tableOutput("Table16")
                            #     )    
                            #     
                            # )
                            fluidRow(
                                column(width = 4,
                                       h2("Ratio 1 "),
                                       tableOutput("Table15")),
                                column(width = 8,
                                       h2("Ratio 2"),
                                       tableOutput("Table16")),
                            )
                   ),
                   #3.6
                   tabPanel("Top 3 produits" ,
                            h1("Ratio (disp) :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table17")
                                )    
                                
                            ),
                            
                            h1("Ratio (feed) :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table18")
                                )    
                                
                            )
                   ),
                   #3.7
                   # tabPanel("Tonnes de céréales caculé  si les USA diminuaient leur production de produits animaux de 10%",
                   #          h1("Des céréales pourraient être libérées :"),  
                   #          sidebarLayout(
                   #              
                   #              sidebarPanel = NULL,
                   #              mainPanel = mainPanel(
                   #                  tableOutput("Table19")
                   #              )    
                   #          )
                   # )
                   
                   tabPanel("Map - Sous-nutrition" ,
                            h1("Ratio de population en sous-nutrition :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    plotlyOutput("map", width = "100%", height = "100%")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("Map - Production" ,
                            h1("Production par millier de tonne :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    plotlyOutput("map2", width = "100%", height = "100%")
                                )    
                                
                            )
                   )
                  
                    
                   
        )
    )
    # f=reactive({
    #     Sys.sleep(1)
    #     result=paste0("www/",input$A,".png")
    # })
    # output$D <- renderUI({img(src=f())})
    # output$E <- renderUI({p(img(src=f())," ",input$B,",",input$C,"!")})
    
    
    # création de trigger    
    # observeEvent(input$go,{
    #     updateActionButton(session,
    #                        inputId="go",
    #                        label=str_c(str_c(rep("go",input$go+1),
    #                                          collapse=", "),
    #                                    "!")
    #     )
    # })

    
    # ui
    # actionButton("now","Now!"),
    # textOutput("time")
    
    # serveur    
    # reac=eventReactive(input$now,{
    #     as.character(Sys.time())
    # })
    # output$time=renderText({
    #     str_c("The time right now is: ",reac())
    # })
    
    # empécher l'afichage du text jusqu'a se que l'utilisateur rensegne le champ word2
    # output$combi <- renderText({paste(input$word1,isolate(input$word2))})
    # textInput("a","","A"),
    # textOutput("b"),
    # 
    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # )
))
