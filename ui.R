#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                   tabPanel("2.1.1 Alimentation - Analyse",
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
                   
                   tabPanel("2.1.2 Alimentation - Liste de céreal",
                            h1('Proportion des céréales pour l’alimentation animale'),
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table2")
                                )    
                                
                            )
                            ),
                   
                   tabPanel("2.2.1 Alimentation - Proportion ",
                            h1('Dispo alimentaire par produit et pays'),
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table3")
                                )    
                                
                            )
                            
                            ),
                   
                   tabPanel("2.2.2 Alimentation - Ration énergie/poids",
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
                                       h2("Graph violon"),
                                       plotOutput('Table20',width = "50%"))
                            )
                   ),
                   tabPanel("2.2.3 Alimentation - Pourcentage proteine de chaque produit",
                            h1("Pourcetage de protèine par produit et par pays"),
                            sidebarLayout(

                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table5")
                                )

                            )
                           
                   ),
                   
                   tabPanel("2.3.1 Les plus calorique",
                            h1("Les 5 aliments parmis les 20 les plus caloriques"),
                            sidebarLayout(

                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table6")
                                )

                            )
                            
                   ),
                   
                   tabPanel("2.3.2 Les plus protéinés",
                            h1("Les 5 aliments parmis les 20 les plus protéinés"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table7")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("2.4.1 Végétaux uniquement",
                            h1("Disponibilité intérieurement mondiale des végétaux en kcal et en kg protéine par année"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table8")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("2.4.2 Tous végétarien",
                            h1("Nombre humain végétarien nourris si toute la disponibilité intérieure mondiale de produits végétaux était utilisée pour de la nourriture"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table9")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("2.4.3 Rien se perd tous se transforme",
                            h1("All to food"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table10")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("3.1 Proportion de la pop en sous-nutrition " ,
                            h1("Population en sous-nutrition"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table11")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("3.2 Liste des pays en sous-nutrition" ,
                            h1("Pays :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table12")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("3.3 Liste des produits les plus exportés " ,
                            h1(""),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table13")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("3.4 Les plus grandes importations ?" ,
                            h1("Les 200 plus grandes importations de ces produits :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table14")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("3.5 Regrouper les importations par produit" ,
                            h1("Ratio 1 :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table15")
                                )    
                                
                            ),
                            
                            h1("Ratio 2:"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table16")
                                )    
                                
                            )
                   ),
                   
                   tabPanel("3.6 Top 3 produits" ,
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
                   
                   tabPanel("3.7 Tonnes de céréales caculé  si les USA diminuaient leur production de produits animaux de 10%",
                            h1("Des céréales pourraient être libérées :"),  
                            sidebarLayout(
                                
                                sidebarPanel = NULL,
                                mainPanel = mainPanel(
                                    tableOutput("Table19")
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
