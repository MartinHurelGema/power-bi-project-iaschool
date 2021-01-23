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
                   tabPanel("Alimentation",
                            h1('Analyse de la disponibilitée alimentaire'),
                            h1('Proportion des céréales pour l’alimentation animale'),
                            sidebarLayout(
                                
                                sidebarPanel = selectInput("Area", "Area",
                                                           choices = 
                                                               c("All", as.vector(disp_alim_per_item %>% select(Area) %>% distinct()) ),multiple = TRUE),
                                mainPanel = mainPanel(
                                    plotOutput('PieChart')
                                )    
                            ),
                            h1('chifres clés du dataset pour la disponibilitée alimentaire par item et par pays'),
                            sidebarLayout(
                                sidebarPanel = selectInput("Area", "Area",
                                                           choices = 
                                                               c("All", as.vector(disp_alim_per_item %>% select(Area) %>% distinct()) ),multiple = TRUE),
                                mainPanel = tableOutput('tbl')
                            )
                            ),
                   tabPanel("tab 3",img(src="www/cute_cats.jpg")),
                   tabPanel("tab 4",img(src="www/funny_cats.jpg"))
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
