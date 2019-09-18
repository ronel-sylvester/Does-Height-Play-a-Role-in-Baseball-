library(shiny)
library(ggplot2)

ui <- fluidPage(titlePanel("selectInput"),
                selectInput("graph", "Metric:", choices = c("Assists (Log)", 
                                                            "Steal Percentage (Log)", 
                                                            "Probability of Home Run (Log)",
                                                            "Balls Thrown Percentage (Log)",
                                                            "Bad Throw Percentage (Log)"),
                            selected = "Fielding_Effectiveness"),
                mainPanel(
                  plotOutput("selected_graph")),
                plotOutput("selected_graph_2")
)

server <- function(input, output, session){
  output$selected_graph <- renderPlot({
    if(input$graph == "Assists (Log)") {
      
      ggplot(data = Fielding_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
        geom_bar(stat = "identity", aes(fill = significance)) +
        theme(axis.text.x = element_text(size = 8, angle = 45)) +
        xlab("Teams") + ylab("Number of Wins") + 
        ggtitle("Team Wins in Order of Significance Score of Assists Per Game Metric") +
        annotate("text", x = 5, y = 100, label = "Correlation = 0.251892")
      
    } else if(input$graph == "Steal Percentage (Log)") {
      ggplot(data = Steal_Effectiveness_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
        geom_bar(stat = "identity", aes(fill = significance)) +
        theme(axis.text.x = element_text(size = 8, angle = 45)) +
        xlab("Teams") + ylab("Number of Wins") + 
        ggtitle("Team Wins in Order of Significance Score of Steal Percentage Metric") +
        annotate("text", x = 5, y = 100, label = "Correlation = 0.3450484")
      
    } else if(input$graph == "Probability of Home Run (Log)") {
      ggplot(data = Home_Run_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
        geom_bar(stat = "identity", aes(fill = significance)) +
        theme(axis.text.x = element_text(size = 8, angle = 45)) +
        xlab("Teams") + ylab("Number of Wins") + 
        ggtitle("Team Wins in Order of Significance Score of Probability of Home Run/Game") +
        annotate("text", x = 5, y = 100, label = "Correlation = 0.1292867")
      
    } else if(input$graph == "Balls Thrown Percentage (Log)") {
      ggplot(data = Balls_Percentage_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
        geom_bar(stat = "identity", aes(fill = significance)) +
        theme(axis.text.x = element_text(size = 8, angle = 45)) +
        xlab("Teams") + ylab("Number of Wins") + 
        ggtitle("Team Wins in Order of Significance Score of Percentage of Balls Thrown") +
        annotate("text", x = 5, y = 100, label = "Correlation = 0.003301892")
      
    } else if(input$graph == "Bad Throw Percentage (Log)") {
      ggplot(data = Bad_Throw_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
        geom_bar(stat = "identity", aes(fill = significance)) +
        theme(axis.text.x = element_text(size = 8, angle = 45)) +
        xlab("Teams") + ylab("Number of Wins") + 
        ggtitle("Team Wins in Order of Significance Score of Bad Throw Percentage") +
        annotate("text", x = 4.1, y = 100, label = "Correlation = 0.3938388")
    } 
  })
  
  output$selected_graph_2 <- renderPlot({
    if(input$graph == "Assists (Log)") {
      hist(Fielding_Wrangle$Fielder_Log)
      
    } else if(input$graph == "Steal Percentage (Log)") {
      hist(Steal_Effectiveness_Wrangle$Steal_Effectiveness_Log)
      
    } else if(input$graph == "Probability of Home Run (Log)") {
      hist(Home_Run_Wrangle$Home_Run_Log)
      
    } else if(input$graph == "Balls Thrown Percentage (Log)") {
      hist(Balls_Percentage_Wrangle$Balls_Percentage_Log)
      
    } else if(input$graph == "Bad Throw Percentage (Log)") {
      hist(Bad_Throw_Wrangle$Bad_Throw_Log)
      
    } 
  })
}
shinyApp(ui = ui, server = server)