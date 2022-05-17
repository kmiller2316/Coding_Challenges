#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
# devtools::install_github("bdilday/GeomMLBStadiums") 
library(GeomMLBStadiums)
tags <- read.csv("tag_ups.csv", header=T)
FINAL <- readRDS("model.rds")
landing_x <- readRDS("landing_x.rds")
landing_y <- readRDS("landing_y.rds")
ball_vx <- readRDS("ball_vx.rds")
ball_vy <- readRDS("ball_vy.rds")
ball_vz <- readRDS("ball_vz.rds")
ball_v <- readRDS("ball_v.rds")
ball_ax <- readRDS("ball_ax.rds")
ball_ay <- readRDS("ball_ay.rds")
ball_az <- readRDS("ball_az.rds")
ball_apex_x <- readRDS("ball_apex_x.rds")
ball_apex_y <- readRDS("ball_apex_y.rds")
ball_apex_z <- readRDS("ball_apex_z.rds")
time_catch <- readRDS("time_to_catch.rds")
throw_dist <- readRDS("throw_dist.rds")
positions <- data.frame("player_px_7_0" = mean(tags$player_px_7_0), 
                        "player_py_7_0" = mean(tags$player_py_7_0), 
                        "player_px_8_0" = mean(tags$player_px_8_0), 
                        "player_py_8_0" = mean(tags$player_py_8_0), 
                        "player_px_9_0" = mean(tags$player_px_9_0), 
                        "player_py_9_0" = mean(tags$player_py_9_0))
runner_speed <- aggregate(runner_season_top_velo~runner_id, data=tags, FUN=max)
fielder_position <- aggregate(fielder_pos~fielder_id, data=tags, FUN=function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x,ux)))]
}) # generates most common fielder position from dataset (will assume this to be primary position)
for (i in 1:nrow(fielder_position)) {
    player <- fielder_position[i,]
    sub <- subset(tags, fielder_id == player$fielder_id & fielder_pos == player$fielder_pos)
    cols <- grep(player$fielder_pos, colnames(tags))
    starting_x <- mean(sub[,cols[1]])
    starting_y <- mean(sub[,cols[2]])
    fielder_position$starting_x[i] <- starting_x
    fielder_position$starting_y[i] <- starting_y
    fielder_position$throw[i] <- mean(sub[,"throw"])
    fielder_position$exchange[i] <- mean(sub[,"exchange"])
} # finds average starting position and attributes for each fielder at their primary position

preds <- predict(FINAL,tags)
probs <- predict(FINAL, type="prob")
grades <- cbind(tags, preds)
grades$correct <-  ifelse(grades$runs_on_play == grades$preds, "C", "NC")
grades$allowT <- ifelse(grades$runs_on_play=="run",1,0)
grades$allowP <- ifelse(grades$preds=="run",1,0)
fielderT <- aggregate(allowT~fielder_id, data=grades, FUN=mean)
fielderP <- aggregate(allowP~fielder_id, data=grades, FUN=mean)

red_yellow <- quantile(probs$run, 0.25)
yellow_green <- quantile(probs$run, 0.75)

fielders <- merge(fielderT,fielderP, by="fielder_id")
fielders$grade <- ifelse(fielders$allowT < red_yellow, "red", ifelse(fielders$allowT > yellow_green, "green", "yellow"))
average_run <- mean(probs$run)
sd_run <- sd(probs$run)

fielders$scale<-ifelse(fielders$allowT >= average_run+3*sd_run, "20",
                       ifelse(fielders$allowT >= average_run+2.5*sd_run, "25",
                              ifelse(fielders$allowT >= average_run+2*sd_run, "30",
                                     ifelse(fielders$allowT >= average_run+1.5*sd_run,"35",
                                            ifelse(fielders$allowT >= average_run+1*sd_run, "40",
                                                   ifelse(fielders$allowT >= average_run+0.5*sd_run,"45",
                                                          ifelse(fielders$allowT >= average_run, "50",
                                                                 ifelse(fielders$allowT >= average_run-0.5*sd_run, "55",
                                                                        ifelse(fielders$allowT >= average_run-1*sd_run, "60",
                                                                               ifelse(fielders$allowT >= average_run-1.5*sd_run, "65",
                                                                                      ifelse(fielders$allowT >= average_run-2*sd_run, "70",
                                                                                             ifelse(fielders$allowT >= average_run-2.5*sd_run,"75","80"
                                                                                             ))))))))))))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title="Tag Up Central", titleWidth = 450),
    dashboardSidebar(disable = T),
    dashboardBody(
        tabsetPanel(
            tabPanel(
                title = "Red Rover",
                icon = icon("running"),
                # add in drop downs for the user inputs for go/no-go decision
                flowLayout(
                    fluidRow(
                    column(width = 12,
                           selectInput(inputId = "fielder",
                                       label = "Fielder:",
                                       choices = c("2"),
                                       width = "40%")
                           )
                ),
                
                fluidRow(
                    column(width = 12,
                           selectInput(inputId = "runner",
                                       label = "Runner:",
                                       choices = c("1"),
                                       width = "40%")
                           )
                ),
                
                fluidRow(
                    column(width = 12,
                           numericInput(inputId = "inning",
                                        label = "Inning:",
                                        value = 7,
                                        min = 0,
                                        max = 9,
                                        step = 1,
                                        width = "40%")
                           )
                ),
                
                fluidRow(
                    column(width = 12,
                           numericInput(inputId = "run_diff",
                                        label = "Batting Run Differential:",
                                        value = 0,
                                        min = -19,
                                        max = 19,
                                        step = 1,
                                        width = "40%")
                    )
                ),
                
                fluidRow(
                    column(width = 12,
                           sliderInput(inputId = "launch_angle",
                                       label = "Lauch Angle:",
                                       min = round(min(tags$launch_angle),0),
                                       max = round(max(tags$launch_angle),0),
                                       value = round(mean(tags$launch_angle),0),
                                       width = "75%")
                           )
                ),
                
                fluidRow(
                    column(width = 12,
                           sliderInput(inputId = "launch_direction",
                                       label = "Lauch Direction:",
                                       min = round(min(tags$launch_direction),0),
                                       max = round(max(tags$launch_direction),0),
                                       value = round(mean(tags$launch_direction),0),
                                       width = "75%")
                    )
                ),
                
                fluidRow(
                    column(width = 12,
                           sliderInput(inputId = "launch_speed",
                                       label = "Lauch Speed:",
                                       min = round(min(tags$launch_speed),0),
                                       max = round(max(tags$launch_speed),0),
                                       value = round(mean(tags$launch_speed),0),
                                       width = "75%")
                    )
                )),

                fluidRow(
                    column(width = 12,
                           actionButton(inputId = "go", label = "Update")
                           )
                ),
                
                fluidRow(
                    box(textOutput(outputId = "Go_NoGo") %>% withSpinner(color="#0dc5c1", hide.ui = F))
                )
            ),
            
            tabPanel(
                title = "Stop Light",
                icon = icon("baseball-ball"),
                # add in fielder choice and plot
                
                fluidRow(
                    column(width = 12,
                           selectInput(inputId = "outfielder",
                                       label = "Select Outfielder",
                                       choices = c("2"),
                                       width = "35%")
                           )
                ),
                
                fluidRow(
                    box(plotOutput(outputId = "plays"))
                ),
                
                fluidRow(
                    box(htmlOutput(outputId = "light"))
                )
            )
        )
    )
)


server <- function(input, output, session) {

    # Tab for Decision
    observe({
        updateSelectInput(
            session = session,
            inputId = "fielder",
            choices = sort(unique(fielder_position$fielder_id)),
            selected = sort(unique(fielder_position$fielder_id))[1]
        )
    })
    
    observe({
        updateSelectInput(
            session = session,
            inputId = "runner",
            choices = sort(unique(runner_speed$runner_id)),
            selected = sort(unique(runner_speed$runner_id))[1]
        )
    })
    
    # reactive expression
    text_reactive <- eventReactive(input$go, {
        fielder_info <- fielder_position %>%
            filter(fielder_id == input$fielder)
        
        runner_info <- runner_speed %>%
            filter(runner_id == input$runner)
        
        df <- cbind(runner_info, fielder_info)
        colnames(df)[5:6] <- c(paste("player_px_",df$fielder_pos[1],"_0",sep=""), paste("player_py_",df$fielder_pos[1],"_0",sep=""))
        for (i in 1:ncol(positions)) {
            loop_i <-  colnames(positions)[i]
            if (loop_i %in% colnames(df)) {next} else {df[,loop_i] <- positions[,i]}
        }
        df <- df %>%
            mutate(inning = input$inning) %>%
            mutate(batting_run_differential = input$run_diff) %>%
            mutate(launch_angle = input$launch_angle) %>%
            mutate(launch_direction = input$launch_direction) %>%
            mutate(launch_speed = input$launch_speed)
        df <- df %>%
            mutate(landing_location_x = predict(landing_x,df)) %>%
            mutate(landing_location_y = predict(landing_y,df)) %>%
            mutate(ball_vx_0 = predict(ball_vx,df)) %>%
            mutate(ball_vz_0 = predict(ball_vz,df)) %>%
            mutate(ball_vy_0 = predict(ball_vy,df)) %>%
            mutate(ball_v_0 = predict(ball_v,df)) %>%
            mutate(ball_ax_0 = predict(ball_ax,df)) %>%
            mutate(ball_az_0 = predict(ball_az,df)) %>%
            mutate(ball_ay_0 = predict(ball_ay,df)) %>%
            mutate(ball_apex_x = predict(ball_apex_x,df)) %>%
            mutate(ball_apex_y = predict(ball_apex_y,df)) %>%
            mutate(ball_apex_z = predict(ball_apex_z,df)) 
        df <- df %>%
            mutate(throw_dist = predict(throw_dist,df)) %>%
            mutate(time_to_catch = predict(time_catch,df)) %>%
            mutate(runs_on_play = NULL) %>%
            as.data.frame()
        
        for (i in 1:nrow(df)) {
            cols <- grep(df$fielder_pos[i],colnames(df))
            fielder_x <- df[i,18] - df[i,cols[1]]
            fielder_y <- df[i,19] - df[i,cols[2]]
            df$fielder_distance_x[i] <- fielder_x
            df$fielder_distance_y[i] <- fielder_y
        }
        
        p <- predict(FINAL,df)
        if (p == "run") {decision <- "Go"} else {decision <- "No Go"}
        paste("The provided situation recommends a", decision, "decision.")
    })
    
    output$Go_NoGo <- renderText({
        text_reactive()
    })
    
    
    # Tab for Grades
    observe({
        updateSelectInput(
            session = session,
            inputId = "outfielder",
            choices = sort(unique(fielders$fielder_id)),
            selected = sort(unique(fielders$fielder_id))[1]
        )
    })
    
    output$plays <- renderPlot({
        batted_ball_result <- tags[,c("outs_on_play","runs_on_play","landing_location_x","landing_location_y","runner_id","fielder_id")] %>%
            filter(fielder_id == input$outfielder)
        fig <- batted_ball_result %>%
            ggplot(aes(x=landing_location_x, y=landing_location_y, color=runs_on_play)) +
            geom_spraychart(stadium_ids = "royals",
                            stadium_transform_coords = TRUE,
                            stadium_segments = "all") +
            theme_void() +
            coord_fixed() +
            ggtitle(paste("Balls Fielded by Outfielder",input$outfielder)) +
            theme(plot.title = element_text(hjust = 0.5))
        fig
    })
    
    output$light <- renderUI({
        of <- fielders %>%
            filter(fielder_id == input$outfielder)
        light <- of$grade
        scale <- of$scale
        str1 <- paste("Outfielder", input$outfielder, "is a", light, "light.")
        str2 <- paste("Outfielder", input$outfielder, "grades at a", scale, "at preventing runners from scoring.")
        HTML(paste(str1,str2,sep="<br/>"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
