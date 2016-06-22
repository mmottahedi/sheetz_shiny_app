library(shinydashboard)
library(ggplot2)
# library(repr)
library(rpart)
library(lubridate)
library(reshape)
library(rlist)
library(tidyr)
library(class)
library(party)
library(plotly)
library(dplyr)




source('ggplot_smooth_func.R')
df <- read.csv('df_with_avg_temp.csv')
df$Start.Date <- as.Date(df$Start.Date)
df$End.Date <- as.Date(df$End.Date)
df$OPEN <- as.Date(df$OPEN, format = "%m/%d/%Y")
df$Cost <- (as.numeric(df$Cost))

df1 <- filter(df, NaturalGas==FALSE , car_wash == FALSE, Native.Use != 0, Unit=='kWh')

stores <- as.numeric(unique(df1$Store))


ui <- dashboardPage(
    dashboardHeader(title = "Sheetz"),

    dashboardSidebar(
        menuItem("Store Regression Plot", tabName = "regression", icon = icon("dashboard")),
        menuItem("Bill Cost Regression", tabName = "Cost", icon = icon("dashboard"))),



     dashboardBody(

        tabItems(

            tabItem(tabName = "regression",
                    fluidRow(
                        box(plotOutput("regressionPlot")),

                        box(
                            title = "Store",
                           selectInput(inputId = "store_num", label = "Store number:", choices = stores, selected = 3, multiple = T)),

                        box(
                            title = "Date Range",
                            dateRangeInput(inputId = "date_range", label = 'Date Range' , start = "2014-12-01", end = "2016-06-06"))
                    )),




                    tabItem(tabName = "Cost",
                    fluidRow(
                        box(plotOutput("costplot")),

                        box(
                            title = "Store",
                            selectInput(inputId = "cost_store_num", label = "Store number:", choices = stores, selected = 3, multiple = T)),

                        box(
                            title = "Date Range",
                            dateRangeInput(inputId = "cost_date_range", label = 'Date Range' , start = "2014-12-01", end = "2016-06-06"))
                    )

                    )
        )
    )
)



server <- function(input, output){

    output$regressionPlot <- renderPlot({
    store <- as.numeric(input$store_num)
    date_range <- input$date_range
    temp <- df %>%
                filter(Store %in% store,
                       Start.Date > date_range[1] & Start.Date < date_range[2],
                       NaturalGas==FALSE , car_wash == FALSE, Native.Use != 0, Unit=='kWh') %>%
                group_by(Store) %>%
                mutate(monthly_eui  = Native.Use / (Days * Size )) %>%
                mutate(OPEN= year(OPEN)) %>%
                mutate(month= month(Start.Date)) %>%
                mutate(year= month(Start.Date)) %>%
                arrange(desc(Start.Date))



    p <- ggplot(data = temp, aes(x=temp_mean, y=monthly_eui, color = factor(Store)))+
                                geom_point(alpha = 0.7) +
                                facet_wrap(~Group) +
                                theme(legend.position = 'bottom') +
                                geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se=F) +
                                stat_smooth_func(geom = "text",method="lm",formula = y ~ poly(x ,2),hjust=0, vjust=0, parse=T) +
                                ylab("Monthly Average (kWh)") + xlab("Average Temp During Billing Period (C)")
    print(p)
    })




    output$costplot <- renderPlot({
        store <- as.numeric(input$cost_store_num)
        date_range <- input$cost_date_range
        temp <- df %>%
            filter(Store %in% store,
                   Start.Date > date_range[1] & Start.Date < date_range[2],
                   NaturalGas==FALSE , car_wash == FALSE, Native.Use != 0, Unit=='kWh') %>%
            group_by(Store) %>%
            arrange(desc(Start.Date))



        p <- ggplot(data = temp, aes(x=temp_mean, y=Cost, color = factor(Store)))+
            geom_point(alpha = 0.7) +
            facet_wrap(~Group) +
            theme(legend.position = 'bottom') +
            geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se=F) +
            stat_smooth_func(geom = "text",method="lm",formula = y ~ poly(x ,2),hjust=0, vjust=0, parse=T) +
            ylab("Cost ($)") + xlab("Average Temp During Billing Period (C)")
        print(p)
    })
}

shinyApp(ui, server)