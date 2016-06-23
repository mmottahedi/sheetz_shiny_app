#preprocessing stuff---------------------
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
df1 <- filter(df, NaturalGas == FALSE , car_wash == FALSE, Native.Use != 0, Unit == 'kWh')
stores <- as.numeric(unique(df1$Store))

#UI funtion----------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Sheetz"),
    dashboardSidebar(
        menuItem("Store Regression Plot", tabName = "regression", icon = icon("dashboard")),
        menuItem("Bill Cost Regression", tabName = "Cost", icon = icon("dashboard")),
        menuItem("PCA Analysis on Annual Native Use", tabName = "pca_native", icon = icon("dashboard")),
        menuItem("PCA and K-means Clustering on Annual kWh", tabName = "kmeans", icon = icon("dashboard"))),

#dashboard body------------------------
     dashboardBody(

#tabItems------------------------------
        tabItems(

#Regression on monthly EUI tab----------
             tabItem(tabName = "regression",
                    fluidRow(
                        box(plotOutput("regressionPlot")),
                        box(title = "Store",
                           selectInput(inputId = "store_num",
                                       label = "Store number:",
                                       choices = stores, selected = 3,
                                       multiple = T)),
                        box(title = "Date Range",
                            dateRangeInput(inputId = "date_range",
                                           label = 'Date Range' ,
                                           start = "2014-12-01",
                                           end = "2016-06-06")))),

#Regression on monthly bill cost tab-----------------------------------
            tabItem(tabName = "Cost",
                fluidRow(
                    box(plotOutput("costplot")),
                    box(title = "Store",
                        selectInput(inputId = "cost_store_num",
                                    label = "Store number:",
                                    choices = stores, selected = 3,
                                    multiple = T)),
                    box(title = "Date Range",
                        dateRangeInput(inputId = "cost_date_range",
                                       label = 'Date Range' ,
                                       start = "2014-12-01",
                                       end = "2016-06-06")))),

#PCA Native USE TAB--------------------------------------------------
            tabItem(tabName = "pca_native",
                    fluidRow(
                        box(plotOutput("pca_cost")),
                        box(title = 'Color code',
                            selectInput(inputId = "select_color",
                                        label = "column",
                                        choices = c('df2$Group', 'df2$Store'))))),

#PCA & kmeans tab---------------------------------
            tabItem(tabName = "kmeans",
                    fluidRow(
                        tabBox(title = "X", id = "tab1",
                               tabPanel("Annual kWh",
                                        selectInput(inputId = "kmeans_num_1",
                                                    label = "Number of Clusters:",
                                                    choices = c(1,2,3)),
                                        plotOutput("kwh_kmeans"),
                                        plotOutput("sum_stat_1"),
                                        plotOutput("sum_stat_2"),
                                        plotOutput("sum_stat_3")),
                               tabPanel("Annual Cost",
                                        selectInput(inputId = "kmeans_num_2",
                                                    label = "Number of Clusters:",
                                                    choices = c(1,2,3)),
                                        plotOutput("cost_kmeans"),
                                        plotOutput("sum_stat_4"),
                                        plotOutput("sum_stat_5"),
                                        plotOutput("sum_stat_6")
                               ))))
                )
    )
)

#Server function-------------------------
server <- function(input, output){

#EUI regression server----------------------------
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

#Cost regression sever------------------------------
    output$costplot <- renderPlot({
        store <- as.numeric(input$cost_store_num)
        date_range <- input$cost_date_range
        temp <- df %>%
            filter(Store %in% store,
                   Start.Date > date_range[1] & Start.Date < date_range[2],
                   NaturalGas==FALSE , car_wash == FALSE, Native.Use != 0,
                   Unit=='kWh') %>%
            group_by(Store) %>%
            arrange(desc(Start.Date))



        p <- ggplot(data = temp, aes(x = temp_mean, y = Cost,
                                     color = factor(Store))) +
            geom_point(alpha = 0.7) +
            facet_wrap(~Group) +
            theme(legend.position = 'bottom') +
            geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = F) +
            stat_smooth_func(geom = "text",method = "lm",
                             formula = y ~ poly(x ,2),
                             hjust = 0, vjust = 0, parse = T) +
            ylab("Cost ($)") + xlab("Average Temp During Billing Period (C)")
        print(p)})

#PCA on native use server part-------------------
    output$pca_cost <- renderPlot({
        temp <- df %>%
            filter( car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = factor(month(Start.Date),ordered = F)) %>%
            mutate(year = factor(year(Start.Date),ordered = F))

        temp1 <-  temp[c("Store","Group", "Native.Use", "Size", "month",
                         "year", "open", "temp_mean")]

        temp1 <- na.exclude(temp1)

        df1 <- temp1[c("Native.Use", "Size", "month", "year", "open",
                       "temp_mean")]
        df2 <- temp1[c('Group', "Store")]
        head(df1)
        df1$month <- as.numeric(df1$month)
        df1$year <- as.numeric(df1$year)

        pca <- prcomp(df1, center = TRUE,
                      scale. = TRUE)

        pca_x <- predict(pca, df1)
        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x, df2$Group, df2$Store)

        names(pca_x)
        df2$Store <- as.factor(df2$Store)

        p <- ggplot(pca_x, aes_string(x='PC2', y= 'PC1', color =input$select_color)) + geom_point(alpha=0.7) +
            theme(legend.position="none", legend.text = element_text(colour="blue", size = 8))
        print(p)})

#PCA & kmean clustering tab-------------------------
    output$kwh_kmeans <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(pca_x, aes(x = PC2, y = PC1, label = df4$Store,
                               color = factor(class))) +
            geom_text(alpha = 0.7) +
            theme(legend.position = "top",
                  legend.text = element_text(colour = "blue", size = 8))
        print(p)
    })

#summary statistics plot------------------------------
    output$sum_stat_1 <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(df4, aes(x = ann_native_use)) + geom_histogram(bins = 10) + facet_grid(.~class)
        print(p)
    })

#summary statistics plot------------------------------
    output$sum_stat_2 <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(df4, aes(x = size)) + geom_histogram(bins=10) + facet_grid(.~class)
        print(p)
    })

#summary statistics plot------------------------------
    output$sum_stat_3 <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(df4, aes(x = open)) + geom_histogram(bins=10) + facet_grid(.~class)
        print(p)
    })


#PCA & kmean clustering tab for cost------------------------
    output$cost_kmeans <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(annual_cost = mean(Cost),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(Cost, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_2,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(pca_x, aes(x = PC2, y = PC1, label = df4$Store,
                               color = factor(class))) +
            geom_text(alpha = 0.7) +
            theme(legend.position = "top",
                  legend.text = element_text(colour = "blue", size = 8))
        print(p)
    })

#summary statistics plot------------------------------
    output$sum_stat_1 <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(df4, aes(x = ann_native_use)) + geom_histogram(bins = 10) + facet_grid(.~class)
        print(p)
    })

#summary statistics plot------------------------------
    output$sum_stat_2 <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(df4, aes(x = size)) + geom_histogram(bins=10) + facet_grid(.~class)
        print(p)
    })

#summary statistics plot------------------------------
    output$sum_stat_3 <- renderPlot({
        df <- na.exclude(df)
        df %>%
            filter(car_wash == F, Native.Use != 0, Unit == 'kWh') %>%
            mutate(open = year(OPEN)) %>%
            mutate(month = month(Start.Date)) %>%
            mutate(year = year(Start.Date)) %>%
            group_by(Store) %>%
            summarize(ann_native_use = mean(Native.Use),
                      temp_mean = median(temp_mean),
                      size = median(Size),year = median(year),
                      open = median(open)) -> df3
        df4 <- df3 %>% select(ann_native_use, temp_mean, size, year, open )
        df5 <- df %>% select(Group, Store)


        pca <- prcomp(df4, center = TRUE,
                      scale. = TRUE)
        pca_x <- predict(pca, df4)

        for (i in 1:dim(df)[1]) {
            store <- df$Store[i]
            group <- df$Group[i]

            df4$Store[df3$Store == store] <- store
            df4$Group[df3$Store == store] <- group
        }

        pca_x <- as.data.frame(pca_x)
        pca_x <- cbind(pca_x , df4$Group, df4$Store)

        km <- kmeans(subset(pca_x, select = c(PC1, PC2)), input$kmeans_num_1,
                     iter.max = 100, nstart = 10)

        pca_x$class <- km$cluster
        df4$class <- km$cluster

        p <- ggplot(df4, aes(x = open)) + geom_histogram(bins=10) + facet_grid(.~class)
        print(p)
    })
}

shinyApp(ui, server)
