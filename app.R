Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
#install.packages("MASS", version = "7.3-58.4")

library(shiny)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
# 
ui <- fluidPage(
  titlePanel("NCCU_DS2023_hw4_110971013"),
  navbarPage("政大資訊科學系 110971013 洪明義",
    tabPanel("iris",
      tabsetPanel(
        tabPanel("Plot",
          fluidRow(
            column(4,
              radioButtons("iris_var_x", "請選擇欄位變數x:",choices =  colnames(iris[, 1:4]),selected = "Sepal.Length"),
              radioButtons("iris_var_y", "請選擇欄位變數y:",choices =  colnames(iris[, 1:4]),selected = "Sepal.Width")
            ),
            column(8,plotOutput("irisPlot"))
            )
          ),
          tabPanel("Summary", verbatimTextOutput("irisSummary")), 
                        tabPanel("Table", tableOutput("irisTable"))
                      )       
             ),
             tabPanel("PCA",
                      tabsetPanel(
                        tabPanel("Plot",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("PCs_1", "請選擇主成分1:",
                                                  choices = c("PC1", "PC2", "PC3", "PC4"),
                                                  selected =c("PC1")),
                                     radioButtons("PCs_2", "請選擇主成分2:",
                                                  choices = c("PC1", "PC2", "PC3", "PC4"),
                                                  selected =c("PC2"))
                                   ),
                                   mainPanel(plotOutput("pcaPlot")))
                        ),
                        tabPanel("Summary", verbatimTextOutput("pcaSummary")),
                        tabPanel("Table", tableOutput("pcaTable"))
                      )
             ),
             tabPanel("CA",
                      tabsetPanel(
                        tabPanel("Plot",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("caDims_1", "請選擇維度1:",
                                                  choices = c("Dim1", "Dim2", "Dim3"),
                                                  selected =c("Dim1")),
                                     radioButtons("caDims_2", "請選擇維度2:",
                                                  choices = c("Dim1", "Dim2", "Dim3"),
                                                  selected =c("Dim2"))
                                   ),
                                   mainPanel(
                                     plotOutput("caPlot")
                                   )
                                 )
                        ),
                        tabPanel("Summary", verbatimTextOutput("caSummary")),
                        tabPanel("Table", tableOutput("caTable"))
                      )
             )
             
  )
)

#---=== iris data  ===---
server <- function(input, output) {
  options(encoding = "UTF-8")
    # iris data -> Plot
    output$irisPlot <- renderPlot({
        req(input$iris_var_x,input$iris_var_y)
        if(input$iris_var_x ==input$iris_var_y){
          showNotification("x,y不能選同一個變數")
          return(NULL)
        }
        
        data(iris)
        x_var <- input$iris_var_x
        y_var <- input$iris_var_y
        #---===  ===---
        ggplot(iris, aes_string(x = x_var, y = y_var, color = "Species")) +
            geom_point() +
            labs(title = paste("Iris Dataset", x_var, "vs", y_var), x = x_var, y = y_var) +
            theme_bw()
        #---=== ===---
    })
    #---=== Summary ===---
    # iris data -> Summary
    output$irisSummary <- renderPrint({
        data(iris)
        summary(iris)
    })
    #---=== Table ===---
    # iris data -> Table
    output$irisTable <- renderTable({
        data(iris)
        iris
    })
    
    #---=== PCA ===---    
    iris.pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
    
    # pca -> Plot
    output$pcaPlot <- renderPlot({
        req(input$PCs_1,input$PCs_2)
        if (input$PCs_1 == input$PCs_2 ) {
            showNotification("請選不同主成分")
            return(NULL)
        }
        
        selected_PCs <- c(as.integer(gsub("PC", "", input$PCs_1)),as.integer(gsub("PC", "", input$PCs_2)))
        ggbiplot(iris.pca, choices = selected_PCs, obs.scale = 1, var.scale = 1,
                 groups = iris$Species, ellipse = TRUE, circle = TRUE) +
            scale_color_discrete(name = '') +
            theme_bw()
    })
    #---=== Summary ===---
    # pca -> Summary
    output$pcaSummary <- renderPrint({
        summary(iris.pca)
    })
    #---=== Table ===---
    # pca -> Table
    output$pcaTable <- renderTable({
        pca_scores <- data.frame(iris.pca$x)
        colnames(pca_scores) <- paste("PC", 1:ncol(pca_scores), sep = "")
        pca_scores <- cbind(pca_scores, Species = iris$Species)
        pca_scores
    })

    #---=== CA ===--- 
    iris_ca <- CA(iris[, -5])
    
    # ca -> Plot
    output$caPlot <- renderPlot({
        req(input$caDims_1,input$caDims_2)
        if (input$caDims_1==input$caDims_2) {
          showNotification("請選不同維度")
            return(NULL)
        }
        
        selected_dims <- c(as.integer(gsub("Dim", "", input$caDims_1)),as.integer(gsub("Dim", "", input$caDims_2)))
        fviz_ca_biplot(iris_ca, axes = selected_dims, repel = TRUE, col.row = iris$Species,
                       ggtheme = theme_bw())
    })
    
    # ca -> Summary
    output$caSummary <- renderPrint({
        summary(iris_ca)
    })
    
    # ca -> Table
    output$caTable <- renderTable({
        ca_scores <- data.frame(iris_ca$row$coord)
        colnames(ca_scores) <- paste("Dim", 1:ncol(ca_scores), sep = "")
        ca_scores <- cbind(ca_scores, Species = iris$Species)
        ca_scores
    })
}
shinyApp(ui = ui, server = server)

