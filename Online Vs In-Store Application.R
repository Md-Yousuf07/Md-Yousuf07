library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(dashboardthemes)
library(shinyalert)

da=read.csv("C:/Users/Md Yousuf/Downloads/100% accurate shopping.csv",header = T)

da$Occupation=as.factor(da$Occupation)
da$Gender=as.factor(da$Gender)
da$shop_mode=as.factor(da$shop_mode)
da$o_preference=as.factor(da$o_preference)
da$o_frequency=as.factor(da$o_frequency)
da$i_frequency=as.factor(da$i_frequency)
da$deals_comparison=as.factor(da$deals_comparison)
da$o_products=as.factor(da$o_products)
da$i_products=as.factor(da$i_products)
da$o_satisfaction=as.factor(da$o_satisfaction)
da$i_satisfaction=as.factor(da$i_satisfaction)
da$o_reason=as.factor(da$o_reason)
da$i_reason=as.factor(da$i_reason)                      
da$i_rating=as.factor(da$i_rating)
da$o_rating=as.factor(da$o_rating)


da=da %>% 
    mutate(states=case_when(state=="Tamilnadu"|state=="Kerala"|state=="Andhra Pradesh"|state=="Karnataka"~"South",
                            state=='Himachal pradesh'|state=='Rajasthan'~"North",TRUE~"Others"))
da$states=as.factor(da$states)

da=da %>% 
    mutate(i_product=case_when(i_products=="Daily essentials"|i_products=="Food items"|i_products=="Medicine"|i_products=="Groceries"~"Eating Items",i_products=="Books"~"Stationaries",TRUE~"Other Items"))
da$i_product=as.factor(da$i_product)

da=da %>% 
    mutate(o_product=case_when(o_products=="Daily essentials"|o_products=="Food items"|o_products=="Medicine"|o_products=="Groceries"~"Eating items",o_products=="Books"|o_products=="Art materials"~"Stationaries",TRUE~"Other Items"))
da$o_product=as.factor(da$o_product)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Online Vs In-Store"),
                    dashboardSidebar(sidebarMenu(id = "tab1", selected = "home",
                                                 menuItem("Home", tabName = "home"),
                                                 menuItem("Behaviour", tabName = "behaviour"),
                                                 menuItem("Preference", tabName = "preference"),
                                                 menuItem("Feedback", tabName = "feedback"))),
                    dashboardBody(
                      shinyDashboardThemes(
                        theme = "blue_gradient"
                      ),
                            tabItems(
                            tabItem("home",
                                    fluidRow(column(12,offset=3,h2("Welcome to the Online Vs In-Store data analysis"))),
                                    img(src = "9-94594_online-store-vs-offline-store-hd-png-download.png", height =564, width = 600,
                                        style="display: block; margin-left: auto; margin-right: auto;")
                            ),
                            tabItem("behaviour",
                                    tabsetPanel(type = "tabs", selected = "Online", id = "intabset1",
                                    tabPanel("Online", value = "Online",
                                             fluidRow(h1(" ")),
                                             fluidRow(h1(" ")),
                                             fluidRow(column(3,selectizeInput("selectPer2", "Select the basic variable",
                                                                                       choices = c("Age", "Occupation", "Gender","None"))),
                                                 column(3,selectizeInput("selectPer1", "Select the behaviour variable",
                                                     choices = c("Shopping mode", "Online frequency"))),
                                                 column(3,sliderInput(
                                                   "Slider7", "Select the range of online purchase",
                                                   min = min(da$o_purchase),
                                                   max = max(da$o_purchase),
                                                   value = c(min = min(da$o_purchase), max = max(da$o_purchase)))),
                                             
                                             column(3,uiOutput("us1"))),
                                             fluidRow(column(12,plotlyOutput("shop_mode_age_ob"))),
                                             fluidRow(dataTableOutput("shop_mode_age_ob_1")),
                                             fluidRow(column(8,uiOutput("us11")),
                                             column(3,actionButton(inputId = "button1", label = "Next"))),
                                           
                                
        
                                             
                                             
                                    ),
                                    tabPanel("In-Store", value = "In-Store",
                                             fluidRow(h1(" ")),
                                             fluidRow(h1(" ")),
                                             fluidRow(column(3,selectizeInput("selectPer4", "Select the basic variable",
                                                                                                choices = c("Age", "Occupation", "Gender","None"))),
                                                      column(3,selectizeInput("selectPer3", "Select the behaviour variable",
                                                                                       choices = c("Shopping mode", "In-Store frequency"))),
                                                      column(3,sliderInput(
                                                        "Slider10", "Select the range of In-store purchase",
                                                        min = min(da$i_purchase),
                                                        max = max(da$i_purchase),
                                                        value = c(min = min(da$i_purchase), max = max(da$i_purchase)))),
                                             column(3,uiOutput("us2"))),
                                             fluidRow(column(12,plotlyOutput("shop_mode_age_ib"))),
                                             fluidRow(dataTableOutput("shop_mode_age_ib_1")),
                                             fluidRow(column(2,actionButton(inputId = "button2", label = "Back")),
                                             column(8,uiOutput("us12")))          
                                           ))),
                            tabItem("preference",
                                    tabsetPanel(type = "tabs", selected = "Online", id = "intabset2",
                                                tabPanel("Online", value = "Online",
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(3,selectizeInput("selectPer6", "Select the basic variable",
                                                                                                            choices = c("Age", "Occupation", "Gender"))),
                                                              column(3,selectizeInput("selectPer5", "Select the preference variable",
                                                                                                   choices = c("Deals & Comparison", "Online Product"))),
                                                              column(3,sliderInput(
                                                                    "Slider8", "Select the range of online purchase",
                                                                    min = min(da$o_purchase),
                                                                    max = max(da$o_purchase),
                                                                    value = c(min = min(da$o_purchase), max = max(da$o_purchase)))),
                                                         column(3,uiOutput("us3"))),
                                                         fluidRow(column(12,plotlyOutput("deals_comparison_age_op"))),
                                                         fluidRow(dataTableOutput("deals_comparison_age_op_1")),
                                                         fluidRow(column(8,uiOutput("us13")),
                                                                  (column(3,actionButton(inputId = "button3", label = "Next"))))
                                                         
                                                         
                                                         
                                                ),
                                                tabPanel("In-Store", value = "In-Store",
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(3,selectizeInput("selectPer8", "Select the basic variable",
                                                                                                            choices = c("Age", "Occupation", "Gender"))),
                                                                  column(3,selectizeInput("selectPer7", "Select the preference variable",
                                                                                                   choices = c("Deals & Comparison", "In-Store Product"))),
                                                                  column(3,sliderInput(
                                                                    "Slider11", "Select the range of In-store purchase",
                                                                    min = min(da$i_purchase),
                                                                    max = max(da$i_purchase),
                                                                    value = c(min = min(da$i_purchase), max = max(da$i_purchase)))),
                                                         column(3,uiOutput("us4"))),
                                                         fluidRow(column(12,plotlyOutput("deals_comparison_age_ip"))),
                                                         fluidRow(dataTableOutput("deals_comparison_age_ip_1")),
                                                         fluidRow(column(2,actionButton(inputId = "button4", label = "Back")),
                                                                  
                                                         column(8,uiOutput("us14")))
                                                         
                                                )
                                                
                                    )
                                    
                            ),
                            tabItem("feedback",
                                    tabsetPanel(type = "tabs", selected = "Online", id = "intabset3",
                                                tabPanel("Online", value = "Online",
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(3,selectizeInput("selectPer10", "Select the basic variable",
                                                                                                            choices = c("Age", "Occupation", "Gender"))),
                                                              column(3,selectizeInput("selectPer9", "Select the feedback variable",
                                                                                                   choices = c("Online Rating", "Online Reason"))),
                                                              column(3,sliderInput(
                                                                    "Slider9", "Select the range of online purchase",
                                                                    min = min(da$o_purchase),
                                                                    max = max(da$o_purchase),
                                                                    value = c(min = min(da$o_purchase), max = max(da$o_purchase)))),
                                                         
                                                         column(3,uiOutput("us5"))),
                                                         fluidRow(column(12,plotlyOutput("o_rating_age_of"))),
                                                         fluidRow(dataTableOutput("o_rating_age_of_1")),
                                                         fluidRow(column(6,uiOutput("us15")),
                                                         (column(3,actionButton(inputId = "button5", label = "Next"))))
                                                         
                                                ),
                                                tabPanel("In-Store", value = "In-Store",
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(3,selectizeInput("selectPer12", "Select the basic variable",
                                                                                                            choices = c("Age", "Occupation", "Gender"))),
                                                                  column(3,selectizeInput("selectPer11", "Select the feedback variable",
                                                                                                   choices = c("In-Store Rating", "In-Store Reason"))),
                                                                  column(3,sliderInput(
                                                                    "Slider12", "Select the range of In-store purchase",
                                                                    min = min(da$i_purchase),
                                                                    max = max(da$i_purchase),
                                                                    value = c(min = min(da$i_purchase), max = max(da$i_purchase)))),
                                                         
                                                         column(3,uiOutput("us6"))),
                                                         fluidRow(column(12,plotlyOutput("i_rating_age_if"))),
                                                         fluidRow(dataTableOutput("i_rating_age_if_1")),
                                                         fluidRow(column(1,actionButton(inputId = "button6", label = "Back")),
                                                         column(6,uiOutput("us16")))
                                                         
                                                         
                                                )
                                                    
                                    )
                                    
                            ))))


server <- function(input, output, session){
    
  output$us1 <- renderUI({
    if(input$selectPer2 == "Age"){
      sliderInput(
        "Slider1", "Select the age group",
        min = min(da$age),
        max = max(da$age),
        value = c(min = min(da$age), max = max(da$age)))
    }else{
      h1("")
    }
    
  })
  output$us2 <- renderUI({
    if(input$selectPer4 == "Age"){
      sliderInput(
        "Slider2", "Select the age group",
        min = min(da$age),
        max = max(da$age),
        value = c(min = min(da$age), max = max(da$age)))
    }else{
      h1("")
    }
    
  })
  output$us3 <- renderUI({
    if(input$selectPer6 == "Age"){
      sliderInput(
        "Slider3", "Select the age group",
        min = min(da$age),
        max = max(da$age),
        value = c(min = min(da$age), max = max(da$age)))
    }else{
      h1("")
    }
    
  })
  output$us4 <- renderUI({
    if(input$selectPer8 == "Age"){
      sliderInput(
        "Slider4", "Select the age group",
        min = min(da$age),
        max = max(da$age),
        value = c(min = min(da$age), max = max(da$age)))
    }else{
      h1("")
    }
    
  })
  output$us5 <- renderUI({
    if(input$selectPer10 == "Age"){
      sliderInput(
        "Slider5", "Select the age group",
        min = min(da$age),
        max = max(da$age),
        value = c(min = min(da$age), max = max(da$age)))
    }else{
      h1("")
    }
    
  })
  output$us6 <- renderUI({
    if(input$selectPer12 == "Age"){
      sliderInput(
        "Slider6", "Select the age group",
        min = min(da$age),
        max = max(da$age),
        value = c(min = min(da$age), max = max(da$age)))
    }else{
      h1("")
    }
    
  })
  output$us11 <- renderUI({
    if(input$selectPer2 != "Age"){
      fluidRow(column(1,offset=6,fluidRow(downloadButton("download1", label = "Download"))))
    }else{
      h1("")
    }
    
  })
  output$us12 <- renderUI({
    if(input$selectPer4 != "Age"){
      fluidRow(column(1,offset=6,fluidRow(downloadButton("download2", label = "Download"))))
    }else{
      h1("")
    }
    
  })
  output$us13 <- renderUI({
    if(input$selectPer6 != "Age"){
      fluidRow(column(1,offset=6,fluidRow(downloadButton("download3", label = "Download"))))
    }else{
      h1("")
    }
    
  })
  output$us14 <- renderUI({
    if(input$selectPer8 != "Age"){
      fluidRow(column(1,offset=6,fluidRow(downloadButton("download4", label = "Download"))))
    }else{
      h1("")
    }
    
  })
  output$us15 <- renderUI({
    if(input$selectPer10 != "Age"){
      fluidRow(column(1,offset=6,fluidRow(downloadButton("download5", label = "Download"))))
    }else{
      h1("")
    }
    
  })
  output$us16 <- renderUI({
    if(input$selectPer12 != "Age"){
      fluidRow(column(1,offset=6,fluidRow(downloadButton("download6", label = "Download"))))
    }else{
      h1("")
    }
    
  })
  output$download1 <- downloadHandler(
    
    filename = function(){
      
    paste(input$selectPer1," and ",input$selectPer2,".csv", sep = "")
    },
    content = function(file){
      if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "None"){
        da1=da %>% group_by(shop_mode) %>% summarize(Average=round(mean(o_purchase),2),
                                                     Standard_deviation=round(sd(o_purchase),2),
                                                     Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                     Number_of_observations=n())
      }
      else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "None"){
        da1=da %>% group_by(o_frequency) %>% summarize(Average=round(mean(o_purchase),2),
                                                       Standard_deviation=round(sd(o_purchase),2),
                                                       Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                       Number_of_observations=n())
      }
      else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Occupation"){
        da1=da %>% group_by(Occupation,shop_mode) %>% summarize(Average=round(mean(o_purchase),2),
                                                                Standard_deviation=round(sd(o_purchase),2),
                                                                Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                Number_of_observations=n())
        
        
      }
      else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "Occupation"){
        da1=da %>% group_by(Occupation,o_frequency) %>% summarize(Average=round(mean(o_purchase),2),
                                                                  Standard_deviation=round(sd(o_purchase),2),
                                                                  Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                  Number_of_observations=n())
        
        
      }
      else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Gender"){
        da1=da %>% group_by(Gender,shop_mode) %>% summarize(Average=round(mean(o_purchase),2),
                                                            Standard_deviation=round(sd(o_purchase),2),
                                                            Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                            Number_of_observations=n())
      }
      else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "Gender"){
        da1=da %>% group_by(Gender,o_frequency) %>% summarize(Average=round(mean(o_purchase),2),
                                                              Standard_deviation=round(sd(o_purchase),2),
                                                              Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                              Number_of_observations=n())
        
        
      }
      
      
      write.csv(da1, file, row.names = TRUE)
    }
    
  )
  output$download2 <- downloadHandler(
    
    filename = function(){
      
      paste(input$selectPer3," and ",input$selectPer4,".csv", sep = "")
    },
    content = function(file){
      if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "None"){
        da2=da %>% group_by(shop_mode) %>% summarize(Average=round(mean(i_purchase),2),
                                                     Standard_deviation=round(sd(i_purchase),2),
                                                     Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                     Number_of_observations=n())  } 
      else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "None"){
        da2=da %>% group_by(i_frequency) %>% summarize(Average=round(mean(i_purchase),2),
                                                       Standard_deviation=round(sd(i_purchase),2),
                                                       Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                       Number_of_observations=n())   }
      
      else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Occupation"){
        da2=da %>% group_by(Occupation,shop_mode) %>% summarize(Average=round(mean(i_purchase),2),
                                                                Standard_deviation=round(sd(i_purchase),2),
                                                                Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                Number_of_observations=n())   
        
        
      }
      else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "Occupation"){
        da2=da %>% group_by(Occupation,i_frequency) %>% summarize(Average=round(mean(i_purchase),2),
                                                                  Standard_deviation=round(sd(i_purchase),2),
                                                                  Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                  Number_of_observations=n())
        
        
      }
      else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Gender"){
        da2=da %>% group_by(Gender,shop_mode) %>% summarize(Average=round(mean(i_purchase),2),
                                                            Standard_deviation=round(sd(i_purchase),2),
                                                            Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                            Number_of_observations=n())
        
      }
      else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "Gender"){
        da2=da %>% group_by(Gender,i_frequency) %>% summarize(Average=round(mean(i_purchase),2),
                                                              Standard_deviation=round(sd(i_purchase),2),
                                                              Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                              Number_of_observations=n())
        
        
        
        
      }
      write.csv(da2, file, row.names = TRUE)
    }
    
  )
      output$shop_mode_age_ob_1 <- renderDataTable({
        
      if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "None"){
        da1=da %>% group_by(shop_mode) %>% summarize(Average=round(mean(o_purchase),2),
                                                                Standard_deviation=round(sd(o_purchase),2),
                                                                Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                Number_of_observations=n())
      }
      else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "None"){
        da1=da %>% group_by(o_frequency) %>% summarize(Average=round(mean(o_purchase),2),
                                                                Standard_deviation=round(sd(o_purchase),2),
                                                                Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                Number_of_observations=n())
      }
      else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Occupation"){
            da1=da %>% group_by(Occupation,shop_mode) %>% summarize(Average=round(mean(o_purchase),2),
                                                                    Standard_deviation=round(sd(o_purchase),2),
                                                                    Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                    Number_of_observations=n())
            
            
        }
        else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "Occupation"){
            da1=da %>% group_by(Occupation,o_frequency) %>% summarize(Average=round(mean(o_purchase),2),
                                                                      Standard_deviation=round(sd(o_purchase),2),
                                                                      Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                      Number_of_observations=n())
            
            
        }
        else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Gender"){
            da1=da %>% group_by(Gender,shop_mode) %>% summarize(Average=round(mean(o_purchase),2),
                                                                Standard_deviation=round(sd(o_purchase),2),
                                                                Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                Number_of_observations=n())
        }
        else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "Gender"){
            da1=da %>% group_by(Gender,o_frequency) %>% summarize(Average=round(mean(o_purchase),2),
                                                                  Standard_deviation=round(sd(o_purchase),2),
                                                                  Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                  Number_of_observations=n())
            
            
        }
        
        
        
    })
    
    
    output$shop_mode_age_ib_1 <- renderDataTable({
       if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "None"){
        da2=da %>% group_by(shop_mode) %>% summarize(Average=round(mean(i_purchase),2),
                                                                Standard_deviation=round(sd(i_purchase),2),
                                                                Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                Number_of_observations=n())  } 
        else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "None"){
        da2=da %>% group_by(i_frequency) %>% summarize(Average=round(mean(i_purchase),2),
                                                                Standard_deviation=round(sd(i_purchase),2),
                                                                Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                Number_of_observations=n())   }
        
         else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Occupation"){
            da2=da %>% group_by(Occupation,shop_mode) %>% summarize(Average=round(mean(i_purchase),2),
                                                                    Standard_deviation=round(sd(i_purchase),2),
                                                                    Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                    Number_of_observations=n())   
            
            
        }
        else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "Occupation"){
            da2=da %>% group_by(Occupation,i_frequency) %>% summarize(Average=round(mean(i_purchase),2),
                                                                      Standard_deviation=round(sd(i_purchase),2),
                                                                      Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                      Number_of_observations=n())
            
            
        }
        else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Gender"){
            da2=da %>% group_by(Gender,shop_mode) %>% summarize(Average=round(mean(i_purchase),2),
                                                                Standard_deviation=round(sd(i_purchase),2),
                                                                Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                Number_of_observations=n())
            
        }
        else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "Gender"){
            da2=da %>% group_by(Gender,i_frequency) %>% summarize(Average=round(mean(i_purchase),2),
                                                                  Standard_deviation=round(sd(i_purchase),2),
                                                                  Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                  Number_of_observations=n())
            
            
            
            
        }
        
        
    })
    output$deals_comparison_age_op_1 <- renderDataTable({
        
        if(input$selectPer5 == "Deals & Comparison" & input$selectPer6 == "Occupation"){
            da3=da %>% group_by(Occupation,deals_comparison) %>% summarize(Average=round(mean(o_purchase),2),
                                                                           Standard_deviation=round(sd(o_purchase),2),
                                                                           Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                           Number_of_observations=n())
            
            
        }
        else if(input$selectPer5 == "Online Product" & input$selectPer6 == "Occupation"){
            da3=da %>% group_by(o_product,Occupation) %>% summarize(Average=round(mean(o_purchase),2),
                                                                    Standard_deviation=round(sd(o_purchase),2),
                                                                           Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                           Number_of_observations=n())
            
            
            
        }
        else if(input$selectPer5 == "Deals & Comparison" & input$selectPer6 == "Gender"){
            da3=da %>% group_by(Gender,deals_comparison) %>% summarize(Average=round(mean(o_purchase),2),
                                                                       Standard_deviation=round(sd(o_purchase),2),
                                                                           Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                           Number_of_observations=n())
            
            
        }
        else if(input$selectPer5 == "Online Product" & input$selectPer6 == "Gender"){
            da3=da %>% group_by(o_product,Gender) %>% summarize(Average=round(mean(o_purchase),2),
                                                                Standard_deviation=round(sd(o_purchase),2),
                                                                    Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                    Number_of_observations=n())
            
            
        }
        
        
    })
    output$deals_comparison_age_ip_1 <- renderDataTable({
        
      if(input$selectPer7 == "Deals & Comparison" & input$selectPer8 == "Occupation"){
          da4=da %>% group_by(Occupation,deals_comparison) %>% summarize(Average=round(mean(i_purchase),2),
                                                                         Standard_deviation=round(sd(i_purchase),2),
                                                                         Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                         Number_of_observations=n())
          
            
            
        }
        else if(input$selectPer7 == "In-Store Product" & input$selectPer8 == "Occupation"){
            da4=da %>% group_by(Occupation,i_product) %>% summarize(Average=round(mean(i_purchase),2),
                                                                    Standard_deviation=round(sd(i_purchase),2),
                                                                     Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                     Number_of_observations=n())
            
            
        }
        else if(input$selectPer7 == "Deals & Comparison" & input$selectPer8 == "Gender"){
            da4=da %>% group_by(Gender,deals_comparison) %>% summarize(Average=round(mean(i_purchase),2),
                                                                       Standard_deviation=round(sd(i_purchase),2),
                                                                        Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                        Number_of_observations=n())
            
            
        }
        else if(input$selectPer7 == "In-Store Product" & input$selectPer8 == "Gender"){
            da4=da %>% group_by(Gender,i_product) %>% summarize(Average=round(mean(i_purchase),2),
                                                                Standard_deviation=round(sd(i_purchase),2),
                                                                Minimum=min(i_purchase),Maximum=max(i_purchase),
                                                                Number_of_observations=n())
            
            
            
        }
        
        
    })
    output$o_rating_age_of_1 <- renderDataTable({
        
        if(input$selectPer9 == "Online Rating" & input$selectPer10 == "Occupation"){
            da5=da %>% group_by(o_rating,Occupation) %>% summarize(Average=round(mean(o_purchase),2),
                                                                   Standard_deviation=round(sd(o_purchase),2),
                                                                    Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                    Number_of_observations=n())
            
        }
        else if(input$selectPer9 == "Online Reason" & input$selectPer10 == "Occupation"){
            da5=da %>% group_by(o_reason,Occupation) %>% summarize(Average=round(mean(o_purchase),2),
                                                                   Standard_deviation=round(sd(o_purchase),2),
                                                                   Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                   Number_of_observations=n())
            
        }
        else if(input$selectPer9 == "Online Rating" & input$selectPer10 == "Gender"){
            da5=da %>% group_by(o_rating,Gender) %>% summarize(Average=round(mean(o_purchase),2),
                                                               Standard_deviation=round(sd(o_purchase),2),
                                                                   Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                                   Number_of_observations=n())
            
            
        }
        else if(input$selectPer9 == "Online Reason" & input$selectPer10 == "Gender"){
            da5=da %>% group_by(o_reason,Gender) %>% summarize(Average=round(mean(o_purchase),2),
                                                               Standard_deviation=round(sd(o_purchase),2),
                                                               Minimum=min(o_purchase),Maximum=max(o_purchase),
                                                               Number_of_observations=n())
            
            
        }
        
        
    })
    output$i_rating_age_if_1 <- renderDataTable({
        
       if(input$selectPer11 == "In-Store Rating" & input$selectPer12 == "Occupation"){
           da6=da %>% group_by(i_rating,Occupation) %>% summarize(Average=round(mean(i_purchase),2),
                                                                  Standard_deviation=round(sd(i_purchase),2),
                                                                  Minimum=min(o_purchase),Maximum=max(i_purchase),
                                                                  Number_of_observations=n())
           
        
        }
        else if(input$selectPer11 == "In-Store Reason" & input$selectPer12 == "Occupation"){
            da6=da %>% group_by(i_reason,Occupation) %>% summarize(Average=round(mean(i_purchase),2),
                                                                   Standard_deviation=round(sd(i_purchase),2),
                                                                   Minimum=min(o_purchase),Maximum=max(i_purchase),
                                                                   Number_of_observations=n())
            
            
        }
        else if(input$selectPer11 == "In-Store Rating" & input$selectPer12 == "Gender"){
            da6=da %>% group_by(i_rating,Gender) %>% summarize(Average=round(mean(i_purchase),2),
                                                               Standard_deviation=round(sd(i_purchase),2),
                                                                   Minimum=min(o_purchase),Maximum=max(i_purchase),
                                                                   Number_of_observations=n())
            
            
        }
        else if(input$selectPer11 == "In-Store Reason" & input$selectPer12 == "Gender"){
            da6=da %>% group_by(i_reason,Gender) %>% summarize(Average=round(mean(i_purchase),2),
                                                               Standard_deviation=round(sd(i_purchase),2),
                                                               Minimum=min(o_purchase),Maximum=max(i_purchase),
                                                               Number_of_observations=n())
            
        }
        
    })

    output$shop_mode_age_ob <- renderPlotly({
        if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "None"){
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
          plot_ly(da, x = ~o_purchase, type = "histogram",color=~shop_mode,colors=c(I("darkgreen"),I('orange')),stroke=I('black')) %>% 
         layout(title = 'Online purchase based on Shopping mode',
                     xaxis = list(title= "Online purchase"),legend=list(x=0,y=-.5,orientation='h'))
        
        }
      else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "None"){
        da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
        plot_ly(da, x = ~o_purchase, type = "histogram",color=~o_frequency,colors=c(I("maroon"),I('purple'),I("brown"),("violet")),stroke=I('black')) %>% 
        layout(title = 'Online purchase based on Online frequency',
                   xaxis = list(title= "Online purchase"),legend=list(x=0,y=-.5,orientation='h'))
      
      }
      
        else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Age"){
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
            da <- da[which(da$age %in% (input$Slider1[1]:input$Slider1[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~o_purchase,
                    color=~shop_mode,
                    type = "scatter",
                    colors='Set2', mode='markers'
            )%>% 
                layout(title = 'Age Vs Online purchase based on Shopping mode',
                       xaxis = list(title= "Age"),yaxis = list(title = "Online purchase"),legend=list(x=0,y=-.5,orientation='h'))
        } 
                
        else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "Age"){
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
            da <- da[which(da$age %in% (input$Slider1[1]:input$Slider1[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~o_purchase,
                    color=~o_frequency,
                    type = "scatter",
                    colors='Set2', mode='markers'
            )%>% 
                layout(title = 'Age Vs Online purchase based on Online frequency',
                       xaxis = list(title= "Age"),yaxis = list(title = "Online purchase"),legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Occupation"){
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
            a1=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~shop_mode)+
                labs(title="Online purchase based on Occupation and Shopping mode", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold")  
                      )
            ggplotly(a1)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer1 == "Online frequency" & input$selectPer2 == "Occupation"){
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
            a2=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~o_frequency)+ 
                labs(title="Online purchase based on Occupation and Online frequency", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic")
                        
                      )
            ggplotly(a2)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer1 == "Shopping mode" & input$selectPer2 == "Gender"){
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
            a3=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~shop_mode)+ 
                labs(title="Online purchase based on Gender and Shopping mode", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold")  
                      )
            ggplotly(a3)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else{
          da <- da %>% filter(o_purchase>=input$Slider7[1] & o_purchase<=input$Slider7[2])
            a4=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~o_frequency)+ 
                labs(title="Online purchase based on Gender and Online frequency", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                      )
            ggplotly(a4)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        
        
    })
    
    output$shop_mode_age_ib <- renderPlotly({
      if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "None"){
        da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
        plot_ly(da, y = ~i_purchase, type = "box",color=~shop_mode,colors=c(I("blue"),I('violet')),stroke=I('black')) %>% 
          layout(title = 'In-store purchase based on Shopping mode',
                 xaxis = list(title= "In-store  purchase"),legend=list(x=0,y=-.5,orientation='h'))
        
      }
      else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "None"){
        da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
        plot_ly(da, y = ~i_purchase, type = "box",color=~i_frequency,colors=c(I("brown"),I('purple'),I("red"),("blue")),stroke=I('black')) %>% 
          layout(title = 'In-store purchase based on In-store  frequency',
                 xaxis = list(title= "In-store  purchase"),legend=list(x=0,y=-.5,orientation='h'))
        
      }
      
      
      else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Age"){
        da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
            da <- da[which(da$age %in% (input$Slider2[1]:input$Slider2[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~i_purchase,
                    color=~shop_mode,
                    type = "scatter",
                    colors=c(I("brown"),I('blue')), mode='markers'
            ) %>% 
                layout(title = 'Age Vs In-store purchase based on Shopping mode',
                       xaxis = list(title= "Age"),yaxis = list(title = "Shopping Mode"),legend=list(x=0,y=-.5,orientation='h'))
        } 
        
        else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "Age"){
          da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
            da <- da[which(da$age %in% (input$Slider2[1]:input$Slider2[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~i_purchase,
                    color=~i_frequency,
                    type = "scatter",
                    colors=c(I("purple"),I('darkgreen')), mode='markers'
            ) %>% 
                layout(title = 'Age Vs In-store purchase based on In-store  frequency',
                       xaxis = list(title= "Age"),yaxis = list(title = "In-store  frequency"),legend=list(x=0,y=-.5))
            
            
        }
        else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Occupation"){
          da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
            a5=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~shop_mode)+
                theme(legend.position = "bottom")+
                labs(title="In-store purchase based on Occupation and Shopping mode", 
                     x="In-store  Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )
            ggplotly(a5)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
            
        }
        else if(input$selectPer3 == "In-Store frequency" & input$selectPer4 == "Occupation"){
          da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
            a6=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~i_frequency)+
                theme(legend.position = "bottom")+
                labs(title="In-store purchase based on Occupation and In-store  frequency", 
                     x="In-store  Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )
            ggplotly(a6)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
            
        }
        else if(input$selectPer3 == "Shopping mode" & input$selectPer4 == "Gender"){
          da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
            a7=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~shop_mode)+ 
                labs(title="In-store purchase based on Gender and Shopping mode", 
                     x="In-store  Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )
            ggplotly(a7)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else{
          da <- da %>% filter(i_purchase>=input$Slider10[1] & i_purchase<=input$Slider10[2])
            a8=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~i_frequency)+ 
                labs(title="In-store purchase based on Gender and In-store  frequency", 
                     x="In-store Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            
            ggplotly(a8)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        
        
    })
    output$deals_comparison_age_op <- renderPlotly({
        
        if(input$selectPer5 == "Deals & Comparison" & input$selectPer6 == "Age"){
          da <- da %>% filter(o_purchase>=input$Slider8[1] & o_purchase<=input$Slider8[2])
            da <- da[which(da$age %in% (input$Slider3[1]:input$Slider3[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~o_purchase,
                    color=~deals_comparison,
                    type = "scatter",
                    colors='Set2', mode='markers'
            )%>% 
                layout(title = 'Age Vs Online purchase based on Deals & Comparison',
                       xaxis = list(title= "Age"),yaxis = list(title = "Online Purchase"),legend=list(x=0,y=-.5,orientation='h'))
        } 
        
        else if(input$selectPer5 == "Online Product" & input$selectPer6 == "Age"){
            da <- da[which(da$age %in% (input$Slider3[1]:input$Slider3[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~o_purchase,
                    color=~o_product,
                    type = "scatter",
                    colors='Set2', mode='markers'
            )%>% 
                layout(title = 'Age Vs Online purchase based on Online products',
                       xaxis = list(title= "Age"),yaxis = list(title = "Online Purchase"),legend=list(x=0,y=-.5,orientation='h'))
            
            
        }
        else if(input$selectPer5 == "Deals & Comparison" & input$selectPer6 == "Occupation"){
            a9=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~deals_comparison)+  
                labs(title="Online Purchase based on Occupation and Deals & Comparison", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a9)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer5 == "Online Product" & input$selectPer6 == "Occupation"){
            a10=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~o_product)+
                labs(title="Online Purchase based on Occupation and Online product", 
                     x="Online Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a10)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer5 == "Deals & Comparison" & input$selectPer6 == "Gender"){
            a11=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~deals_comparison)+
                labs(title="Online Purchase based on Gender and Deals & Comparison", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a11)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else{
            a12=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~o_product)+ 
                labs(title="Online Purchase based on Gender and Online product", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a12)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        
        
    })
    
    output$deals_comparison_age_ip <- renderPlotly({
        
        if(input$selectPer7 == "Deals & Comparison" & input$selectPer8 == "Age"){
          da <- da %>% filter(i_purchase>=input$Slider11[1] & i_purchase<=input$Slider11[2])
            da <- da[which(da$age %in% (input$Slider4[1]:input$Slider4[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~i_purchase,
                    color=~deals_comparison,
                    type = "scatter",
                    colors=c(I("green2"),I('orange'),I('brown')), mode='markers'
            ) %>% 
                layout(title = 'Age Vs In-store purchase based on Deals & Comparison',
                       xaxis = list(title= "Age"),yaxis = list(title = "Deals & Comparison"),legend=list(x=0,y=-.5,orientation='h'))
        } 
        
        else if(input$selectPer7 == "In-Store Product" & input$selectPer8 == "Age"){
          da <- da %>% filter(i_purchase>=input$Slider11[1] & i_purchase<=input$Slider11[2])
            da <- da[which(da$age %in% (input$Slider4[1]:input$Slider4[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~i_purchase,
                    color=~i_product,
                    type = "scatter",
                    colors=c(I("red"),I('blue'),I('green')), mode='markers'
            ) %>% 
                layout(title = 'Age Vs In-store purchase based on In-store product',
                       xaxis = list(title= "Age"),yaxis = list(title = "In-store product"),legend=list(x=0,y=-.5,orientation='h'))
            
            
        }
        else if(input$selectPer7 == "Deals & Comparison" & input$selectPer8 == "Occupation"){
          da <- da %>% filter(i_purchase>=input$Slider11[1] & i_purchase<=input$Slider11[2])
            a13=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~deals_comparison)+  
                labs(title="In-store Purchase based on Occupation and Deals & Comparison", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a13)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer7 == "In-Store Product" & input$selectPer8 == "Occupation"){
          da <- da %>% filter(i_purchase>=input$Slider11[1] & i_purchase<=input$Slider11[2])
            a14=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~i_product)+
                labs(title="In-store Purchase based on Occupation and In-store products", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a14)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer7 == "Deals & Comparison" & input$selectPer8 == "Gender"){
          da <- da %>% filter(i_purchase>=input$Slider11[1] & i_purchase<=input$Slider11[2])
            a15=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~deals_comparison)+
                labs(title="In-store purchase based on Gender and Deals & Comparison", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a15)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else{
          da <- da %>% filter(i_purchase>=input$Slider11[1] & i_purchase<=input$Slider11[2])
            a16=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~i_product)+ 
                labs(title="In-store purchase based on Gender and In-store product", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )   
            ggplotly(a16)%>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        
        
    })
    output$o_rating_age_of <- renderPlotly({
        
        if(input$selectPer9 == "Online Rating" & input$selectPer10 == "Age"){
          da <- da %>% filter(o_purchase>=input$Slider9[1] & o_purchase<=input$Slider9[2])
            da <- da[which(da$age %in% (input$Slider5[1]:input$Slider5[2])),]
            plot_ly(da, 
                    x = ~o_purchase,
                    y= ~age,
                    color=~o_rating,
                    type = "scatter",
                    mode='markers',colors = c(I("red"),I("blue"),I("purple"),I("green"))) %>% 
                layout(title = 'Age Vs Online Purchase based on Online Rating',barmode='stack',
                       xaxis = list(title= "Online Purchase"),yaxis = list(title = "Age"),
                       legend=list(x=0,y=-.5,orientation='h'))
        } 
        
        else if(input$selectPer9 == "Online Reason" & input$selectPer10 == "Age"){
            da <- da[which(da$age %in% (input$Slider5[1]:input$Slider5[2])),]
            plot_ly(da, 
                    x = ~age,
                    y= ~o_purchase,
                    color=~o_reason,
                    type = "scatter",
                    mode='markers',colors = c(I("red"),I("blue"),I("purple"),I("green")))%>% 
                layout(title = 'Age Vs Online Purchase based on Online reason',barmode='stack',
                       xaxis = list(title= "Age"),yaxis = list(title = "Online Purchase"),legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer9 == "Online Rating" & input$selectPer10 == "Occupation"){
            a17=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~o_rating)+
                labs(title="Online Purchase based on Occupation and Online Rating", 
                     x="Online purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a17) %>%
                layout(barmode='stack',legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer9 == "Online Reason" & input$selectPer10 == "Occupation"){
            a18=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~o_reason)+
                labs(title="Online Purchase based on Occupation and Online Reason", 
                     x="Online Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a18) %>%
                layout(barmode='stack',legend=list(x=0,y=-.5,orientation='h'))
        }
        else if(input$selectPer9 == "Online Rating" & input$selectPer10 == "Gender"){
            a19=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~o_rating)+
                labs(title="Online purchase based on Gender and Online Rating", 
                     x="Online Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a19) %>%
                layout(barmode='stack',legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else{
            a20=ggplot(da,aes(x=o_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~o_reason)+
                labs(title="Online Purchase based on Gender and Online Reason", 
                     x="Online Purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a20) %>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        
        
    })
   
    output$i_rating_age_if <- renderPlotly({
        
        if(input$selectPer11 == "In-Store Rating" & input$selectPer12 == "Age"){
          da <- da %>% filter(i_purchase>=input$Slider12[1] & i_purchase<=input$Slider12[2])
            da <- da[which(da$age %in% (input$Slider6[1]:input$Slider6[2])),]
            plot_ly(da, x = ~age, y= ~i_purchase,
                    type = "scatter",mode='markers',color=~i_rating,
                    colors = c(I("red"),I("blue"),I("brown"),I("green"),I("yellow")))%>% 
                layout(title = 'Age  Vs In-store purchase based on In-store rating',
                       xaxis = list(title= "Age"),yaxis = list(title = "In-store purchase"),legend=list(x=0,y=-.5,orientation='h'))
            
        } 
        
        else if(input$selectPer11 == "In-Store Reason" & input$selectPer12 == "Age"){
          da <- da %>% filter(i_purchase>=input$Slider12[1] & i_purchase<=input$Slider12[2])
            da <- da[which(da$age %in% (input$Slider6[1]:input$Slider6[2])),]
            plot_ly(da, x = ~age, y= ~i_purchase,
                    type = "scatter",mode='markers',
                    color=~i_reason,colors = c(I("red"),I("blue"),I("purple"),I("green")))%>% 
                layout(title = 'Age  Vs In-store purchase based on In-store reason',
                       xaxis = list(title= "Age"),yaxis = list(title = "In-store purchase"),
                       legend=list(x=0,y=-.5,orientation='h'))
            
            
        }
        else if(input$selectPer11 == "In-Store Rating" & input$selectPer12 == "Occupation"){
          da <- da %>% filter(i_purchase>=input$Slider12[1] & i_purchase<=input$Slider12[2])
            a21=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~i_rating)+
                labs(title="In-store Purchase based on Occupation and In-store rating ", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a21) %>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else if(input$selectPer11 == "In-Store Reason                                        " & input$selectPer12 == "Occupation"){
          da <- da %>% filter(i_purchase>=input$Slider12[1] & i_purchase<=input$Slider12[2])
            a22=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Occupation,alpha = 0.10))+facet_grid(~i_reason)+
                labs(title="In-store Purchase based on Occupation and In-store reason", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a22) %>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
        }
        else if(input$selectPer11 == "In-Store Rating" & input$selectPer12 == "Gender"){
          da <- da %>% filter(i_purchase>=input$Slider12[1] & i_purchase<=input$Slider12[2])
            a23=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~i_rating)+
                labs(title="In-store purchase based on Gender and In-store Rating", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a23) %>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        else{
          da <- da %>% filter(i_purchase>=input$Slider12[1] & i_purchase<=input$Slider12[2])
            a24=ggplot(da,aes(x=i_purchase))+
                geom_density(aes(fill=Gender,alpha = 0.10))+facet_grid(~i_rating)+
                labs(title="In-store purchase based on Gender and In-store Reason", 
                     x="In-store purchase")+
                theme(plot.title=element_text(hjust=0.5,vjust=0,color="black",size=12, face="italic"),
                      axis.title.x=element_text(color="black", size=10, face="bold") 
                )  
            ggplotly(a24) %>%
                layout(legend=list(x=0,y=-.5,orientation='h'))
            
        }
        
    })
    observeEvent(input$button1,{
        updateTabsetPanel(session = session, "intabset1", selected = "In-Store")
    })
    observeEvent(input$button2,{
        updateTabItems(session = session, "intabset1", selected = "Online")
    })
    
    observeEvent(input$button3,{
        updateTabsetPanel(session = session, "intabset2", selected = "In-Store")
    })
    observeEvent(input$button4,{
        updateTabsetPanel(session = session, "intabset2", selected = "Online")
    })
    observeEvent(input$button5,{
        updateTabsetPanel(session = session, "intabset3", selected = "In-Store")
    })
    observeEvent(input$button6,{
        updateTabsetPanel(session = session, "intabset3", selected = "Online")
    })
    
    
}

shinyApp(ui, server)

