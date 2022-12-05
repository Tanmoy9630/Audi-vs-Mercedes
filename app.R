library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(ggpubr)
audi=read.csv("audi.csv")
merc=read.csv("merc.csv")
ui <- dashboardPage(
  dashboardHeader(title = "Audi vs Mercedes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",tabName = "About"),
      menuItem("Audi",tabName = "Audi"),
      menuItem("Mercedes",tabName = "Mercedes"),
      menuItem("Audi vs Mercedes",tabName = "AudivsMercedes"),
      menuItem("Conclusion",tabName = "Conclusion")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "About",
              fluidPage(
                fluidRow(
                  column(12,tags$h1(tags$b("About The Project"),align="center"),
                         tags$img(src="audivsmercedes.png",style="display:block;margin-left:auto;margin-right:auto;"),
                         tags$h2(tags$b("Introduction")),
                         tags$p("The main goal of this project is to provide a Visual representation of the performance of Audi and Mercedes used cars. Based on this a buyer can make a better decision in choosing the Brand and Model.
                                This project also focus on what people prefers in their cars in terms of Transmission,fuel type and how the preference changes over time."),
                         tags$h3(tags$b("About The Dataset")),
                         tags$p("The two data set I have used are:-"),
                         tags$ul("1.	Audi used cars(2002-2020)"),
                         tags$ul("2.	Mercedes used cars(1997-2020)"),
                         tags$p("Both the data set contains information on price, transmission, mileage, fuel type, road Tax, miles per gallon(mpg),engine size and year of purchasing corresponding to different used car model."),
                         tags$h3(tags$b("Objective of The Project")),
                         tags$li("Audi",
                                 tags$ul("1.	Which Model of the Audi Brand is popular?"),
                                 tags$ul("2.	Model-wise comparison of price, miles per gallon, Road Tax, etc."),
                                 tags$ul("3.	Which Model comes with lower price and provide better MPG(miles per gallon) ?"),
                                 tags$ul("4.  What type of Transmission, Fuel type do people prefers in their Audi cars, Also, how the preference changes over time?")),
                         tags$li("Mercedes",
                                 tags$ul("1.	which Model of the Mercedes Brand is popular?"),
                                 tags$ul("2.	Model-wise comparison of price, miles per gallon, Road Tax, etc."),
                                 tags$ul("3.  Which Model comes with lower price and provide better MPG(miles per gallon) ?"),
                                 tags$ul("4.	What type of Transmission, Fuel type do people prefers in their Mercedes cars, Also, how the preference changes over time?")),
                         tags$li("Audi vs Mercedes",
                                 tags$ul("1.	On average,for a given price range, which brand’s car provides better performance(MPG)?")),
                         tags$h3(tags$b("Methodology")),
                         tags$p("We will be visualizing various Uni-variate and Multivariate plots to understand the relationship between the variables. Also, we will draw some Time series-based charts to witness how people’s preference for what they want in their cars have changed over time")
                  )
                )
              )
      ),
      tabItem(tabName = "Audi",
              fluidPage(
                navbarPage("Option",
                           tabPanel("Comparison",
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarPanel("What you want to compare?",
                                                     selectInput("acvar",
                                                                 label="Select any one",
                                                                 choices = list("Popularity",
                                                                                "MPG",
                                                                                "Price",
                                                                                "Road Tax",
                                                                                "Fuel Type",
                                                                                "Transmission"),
                                                                 selected = "Popularity")),
                                        mainPanel(plotOutput("acbar"),htmlOutput("actext"))
                                        
                                      )
                                    )
                           ),
                           tabPanel("Preference",
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarPanel("Here, we will see people's preference based on the selected variable",
                                                     radioButtons("apvar",label="Select any one",
                                                                  choices=list("Fuel Type","Transmission"))),
                                        mainPanel(plotOutput("apbar1"),
                                                  plotOutput("apbar2"),
                                                  htmlOutput("aptext"))
                                      )
                                    )
                           ),
                           tabPanel("Bivariate Plots",
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarPanel("Some Bivariate plots to understand the relationships between the selected variables",
                                          selectInput("abvar",
                                                                 label="Select any one",
                                                                 choices = list("Price vs MPG",
                                                                                "Price vs Year",
                                                                                "Price vs Transmission",
                                                                                "Price vs Fueltype",
                                                                                "FuelType vs MPG",
                                                                                "Transmission vs MPG",
                                                                                "Year vs Tax"),
                                                                 selected = "Price vs MPG")),
                                        mainPanel(plotOutput("abbar"),htmlOutput("abtext"))
                                        
                                      )
                                    )
                           )
                )
              )),
      tabItem(tabName = "Mercedes",
              fluidPage(
                navbarPage("Option",
                           tabPanel("Comparison",
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarPanel("What you want to compare?",
                                                     selectInput("mcvar",
                                                                 label="Select any one:",
                                                                 choices = list("Popularity",
                                                                                "MPG",
                                                                                "Price",
                                                                                "Road Tax",
                                                                                "Fuel Type",
                                                                                "Transmission"),
                                                                 selected = "Popularity")),
                                        mainPanel(plotOutput("mcbar"),htmlOutput("mctext"))
                                      
                                      )
                                    )
                           ),
                           tabPanel("Preference",
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarPanel("Here we will see people's preference based on the selected variable",
                                                     radioButtons("mpvar",label="Select any one",
                                                                  choices=list("Fuel Type","Transmission"))),
                                        mainPanel(plotOutput("mpbar1"),
                                                  plotOutput("mpbar2"),
                                                  htmlOutput("mptext"))
                                      )
                                    )
                           ),
                           tabPanel("Bivariate Plots",
                                    fluidPage(
                                      sidebarLayout(
                                        sidebarPanel("Some Bivariate plots to understand the relationships between the selected variables",
                                          selectInput("mbvar",
                                                                 label="Select any one",
                                                                 choices = list("Price vs MPG",
                                                                                "Price vs Year",
                                                                                "Price vs Transmission",
                                                                                "Price vs Fueltype",
                                                                                "FuelType vs MPG",
                                                                                "Transmission vs MPG",
                                                                                "Year vs Tax"),
                                                                 selected = "Price vs MPG")),
                                        mainPanel(plotOutput("mbbar"),htmlOutput("mbtext"))
                                        
                                      )
                                    )
                           )
                )
              )
      ),
      tabItem(tabName ="AudivsMercedes",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(helpText("Here we will see a brief Brand wise comparison for the given price range:"),
                    sliderInput("slider1","Enter the Price Range",
                                min=0,max=150000,value=c(20000,40000),step = 5000)
                  ),
                  mainPanel(
                    p(h4("Here is a brief comparison between Audi and Mercedes is given:")),
                    p("For the year 2020, in the different Price ranges, which Brand provides a better Average MPG is shown here and also a model-wise average mpg comparison for those models which are available in the given price range ."),
                    uiOutput("ambar")
                  )
                )
              )
      ),
      tabItem(tabName = "Conclusion",
              fluidPage(
                fluidRow(
                  column(12,tags$h2(tags$b("Conclusion"),align="center")),
                  tags$p("From all the univariate and bivariate plots and charts I have reach to certain number of conclusion:"),
                  tags$ol(h4("Audi"),
                          tags$li("A3 model is the most popular model of the Audi Brand"),
                          tags$li("From the scatter plot it is clear that model like  A1,A3,A4,A6 comes with lower price and provide good mpg"),
                          tags$li("In 2020, people want Petrol as a Fuel Type in their Audi cars more than Diesel, But Hybrid is less preferred.
                                  we also see that before 2017, people used to prefer diesel Audi cars, But from 2017 onward people started preferring petrol Audi cars more than the diesel one"),
                          tags$li("Nowadays, people want Semi-automatic transmission in their Audi cars.
                                  we also see that before 2018,people used to prefer Manual Audi car, But from 2018 onward people started preferring Semi-automatic Audi car more than the manual one")),
                  
                  tags$ol(h4("Mercedes"),
                          tags$li("C Class is the most popular Model of Mercedes Brand."),
                          tags$li("Model like  A,B,C,E Class comes with lower price and provide good mpg"),
                          tags$li("E Class has the highest MPG where as G Class has the lowest MPG"),
                          tags$li("Nowadays, people want Automatic transmission in their Mercedes cars.
                                  we also see that before 2016,people used to prefer Automatic Mercedes car, But from 2016 onward people started preferring Semi-automatic Mercedes car more than the Automatic one")),
                  tags$ol(h4("Audi vs Mercedes"),
                          tags$li(p("In the last part of the project,we see that in some price ranges Audi brand is better where as in some other price ranges Mercedes is better."),
                                  p("For Example:-In the price range of 20000-40000 Euros, Mercedes Brand provides better average MPG and maximum MPG than Audi. The C class Model of Mercedes gives the maximum MPG which is above 55 miles per gallon whereas the A3 model of Audi gives only 50 mpg."),
                                  p("So one can use the double-ended range slider to give the price range as input and obtain the desire result.")))
                )
              ))
    )
  )
)



server <- function(input, output) {
  output$acbar=renderPlot({
    if(input$acvar=="Popularity"){
      model=data.frame(table(audi$model))
      colnames(model)=c("Model","Count")
      ggplot(model,aes(x=reorder(Model,+Count),y=Count,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Models Popularity")+xlab("Models")
      
    }
    else if (input$acvar=="MPG"){
      avg_mpg=aggregate(x=audi$mpg,by=list(audi$model),FUN=mean)
      colnames(avg_mpg)=c("Model","Avg_MPG")
      ggplot(avg_mpg,aes(x=reorder(Model,+Avg_MPG),y=Avg_MPG,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Model-wise Average mpg comparision")+xlab("Models")
      
    }
    else if (input$acvar=="Price"){
      data=subset(audi,year>2019)
      avg_price=aggregate(x=data$price,by=list(data$model),FUN=mean)
      colnames(avg_price)=c("Model","Price")
      ggplot(avg_price,aes(x=reorder(Model,+Price),y=Price,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Model-wise current Price comparison")+xlab("Models")
      
    }
    else if (input$acvar=="Road Tax"){
      data=subset(audi,year>2019)
      avg_tax=aggregate(x=data$tax,by=list(data$model),FUN=mean)
      colnames(avg_tax)=c("Model","Road_Tax")
      ggplot(avg_tax,aes(x=reorder(Model,+Road_Tax),y=Road_Tax,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("Models")
      
    }
    else if (input$acvar=="Fuel Type"){
      ggplot(audi,aes(model,..count..))+
        geom_bar(aes(fill=fuelType),position = "dodge")+
        theme(axis.text.x=element_text(angle=90,hjust=1))+
        labs(title="Model-wise preference of Fuel Type")
      
    }
    else if (input$acvar=="Transmission"){
      ggplot(audi,aes(model,..count..))+
        geom_bar(aes(fill=transmission),position = "dodge")+
        theme(axis.text.x=element_text(angle=90,hjust=1))+
        labs(title="Model-wise preference of Transmission")
    }
  })
  output$actext=renderUI({
    if (input$acvar=="Popularity"){
      "A3 is the most popular Model of Audi Brand."
    }
    else if(input$acvar=="MPG"){
      "A1 model has the highest MPG,where as R8 Model has the lowest MPG and it is obvious since R8 is a racing car."
    }
    else if(input$acvar=="Price"){
      "Based on the 2020 Data, A1 model have the lowest price and R8 model have the highest price."
    }
    else if(input$acvar=="Road Tax"){
      "Road tax are almost same for all the Model"
    }
    else if(input$acvar=="Fuel Type"){
      tags$li("Few Obsevations",
        tags$ul("* A1 Model with Petrol fuel type is the most preferred one."),
        tags$ul("* With Diesel fuel type, A4 Model is more preferable"),
        tags$ul("* People prefer diesel variant of A6 model much more than it's petrol variant"),
        tags$ul("* people likes petrol and diesel variant of Q3 Model equally"))
    }
    else if(input$acvar=="Transmission"){
      "People prefer manual transmission  in model like A1, A2, A4, Q2, Q3 where as semi-Automatic in model A6, Q5 and Automatic in Q7,A5 models"
    }
    })
  output$apbar1=renderPlot({
    if(input$apvar=="Fuel Type"){
      data=subset(audi,fuelType!="Hybrid")
      ggplot(data=data,aes(x=year,fill=fuelType))+geom_bar(position="dodge")+
        labs(title="Change of Fuel type preference over time")+geom_vline(xintercept = 2016.5,color="red",size=1)
    }
    else if(input$apvar=="Transmission"){
      ggplot(data=audi,aes(x=year,fill=transmission))+geom_bar(position = "dodge")+
        labs(title="Change of Transmission preference over time")+geom_vline(xintercept = 2017.5,color="red",size=1)
    }
  })
  output$aptext=renderUI({
    if (input$apvar=="Fuel Type"){
      tags$li("Few Observations",
        tags$ul("Nowadays, people want Petrol as Fuel Type in their Audi cars more than Diesel,But Hybrid is less preferred"),
        tags$ul("we see also that before 2017,people used to prefer diesel Audi car, But from 2017 onward people started preferring petrol Audi car more than the diesel one")
      )
    }
    else{
      tags$li("Few Observations",
              tags$ul("Nowadays, people want Semi-automatic transmission in their Audi cars"),
              tags$ul("we also see that before 2018,people used to prefer Manual Audi car, But from 2018 onward people started preferring Semi-automatic Audi car more than the manual one")
      )
      
    }
  })
  output$apbar2=renderPlot({
    if(input$apvar=="Fuel Type"){
      data=subset(audi,year>2019)
      fueltype=data.frame(table(data$fuelType))
      colnames(fueltype)=c("Fuel_Type","Count")
      ggplot(fueltype,aes(x=Fuel_Type,y=Count,fill=Fuel_Type))+
        geom_bar(stat="identity")+labs(title="Fuel Type Preference in 2020")
    }
    else if(input$apvar=="Transmission"){
      data=subset(audi,year>2019)
      fueltype=data.frame(table(data$transmission))
      colnames(fueltype)=c("Transmission","Count")
      ggplot(fueltype,aes(x=Transmission,y=Count,fill=Transmission))+
        geom_bar(stat="identity")+labs(title="Transmission Preference in 2020") 
    }
  })
  output$abbar=renderPlot({
    if(input$abvar=="Price vs MPG"){
      price_mpg=aggregate(x=as.data.frame(cbind(audi$price,audi$mpg)),by=list(audi$model),FUN=mean)
      colnames(price_mpg)=c("Model","Price","MPG")
      ggplot(data=price_mpg,aes(Price,MPG))+geom_point()+geom_text_repel(aes(label=Model))
    }
    else if(input$abvar=="Price vs Year"){
      price_year=aggregate(x=audi$price,by=list(audi$year),FUN=mean)
      colnames(price_year)=c("Year","Price")
      ggplot(price_year,aes(x=Year,y=Price,fill=Year))+geom_bar(stat="identity")
      
    }
    else if(input$abvar=="Price vs Fueltype"){
      price_fuel=aggregate(x=audi$price,by=list(audi$fuelType),FUN=mean)
      colnames(price_fuel)=c("FuelType","Price")
      ggplot(price_fuel,aes(x=FuelType,y=Price,fill=FuelType))+geom_bar(stat="identity")
      
    }
    else if(input$abvar=="Price vs Transmission"){
      price_trans=aggregate(x=audi$price,by=list(audi$transmission),FUN=mean)
      colnames(price_trans)=c("Transmission","Price")
      ggplot(price_trans,aes(x=Transmission,y=Price,fill=Transmission))+geom_bar(stat="identity")
      
    }
    else if(input$abvar=="FuelType vs MPG"){
      fuel_mpg=aggregate(x=audi$mpg,by=list(audi$fuelType),FUN=mean)
      colnames(fuel_mpg)=c("FuelType","MPG")
      ggplot(fuel_mpg,aes(x=FuelType,y=MPG,fill=FuelType))+geom_bar(stat="identity")
    }
    else if(input$abvar=="Transmission vs MPG"){
      trans_mpg=aggregate(x=audi$mpg,by=list(audi$transmission),FUN=mean)
      colnames(trans_mpg)=c("Transmission","MPG")
      ggplot(trans_mpg,aes(x=Transmission,y=MPG,fill=Transmission))+geom_bar(stat="identity")
    }
    else{
      tax_year=aggregate(x=audi$tax,by=list(audi$year),FUN=mean)
      colnames(tax_year)=c("Year","TAX")
      ggplot(tax_year,aes(x=Year,y=TAX,fill=Year))+geom_bar(stat="identity")
    }
  })
  output$abtext=renderUI({
    if(input$abvar=="Price vs MPG"){
      "It is clear from the scatter plot that model like  A1,A3,A4,A6 comes with relatively low price and provide good mpg"
    }
    else if(input$abvar=="Price vs Year"){
      
    }
    else if(input$abvar=="Price vs Transmission"){
      "Semi-Automatic and Automatic Transmission comes with higher price range with respect to manual" 
    }
    else if(input$abvar=="Price vs Fueltype"){
      "Hybrid Audi car are little bit more expensive compare to other." 
    }
    else if(input$abvar=="FuelType vs MPG"){
      "Hybrid cars are more efficient"
    }
    else if(input$abvar=="Transmission vs MPG"){
      "Manual cars are slightly more efficient in terms of mpg"
    }
  })
  
  output$mcbar=renderPlot({
    if(input$mcvar=="Popularity"){
      model=data.frame(table(merc$model))
      colnames(model)=c("Model","Count")
      ggplot(model,aes(x=reorder(Model,+Count),y=Count,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Models Popularity")+xlab("Models")
      
    }
    else if (input$mcvar=="MPG"){
      avg_mpg=aggregate(x=merc$mpg,by=list(merc$model),FUN=mean)
      colnames(avg_mpg)=c("Model","Avg_MPG")
      ggplot(avg_mpg,aes(x=reorder(Model,+Avg_MPG),y=Avg_MPG,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Model-wise Average mpg comparision")+xlab("Models")
      
    }
    else if (input$mcvar=="Price"){
      data=subset(merc,year>2019)
      avg_price=aggregate(x=data$price,by=list(data$model),FUN=mean)
      colnames(avg_price)=c("Model","Price")
      ggplot(avg_price,aes(x=reorder(Model,+Price),y=Price,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Model-wise current Price comparison")+xlab("Models")
      }
    else if (input$mcvar=="Road Tax"){
      data=subset(merc,year>2019)
      avg_tax=aggregate(x=data$tax,by=list(data$model),FUN=mean)
      colnames(avg_tax)=c("Model","Road_Tax")
      ggplot(avg_tax,aes(x=Model,y=Road_Tax,fill=Model))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1))
      
    }
    else if (input$mcvar=="Fuel Type"){
      data=subset(merc,fuelType!="Other")
      ggplot(data,aes(model,..count..))+
        geom_bar(aes(fill=fuelType),position = "dodge")+
        theme(axis.text.x=element_text(angle=90,hjust=1))+
        labs(title="Model-wise preference of Fuel Type")
      
    }
    else if (input$mcvar=="Transmission"){
      data=subset(merc,transmission!="Other")
      ggplot(data,aes(model,..count..))+
        geom_bar(aes(fill=transmission),position = "dodge")+
        theme(axis.text.x=element_text(angle=90,hjust=1))+
        labs(title="Model-wise preference of Transmission")
    }
  })
  output$mctext=renderUI({
    if (input$mcvar=="Popularity"){
      "C Class is the most popular Model of Mercedes Brand."
    }
    else if(input$mcvar=="MPG"){
      "E Class has the highest MPG where as G Class has the lowest MPG"
    }
    else if(input$mcvar=="Price"){
      "Based on the 2020 Data, B Class have the lowest price and G Class have the highest price"
    }
    else if(input$mcvar=="Road Tax"){
      "Road tax are almost same for all the Model except X class"
    }
    else if(input$mcvar=="Fuel Type"){
      "People prefer Diesel fuel Type in almost all the model"
    }
    else if(input$mcvar=="Transmission"){
      "People prefer Semi-Automatic transmission in almost all the model"
    }
  })
  output$mpbar1=renderPlot({
    if(input$mpvar=="Fuel Type"){
      data=subset(merc,fuelType!="Other")
      ggplot(data=data,aes(x=year,fill=fuelType))+geom_bar(position="dodge")+
        labs(title="Change of Fuel type preference over time")
    }
    else if(input$mpvar=="Transmission"){
      data=subset(merc,transmission!="Other")
      ggplot(data=data,aes(x=year,fill=transmission))+geom_bar(position = "dodge")+
        labs(title="Change of Transmission preference over time")+geom_vline(xintercept = 2015.5,color="red",size=1)
    }
  })
  output$mptext=renderUI({
    if (input$mpvar=="Fuel Type"){
      tags$li("Few Observations",
              tags$ul("Nowadays, people equally prefer petrol and diesel as Fuel Type,But Hybrid is less preferred"),
              tags$ul("we see that people prefer diesel Mercedes car from the beginning, But from 2019 people started preferring petrol Mercedes as well"))
    }
    else{
      tags$li("Few Observations",
              tags$ul("Nowadays, people want Automatic transmission in their Mercedes cars"),
              tags$ul("we also see that before 2016,people used to prefer Automatic Mercedes car, But from 2016 onward people started preferring Semi-automatic Mercedes car more than the Automatic one")
      )
      
    }
  })
  
  output$mpbar2=renderPlot({
    if(input$mpvar=="Fuel Type"){
      data=subset(merc,(year>2019 )& (fuelType!="Other"))
      fueltype=data.frame(table(data$fuelType))
      colnames(fueltype)=c("Fuel_Type","Count")
      ggplot(fueltype,aes(x=Fuel_Type,y=Count,fill=Fuel_Type))+
        geom_bar(stat="identity")+labs(title="Fuel Type Preference in 2020")
    }
    else if(input$mpvar=="Transmission"){
      data=subset(merc,(year>2019 )& (transmission!="Other"))
      fueltype=data.frame(table(data$transmission))
      colnames(fueltype)=c("Transmission","Count")
      ggplot(fueltype,aes(x=Transmission,y=Count,fill=Transmission))+
        geom_bar(stat="identity")+labs(title="Transmission Preference in 2020") 
    }
  })
  
  output$mbbar=renderPlot({
    if(input$mbvar=="Price vs MPG"){
      price_mpg=aggregate(x=as.data.frame(cbind(merc$price,merc$mpg)),by=list(merc$model),FUN=mean)
      colnames(price_mpg)=c("Model","Price","MPG")
      ggplot(data=price_mpg,aes(Price,MPG))+geom_point()+geom_text_repel(aes(label=Model))
    }
    else if(input$mbvar=="Price vs Year"){
      price_year=aggregate(x=merc$price,by=list(merc$year),FUN=mean)
      colnames(price_year)=c("Year","Price")
      ggplot(price_year,aes(x=Year,y=Price,fill=Year))+geom_bar(stat="identity")
      
    }
    else if(input$mbvar=="Price vs Fueltype"){
      price_fuel=aggregate(x=merc$price,by=list(merc$fuelType),FUN=mean)
      colnames(price_fuel)=c("FuelType","Price")
      ggplot(price_fuel,aes(x=FuelType,y=Price,fill=FuelType))+geom_bar(stat="identity")
      
    }
    else if(input$mbvar=="Price vs Transmission"){
      price_trans=aggregate(x=merc$price,by=list(merc$transmission),FUN=mean)
      colnames(price_trans)=c("Transmission","Price")
      ggplot(price_trans,aes(x=Transmission,y=Price,fill=Transmission))+geom_bar(stat="identity")
      
    }
    else if(input$mbvar=="FuelType vs MPG"){
      fuel_mpg=aggregate(x=merc$mpg,by=list(merc$fuelType),FUN=mean)
      colnames(fuel_mpg)=c("FuelType","MPG")
      ggplot(fuel_mpg,aes(x=FuelType,y=MPG,fill=FuelType))+geom_bar(stat="identity")
    }
    else if(input$mbvar=="Transmission vs MPG"){
      trans_mpg=aggregate(x=merc$mpg,by=list(merc$transmission),FUN=mean)
      colnames(trans_mpg)=c("Transmission","MPG")
      ggplot(trans_mpg,aes(x=Transmission,y=MPG,fill=Transmission))+geom_bar(stat="identity")
    }
    else{
      tax_year=aggregate(x=merc$tax,by=list(merc$year),FUN=mean)
      colnames(tax_year)=c("Year","TAX")
      ggplot(tax_year,aes(x=Year,y=TAX,fill=Year))+geom_bar(stat="identity")
    }
  })
  output$mbtext=renderUI({
    if(input$mbvar=="Price vs MPG"){
      "It is clear from the scatter plot that model like  A,B,C,E Class comes with relatively low price and provide good mpg"
    }
    else if(input$mbvar=="Price vs Year"){
     
    }
    else if(input$mbvar=="Price vs Fueltype"){
      "Petrol Mercedes car are little bit more expensive compare to other."  
    }
    else if(input$mbvar=="Price vs Transmission"){
     "Semi-Automatic and Automatic Transmission comes with higher price range with respect to manual"
    }
    else if(input$mbvar=="FuelType vs MPG"){
      "Hybrid cars are more efficient"
    }
    else if(input$mbvar=="Transmission vs MPG"){
     "Manual cars are slightly more efficient in terms of mpg"
    }
  })
  
  output$myplot=renderPlot({
    dataa=subset(audi,price>=input$slider1[1] & price<=input$slider1[2] & year>2019)
    datam=subset(merc,price>=input$slider1[1] & price<=input$slider1[2] & year>2019)
    model_mpg_audi=aggregate(x=dataa$mpg,by=list(dataa$model),FUN=mean)
    model_mpg_merc=aggregate(x=datam$mpg,by=list(datam$model),FUN=mean)
    
    model_mpg_audi$Brand=c(rep("Audi"))
    model_mpg_merc$Brand=c(rep("Mercedes"))
    colnames(model_mpg_audi)=c("Model","avg_mpg","Brand")
    colnames(model_mpg_merc)=c("Model","avg_mpg","Brand")
    
    
    a=ggplot(model_mpg_audi,aes(x=Model,y=avg_mpg))+
      geom_bar(stat="identity",fill="#F8766D")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+
      labs(title="Audi")
    
    b=ggplot(model_mpg_merc,aes(x=Model,y=avg_mpg))+
      geom_bar(stat="identity",fill="#00BFC4")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+
      labs(title="Mercedes")
    
    tab=data.frame(rbind(mean(model_mpg_audi$avg_mpg),mean(model_mpg_merc$avg_mpg)))
    tab=cbind(c("Audi","Merc"),tab)
    colnames(tab)=c("Brand","Avg_MPG")
    c=ggplot(tab,aes(x=Brand,y=Avg_MPG,fill=Brand))+geom_bar(stat="identity")+
      labs(title=paste("Avgerage MPG in ",input$slider1[1],"-",input$slider1[2]," Euros"))+
      theme(text=element_text(size=8))
    
    tab=data.frame(rbind(max(model_mpg_audi$avg_mpg),max(model_mpg_merc$avg_mpg)))
    tab=cbind(c("Audi","Merc"),tab)
    colnames(tab)=c("Brand","Max_MPG")
    d=ggplot(tab,aes(x=Brand,y=Max_MPG,fill=Brand))+geom_bar(stat="identity")+
      labs(title=paste("Maximum MPG in ",input$slider1[1],"-",input$slider1[2]," Euros"))+ theme(text=element_text(size=8))
    ggarrange(a,b,c,d,ncol=2,nrow=2)
  })
  output$ambar=renderUI({
    dataa=subset(audi,price>=input$slider1[1] & price<=input$slider1[2] & year>2019)
    datam=subset(merc,price>=input$slider1[1] & price<=input$slider1[2] & year>2019)
    if(dim(dataa)[1]==0||dim(datam)[1]==0){
       p(h4("NO CARS ARE AVAILABLE IN THIS PRICE RANGE!"))
    }
    else{
      plotOutput("myplot")
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)