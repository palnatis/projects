library('ggplot2')
library('lubridate')
library('dplyr')
library('leaflet')
library('stringi')
library('htmltools')
library('shiny')
library('shinythemes')
library("tidyr")
library("caret")


#To Load Data
df <- read.csv("Crimes_2020.csv", header = TRUE)

ChicagoCrimes_2020 <- as.data.frame(df)%>% select(Case.Number, Date, Primary.Type, Location.Description,Latitude,Longitude,Arrest)

#Converting Month, TimeOfDay, and Date from date 
ChicagoCrimes_2020$Month     <- months(fast_strptime(ChicagoCrimes_2020$Date, "%m/%d/%Y %H:%M"))
ChicagoCrimes_2020$TimeOfDay <- hour(strptime(ChicagoCrimes_2020$Date, "%m/%d/%Y %H:%M"))
ChicagoCrimes_2020$Date      <- as.Date(ChicagoCrimes_2020$Date, format = "%m/%d/%Y")



#removing redendant rows
ChicagoCrimes_2020 <- distinct(ChicagoCrimes_2020, Case.Number, .keep_all= TRUE)


#Removing rows which has NA values
ChicagoCrimes_2020 <- ChicagoCrimes_2020[complete.cases(ChicagoCrimes_2020),]

#converting Primary.Type to character string
ChicagoCrimes_2020$Primary.Type <- as.character(ChicagoCrimes_2020$Primary.Type)
ChicagoCrimes_2020$ArrestType <- as.numeric(ChicagoCrimes_2020$Arrest)


#Making all kinds of crimes into following category

#Making all related to NARCOTICS and OTHER NARCOTIC VIOLCATION TO NARCOTICS
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$Primary.Type %in% c('NARCOTICS', 'OTHER NARCOTIC VIOLATION'),
                                           "NARCOTICS", ChicagoCrimes_2020$Primary.Type)

#making all 'ROBBERY', 'THEFT', 'BURGLARY', 'MOTOR VEHICLE THEFT'  into THEFT 
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$Primary.Type %in% c('ROBBERY', 'THEFT', 'BURGLARY', 'MOTOR VEHICLE THEFT'),
                                           "THEFT", ChicagoCrimes_2020$PrimaryTypeTB)

#making all  'PUBLIC PEACE VIOLATION', 'INTERFERENCE WITH PUBLIC OFFICER' 
#,'LIQUOR LAW VIOLATION', 'STALKING', 'GAMBLING', 'INTIMIDATION', 'OBSCENITY', 
#'NON-CRIMINAL', 'PUBLIC INDECENCY', 'NON-CRIMINAL (SUBJECT SPECIFIED) INTO 
#'"NON VIOLENCE"

ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('PUBLIC PEACE VIOLATION', 'INTERFERENCE WITH PUBLIC OFFICER'
                                                                                   ,'LIQUOR LAW VIOLATION', 'STALKING', 'GAMBLING',
                                                                                   'INTIMIDATION', 'OBSCENITY', 'NON-CRIMINAL', 'PUBLIC INDECENCY',
                                                                                   'NON-CRIMINAL (SUBJECT SPECIFIED)'),"NON VIOLENCE", ChicagoCrimes_2020$PrimaryTypeTB)

#making all 'CRIM SEXUAL ASSAULT', 'SEX OFFENSE', 'PROSTITUTION' INTO SEXUAL
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('CRIM SEXUAL ASSAULT', 'SEX OFFENSE', 'PROSTITUTION')
                                           , "SEXUAL", ChicagoCrimes_2020$PrimaryTypeTB)

#making all 'WEAPONS VIOLATION', 'OFFENSE INVOLVING CHILDREN', 'KIDNAPPING', 'CONCEALED CARRY LICENSE VIOLATION', 'HUMAN TRAFFICKING' INTO  "VIOLENCE"
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('WEAPONS VIOLATION', 'OFFENSE INVOLVING CHILDREN', 'KIDNAPPING',
                                                                                   'CONCEALED CARRY LICENSE VIOLATION', 'HUMAN TRAFFICKING'), "VIOLENCE", ChicagoCrimes_2020$PrimaryTypeTB)

#Changing big column name to small column names
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('CRIMINAL DAMAGE'), "DAMAGE", ChicagoCrimes_2020$PrimaryTypeTB)
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('CRIMINAL TRESPASS'), "TRESPASS", ChicagoCrimes_2020$PrimaryTypeTB)
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('DECEPTIVE PRACTICE'), "DECEPTIVE", ChicagoCrimes_2020$PrimaryTypeTB)
ChicagoCrimes_2020$PrimaryTypeTB <- ifelse(ChicagoCrimes_2020$PrimaryTypeTB %in% c('OTHER OFFENSE'), "OTHERS", ChicagoCrimes_2020$PrimaryTypeTB)


#Converting their type to title cases
ChicagoCrimes_2020$PrimaryTypeTB <- stri_trans_totitle(ChicagoCrimes_2020$PrimaryTypeTB)
ChicagoCrimes_2020$Location.Description <- stri_trans_totitle(ChicagoCrimes_2020$Location.Description)


Frequency_of_crimetype_by_month <- ChicagoCrimes_2020 %>% group_by(PrimaryTypeTB,Month) %>% summarise(Freq = n(),.groups = 'drop')
Crimes_on_map_by_date <- ChicagoCrimes_2020 %>% group_by(PrimaryTypeTB,Location.Description) %>% summarise(Freq = n(),.groups = 'drop')
PeaktimesofCrimes <- ChicagoCrimes_2020 %>% group_by(PrimaryTypeTB, TimeOfDay) %>% summarise(Freq = n(),.groups = 'drop')
Month <- sort(factor(rev(unique(ChicagoCrimes_2020$Month)),levels=month.name))
Location <- names(rev(sort(table(ChicagoCrimes_2020$Location.Description)))[1:15])



newDf <-  as.data.frame(ChicagoCrimes_2020) %>% select(ArrestType,PrimaryTypeTB,Latitude,Longitude)

  
size <- floor(0.75 * nrow(newDf))
index <- sample(seq_len(nrow(newDf)), size = size)
trainSet <- newDf[index, ]
testSet <- newDf[-index,]


#Building the model
model <- train(ArrestType ~ . , data=trainSet, method="lm")

#predicting the model
preds <- predict(model, testSet)

#if value is greater than 0.5
predictArrestType <- as.numeric(preds > 0.5)
accuracy <- mean(testSet$ArrestType == predictArrestType)

#computing Mean Square Error and Root Mean Square Error
mse <- mean((testSet$ArrestType - predictArrestType)^2)
rmse <- sqrt(mse)

#writing the Coinfusion Matrix for prediction
expected_value <- factor(testSet$ArrestType)
predicted_value <- factor(predictArrestType)
cm <- confusionMatrix(data=predicted_value, reference = expected_value)


ui <- fluidPage(
 
  
  # Tab layout defining with a input and output definitions
  navbarPage("Crimes of Chicago_2020",
             tabPanel("Frequency of crime by month & Crime type", selectInput("MonthInput", "Select month", choices = month),
                       mainPanel( plotOutput("Frequency_of_crimetype_by_month"))),

             tabPanel("Location of crimes by date on a map", dateInput("dateInputforMap", "Select Date", value = "2020-01-01",format = "yyyy-mm-dd",min = "2020-01-01", max = "2020-12-31"),
                       mainPanel( leafletOutput("Map",width = "200%", height = 800))),
             
             tabPanel("A heatmap using the type of the crime and the hour of the day when the crime was committed",
                      mainPanel(plotOutput("PeaktimesofCrimes")))

           ),
           theme = shinytheme("cerulean"),
           fluidRow(
             HTML("<style> #show_values {height: 100px; width: 20px;} </style>"),
             HTML('<div style="position: relative; top: -36px; left: 30px;">'),
             HTML("</div>")
             
           ) # fluidRow
)

server <- function(input, output, session) {
  
  output$Frequency_of_crimetype_by_month <- renderPlot({
    plot_data1 <- filter(Frequency_of_crimetype_by_month, Month == input$MonthInput) %>% select(PrimaryTypeTB, Freq)
    ggplot(plot_data1, aes(x=PrimaryTypeTB, y=Freq))  + geom_bar(stat="identity", width=0.8, fill="cyan") +
      geom_text(aes(label=format(as.numeric(Freq),nsmall = 0, big.mark = ",")), vjust=1.6, color="black", size=3.5) +
      labs(x = 'Crime Type', y = 'Frequency') + ggtitle('Frequency of Crime by Month & Type') +
      theme(plot.title = element_text(size = 20,face = 'bold', hjust = 0.5),axis.text.x = element_text(size=10,angle = 90, vjust = 0.5, hjust=1))
  })

  output$Map <- renderLeaflet({
    plot_data3 <- filter(ChicagoCrimes_2020, Date == input$dateInputforMap) %>% select(PrimaryTypeTB, Location.Description, Latitude,
                                                                                       Longitude, TimeOfDay)
    label <- lapply(seq(nrow(plot_data3)), function(i) {
      paste0( '<p>','<b> Crime Type: ', plot_data3[i, "PrimaryTypeTB"] ,'</b>', '<p></p>',
              '<b> Time of the day: ', plot_data3[i, "TimeOfDay"] , '</b>', '<p></p>',
              '<b> Location : ', plot_data3[i, "Location.Description"], '</b> ', '</p>' )
    })
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = plot_data3,lng = ~Longitude, lat = ~Latitude, label =lapply(label, htmltools::HTML),
                 labelOptions = lapply(1:nrow(plot_data3), function(x) {
                   labelOptions(opacity=0.9)
                 }))
  })

  output$PeaktimesofCrimes <- renderPlot({
    ggplot(PeaktimesofCrimes, aes(x=PrimaryTypeTB, y=TimeOfDay, fill = Freq )) +
      geom_tile()+ scale_fill_gradient('Freq', low = 'cyan', high = 'green') +
      geom_text(aes(label =format(as.numeric(Freq),nsmall = 0, big.mark = ",") )) + scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24)) +
      labs(x = 'Crime Type', y = 'Time of the day') + ggtitle('Crime Type vs Time of the day') +
      theme(plot.title = element_text(size = 20, face = 'bold', hjust = 0.5),axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust=1))
  })
  
}
shinyApp(ui = ui, server = server)
