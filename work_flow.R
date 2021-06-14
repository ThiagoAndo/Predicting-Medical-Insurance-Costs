
library(tidyverse)  # data manipulation

library(ggplot2)    # visualization

library(readr)      # data manipulation

library(caret)      # Machine Learning

library(mgcv)       # Machine Learning



files <- dir("data", full = T)


if(length(files)==0 ){
   warning("FIRST YOU MUST CREATE A NEW R PROJEC THEN COPY THE FILES DOWNLOADED FROM GIT HUB AND PAST ALL INTO THE NEW PROJECT FOLDER") }

medical_data <- read_csv(files, col_types = "nnfnnffn") 

medical_data <- medical_data %>%
   mutate(charges = exp(logcharges))%>% 
   select(- logcharges)






#--------------------------------------------------------------------------------------------


# Data cleaning and pre-processing

paste0("There are ",sum(duplicated(medical_data)), " duplicated observations") 

visdat::vis_miss(medical_data)

#--------------------------------------------------------------------------------------------


#Data Analysis

# The skim  providing a strong set of summary statistics that are generated for a variety of different data types.
skimr::skim(medical_data)


summary(medical_data)




ggplot(medical_data, aes(region))+
   geom_bar()+
   geom_text(aes(label = ..count..), stat = "count", vjust = -.3, colour = "black",
             fontface = "bold", size = 4) +
   xlab("Regions") +
   ylab("Count") +
   ggtitle("Count of Regions")+
   theme_minimal()+
   theme(
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 17),
      axis.text.x = element_text(size=10, face="bold", colour = "black"),
      legend.position = "none")



ggplot(medical_data, aes(children))+
   geom_bar()+
   geom_text(aes(label = ..count..), stat = "count", vjust = -.3, colour = "black",
             fontface = "bold", size = 4) +
   xlab("Number of Children") +
   ylab("Count") +
   ggtitle("Count of the Incidences of the Number of Children")+
   theme_minimal()+
   theme(
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 17),
      axis.text.x = element_text(size=10, face="bold", colour = "black"),
      legend.position = "none")





ggplot(medical_data, aes(y = charges, x = as.factor(children)))+
   geom_violin()+ 
   geom_boxplot(width=0.1)+
   scale_y_continuous(labels=scales::dollar_format())+
   theme_minimal()+
   xlab("Number of children covered by health insurance") +
   ylab("Charges") +
   ggtitle("Distribution of Charges Among the Numbers of children covered")+
   theme(
      plot.title = element_text(size = 15),
      axis.text.x=element_text(size = 12, color="black"),
      strip.text.x = element_text(size = 14))


ggplot(medical_data, aes(charges))+
   geom_density()+
   facet_grid()+
   xlab("Charges") +
   ylab("Density") +
   ggtitle("Distribution of Charges")+
   scale_x_continuous(labels=scales::dollar_format())+
   theme_minimal()+
   theme(
      plot.title = element_text(size = 15))


ggplot(medical_data, aes(bmi, fill = sex))+
   geom_density(alpha = .5)+
   scale_fill_manual(name="Gender",values=c("#EC2C73","#FFDD30"))+
   facet_grid()+
   xlab("Charges") +
   ylab("Density") +
   ggtitle("Distribution of Body Mass Index ")+
   theme_minimal()+
   theme(
      plot.title = element_text(size = 15))



ggplot(medical_data, aes(y = charges, x = region, color = region))+
   geom_violin(trim=TRUE)+ 
   geom_boxplot(width=0.1)+
   facet_grid(cols = vars(smoker),labeller = label_both)+
   scale_y_continuous(labels=scales::dollar_format())+
   theme_minimal()+
   xlab("Region") +
   ylab("Charges") +
   ggtitle("Distribution of Charges Among Regions and Smoking Status")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14),
      legend.position = "none")


ggplot(medical_data , aes(x = smoker, y = charges, color = age))+
   geom_point(alpha = .5, size  = 4.5)+
   scale_color_viridis_c(option = "plasma")+
   geom_boxplot(alpha = .3)+
   scale_y_continuous(labels=scales::dollar_format())+
   theme_dark()+
   xlab("Smoker") +
   ylab("Charges") +
   ggtitle("Distribution of Charges Between Smoking Status and Age")+
   theme(
      plot.title = element_text(size = 15),
      axis.text.x=element_text(size = 12, color="black"),
      strip.text.x = element_text(size = 14))



ggplot(medical_data, aes(x = age, y = charges, color = smoker))+
   geom_point(alpha=.4,size=4)+  
   facet_grid(cols = vars(smoker),labeller = label_both)+
   scale_color_manual(name="Gender",values=c("#EC2C73","#FFDD30"))+
   geom_smooth(method = "lm",se = FALSE, color = "Black")+
   theme_minimal()+
   xlab("Age") +
   ylab("Charges") +
   ggtitle("Distribution of Charges Among Regions and Smoking Status")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14),
      legend.position = "none")



ggplot(medical_data, aes(x = bmi, y = charges, color = smoker))+
   geom_point(alpha=4/5,size=4)+
   scale_y_continuous(labels = scales::dollar) +
   geom_smooth(method = "lm",se = F)+
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Body mass index") +
   ylab("Charges") +
   ggtitle("Body mass index vs Charges")+
   theme(
      plot.title = element_text(size = 15),
      axis.text.x=element_text(size = 12, color="black"),
      strip.text.x = element_text(size = 14))




#Splitting Data


set.seed(341)

index <- createDataPartition(medical_data$charges, p = 0.75, list = FALSE)

train_full <- medical_data[index, ]


test_full  <- medical_data[-index, ]



set.seed(341)
model_normal_g <- train(
   log(charges) ~ age + sex + bmi + children+ smoker + region, 
   train_full,
   method = "gam",
   trControl = trainControl(
      method = "cv", 
      number = 5,
      verboseIter = FALSE
   )
)
model_normal_g$results[2,c(3,4)]



set.seed(341)
test_full <- test_full %>%
   mutate( pred_full = predict(model_normal_g,test_full),
           pred_full = exp(pred_full)) 



ggplot(test_full, aes(x = charges, y = pred_full, color = smoker ))+
   geom_point(alpha=.7,size=4)+
   geom_abline(color = "blue")+
   scale_y_continuous(labels = scales::dollar) +
   scale_x_continuous(labels = scales::dollar) +
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Actual Charges") +
   ylab("Predicted Charges") +
   labs(title = "Predicted Charges vs Actual Charges",
        subtitle = "Whole Data Model Predictions")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14))



train_full_smokers <- train_full %>%
   filter(smoker == "yes")

set.seed(341)
model_smoker_g <- train(
   log(charges) ~ age + sex + bmi + children+ smoker + region, 
   train_full_smokers,
   method = "gam",
   trControl = trainControl(
      method = "cv", 
      number = 5,
      verboseIter = FALSE
   )
)


model_smoker_g$results[2,c(3,4)]



set.seed(341)

test_full <- test_full %>%
   mutate( pred_sep = ifelse(smoker == "yes", 
                             predict(model_smoker_g,test_full), NA),
           pred_sep = exp(pred_sep))

test_full <- test_full %>%
   mutate( pred_sep = ifelse(smoker == "yes",pred_sep, pred_full))


postResample(pred = test_full$pred_sep, 
             obs = test_full$charges)







test_full %>%
   filter(smoker == "no")%>%
   ggplot( aes(x = charges, y = pred_full ))+
   geom_point(alpha=.7,size=4)+
   geom_abline(color = "blue")+
   scale_y_continuous(labels = scales::dollar)+
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Actual Charges") +
   ylab("Predicted Charges") +
   labs(title = "Predicted Charges vs Actual Charges For no Smokers",
        subtitle = "Whole Data Model Predictions")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14))+
   scale_x_continuous(breaks=seq(0,40000, by=5000),labels = scales::dollar)+
   coord_cartesian(ylim = c(0, 20000))




test_full %>%
   filter(smoker == "yes")%>%
   ggplot(aes(x = charges, y = pred_full ))+
   geom_point(alpha=.7,size=4)+
   scale_y_continuous(labels = scales::dollar)+
   scale_x_continuous(labels = scales::dollar)+
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Actual Charges") +
   ylab("Predicted Charges") +
   labs(title = "Predicted Charges vs Actual Charges For Smokers",
        subtitle = "Whole Data Model")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14))+
   geom_abline(color = "blue")


test_full %>%
   filter(smoker == "yes")%>%
   ggplot(aes(x = charges, y = pred_sep ))+
   geom_point(alpha=.7,size=4)+
   scale_y_continuous(labels = scales::dollar)+
   scale_x_continuous(labels = scales::dollar)+
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Actual Charges") +
   ylab("Predicted Charges") +
   labs(title = "Predicted Charges vs Actual Charges For Smokers",
        subtitle = "Smoker Data Model")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14))+
   geom_abline(color = "blue")




ggplot(test_full, aes(x = charges, y = pred_sep, color = smoker ))+
   geom_point(alpha=.7,size=4)+
   geom_abline(color = "blue")+
   scale_y_continuous(labels = scales::dollar) +
   scale_x_continuous(labels = scales::dollar) +
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Actual Charges") +
   ylab("Predicted Charges") +
   labs(title = "Predicted Charges vs Actual Charges For Smokers",
        subtitle = "Mixed Predictions")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14))




###Random Forest

set.seed(341)
model_ranger <- train(
   charges ~ age + sex + bmi + children+ smoker + region,
   tuneLength = 3,
   data = train_full, 
   method = "ranger",
   trControl = trainControl(
      method = "cv", 
      number = 5, 
      verboseIter = FALSE
   )
)
model_ranger$results[which.min(model_ranger$results$RMSE),c("RMSE","Rsquared")]




set.seed(341)
test_full <- test_full %>%
   mutate( pred_full = predict(model_ranger,test_full)) 


postResample(pred = test_full$pred_full, 
             obs = test_full$charges)




ggplot(test_full, aes(x = charges, y = pred_full, color = smoker ))+
   geom_point(alpha=.7,size=4)+
   geom_abline(color = "blue")+
   scale_y_continuous(labels = scales::dollar) +
   scale_x_continuous(labels = scales::dollar) +
   scale_color_manual(name="Smoker",values=c("#EC2C73","#FFDD30"))+
   theme_minimal()+
   xlab("Actual Charges") +
   ylab("Predicted Charges") +
   labs(title = "Predicted Charges vs Actual Charges",
        subtitle = "Whole Data Model Predictions")+
   theme(
      plot.title = element_text(size = 15),
      strip.text.x = element_text(size = 14))




