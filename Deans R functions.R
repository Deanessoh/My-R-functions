# Dean Essoh Assignment 3.2

#3.2.1 
#Answer


# Remind_me function

remind_me <- function(){
  print("Familiy birthdays")
  x <- c("Mam = 17.08","Dad = 06.03","Ben = 22.01",
                    "Ciara = 24.01", "Noah = 20.01")
  return(x)
}



# Cheat function

cheat<- function(x){
  
  if(x == "Q_3_1_2"){
  
  
  df <- read.csv("schiphol_data.csv")
                 
                 y <- df$TMIN
                 x<- df$DATE
              myplot<- plot(x, y, xlab = "Time", ylab ="Temparature")
return(myplot)
  }   else if(x == "Q_3_1_3"){install.packages("titanic")
    library("titanic")
    
    mydata <- titanic_train
    
    myplot <- ggplot( mydata, aes(Sex, fill = factor(Survived))) +
      geom_bar() +
      labs(fill = "How did it go?") +
      scale_fill_discrete(labels=c("dead", "alive"))
    return(myplot) 
  } else if(x == "Q_3_1_7"){data(cars)
   myplot <- ggplot(cars,aes(speed,dist)) + 
      geom_point() +
      geom_smooth(method = 'loess', formula = 'y ~ x')
    return(myplot)
  }
}











#3.2.2
#Answer

install.packages("devtools")
library(devtools)
install_github("djnavarro/jasmines")
library(jasmines)
library(dplyr)



make_art <- function(random_seed){
  art_work<- use_seed(random_seed) %>% 
    scene_discs(
      rings = 12, 
      points = 50000, 
      size = 40
    ) %>%
    mutate(ind = 1:n()) %>%
    unfold_warp(
      iterations = 10,
      scale = .5, 
      output = "layer" 
    ) %>%
    unfold_tempest(
      iterations = 5,
      scale = .01
    ) %>%
    style_ribbon(
      color = "#E0542E98",
      colour = "ind",
      alpha = c(1,1),
      background = "#4D718610"
    )
  return(art_work)
  
  
  
}



