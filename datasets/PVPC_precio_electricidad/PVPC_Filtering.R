library("readxl")
library(tidyverse)

get_date_from_name = function(file_name) {
  new_file_name = file_name %>% str_replace("PVPC_DETALLE_DD_", "")
  new_file_name = new_file_name %>% str_replace(".xls", "")
  
  year = substr(new_file_name, 1, 4)
  month = substr(new_file_name, 5, 6)
  day = substr(new_file_name, 7, 8)
  
  return(paste(year,"/",month,"/",day, sep=""))
}

x = list.files(path=".")

global_data <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(global_data) <- c("Fecha", "Hora", "PVPC", "Dia")

model1 = x[1:2618]
progressBar = txtProgressBar(min = 1, max = length(model1), initial = 1) 
for (i in 1:length(model1)) {
  dataset <- suppressMessages(read_excel(model1[i]))
  dataset <- as.data.frame(dataset)
  dataset <- dataset[,1:5]
  dataset <- dataset[-c(1,2,3),]
  dataset <- na.omit(dataset)

  colnames(dataset) = c("Fecha", "Hora", "Peaje", "Periodo", "PVPC")
  
  dataset <- dataset %>% filter(Peaje == "2.0.DHS")
  rownames(dataset) <- NULL
  dataset <- dataset %>% mutate(Fecha=get_date_from_name(model1[i]))
  dataset <- dataset %>% mutate(Dia=weekdays(as.Date(Fecha)))
  dataset <- dataset %>% select(-c(Peaje, Periodo))
  
  global_data <- rbind(global_data, dataset)
  
  setTxtProgressBar(progressBar,i)
}

model2 = x[2619:3122]
progressBar = txtProgressBar(min = 1, max = length(model2), initial = 1) 
for (i in 1:length(model2)) {
  dataset <- suppressMessages(read_excel(model2[i]))
  dataset <- as.data.frame(dataset)
  dataset <- dataset[,1:5]
  dataset <- dataset[-c(1,2,3),]
  dataset <- na.omit(dataset)
  
  colnames(dataset) = c("Fecha", "Hora", "Peaje", "Periodo", "PVPC")
  
  dataset <- dataset %>% filter(Peaje == "2.0TD")
  rownames(dataset) <- NULL
  dataset <- dataset %>% mutate(Fecha=get_date_from_name(model2[i]))
  dataset <- dataset %>% mutate(Dia=weekdays(as.Date(Fecha)))
  dataset <- dataset %>% select(-c(Peaje, Periodo))
  
  global_data <- rbind(global_data, dataset)
  
  setTxtProgressBar(progressBar,i)
}

