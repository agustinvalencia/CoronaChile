rm(list=ls())
library(ggplot2)
path = "who_covid_19_sit_rep_time_series.csv"
raw = read.csv2(path, sep = ",", header = T, check.names = F)

# disaggregating data for better handling
countries = raw$`Country/Region`
data = raw[,4:ncol(raw)]
data[is.na(data)] = 0
dates = as.Date(colnames(data), format = "%m/%d/%y")
colnames(data) = c()

drop_initial_zeros = function(x){
    for (i in 1:length(x)) {
        if(x[i] != 0){
            return(x[i:length(x)])
        }
    }
}

# Chile
chile_data <- read.csv2("corona_data.csv", sep = ",", header = TRUE)
chile <- cumsum(chile_data$NewCases)
n = length(chile)

# Others
spain   = drop_initial_zeros(as.numeric(data[which(countries == "Spain"),   ]))
france  = drop_initial_zeros(as.numeric(data[which(countries == "France"),  ]))
canada  = drop_initial_zeros(as.numeric(data[which(countries == "Canada"),  ]))
finland = drop_initial_zeros(as.numeric(data[which(countries == "Finland"), ]))
germany = drop_initial_zeros(as.numeric(data[which(countries == "Germany"), ]))
italy   = drop_initial_zeros(as.numeric(data[which(countries == "Italy"),   ]))
brazil   = drop_initial_zeros(as.numeric(data[which(countries == "Brazil"),   ]))

spain   = spain   [1:40]  
france  = france  [1:40]  
canada  = canada  [1:40]  
finland = finland [1:40]  
germany = germany [1:40]  
italy   = italy   [1:40]  
brazil  = brazil  [1:40]  

ggplot() + ggtitle("Progreso Corona Virus") +
    geom_line(aes(x=1:length(chile),   y=chile,   color="chile")) +
    geom_line(aes(x=1:length(spain),  y=spain,  color="spain")) +
    geom_line(aes(x=1:length(italy),   y=italy,   color="italy")) +
    geom_line(aes(x=1:length(brazil),   y=brazil,   color="brazil")) +
    geom_point(aes(x=1:length(chile),   y=chile,   color="chile")) +
    geom_point(aes(x=1:length(spain),  y=spain,  color="spain")) +
    geom_point(aes(x=1:length(italy),   y=italy,   color="italy")) +
    geom_point(aes(x=1:length(brazil),   y=brazil,   color="brazil")) +
    xlab("Day") + ylab("Cases")








ggplot() + ggtitle("Progreso Corona Virus") +
    geom_point(aes(x=1:length(france),  y=france,  color="france")) +
    geom_point(aes(x=1:length(spain),  y=spain,  color="spain")) +
    geom_point(aes(x=1:length(canada),  y=canada,  color="canada")) +
    geom_point(aes(x=1:length(germany),  y=germany,  color="germany")) +
    geom_point(aes(x=1:length(chile),   y=chile,   color="chile")) +
    geom_point(aes(x=1:length(finland), y=finland, color="finland")) +
    geom_line(aes(x=1:length(france),  y=france,  color="france")) +
    geom_line(aes(x=1:length(spain),  y=spain,  color="spain")) +
    geom_line(aes(x=1:length(canada),  y=canada,  color="canada")) +
    geom_line(aes(x=1:length(germany),  y=germany,  color="germany")) +
    geom_line(aes(x=1:length(chile),   y=chile,   color="chile")) +
    geom_line(aes(x=1:length(finland), y=finland, color="finland")) +
    xlab("Day") + ylab("Cases")
