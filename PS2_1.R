#Author:SUNTAOTAO
#Date:20201016
#Liyuan explained to me how to return the results.
#TA explained to me that "for in" can't be used with tible.

library(tidyr)
library(dplyr)
library(ggplot2)

#1.1Read the .txt (or .tsv) file (signif.txt) with R
Eqs_Data <- read.table(file = "signif.txt", sep = "\t",header = TRUE,stringsAsFactors = F,quote = "")
head(Eqs_Data)

#1.1Covert to a tibble object named Sig_Eqs
Sig_Eqs <- as_tibble(Eqs_Data)
Sig_Eqs

#1.2Compute the total number of deaths caused by earthquakes since 2150 B.C. in each country
Sig_Eqs  %>% 
  filter(YEAR >= -2150) %>% 
  select(COUNTRY,DEATHS) %>% 
  #filter(DEATHS != 'NA') %>%
  group_by(COUNTRY) %>% 
  summarize(Eqs_Deaths_Cty = sum(DEATHS,na.rm = T)) %>% 
  arrange(desc(Eqs_Deaths_Cty)) %>% 
  head(10)

#1.3Compute the total number of earthquakes with magnitude larger than 6.0
Eqs_Plot <- Sig_Eqs  %>% 
  select(YEAR,EQ_PRIMARY) %>% 
  filter(EQ_PRIMARY>6) %>% 
  group_by(YEAR) %>% 
  summarize(n = n()) %>% 
  # Make the plot 
  ggplot(aes(x=YEAR, y=n)) + 
  geom_line()
ggsave("Eqs_Plot.png",Eqs_Plot,height = 6,width = 15,units = "cm")

#1.4Write a function CountEq_LargestEq that returns both
#(1) the total number of earthquakes since 2150 B.C. in a given country AND 
#(2) the date of the largest earthquake ever happened in this country.
CountEq_LargestEq <- function(Country_Name){
  results <- c(Sig_Eqs  %>% 
    select(COUNTRY,YEAR,MONTH,DAY,EQ_PRIMARY) %>% 
    filter(COUNTRY == Country_Name) %>%
        mutate(Eqs_Num_Cty=n(),
           Eqs_Max=max(EQ_PRIMARY,na.rm=T)) %>%
    #仅保留最大震级的一行数据
    filter(EQ_PRIMARY==Eqs_Max) %>%
    #mutate(Eq_Max_Date=as.Date(c(YEAR,MONTH,DAY)),"%Y%m%d") %>%
    mutate(Eq_Max_Date=paste(YEAR,MONTH,DAY,sep = "-"))%>%
      select(COUNTRY,Eqs_Num_Cty,Eqs_Max,Eq_Max_Date) %>%
      head(1)) 
  return(results)
}

#Get the country names and delete the duplicated ones
Country_All <- Sig_Eqs  %>% 
  select(COUNTRY)
na.omit(Country_All)
Country_Names<-unique(Country_All)
print(typeof(Country_Names))


#Apply CountEq_LargestEq to each country,
#report your results in a descending order.
Eqs_Num_Result <-list()
#不能直接写Name_i in Country_Names(是tible 格式不能 for in)，所以要直接提取列
for(Name_i in Country_Names$COUNTRY){
  #print(Name_i)
  Eqs_Num_Add <- CountEq_LargestEq(Name_i)
  #print(Eqs_Num_Add_i)
  Eqs_Num_Result <-rbind(Eqs_Num_Result,Eqs_Num_Add)
}

write.table(Eqs_Num_Result,"PS2_1_report.csv",sep="," )
