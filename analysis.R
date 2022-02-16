library(tidyverse)
library(ggplot2)
library(zoo)
library(directlabels)
library(scales)

load('examdata.RData')

# ------- Question 1 ---------------------------------------------------------------------------
# first case of covid
min(df_cases$date)

states_aus=c("nsw","vic","qld","sa", "wa", "tas","nt","act","australia")
x <- c("mean", "std", "min", "Q1", "median", "max")
dscrb <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(dscrb) <- x
for(i in states_aus){

  five_number_summary <- df_cases %>% 
    summarise_at(all_of(i),
                 list(mean=~ mean(., na.rm = TRUE), std=~ sd(., na.rm = TRUE), min=min, Q1=~quantile(., probs = 0.25),
                      median=median, max=max) , )
  dscrb[i,] <- five_number_summary
}
dscrb


 # moving average with window of 7 days
my_data <- df_cases %>% mutate(roll_vic=rollapply(vic,7,mean,align='left',fill=NA),
                          roll_nsw=rollapply(nsw,7,mean,align='left',fill=NA))

pivote_NewCases <- my_data[,c("date",'roll_vic','roll_nsw')]  %>%
  pivot_longer(cols = - date, names_to =  "location", values_to = "cases")
pivote_NewCases$location <- toupper(sub(".*_", "", pivote_NewCases$location))



pivote_NewCases %>% ggplot(aes(x = date, y = cases, color = location, group = location)) + geom_line( size = 1)+
  geom_hline(aes(yintercept=20), linetype=2 , linetype='dotted',  col = 'red')+
  geom_text(aes(min(date),20,label = 20, vjust =-0.3 ), col='red')+
  xlab("Date \n * Crossing the horizontal line 20 implies a wave") +
  ylab("Number of daily new Cases")+
  ggtitle("Number of daily new Cases for NSW and VIC")+
  scale_y_continuous(n.breaks = 10)


# ---------- Question 2 ----------------------------------------------------------------------------

 states_percent <- c('percent_vic','percent_nsw','percent_qld','percent_act','percent_wa','percent_sa','percent_nt','percent_tas') 
# proper data for plot by making pivot table
pivot_vaccination_state <- df[,c("date",states_percent)]  %>%
  pivot_longer(cols = - date, names_to =  "location", values_to = "cases")

# plot vaccination rate according to second dose ratio (just above 12 year old(12+))
pivot_vaccination_state$location <- toupper(sub(".*_", "", pivot_vaccination_state$location))
pivot_vaccination_state %>% ggplot(aes(x = date, y = cases, color = location, group = location)) + geom_line()+
  labs(y='Rate of Vaccination' , x= 'Date') +
   geom_dl(aes(label = location), method = list(dl.combine("last.points")), cex = 1.3)+ # labeling each line
  ggtitle("Rate of Vaccination for above 12 year old in states of Australia")+
  scale_y_continuous(n.breaks = 10)

ggplot(df, aes(x=date, y=percent_australia)) + geom_line(col= 'red',size=2)+
  labs(y='Rate of Vaccination' , x= 'Date')+
  ggtitle("Rate of Vaccination for above 12 year old in Australia")+
  scale_y_continuous(n.breaks = 10)


# predicting eventhough was not needed !
df %>%  ggplot(aes(x=date,y = percent_australia)) + geom_point(colour='red')+
 # geom_line(aes(y = aus), colour="red")+
  scale_y_continuous(limits = c(0,100),n.breaks = 10)+ 
  scale_x_date(date_breaks = "3 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2021-03-23','2022-05-20')))+
  stat_smooth(method="lm",fullrange=TRUE)+labs(y='Rate of Vaccination' , x= 'Date')+
  ggtitle("Prediction of Vaccination Rate for 12+ in Australia")
  

# trun the question table to dataframe
pop_df = data.frame(location=c("nsw","vic","qld","sa", "wa", "tas","nt","act","australia"),
                       population=c(8176.4,6648.6,5206.4,1771.7,2675.8,542.0,247.0,431.8,25704.3),
                    stringsAsFactors = FALSE)

# make pivot and join (broadcasting by location will happend)
pivot_vac_pop <- df[,c("date",pop_df[['location']])]  %>%
  pivot_longer(cols = - date, names_to =  "location", values_to = "cases")
pivot_vac_pop <- pivot_vac_pop %>% full_join(pop_df, by="location")
# df vaccinated per population
pivot_vac_pop[,'pop_prc'] <- (pivot_vac_pop['cases'] * 100 / pivot_vac_pop['population']) / 1000

# plot vaccination rate according to diving vaccinated by population ratio (all population)
pivot_vac_pop$location <- toupper(pivot_vac_pop$location)
pivot_vac_pop[pivot_vac_pop[,'location'] != 'AUSTRALIA',] %>% ggplot(aes(x = date, y = pop_prc, color = location, group = location)) + geom_line()+
  labs(x='Date',y='Percent of Vaccination ') + scale_y_continuous(n.breaks = 10)+
  geom_dl(aes(label = location), method = list(dl.combine("last.points")), cex = 1.3)+
  ggtitle("Rate of Vaccination for all population in states of Australia")

# plot vaccination rate according to diving vaccinated by population ratio (for all population)
pivot_vac_pop[pivot_vac_pop[,'location'] == 'AUSTRALIA',] %>% ggplot(aes(x = date, y = pop_prc, color = location, group = location)) + geom_line(size=2,col='red')+
  labs(x='Date',y='Percent of Vaccination ') +scale_y_continuous(limits = c(0,100),n.breaks = 10)+
  ggtitle("Rate of Vaccination for all population in Australia")

# ---------------------- Question 3 -----------------------------------------------------------
#  relationship between vaccination rates and the number of new cases

case_and_vaccination <- df[,c("date",states_percent,'percent_australia')] %>% full_join(df_cases, by="date")
# Chage null to zero value (many row are zero because at start of covid there was not any vaccination )
case_and_vaccination[is.na(case_and_vaccination)] <- 0

# matrix of correlation for desired states
case_vac_cor <- case_and_vaccination[-1]%>% cor
case_vac_cor <- case_vac_cor[states_aus, c(states_percent,'percent_australia')]

case_vac_cor %>% reshape2::melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  coord_fixed() + labs(y = 'Rate of vaccination', x = 'New Covid Cases per day')+
  ggtitle("Relationship between cases & Vaccination")


# Approach second --> weekly
df_cases_ <- df_cases
#  relationship between vaccination rates and the number of new cases by week (option by=7)
df_cases_[-1] <- lapply(df_cases_[states_aus], function(x){rollapply(x,7,sum,align='left',fill=NA,by=7)})
df_cases_ <- df_cases_ %>% drop_na()

#join cases and vaccination so we have vaccination for the time of given summation case
df_cases_<- df_cases_ %>% inner_join(df[c('date',states_percent,'percent_australia')], by="date")
df_cases_[is.na(df_cases_)] <- 0
case_vac_cor_week <- df_cases_[-1]%>% cor
case_vac_cor_week <- case_vac_cor_week[states_aus, c(states_percent,'percent_australia')]

case_vac_cor_week %>% reshape2::melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  coord_fixed() + labs(y = 'Rate of vaccination', x = 'New Covid Cases per week')+
  ggtitle("Relationship between cases & Vaccination")


selected_state <- c('vic','nsw')

plt <- ggplot(case_and_vaccination)
for(i in as.list(selected_state)){
plt <- plt+ geom_point(aes_string(x = i, y = paste('percent_',i,sep=''), color=shQuote(i))) 
}
plt+ ggtitle("Relatioship with scatter plot")+ labs(x='Cases', y= 'Vaccinatin Rate')



# ----------------------------------------------------------------------------------------

rm(list=ls())


