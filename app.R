library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(zoo)
library(reshape2)

load('examdata.RData')

   # rol_num <- 7
# state <- c('vic','nsw')

  # states <- c('vic','nsw','qld','act','wa','sa','nt','tas') 
ploti <- function(state, rol_num){

  df_cases[-1] <- lapply(df_cases[state], function(x){rollapply(x,as.numeric(rol_num),mean,align='left',fill=NA)})

  pivote_NewCases <- df_cases[,c("date",state)]  %>%
    pivot_longer(cols = - date, names_to =  "location", values_to = "cases")
  pivote_NewCases$location <- toupper(sub(".*_", "", pivote_NewCases$location))
  
  plt <- pivote_NewCases %>% ggplot(aes(x = date, y = cases, color = location, group = location)) + geom_line( size = 1)+
    geom_hline(aes(yintercept=20), linetype=2 , linetype='dotted',  col = 'red')+
    geom_text(aes(min(date),20,label = 20, vjust =-0.3 ), col='red')+
    xlab("Date \n * Cross the plot to horizontal line at 20 case implies the wave") +
    ylab("Number of daily new Cases") 
return(plt)
  
  # plt <- ggplot(df_cases[c("date",state)])
  # for(i in state){
  #   plt <- plt+ geom_line(aes_string(x = "date", y = i, color=shQuote(i))) 
  # }
  # 
  # plt+ 
  #   geom_hline(aes(yintercept=20), linetype=2 , linetype='dotted',  col = 'red')+
  #   geom_text(aes(min(date),20,label = 20, vjust =-0.3 ), col='red')+
  #   xlab("Date \n * Cross the plot to horizontal line at 20 case implies the wave") +
  #   ylab("Number of Covid Cases")
}


# states <- c('vic','nsw','qld','act','wa','sa','nt','tas')
# states_percent <- c('percent_vic','percent_nsw','percent_qld','percent_act','percent_wa','percent_sa','percent_nt','percent_tas') 

plot_rate_vac_12above <- function(states_percent){
  
    # proper data for plot by making pivot table
  pivot_vaccination_state <- df[,c("date",states_percent)]  %>%
    pivot_longer(cols = - date, names_to =  "location", values_to = "cases")
  
  # plot vaccination rate according to second dose ratio (just above 12 year old(12+))
  pivot_vaccination_state$location <- toupper(sub(".*_", "", pivot_vaccination_state$location))
  plt <- pivot_vaccination_state %>% ggplot(aes(x = date, y = cases, color = location, group = location)) + 
    geom_line()+ ggtitle("Vaccination Rate for 12+")+
       ylab("Vaccination percent") + scale_y_continuous(limits = c(0,100))
  return(plt)
}

pop_df = data.frame(location=c("nsw","vic","qld","sa", "wa", "tas","nt","act","australia"),
                    population=c(8176.4,6648.6,5206.4,1771.7,2675.8,542.0,247.0,431.8,25704.3),
                    stringsAsFactors = FALSE)
  # make pivot and join (broadcasting by location will happend)
pivot_vac_pop <- df[,c("date",pop_df[['location']])]  %>%
  pivot_longer(cols = - date, names_to =  "location", values_to = "cases")
pivot_vac_pop <- pivot_vac_pop %>% full_join(pop_df, by="location")
# df vaccinated per population
pivot_vac_pop[,'pop_prc'] <- (pivot_vac_pop['cases'] * 100 / pivot_vac_pop['population']) / 1000
pivot_vac_pop$location <- toupper(pivot_vac_pop$location)

plot_rate_vac_population <- function(stas){
  
  states <- toupper(sub(".*_", "", stas))
    # plot vaccination rate according to diving vaccinated by population ratio (all population)
  plt <- pivot_vac_pop[pivot_vac_pop[,'location'] == states,] %>% ggplot(aes(x = date, y = pop_prc, color = location, group = location)) + 
    geom_line()+ ggtitle("Vaccination Rate for all Population")+
    ylab("Vaccination percent") +scale_y_continuous(limits = c(0,100))
    return(plt)
}

heatmap_daily <- function(wk,stats_pr){

  state <- sub(".*_", "", stats_pr)
  if(is.null(wk)){
    case_and_vaccination <- df[,c("date",stats_pr)] %>% full_join(df_cases[c('date',state)], by="date")
    # Chage null to zero value (many row are zero because at start of covid there was not any vaccination )
    case_and_vaccination[is.na(case_and_vaccination)] <- 0
    
    # matrix of correlation for desired states
    case_vac_cor <- case_and_vaccination[-1]%>% cor
  } else{

    df_cases_ <- df_cases
    #  relationship between vaccination rates and the number of new cases by week (option by=7)
    df_cases_[-1] <- lapply(df_cases_[state], function(x){rollapply(x,7,sum,align='left',fill=NA,by=7)})
    df_cases_ <- df_cases_ %>% drop_na()
    
    #join cases and vaccination so we have vaccination for the time of given summation case
    df_cases_<- df_cases_ %>% inner_join(df[c('date',stats_pr)], by="date")
    df_cases_[is.na(df_cases_)] <- 0
    case_vac_cor <- df_cases_[-1]%>% cor
  }
  if(length(stats_pr) > 1){
      case_vac_cor <- case_vac_cor[state, c(stats_pr)]
    }
  case_vac_cor %>% melt() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile()+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    coord_fixed() + labs(x = 'Rate of vaccination', y = 'New Covid Cases')
}

scatter_plt <- function(states){
  # states <- c('percent_vic','percent_nsw', 'percent_wa')
  state <- sub(".*_", "", states)
  case_and_vaccination <- df[c("date",states)] %>% full_join(df_cases[c("date",state)], by="date")
  plt <- ggplot(case_and_vaccination)
  
  for(i in state){
  plt <- plt+ geom_point(aes_string(x = i, y = paste('percent_',i,sep=''), color=shQuote(i))) 
  }
  
  plt+ ggtitle("Relatioship with scatter plot")+ labs(x='Cases', y= 'Vaccinatin Rate')
}

#-------------------------- Dashboard ---------------------------------------

ui <- dashboardPage(
  # header
  dashboardHeader(title = "Covid-19 in Australia"),
  # side menu
  dashboardSidebar( sidebarMenu(
    menuItem("Covid Cases in States", tabName = "cases"),
    menuItem("Vaccination", tabName = "vaccin"),
    menuItem("Relationship Cases & Vaccin", tabName = "rel"))),
  # dashboard body with three stacked panels
  dashboardBody( tabItems(
    # three separate tabs / pages
    tabItem("cases", fluidRow(h1("Plot"),plotOutput(outputId = "cases")), 
                    fluidRow( column(3, checkboxGroupInput("check1", label = h3("STATES"), 
                              choices = list("Victoria" = 'vic', "NSW" = 'nsw'), selected = 'nsw')),
                              column(9, sliderInput("slider", label = h3("Slider"), min = 3, max = 35,step = 2, value = 9))),
                    fluidRow(column(7, wellPanel(p("Plot indicate the number of daily new covid cases. Whenever the line of each state crosses the horizontal line of 20, a new wave occurs. Three waves are apparent from the VIC trend. During the first wave, NSW stands above VIC, but VIC indicates substantially more daily new cases than NSW in two other waves.")), offset = 3))),
    
    tabItem("vaccin", fluidRow(column(6,plotOutput(outputId = "vac_above12")),column(6,plotOutput(outputId = "vac_pop"))),
                    fluidRow( column(3, checkboxGroupInput("check2", label = h3("STATES"), 
                              choices = list("Victoria" = 'percent_vic', "NSW" = 'percent_nsw',"QLD" = 'percent_qld', "ACT" = 'percent_act',"WA" = 'percent_wa',
                                             "SA" = 'percent_sa',"NT" = 'percent_nt', "TAS" = 'percent_tas', "AUSTRALIA"='percent_australia'), selected = 'percent_australia'))),
                    fluidRow(wellPanel(p("From about April 2021, the vaccination program was started for every people above 12 years old. The plot shows a steep slop during in second half of the year. it implies that about 10% of people over 12 got second dose by July and more than 80% got vaccinated by end of the year. The right plot indicate vaccination trend for all population (not only above 12).")), offset = 2)),
                    #fluidRow(column(4, wellPanel(p("Width 4 offset 6")), offset = 4))),
    
    tabItem("rel", fluidRow(column(6,plotOutput(outputId = "vac1")),column(6,plotOutput(outputId = "vac2"))),
            fluidRow( column(3, checkboxGroupInput("week", label = h3("Per week"), 
                                                   choices = list("Weekly" = 'week'), selected = 0))),
            fluidRow( column(3, checkboxGroupInput("check3", label = h3("STATES"), 
                                                   choices = list("Victoria" = 'percent_vic', "NSW" = 'percent_nsw',"QLD" = 'percent_qld', "ACT" = 'percent_act',"WA" = 'percent_wa',
                                                                  "SA" = 'percent_sa',"NT" = 'percent_nt', "TAS" = 'percent_tas', "AUSTRALIA"='percent_australia'), selected = 'percent_australia')),
                      column(7, fluidRow(wellPanel(p("The heatmap plot shows a positive relationship between the number of daily cases and the vaccination rate in some states. It implies that by increasing the vaccination rate, the number of patients also rises, which shows that the vaccination could not immediately impact the number of cases. The scatter plot confirms the heatmap and suggests that for NSW and VIC, there was a growing number of cases by increasing the vaccination rate, but the trend gets reversed by 50% vaccination rate.")), offset = 2))))
  ))
)
# no changes to server function
server <- function(input, output) {
  output$cases <- renderPlot({ 
ploti(input$check1, input$slider)
    })
  output$vac_above12 <- renderPlot({ 
plot_rate_vac_12above(input$check2)
    })
  output$vac_pop <- renderPlot({ 
    plot_rate_vac_population(input$check2)
  })
  output$vac1 <- renderPlot({ heatmap_daily(input$week,input$check3) })
  output$vac2 <- renderPlot({ scatter_plt(input$check3) })
}
# you don't need to store the result
shinyApp(ui = ui, server = server)

