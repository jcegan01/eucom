#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(shiny)
#library(shinyWidgets)


theme_set( theme_bw() + theme( legend.position="bottom" ) +
             theme( legend.title=element_blank() ) )

# load data and assumptions pre calculated in pre_proc.Rmd
load( 'app.RData' )

# sidebar text: R1 average
popcols <- list('people_vaccinated_per_hundred' = 'of population vaccinated (1+ doses) in '
                , 'people_vaccinated_per_hundredover16' = 'of population over 16 years-old vaccinated (1+ doses) in ' )
datesdfavg <- datesdfavg %>% mutate( day = as.numeric(format(date, format='%d'))
                                     , dlabel = paste0(ifelse(day>20,'late',ifelse(day>10,'mid','early')),format(date, format=' %B')) )
r1text <- '<p>Projections for vaccinations in FEMA Region 1: </p> <ul>'
for( r in 1:nrow(datesdfavg) ){
  r1text <- paste0(r1text, '<li>', datesdfavg$threshold[r], '% ', popcols[[datesdfavg$colref[r]]]
                   , datesdfavg$dlabel[r], '</li>' )
}
r1text <- paste(r1text, '</ul>')

# UI

ui <- fluidPage(
  # Application title
  titlePanel(paste0("EUCOM Vaccinations (", latest_history_date, " data)")),
    tabsetPanel(
      tabPanel('Ukraine', panel1() ), 

      tabPanel('Other country',
               sidebarLayout(
                 sidebarPanel( tags$style(type = "text/css", "label { font-size: 13px; }" ),
                               radioButtons( 'varP', 'Select the variable for projection:',
                                             choices = list('Percentage of population vaccinated (1+ doses)' = 'people_vaccinated_per_hundred'
                                                            , 'Percentage of population over 16 years-old vaccinated (1+ doses)' = 'people_vaccinated_per_hundredover16'),
                                             selected = 'people_vaccinated_per_hundred', width = NULL ),
                               h1("____________"), HTML( r1text )
                 ),
                 mainPanel( plotOutput("distProjection", height = "550px") )
               ) ) # closing of tab with Immunization thresholds
    ) # closing of tabsetPanel
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  stateInput <- reactive({
    # This reactive function is executed every time the state changes
    scp <- sc %>% filter( location == input$state )
    
    # Default growth in daily vaccinations
    b <- max( c(dvacgr$b[dvacgr$location==input$state], 0) )
    if ( input$state %in% c('Connecticut','Massachusetts','Rhode Island') )
      bm <- 2000
    else # ME, NH or VT
      bm <- 1000
    
    updateSliderInput(session, 'growth', value=round(b), min=-bm, max=bm )
    
    for ( i in 1:7 ){
      updateNumericInput(session,paste0('N',i), value=scp[i,'Ni'], min=0, max=scp[i,'Ni']*2 )
      updateSliderInput(session, paste0('C',i), value=scp[i,'Ci'], min=0, max=scp[i,'Ci']*2 )
    }
    return( scp )
  })
  
  output$distPlot <- renderPlot({
    scp <- stateInput()
    
    for ( i in 1:7 ){
      scp[i,'Ni'] <- input[[paste0('N',i)]]
      scp[i,'Ci'] <- input[[paste0('C',i)]]
    }
    
    scp <- scp %>% mutate( cap = Ni * Ci )
    
    dt <- dp %>% filter( location==input$state )
    # update max capacity
    sumcap <- sum( scp$cap )
    dt[dt$s=='cap','value'] <- sumcap
    
    # update forecasted growth in daily vaccinations
    g <- input$growth # input$growthslider
    if ( !is.null(g) ){
      a <- dt$value[dt$date==latest_history_date & dt$s=='daily_vaccinations']
      d <- cumsum( c(a, rep(g,sum(dt$date>latest_history_date & dt$s=='daily_vaccinations')) ) )
      dt[dt$date>latest_history_date & dt$s=='daily_vaccinations','value'] <- d[-1]
    }
    # find data where demand exceeds capacity, to draw a warning sign
    tmp <- dt %>% filter( s=='daily_vaccinations' & value>=sumcap )
    
    if ( nrow(tmp) > 0 ){
      exceed_date <- min( tmp$date )
      ymax <- max(dt$value)
      ggplot( dt, aes(x=date, y=value, color=n) ) +
        geom_line( size=1.5 ) +
        geom_vline(xintercept=latest_history_date, linetype="dashed", color="gray", size=1) +
        annotate("text", x=exceed_date-6, y=sumcap + ymax*.05, label=paste('Vaccination reaches\ncapacity on',exceed_date)) +
        theme(axis.title.x = element_blank()) +
        scale_x_date(date_labels = '%b%e' ) +
        ylab('Doses per Day') +
        scale_y_continuous(labels = scales::comma
                           , expand = c(0, ymax*0.05), limits = c(0, NA)) +
        ggtitle( input$state ) +
        theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20) )
    }
    else
      ggplot( dt, aes(x=date, y=value, color=n) ) +
      geom_line( size=1.5 ) +
      geom_vline(xintercept=latest_history_date, linetype="dashed", color="gray", size=1) +
      theme(axis.title.x = element_blank()) +
      scale_x_date(date_labels = '%b%e' ) +
      ylab('Doses per Day') +
      scale_y_continuous(labels = scales::comma
                         , expand = c(0, max(dt$value)*0.05), limits = c(0, NA)) +
      ggtitle( input$state ) +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20) )
  }) # closing of distPlot()
  
  output$distPlot100k <- renderPlot({
    if ( input$var == 'total_vaccinations_per_hundred' )
      yl <- 'Total Number of Doses Administered per Hundred People'
    else
      yl <- 'Percentage of Population Vaccinated (1+ doses)'
    ggplot( dpc, aes_string(x='date', y=input$var, color='type') ) +
      facet_wrap('~ location', nrow=2 ) +
      geom_line( size=1.5 ) +
      scale_color_manual( name=NULL, values=c('red','grey'), breaks=c('value','avgr1')
                          , labels=c('Total in state','Region 1 average') ) +
      geom_vline(xintercept=latest_history_date, linetype="dashed", color="gray", size=1) +
      theme(axis.title.x = element_blank()) +
      scale_x_date(date_labels = '%b%e' ) +
      ylab( yl ) +
      scale_y_continuous( expand=c(0,0) ) + # label=scales::unit_format(unit="%", sep=""), 
      theme( text = element_text(size=20) )
  }) # closing of distPlot100k()
  
  output$distProjection <- renderPlot({
    if ( input$varP == 'people_vaccinated_per_hundred' )
      yl <- 'Percentage of Population Vaccinated (1+ doses)'
    else
      yl <- 'Percentage of Population over 16 Vaccinated (1+ doses)'
    #        labels <- datesdf %>% mutate( label = paste0(threshold,'%: ',format(!!as.name(input$varP), format='%b%e')) )
    labels <- datesdf %>% mutate( day = as.numeric(format(!!as.name(input$varP), format='%d'))
                                  , label = paste0(threshold,'%: ',ifelse(day>20,'late ',ifelse(day>10,'mid ','early ')),format(!!as.name(input$varP), format='%b')) )
    ggplot( dpp %>% filter( !!as.symbol(input$varP) <= 100 ), aes_string(x='date', y=input$varP, color='type') ) +
      facet_wrap(~ location, nrow=2 ) +
      geom_vline(xintercept=latest_history_date, linetype="dashed", color="gray", size=1) +
      geom_line( size=1.5 ) +
      geom_point(data=datesdf, aes_string(x=input$varP, y='threshold'), shape=21, color='black', fill='white', size=3, stroke=0.5 ) +
      geom_label(data=labels, aes_string(x=input$varP, y='threshold', label='label'), hjust=1.1, vjust=0, color='black' ) +
      scale_color_manual( name=NULL, values=c('blue','grey'), breaks=c('value','avgr1')
                          , labels=c('Total in state','Region 1 average') ) +
      theme(axis.title.x = element_blank()) +
      scale_x_date(date_labels = '%b%e', minor_breaks='month', limits=c(as.Date('2021-01-01'),as.Date('2021-09-15')) ) +
      ylab( yl ) +
      scale_y_continuous( #expand=c(0,10), 
        breaks=seq(10, 100, 20), label=scales::unit_format(unit="%", sep="") ) + 
      theme( text = element_text(size=20) )
    
  }) # closing of distProjection()
}

# Run the application 
shinyApp(ui = ui, server = server)
