library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(readxl)
library(plotly)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(sass)
library(leaflet)
library(tmap)

`%notin%` <- Negate(`%in%`)
#### Carregando arquivos #####
load(file = 'Estudantes.rdata')

load(file = 'Estudantes_RA.Rdata')

load(file = 'mapars.rdata')

load('new_groups.rdata')



#### Carregando funções #####

source('Filtros_Laterais.R', encoding = 'UTF-8')
source('tabs.R', encoding = 'UTF-8')



#### Fazendo as escolhas #####


dbHeader <- dashboardHeader(title = tags$a(href='https://www.unb.br/',
                                           "Perfil do Estudante"))


ui <-  tags$html(
  tags$head( 
    tags$style(sass(sass_file('D:/Users/Public/Documents/Docs/shiny-text-mining/www/styles.scss')))
  ),
  dashboardPage(
    skin = "green",
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
        menuItem("Página Inicial", tabName = "analises", icon = icon("chart-line")),
        menuItem("Análise Sócio Economica", tabName = "sobre", icon = icon("chart-pie")),
        menuItem("Mapa", tabName = "mapas", icon = icon("globe-americas")))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "analises", fluidRow(
          filtros_lateral_inicial(),
          
          box(class = 'floating-box',
              tabsetPanel(inicial_ui()),
              width = 9
          ))
          
        ),
        tabItem(tabName = 'sobre',fluidRow(
          
          filtros_lateral_raca_cor(),
          box(class='floating-box',
              tabsetPanel(
                Raca_cor_ui()
              ),width = 9))),
        
        tabItem(tabName = 'mapas',fluidRow(
          
          filtros_mapas(),
          box(class='floating-box',
              tabsetPanel(
                mapa_ui()
              ),width = 9)))
        
      )
    )
  )
)


agrupado <- Estudantes %>% group_by(cor_raca,semestre_ingresso,renda_familiar,sistema_ingresso) %>% count()



server <- function(input,output){
  
  index <- reactive({
    Index = which(names(new_groups)==input$variable)
    Index
  })
  
  df <- reactive({
    Teste = Estudantes %>% group_by(semestre_ingresso,!!new_groups[[index()]]) %>% count()
  })
  
  
  output$inicial <- 
    renderPlotly({
      df() %>% filter(!!new_groups[[index()]]%notin%c('Outro','Nao se aplica','Ignorado')) %>%
        ggplot(aes(x=semestre_ingresso,y=n,group=!!new_groups[[index()]],colour=!!new_groups[[index()]]))+
        geom_line(size=1.2)+
        ggtitle(paste0('Acompanhamento da Variável ',input$variable))+
        
        scale_linetype_manual(values = c('dotted', 'solid')) +
        
        scale_colour_manual(name='Níveis',values=c('#A11D21','#003366','#CC9900','#055b06',
                                                   '#530202','#30075b','#406a53','#038c09',
                                                   '#05163b', '#b94c00', '#48042c')) +
        theme_bw()+
        labs(x='Semestres',y='Número de Alunos Ingressos')+
        theme(legend.position = 'bottom',
              plot.title = element_text(hjust=0.5),
              axis.title.y=element_text(colour='black',size=10),
              axis.title.x=element_text(colour='black',size=10),
              axis.text=element_text(colour='black',size=6.5),
              panel.border=element_blank(),
              axis.line=element_line(colour='black'))
  })
  
  
  
  df_raca_cor <- reactive({
    if(input$renda_familiar=='Todas'&
       input$semestre_ingresso=='Todos' &
       input$cotas=="Todos"){
      Rosca_cor <- agrupado
      
    }else if(input$cotas=='Todos' & input$renda_familiar=="Todas"){
      
      Rosca_cor <- agrupado %>% filter(semestre_ingresso=='22017')
      
    }else if(input$cotas=='Todos' & input$semestre_ingresso=="Todos"){
      Rosca_cor <- agrupado %>% filter(renda_familiar==input$renda_familiar)
      
    }else if(input$renda_familiar=='Todas' & input$semestre_ingresso=="Todos"){
      Rosca_cor <- agrupado %>% filter(sistema_ingresso==input$cotas)
    }
    
    else if(input$renda_familiar=='Todas'){
      Rosca_cor <- agrupado %>% filter(semestre_ingresso==input$semestre_ingresso,
                                       sistema_ingresso==input$cotas)
    }else if(input$semestre_ingresso=='Todos'){
      Rosca_cor <- agrupado %>% filter(renda_familiar==input$renda_familiar,
                                       sistema_ingresso==input$cotas)
    }else if(input$cotas=="Todos"){
      Rosca_cor <- agrupado %>% filter(renda_familiar==input$renda_familiar,
                                       semestre_ingresso==input$semestre_ingresso)
      
    }else{
      Rosca_cor <- agrupado %>% filter(semestre_ingresso==input$semestre_ingresso,
                                       renda_familiar==input$renda_familiar,
                                       sistema_ingresso==input$cotas)
    }
  Rosca_cor <- Rosca_cor %>% group_by(cor_raca) %>% summarise(n = sum(n))
  Rosca_cor
  })

  
  
  output$raca_cor <- renderPlotly({
    df_raca_cor() %>%
      plot_ly(labels = ~cor_raca, values = ~n, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = c('#A11D21','#003366','#CC9900','#055b06',
                                        '#530202','#30075b','#406a53','#038c09',
                                        '#05163b', '#b94c00', '#48042c'),
                            line = list(color = '#FFFFFF', width = 1)))

  })
  
  
  df_mapa <- reactive({
    if(input$semestre_mapas=="Todos" & input$campus_mapas=="Todos"){
      parte1 <- Estudantes_RA
     
    }else if(input$semestre_mapas=="Todos"){
      parte1 <- Estudantes_RA %>% filter(campus==input$campus_mapas)
 
    }else if(input$campus_mapas=="Todos"){
      parte1 <- Estudantes_RA %>% filter(semestre_ingresso==input$semestre_mapas)
 
    }else{
      parte1 <- Estudantes_RA %>% filter(campus==input$campus_mapas,semestre_ingresso==input$semestre_mapas)
  
      
    }
    
    teste= parte1 %>% group_by(ra) %>% summarise(n = sum(n))
    DFMAPA=merge(MAPARS,teste,by=c('ra') ,duplicateGeoms = TRUE) 
    DFMAPA$n[is.na(DFMAPA$n)] <- 0 
    DFMAPA
  })
  
  output$mapa1 <- renderLeaflet({
    map1 <- df_mapa() %>% tm_shape(name='maps')+
      tm_polygons(col = "n", palette = "Greens", title = "Número Ingressantes UNB por RA")
    
    
    
    tmap_leaflet(map1,add.titles = F,
                 in.shiny=T)
    
  })
  

  
}


shinyApp(ui = ui, server = server)





### Variável instituto #####

#### Gráfico vs renda #####

### Raça ###

### Renda Familiar ####

#### Mapa #####

Estudantes$transporte %>% table()

rendas %>% levels()



