
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(readxl)
library(plotly)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(sass)



escolhas_rendas[1] <- 'Todas'
escolhas_rendas[2:7] <- levels(rendas)

escolhas_semestre <- levels(semestres)
escolhas_semestre[13] <- 'Todos'

filtros_lateral_analises <- function() {
  box(class='floating-box',
      selectInput("variable", "Variáveis",
                  choices = nomes_de_apresentacao,
                  selected = "Sistema de Ingresso"
      ),
      selectInput(inputId = 'semestre_ingresso','Semestre de ingresso',
                  choices =escolhas_semestre,
                  selected = "Todos" ),

      selectInput(inputId = 'renda_familiar','Renda Familiar',
                         choices =escolhas_rendas,
                  selected = "Todas" ),
      width = 3
  )
}



inicial_ui <- function(){
  tabPanel("Inicial",plotlyOutput('inicial')
           
  )
}



Raca_cor_ui <- function(){
  tabPanel("Raça & Cor",plotOutput('raca_cor')
           
  )
}



tab_biografia_ui <- function(){
  tabPanel("Biografia", uiOutput('biografia_candidatos'))
}

incial_server <-  function(df,input,output) {

  
  mainPanel()
  
}  


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
        menuItem("Análises", tabName = "analises", icon = icon("chart-bar")))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "analises", fluidRow(
          filtros_lateral_analises(),
          
          box(class = 'floating-box',
              tabsetPanel(inicial_ui(),
                          Raca_cor_ui()),
              width = 9
          ))
          
        ))
    )
  )
)

agrupado <- Estudantes %>% group_by(cor_raca,semestre_ingresso,renda_familiar) %>% count()

agrupado$cor_raca %>% unique()

agrupado <- agrupado %>% filter(cor_raca%notin%c('Outros','Ignorado'))


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
        
        scale_colour_manual(name='Níveis',values=c('#A11D21','#003366','darkgreen',
                                                   '#530202','#05eaef','#ef05c3',
                                                   'green','#406a53',
                                                   '#30075b', '#065102', '#48042c')) +
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
       input$semestre_ingresso=='Todos'){
      Rosca_cor <- agrupado
      
    }else if(input$renda_familiar=='Todas'){
      Rosca_cor <- agrupado %>% filter(semestre_ingresso==input$semestre_ingresso)
    }else if(input$semestre_ingresso=='Todos'){
      Rosca_cor <- agrupado %>% filter(renda_familiar==input$renda_familiar)
    }else{
      Rosca_cor <- agrupado %>% filter(semestre_ingresso==input$semestre_ingresso,
                                       renda_familiar==input$renda_familiar)
    }
  Rosca_cor
  })

  

  
  output$raca_cor <- renderPlot({
    df_raca_cor() %>%
      ggplot(aes(x=1, y=n, fill=cor_raca)) +
      geom_bar(stat="identity", width=1) +
      scale_fill_manual(name='Cor & Raça',values=c('#A11D21','#003366','darkgreen',
                                                   '#530202','#05eaef','#ef05c3',
                                                   'green','#406a53',
                                                   '#30075b', '#065102', '#48042c')) +
      coord_polar("y", start=0) +
      theme_bw()+
      theme(legend.position = 'top',
            axis.title=element_blank(),
            axis.text=element_blank(),
            panel.border=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank())

  })

  
}


shinyApp(ui = ui, server = server)


### Variável instituto #####

#### Gráfico vs renda #####



### Raça ###

### Renda Familiar ####


### N

#### Mapa #####

Estudantes$transporte %>% table()

rendas %>% levels()
