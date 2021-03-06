### Carregando bibliotecas ####

library(pacman)
p_load(shiny,ggplot2,readxl,plotly,shinythemes,tidyverse,shinydashboard,sass,leaflet,tmap,shinyWidgets,sp,reshape2)

#### Primeiro set o diretório do console para o lugar do arquivo, 
### Vá em Session > Set Working ... > To source file location 


`%notin%` <- Negate(`%in%`)

#### Carregando arquivos #####

load(file = 'Estudantes.RDATA')

load(file = 'Estudantes_RA.RDATA')

load(file = 'mapars.RDATA')

load('new_groups.RDATA')

load('Escolaridades.Rdata')


#### Banco necessário ######



#### Carregando funções #####

source('Filtros_Laterais.R', encoding = 'UTF-8')

source('tabs.R', encoding = 'UTF-8')





#### Fazendo as escolhas #####


dbHeader <- dashboardHeader(title = tags$a(href='https://github.com/LabEst2/perfil_estudante',
                                           "Perfil do Estudante"))

##### Página UI ######

ui <-  tags$html(
  tags$head( 
    tags$style(sass(sass_file('www/styles.scss')))
  ),
  dashboardPage(
    skin = "green",
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
        menuItem("Página Inicial", tabName = "inicial", icon = icon("info-circle")),
        menuItem("Visão Geral", tabName = "analises", icon = icon("chart-line")),
        menuItem("Análise Sócio Economica", tabName = "socio_eco", icon = icon("chart-pie")),
        menuItem("Mapa", tabName = "mapas", icon = icon("globe-americas")),
        menuItem("Sobre",tabName = 'sobre', icon = icon('question-circle')))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'inicial',
                fluidRow(
                  tabsetPanel(
                    inicial_ui()
                  )
                  
                )),
        
        tabItem(tabName = "analises", fluidRow(
          filtros_lateral_inicial(),
          
          box(class = 'floating-box',
              tabsetPanel(
                          Linha_temporal_ui(),
                          Instituto_ui(),
                          Curso_ui()),
              width = 12
          ))
          
        ),
        tabItem(tabName = 'socio_eco',fluidRow(
          
          filtros_lateral_raca_cor(),
          box(class='floating-box',
              tabsetPanel(
                Raca_cor_ui(),
                Escolaridae_ui(),
                Ensino_medio_ui(),
                Ensino_fundamental_ui()
                
              ),width = 9))),
        
        tabItem(tabName = 'mapas',fluidRow(
          
          filtros_mapas(),
          box(class='floating-box',
              tabsetPanel(
                mapa_ui()
              ),width = 9))),
        tabItem(tabName = "sobre", sobre_ui())
        
      )
    )
  )
)






server <- function(input,output){
  
  ##### Gráfico Inicial #####

  
  index <- reactive({
    Index = which(names(new_groups)==input$variable)
    Index
  })
  
  df_inicial <- reactive({
    Teste = Estudantes %>% group_by(semestre_ingresso,!!new_groups[[index()]]) %>% count()
  })
  
  
  output$linhatemporal <- 
    renderPlotly({
      df_inicial() %>% filter(!!new_groups[[index()]]%notin%c('Outro','Nao se aplica','Ignorado')) %>%
        ggplot(aes(x=semestre_ingresso,y=n,group=!!new_groups[[index()]],colour=!!new_groups[[index()]]))+
        geom_line(size=1.2)+
        ggtitle(paste0('Evolução por Semestre - ',input$variable))+
        
        scale_linetype_manual(values = c('dotted', 'solid')) +
        
        scale_colour_manual(name='Níveis',values=c('#A11D21','#003366','#CC9900','#406a53',
                                                   '#530202','#30075b','#055b06','#038c09',
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
  
  #### Gráfico de Instituo ######
  
  df_instituto <- reactive({
    grupo_instituto <- Estudantes %>% filter(!!new_groups[[index()]]%notin% c('Ignorado')) %>% 
      group_by(instituto, !!new_groups[[index()]]) %>%
      count() %>%
      filter(instituto!='Ignorado')%>%
      group_by(instituto) %>%
      mutate(freq_relat=n*100/sum(n)) %>%
      mutate(freq_relat=round(freq_relat,digits = 3))
   return(grupo_instituto) 
  })
  
  
  output$Instituto_plot <- renderPlotly({
    df_instituto() %>% 
      ggplot(aes(x=instituto, y=freq_relat, fill= !!new_groups[[index()]] ))+
      geom_col() +
      scale_fill_manual(name = 'Níveis', 
                        values = c('#A11D21','#003366','#CC9900','#406a53',
                                   '#530202','#30075b','#055b06','#038c09',
                                   '#05163b', '#b94c00', '#48042c')) +
      
      
      ggtitle(paste0('Distribuição por Instituto - ',input$variable))+
      theme_bw()+
      labs(x='Instituto',y='Frequência Relativa')+
      theme(legend.position = 'top',
            plot.title = element_text(hjust=0.5),
            axis.title.y=element_text(colour='black',size=10),
            axis.title.x=element_text(colour='black',size=10),
            axis.text=element_text(colour='black',size=6.5),
            panel.border=element_blank(),
            axis.line=element_line(colour='black'))
  })
  
    ##### Reactive para gráfico de Curso #####
  
  df_curso <- reactive({
    grupo_curso <- Estudantes %>% filter(instituto==input$instituto,!!new_groups[[index()]]%notin%c('Ignorado')) %>% 
      group_by(curso, !!new_groups[[index()]]) %>%
      count() %>%
      filter(curso!='Ignorado')%>%
      group_by(curso) %>%
      mutate(freq_relat=n*100/sum(n)) %>%
      mutate(freq_relat=round(freq_relat,digits = 3))
    return(grupo_curso) 
  })
  
  output$Curso_plot <- renderPlotly({
    df_curso() %>% 
      ggplot(aes(x=curso, y=freq_relat, fill= !!new_groups[[index()]] ))+
      geom_col() +
      coord_flip()+
      scale_fill_manual(name = 'Níveis', 
                        values = c('#A11D21','#003366','#CC9900','#406a53',
                                   '#530202','#30075b','#055b06','#038c09',
                                   '#05163b', '#b94c00', '#48042c')) +
      
      
      ggtitle(paste0('Distribuição por curso e por ',input$instituto, ' - ', input$variable))+
      theme_bw()+
      labs(x='',y='Frequência Relativa')+
      theme(legend.position = 'top',
            plot.title = element_text(hjust=0.5),
            axis.title.y=element_text(colour='black',size=10),
            axis.title.x=element_text(colour='black',size=10),
            axis.text=element_text(colour='black',size=6.5),
            panel.border=element_blank(),
            axis.line=element_line(colour='black'))
  })
  
  
  
  
  #### reactive para o gráfico de pizza de raça cor ####
  
  df_raca_cor <- reactive({
  Rosca_cor <- Estudantes %>% filter(semestre_ingresso%in%input$semestre_ingresso,
                                       renda_familiar%in%input$renda_familiar,
                                       sistema_ingresso%in%input$cotas )
    
  Rosca_cor <- Rosca_cor %>% group_by(cor_raca) %>% count()
  Rosca_cor
})

  
  
  output$raca_cor <- renderPlotly({
    df_raca_cor() %>%
      plot_ly(labels = ~cor_raca, values = ~n, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = c('#A11D21','#003366','#CC9900','#406a53',
                                       '#530202','#30075b','#055b06','#038c09',
                                       '#05163b', '#b94c00', '#48042c'),
                            line = list(color = '#FFFFFF', width = 1)))


  })
  
  ### Escolaridade dos Pais ######
  
  df_escolaridade_pais <- reactive({
    escol_pais <- Escolaridades %>% filter(semestre_ingresso%in%input$semestre_ingresso,
                                       renda_familiar%in%input$renda_familiar,
                                       sistema_ingresso%in%input$cotas)
    
    
    
    escol_pais <- escol_pais %>% group_by(variable,value) %>% count
    escol_pais
  })
  
  
  output$escolaridade_pais <- renderPlotly({
    df_escolaridade_pais() %>% 
      ggplot(aes(x=reorder(value,-order(value)),y=n,fill=variable))+geom_col(position = 'dodge')+
      
      scale_fill_manual(name='Escolaridade',values=c('#A11D21',
                                                     '#003366'), labels= c('Mãe',"Pai"))+
      coord_flip()+
      labs(x='',y='')+
      theme_bw()+
      theme(legend.position = 'rigth',
            axis.title.y=element_text(colour='black',size=12),
            axis.title.x=element_text(colour='black',size=12),
            axis.text=element_text(colour='black',size=9.5),
            panel.border=element_blank(),
            axis.line=element_line(colour='black'))+
      theme(legend.position='top')
    
    
  })
  
  
  #### Ensino Médio #####
  
  df_em <- reactive({
    escola_EM <- Estudantes %>% filter(semestre_ingresso%in%input$semestre_ingresso,
                                       renda_familiar%in%input$renda_familiar,
                                       sistema_ingresso%in%input$cotas,
                                       ensino_medio%notin%c('Ignorado'))
    
    escola_EM <- escola_EM %>% group_by(ensino_medio) %>% count()
    escola_EM
  })
  
  
  output$ensino_medio <- renderPlotly({
    df_em() %>% 
      ggplot(aes(x=reorder(ensino_medio,-order(ensino_medio)), y=n))+
      geom_col(fill ='#003366' ) +
      coord_flip()+
      ggtitle("")+
      theme_bw()+
      labs(x='',y='')+
      theme(legend.position = 'top',
            plot.title = element_text(hjust=0.5),
            axis.title.y=element_text(colour='black',size=10),
            axis.title.x=element_text(colour='black',size=10),
            axis.text=element_text(colour='black',size=6.5),
            panel.border=element_blank(),
            axis.line=element_line(colour='black'))
    
    
  })
  
  
  ### Ensino Fundamental ######
  
  
  df_ef <- reactive({
    escola_fund <- Estudantes %>% filter(semestre_ingresso%in%input$semestre_ingresso,
                                       renda_familiar%in%input$renda_familiar,
                                       sistema_ingresso%in%input$cotas,
                                       ensino_fundamental%notin%c('Ignorado'))
    
    escola_fund <- escola_fund %>% group_by(ensino_fundamental) %>% count()
    escola_fund
  })
  
  
  output$ensino_fundamental <- renderPlotly({
    df_ef() %>% 
      ggplot(aes(x=reorder(ensino_fundamental,-order(ensino_fundamental)), y=n))+
      geom_col(fill ='#003366' ) +
      coord_flip()+
      ggtitle("")+
      theme_bw()+
      labs(x='',y='')+
      theme(legend.position = 'top',
            plot.title = element_text(hjust=0.5),
            axis.title.y=element_text(colour='black',size=10),
            axis.title.x=element_text(colour='black',size=10),
            axis.text=element_text(colour='black',size=6.5),
            panel.border=element_blank(),
            axis.line=element_line(colour='black'))
    
    
  })
  
  
  
  #### Reactive para os mapas #####
  
  
  df_mapa <- reactive({
   
    parte1 <- Estudantes_RA %>% filter(campus%in%input$campus_mapas,semestre_ingresso%in%input$semestre_mapas)
  
    
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



