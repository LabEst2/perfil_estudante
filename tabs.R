#### Tab inicial ####

inicial_ui <- function(){
  tabPanel("Inicial",plotlyOutput('inicial')
           
  )
}

##### Raca cor #######
Raca_cor_ui <- function(){
  tabPanel("Raça & Cor",
           plotlyOutput('raca_cor')
           
  )
}

##### Mapas ######

mapa_ui <- function(){
  tabPanel("mapas",
           leafletOutput("mapa1")
           
  )
}


##### Instituto ######


Instituto_ui <- function(){
  tabPanel("Instituto",
           plotlyOutput('Instituto_plot'))
  
}

##### Curso #####


Curso_ui <- function() {
  escolhas_instituto <- unique(Estudantes$instituto)
  escolhas_instituto <- escolhas_instituto[-28]
  escolhas_instituto <- sort(escolhas_instituto)
  
  tabPanel("Curso",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "instituto", 
                           label = "Instituto UNB",
                          choices = escolhas_instituto,
                          selected = 'IE'),
               width = 2),
             
             
             mainPanel(
               plotlyOutput('Curso_plot'),
               style='overflow-y: scroll; max-height: 82vh;'
             )))
}


