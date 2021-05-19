
pacman:: p_load(shiny,shinyWidgets)

nomes_variaveis_filtro <- function(){
  
  nomes_de_apresentacao <- c('Modalidade de Ingresso',"Atendimento Especial","Sexo",
                             'Cor & Raca','Renda Familiar','Pessoas vivem da renda',
                             'Escolaridade Pai','Escolaridade Mãe','Ensino Fundamental',
                             'Ensino Médio','Necessidade Especial','Curso Desejado',
                             'Trocaria de Curso','Curso Preparatório','Sistema de Ingresso')
  
  
  nomes_de_apresentacao <- sort(nomes_de_apresentacao)
  return(nomes_de_apresentacao)
}

##### Filtros iniciais #####

filtros_lateral_inicial <- function() {
 
  box(class='floating-box',
      selectInput("variable", "Variáveis",
                  choices = nomes_variaveis_filtro(),
                  selected = "Sistema de Ingresso"
      ),
      width = 3
  )
}

##### Filtros Raça Cor #####


filtros_lateral_raca_cor <- function() {
  escolhas_rendas <- levels(Estudantes$renda_familiar)
  
  
  escolhas_semestre <- levels(Estudantes_RA$semestre_ingresso)
  
  
  escolhas_cotas <- unique(Estudantes$sistema_ingresso)[1:2]
  
  box(class='floating-box',
       pickerInput(inputId = 'semestre_ingresso',
                   label ='Semestre de ingresso',
                  choices =escolhas_semestre,
                  selected = "22017",
                  options = list('actions-box'=TRUE),
                  multiple = T),
      
      pickerInput(inputId = 'renda_familiar','Renda Familiar',
                  choices =escolhas_rendas,
                  selected =escolhas_rendas,
                  options = list('actions-box'=TRUE),
                  multiple = T),
      
      pickerInput(inputId = 'cotas','Sistema de Ingresso',
                  choices =escolhas_cotas,
                  selected =escolhas_cotas,
                  options = list('actions-box'=TRUE),
                  multiple = T),
      width = 3
  )
}

#### Filtros mapas #####

filtros_mapas <- function(){
  
  escolhas_campus <- unique(Estudantes_RA$campus)
  
  escolhas_semestres_2 <- levels(Estudantes_RA$semestre_ingresso)[6:12]
  
  
  box(class='floating-box',
      pickerInput(inputId = 'semestre_mapas','Semestre de ingresso',
                  choices =escolhas_semestres_2,
                  selected = escolhas_semestres_2,
                  options = list('actions-box'=TRUE),
                  multiple = T),
      
      pickerInput(inputId = 'campus_mapas','Campus Universitário',
                  choices =escolhas_campus,
                  selected =escolhas_campus,
                  options = list('actions-box'=TRUE),
                  multiple = T),
      width = 3
  )
  
}

