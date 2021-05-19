library(tidyverse)
library(readxl)
library(stringr)


###### leitura de dados #####

`%notin%` <- Negate(`%in%`)

Estudantes = read_xlsx('estudanteunb.xlsx')


### Fazendo mudanças pertinentes nas variáveis ###########


### criação de fator semestre ingresso ####

Estudantes$semestre_ingresso <- factor(Estudantes$semestre_ingresso,
                                       levels = unique(Estudantes$semestre_ingresso),
                                       labels = unique(Estudantes$semestre_ingresso))




Estudantes$semestre_ingresso <- ordered(Estudantes$semestre_ingresso,
                                        levels= c("12012","22012","12013","22013","12014","22014"
                                                  ,"12015","22015","12016"
                                                  ,"22016","12017","22017"))




### Cruiação de Fator renda familiar e exlusão de elementos ######

Estudantes <- Estudantes %>% filter(renda_familiar %notin% c("Ignorado","Não possui renda mensal"))

Estudantes$renda_familiar <- as.factor(Estudantes$renda_familiar)

levels(Estudantes$renda_familiar)
Estudantes$renda_familiar <- ordered(Estudantes$renda_familiar,
                                        levels= c("Mais de 20 SM","De 10 a 20 SM" , 
                                                  "De 3 a 10 SM","Até 3 SM" ))


Estudantes$renda_familiar %>% levels()




#### Mudança na modalidade de ingresso #######

Estudantes$modalidade_ingresso %>% unique()

Estudantes$modalidade_ingresso[Estudantes$modalidade_ingresso==
                                 "Admissão para Portador de Diploma de Curso Superior (DCS)"] <- "DCS"


#### Filtro de cor e Raça #######


Estudantes <- Estudantes %>% filter(cor_raca%notin% c('Ignorado','Outros'))


Estudantes$sexo %>% unique()

Estudantes$sexo <- as.factor(Estudantes$sexo)

Estudantes$sexo <- ordered(Estudantes$sexo, 
                           levels = c('Feminino',"Masculino",'Ignorado'))



##### Criar Vetor para variáveis #######


Estudantes$escolaridade_pai %>% unique()

Estudantes$escolaridade_mae <- factor(Estudantes$escolaridade_mae,
                                      levels = c("Ignorado", "Pós-graduação",
                                                 "Ensino superior completo",
                                                 "Ensino superior incompleto","Ensino médio completo",
                                                 "Ensino médio incompleto" ,  "Ensino fundamental completo",
                                                 "Ensino fundamental incompleto","Não sabe ler nem escrever" ,
                                                 "Não sabe informar"),ordered = T)





Estudantes$escolaridade_pai <- factor(Estudantes$escolaridade_pai,
                                      levels  = c("Ignorado", "Pós-graduação",
                                                 "Ensino superior completo",
                                                 "Ensino superior incompleto","Ensino médio completo",
                                                 "Ensino médio incompleto" ,  "Ensino fundamental completo",
                                                 "Ensino fundamental incompleto","Não sabe ler nem escrever" ,
                                                 "Não sabe informar"),ordered = T)


Estudantes$pessoas_vivem_da_renda %>% unique()



Estudantes$pessoas_vivem_da_renda <- factor(Estudantes$pessoas_vivem_da_renda, 
                                levels  = c("9 ou mais","8" ,'7',
                                            '6','5','4','3','2','1','Ignorado'),
                                ordered = T)



#### Crinaod banco Escolaridades #####


Escolaridades <-  Estudantes %>% filter(escolaridade_mae%notin%c('Ignorado'),
                                        escolaridade_pai%notin%c('Ignorado')) %>% select(c(semestre_ingresso, renda_familiar,
                                                                                           sistema_ingresso,
                                                                                           escolaridade_mae,escolaridade_pai))

Escolaridades= melt(Escolaridades,id.vars = c('semestre_ingresso', 'renda_familiar','sistema_ingresso'))


Escolaridades$value <- factor(Escolaridades$value,
                              levels  = c("Pós-graduação","Ensino superior completo",
                                          "Ensino superior incompleto","Ensino médio completo",
                                          "Ensino médio incompleto" ,  "Ensino fundamental completo",
                                          "Ensino fundamental incompleto","Não sabe ler nem escrever" ,
                                          "Não sabe informar"),
                              ordered = T)


Escolaridades$variable <- factor(Escolaridades$variable ,
                                 levels  = c('escolaridade_mae','escolaridade_pai'),
                                 labels = c("Escolaridade Mãe","Escolaridade Pai"))



save(Escolaridades, file="Escolaridades.Rdata")


#### Criando Variável com nomes das variáveis e variáveis em groups que podem ser chamadas #####

Estudantes$beneficio_social %>% unique()


nomes_de_apresentacao <- c('Modalidade de Ingresso',"Atendimento Especial","Sexo",
                           'Cor & Raca','Renda Familiar','Pessoas vivem da renda',
                           'Escolaridade Pai','Escolaridade Mãe','Ensino Fundamental',
                           'Ensino Médio','Necessidade Especial','Curso Desejado',
                           'Trocaria de Curso','Curso Preparatório','Sistema de Ingresso')



groups = c(quo(modalidade_ingresso), quo(atendimento_especial),quo(sexo),
           quo(cor_raca),quo(renda_familiar),quo(pessoas_vivem_da_renda),
           quo(escolaridade_pai),quo(escolaridade_mae),quo(ensino_fundamental),quo(ensino_medio)
           ,quo(necessidade_especial),quo(curso_desejado),quo(trocaria_curso),
           quo(curso_preparatorio),quo(sistema_ingresso))


names(groups) <- nomes_de_apresentacao

ordem <- order(names(groups))

new_groups <- list()

for(i in 1: length(ordem)){
  new_groups[[i]] = groups[[ordem[i]]]
  
}



nomes_de_apresentacao <- sort(nomes_de_apresentacao)

names(new_groups) <- nomes_de_apresentacao

save(new_groups,file = 'new_groups.RDATA')


#### Salvando arquivo rdata ######


save(Estudantes, file = 'Estudantes.RDATA')



##### Gráficos ########


###página inicial ####




Teste <- Estudantes %>% filter(escolaridade_mae%notin%c('Ignorado')) %>% group_by(escolaridade_mae)%>% 
              count()





Teste %>% 
ggplot(aes(x=reorder(escolaridade_mae,-order(escolaridade_mae) ), y=n))+
  geom_col(fill ='#A11D21' ) +
  coord_flip()+
  
  ggtitle("Frequência Absoluta da Escolaridade da Mãe")+
  theme_bw()+
  labs(x='',y='')+
  theme(legend.position = 'top',
        plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(colour='black',size=10),
        axis.title.x=element_text(colour='black',size=10),
        axis.text=element_text(colour='black',size=6.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))

