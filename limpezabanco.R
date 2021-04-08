library(readxl)
library(tidyverse)

dados <- read_excel(choose.files())

# excluindo variaveis

df <- dados[,-c(4,13,15,18,20,21,22,34,35,36,37,38,39,40,41,42,43,44,45,46,47,50)]
df1 <- df[,-c(30,33,35,36,41,43,44,45,46,47,48)]
df2 <- df1[,-c(42,43,44,45,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66)]
df3 <- df2[,-c(43,46,48,50,51,52,53,54,55,56,57)]
df4 <- df3[,-c(48,49,50)]

# juntando variaveis

transp <- vector(mode="numeric",length=45043)
for(i in 1:45043){if(df4$V0221C[i]==1){transp[i] <- "Carro"}else{ifelse(df4$V0223C[i]==1|df4$V0227C[i]==1,transp[i] <- "Publico",transp[i] <- "Outro")}}
for(j in 1:45043){if(transp[j]==0){transp[j] <- "Outro"}}

curso <- vector(mode="numeric",length=45043)
for(i in 1:45043){if(df4$V041C[i]==1&is.na(df4$V0411C[i])==T){curso[i] <- "Sim"}}
for(i in 1:45043){if(is.na(df4$V0411C[i])==F){curso[i] <- df4$V0411C[i]}}
for(i in 1:45043){if(df4$V041C[i]==2){curso[i] <- "Sem Curso"}}
for(i in 1:45043){if(curso[i]==0){curso[i] <- "Nao Especificado"}}
curso <- as.factor(curso)
levels(curso) <- c("Publico ou Gratuito","Particular com Bolsa Integral","Particular com Bolsa Parcial","Particular","Ignorado","Nao","Sim")

auxilio <- vector(mode="numeric",length=45043)
df4$V0541C <- as.character(df4$V0541C)
df4$V0542C <- as.character(df4$V0542C)
df4$V0543C <- as.character(df4$V0543C)
df4$V0544C <- as.character(df4$V0544C)
df4$V0545C <- as.character(df4$V0545C)
df4$V0546C <- as.character(df4$V0546C)
df4$V0547C <- as.character(df4$V0547C)
df4$V0548C <- as.character(df4$V0548C)
df4$V0549C <- as.character(df4$V0549C)
df4$V0550C <- as.character(df4$V0550C)
for(i in 1:45043){ifelse(df4$V0541C[i]!="TRUE"&df4$V0541C[i]!="TRUE"&df4$V0542C[i]!="TRUE"&df4$V0543C[i]!="TRUE"&df4$V0544C[i]!="TRUE"&df4$V0545C[i]!="TRUE"&df4$V0546C[i]!="TRUE"&df4$V0547C[i]!="TRUE"&df4$V0548C[i]!="TRUE"&df4$V0549C[i]!="TRUE"&df4$V0550C[i]!="TRUE",auxilio[i] <- "Nao",auxilio[i] <- "Sim")}
for(i in 1:45043){if(auxilio[i]==0){auxilio[i] <- "Nao"}}

uf <- as.character(df4$V011C)
for(k in 1:45043){if(is.na(uf[k])==T){uf[k] <- "Ignorado"}}
uf <- as.factor(uf)
levels(uf) <- c("Acre", "Maranhão","Minas Gerais","Mato Grosso do Sul","Mato Grosso","Pará","Paraíba","Pernambuco",
                "Piauí","Paraná","Rio de Janeiro","Alagoas","Rio Grande do Norte","Rondônia","Roraima","Rio Grande do Sul","Santa Catarina",
                "Sergipe","São Paulo","Tocantins","Amazonas","Amapá","Bahia","Ceará","Distrito Federal",
                "Espírito Santo","Goiás","Ignorado")

sist <- vector(mode="numeric",length=45043)
for(i in 1:45043){ifelse(df4$V003C[i]==1,sist[i] <- "Nao se aplica",sist[i] <- 0)}
for(i in 1:45043){ifelse(is.na(df4$V003C[i]==T)&sist[i]!="Nao se aplica",sist[i] <- "Ignorado",sist[i] <- sist[i])}
for(i in 1:45043){ifelse(df4$V003C[i]==6&sist[i]!="Nao se aplica"&sist[i]!="Ignorado",sist[i] <- "Sistema Universal",sist[i] <- sist[i])}
for(i in 1:45043){ifelse(df4$V003C[i]==2|df4$V003C[i]==3|df4$V003C[i]==4|df4$V003C[i]==5|df4$V003C[i]==311|df4$V003C[i]==312|df4$V003C[i]==321|df4$V003C[i]==322,sist[i] <- "Sistema de Cotas",sist[i] <- sist[i])}

# substituindo variaveis pelas novas

df5 <- df4[,-c(3,12,20,21,22,23,24,25,26,38,39,48,49,50,51,52,53,54,55,56,57)]
df5$uf <- uf
df5$auxilio <- auxilio
df5$curso <- curso
df5$transporte <- transp
df5$sistema_ingresso <- sist

# renomeando variaveis

names(df5)[names(df5) == "curso"] <- "curso_preparatorio"
names(df5)[names(df5) == "V001C"] <- "semestre_ingresso"
names(df5)[names(df5) == "V004C1"] <- "modalidade_ingresso"
names(df5)[names(df5) == "V0051C"] <- "campus"
names(df5)[names(df5) == "V0052C"] <- "turno"
names(df5)[names(df5) == "V006C"] <- "curso"
names(df5)[names(df5) == "V00701C"] <- "atendimento_especial"
names(df5)[names(df5) == "V008C"] <- "sexo"
names(df5)[names(df5) == "V0094"] <- "idade"
names(df5)[names(df5) == "V010C"] <- "nacionalidade"
names(df5)[names(df5) == "V013C"] <- "estado_civil"
names(df5)[names(df5) == "V014C"] <- "cor_raca"
names(df5)[names(df5) == "V015C1"] <- "religiao"
names(df5)[names(df5) == "V018"] <- "municipio/ra_residencia"
names(df5)[names(df5) == "V019C"] <- "tempo_residencia"
names(df5)[names(df5) == "V020C"] <- "com_quem_reside"
names(df5)[names(df5) == "V021C"] <- "distancia_campus"
names(df5)[names(df5) == "V024C1"] <- "renda_familiar"
names(df5)[names(df5) == "V025C"] <- "beneficio_social"
names(df5)[names(df5) == "V026C"] <- "pessoas_vivem_da_renda"
names(df5)[names(df5) == "V028C"] <- "escolaridade_pai"
names(df5)[names(df5) == "V029C"] <- "escolaridade_mae"
names(df5)[names(df5) == "V031C"] <- "assistencia_medica"
names(df5)[names(df5) == "V034C"] <- "necessidade_especial"
names(df5)[names(df5) == "V035C"] <- "ensino_fundamental"
names(df5)[names(df5) == "V036C"] <- "ensino_medio"
names(df5)[names(df5) == "V037C"] <- "tipo_ensino_medio"
names(df5)[names(df5) == "V0401C"] <- "idioma"
names(df5)[names(df5) == "V042C"] <- "tentativas"
names(df5)[names(df5) == "V043C"] <- "curso_desejado"
names(df5)[names(df5) == "V044C"] <- "trocaria_curso"
names(df5)[names(df5) == "V046C"] <- "grau_maximo"
names(df5)[names(df5) == "V047C"] <- "atividade_economica"
names(df5)[names(df5) == "V048C1"] <- "renda_individual"
names(df5)[names(df5) == "V050C"] <- "horas_trabalho"
names(df5)[names(df5) == "V052C"] <- "perspectiva_profissional"

# renomeando niveis das variaveis

df5$semestre_ingresso <- as.factor(df5$semestre_ingresso)
levels(df5$semestre_ingresso) <- c("12012","22012","12013","22013","12014","22014","12015","22015","12016","22016","12017","22017")

df5$modalidade_ingresso <- as.factor(df5$modalidade_ingresso)
levels(df5$modalidade_ingresso) <- c("Admissão para Portador de Diploma de Curso Superior (DCS)","ENEM","PAS", "Vagas remanescentes","Transferência","Vestibular")

df5$campus <- as.factor(df5$campus)
levels(df5$campus) <- c("Darcy Ribeiro","Ceilândia","Gama","Planaltina")

df5$turno <- as.factor(df5$turno)
levels(df5$turno) <- c("Diurno","Noturno")

df5$curso <- as.factor(df5$curso)
levels(df5$curso) <- c("Administração","Agronomia","Arquitetura e Urbanismo","Arquivologia","Artes Cênicas (Bacharelado/Licenciatura)","Artes Cênicas (Licenciatura)",
                       "Artes Plásticas (Bacharelado/Licenciatura)","Artes Plásticas (Licenciatura)","Biblioteconomia","Biotecnologia","Ciência da Computação (Bacharelado)",
                       "Ciência Política","Ciências Ambientais","Ciências Biológicas (Bacharelado/Licenciatura)","Ciências Biológicas (Licenciatura)","Ciências Contábeis",
                       "Ciências Econômicas","Ciências Farmacêuticas", "Ciências Naturais (Licenciatura)","Ciências Sociais","Computação (Licenciatura)","Comunicação Organizacional",
                       "Comunicação Social","Desenho Industrial (Bacharelado)","Direito","Educação Artística Música (Licenciatura)","Educação Física (Bacharelado)",
                       "Educação Física (Licenciatura)","Enfermagem","Engenharia","Engenharia Ambiental","Engenharia Civil","Engenharia da Computação","Engenharia de Produção",
                       "Engenharia de Redes de Comunicação","Engenharia Elétrica","Engenharia Florestal","Engenharia Mecânica","Engenharia Mecatrônica","Engenharia Química",
                       "Estatística","Filosofia","Filosofia (Licenciatura)","Física (Bacharelado/Licenciatura/Física Computacional)","Física (Licenciatura)","Fisioterapia",
                       "Fonoaudiologia","Geofísica","Geografia","Geologia","Gestão Ambiental","Gestão de Políticas Públicas","Gestão de Saúde","Gestão do Agronegócio",
                       "Gestão em Saúde Coletiva","História","História (Licenciatura)","Letras Espanhol (Licenciatura)","Letras Francês (Bacharelado/Licenciatura)",
                       "Letras Inglês (Bacharelado/Licenciatura)","Letras Japonês (Licenciatura)","Letras Língua Estrangeira Aplicada (Bacharelado)","Letras Português (Bacharelado/Licenciatura)",
                       "Letras Português (Licenciatura)","Letras Português do Brasil como Segunda Língua (Licenciatura)","Letras Tradução Espanhol","Letras Tradução Francês",
                       "Letras Tradução Inglês","Matemática (Bacharelado/Licenciatura)","Matemática (Licenciatura)","Medicina","Medicina Veterinária","Museologia",
                       "Música (Bacharelado)","Música (Licenciatura)","Nutrição","Odontologia","Pedagogia","Psicologia","Química (Bacharelado)","Química (Licenciatura)",
                       "Química Tecnológica","Relações Internacionais","Serviço Social","Teoria Crítica e História da Arte (Bacharelado)","Terapia Ocupacional","Turismo",
                       "Saúde Coletiva","Farmácia","Licenciatura em Língua de Sinais Brasileira/Português LIBRAS","Design (Bacharelado)","Jornalismo","Música (Licenciatura) -  Educação Artística)")

df5$atendimento_especial <- as.factor(df5$atendimento_especial)
levels(df5$atendimento_especial) <- c("Sim","Não","Ignorado")

df5$sexo <- as.factor(df5$sexo)
levels(df5$sexo) <- c("Masculino","Feminino","Ignorado")

df5$nacionalidade <- as.factor(df5$nacionalidade)
levels(df5$nacionalidade) <- c("Brasileiro(a)","Brasileiro(a) nato(a)","Estrangeiro(a)","Estrangeiro(a) naturalizado(a)")

df5$estado_civil <- as.factor(df5$estado_civil)
levels(df5$estado_civil) <- c("Casado(a)","Divorciado(a)","Separado(a) Não Judicialmente","Solteiro(a)","União Estável","Viúvo(a)")

df5$cor_raca <- as.factor(df5$cor_raca)
levels(df5$cor_raca) <- c("Amarela","Branca","Indígena","Parda","Preta","Outros")

df5$religiao <- as.factor(df5$religiao)
levels(df5$religiao) <- c("Católica","Espírita","Evangélica tradicional","Evangélica pentecostal","Outras cristãs","Afrobrasileiras","Orientais","Múltiplas religiosidades ou não definida",
                          "Sem religião - ateu","Outras")

df5$tempo_residencia <- as.factor(df5$tempo_residencia)
levels(df5$tempo_residencia) <- c("Menos de 6 meses","Entre 6 meses e um ano","Entre um e três anos","Entre três e cinco anos","Mais de cinco anos")

df5$com_quem_reside <- as.factor(df5$com_quem_reside)
levels(df5$com_quem_reside) <- c("Com os pais (pai e/ou mãe)","Com parentes ou amigos(as)","Cônjuge","Cônjuge, com filhos","Cônjuge, sem filhos","Em pensão","Em república",
                                 "Em Residência Estudantil - Casa do Estudante Universitário","Sozinho","Outros","Com filhos(as)")

df5$distancia_campus <- as.factor(df5$distancia_campus)
levels(df5$distancia_campus) <- c("Menos de 5 km","Entre 5 e 10 km","Entre 10 e 30 km","Entre 30  e 50 km","Entre 50 e 100 km","Mais de 100 km")

df5$pessoas_vivem_da_renda <- as.factor(df5$pessoas_vivem_da_renda)
levels(df5$pessoas_vivem_da_renda) <- c("1","2","3","4","5","6","7","8","9 ou mais")

df5$escolaridade_mae <- as.factor(df5$escolaridade_mae)
levels(df5$escolaridade_mae) <- c("Não sabe ler nem escrever","Ensino fundamental incompleto","Ensino fundamental completo","Ensino médio incompleto","Ensino médio completo",
                                  "Ensino superior incompleto","Ensino superior completo","Pós-graduação","Não sabe informar")

df5$escolaridade_pai <- as.factor(df5$escolaridade_pai)
levels(df5$escolaridade_pai) <- c("Não sabe ler nem escrever","Ensino fundamental incompleto","Ensino fundamental completo","Ensino médio incompleto","Ensino médio completo",
                                  "Ensino superior incompleto","Ensino superior completo","Pós-graduação","Não sabe informar")

df5$assistencia_medica <- as.factor(df5$assistencia_medica)
levels(df5$assistencia_medica) <- c("Majoritariamente à rede pública","Majoritariamente à rede privada")

df5$necessidade_especial <- as.factor(df5$necessidade_especial)
levels(df5$necessidade_especial) <- c("Sim","Não","N/A")

df5$ensino_fundamental <- as.factor(df5$ensino_fundamental)
levels(df5$ensino_fundamental) <- c("Somente em escolas públicas","Majoritariamente em escolas públicas","Majoritariamente em escolas particulares com bolsa",
                                    "Majoritariamente em escolas particulares","Somente em escolas particulares com bolsa","Somente em escolas particulares")

df5$ensino_medio <- as.factor(df5$ensino_medio)
levels(df5$ensino_medio) <- c("Somente em escolas públicas","Majoritariamente em escolas públicas","Majoritariamente em escolas particulares com bolsa",
                                    "Majoritariamente em escolas particulares","Somente em escolas particulares com bolsa","Somente em escolas particulares")

df5$tipo_ensino_medio <- as.factor(df5$tipo_ensino_medio)
levels(df5$tipo_ensino_medio) <- c("Ensino Médio regular","Supletivo","Educação de Jovens e Adultos","Técnico / Profissionalizante","Magistério","Exame de massa / menção","Telecurso")

df5$idioma <- as.factor(df5$idioma)
levels(df5$idioma) <- c("Sim","Não","Ignorado")

df5$tentativas <- as.factor(df5$tentativas)
levels(df5$tentativas) <- c("Nenhuma, esta é a primeira	","Uma","Duas	","Três","Mais de três","Já realizei, sem concluir, outro(s) curso(s) de graduação na UnB","Já conclui outro(s) curso(s) de graduação na UnB")

df5$curso_desejado <- as.factor(df5$curso_desejado)
levels(df5$curso_desejado) <- c("Sim","Não","Ignorado")

df5$trocaria_curso <- as.factor(df5$trocaria_curso)
levels(df5$trocaria_curso) <- c("Sim","Não")

df5$grau_maximo <- as.factor(df5$grau_maximo)
levels(df5$grau_maximo) <- c("Completar o ensino superior (graduação)","Completar uma pós-graduação (doutorado)","Completar uma pós-graduação (especialização)",
                             "Completar uma pós-graduação (mestrado)","Indefinido","Outros")

df5$atividade_economica <- as.factor(df5$atividade_economica)
levels(df5$atividade_economica) <- c("Nunca trabalhei","Não trabalho no momento","Exerço atividades remuneradas eventualmente","Faço estágio técnico","Trabalho sem carteira assinada",
                                     "Trabalho com carteira assinada","Sou servidor(a) público(a)")

df5$renda_individual <- as.factor(df5$renda_individual)
levels(df5$renda_individual) <- c("Até 3 SM","De 3 a 10 SM","De 10 a 20 SM","Mais de 20 SM","Não possui renda mensal",
                                  "Não sei")

df5$renda_familiar <- as.factor(df5$renda_familiar)
levels(df5$renda_familiar) <- c("Até 3 SM","De 3 a 10 SM","De 10 a 20 SM","Mais de 20 SM","Não possui renda mensal",
                                  "Não sei")

df5$beneficio_social <- as.factor(df5$beneficio_social)
levels(df5$beneficio_social) <- c("Sim","Não","Ignorado","Outros","Outros","Outros","Outros")

df5$horas_trabalho <- as.factor(df5$horas_trabalho)
levels(df5$horas_trabalho) <- c("Menos de 10 horas","Entre 10 e 20 horas","Entre 20 e 30 horas","Entre 30 e 40 horas","Mais de 40 horas")

df5$perspectiva_profissional <- as.factor(df5$perspectiva_profissional)
levels(df5$perspectiva_profissional) <- c("Ainda não me decidi","Pretendo buscar uma atividade na minha área de graduação","Desejo lecionar para ensino fundamental ou médio",
                                          "Desejo lecionar para ensino superior","Pretendo trabalhar em empresa privada","Vou prestar concurso para atividade em órgão ou instituição pública",
                                          "Já tenho trabalho, fora da minha área de graduação, e pretendo continuar nele","Já tenho trabalho, na minha área de graduação, e pretendo continuar nele",
                                          "Outros")

df6 <- data.frame(sapply(df5, function(x) as.character(as.factor(x))))
df6[is.na(df6)] <- "Ignorado"

df6$idade <- as.numeric(df6$idade)

# salvando banco

setwd("D:/UnB/LABEST2")
writexl::write_xlsx(df6,"estudanteunb.xlsx")
