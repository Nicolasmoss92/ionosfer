
setwd("C:\\Users\\----------\\Documents\\R\\Trabalho 2 - Ionosphere") #Define o caminho do ambiente para o folder onde a data est�.

 #A ideia do dataset veio por meio de listas de datasets na UCI Machine Learning Repository(https://archive.ics.uci.edu/ml/index.php), 
 #O download deste dataset foi feito por um repositorio do UCI(https://archive.ics.uci.edu/ml/datasets/ionosphere)

dados <- read.csv(file = "data.csv") #L� o dataset e o atribui os valores na vari�vel "dados"

str(dados) #Mostra as colunas/vari�veis/estrutura do dataset

summary(dados)#Mostra algumas informa��es sobre os dados do dataset, como o tipo das colunas/vari�veis, valores m�nimos, mediana, valores m�ximos...

dim(dados) #Mostra a quantidade de linhas/rows e de colunas/vari�veis

head(dados)#Mostra as 6 primeiras linhas do dataset

tail(dados) #Mostra as 6 �ltimas linhas do dataset

install.packages("neuralnet") #Instala o pacote neuralnet no ambiente (ser� usado futuramente).
require("neuralnet")#Carrega o pacote neuralnet no ambiente

set.seed(102)

install.packages("caret") #Instala o pacote "caret" no ambiente
require("caret") #Carrega o pacote caret no ambiente

indexTreino <- createDataPartition(dados$label, p=.7, list = FALSE, times = 1) 

treinoIono <- dados[indexTreino,] #Define a vari�vel "treinoIono" como o conjunto de treino(70% dos dados)
testeIono <- dados[-indexTreino,] #Define a vari�vel "testeIono" como o conjunto de testes (30% dos dados)



treinoIono$label <- ifelse(treinoIono$label== "g", 1, 0) #Transforma os valores da vari�vel alvo em um valor num�rico de 0 ou 1, 1 para g e 0 para b, "g" significando que � um sinal bom e "b" significando que � um sinal ruim
treinoIono$v1    <- ifelse(treinoIono$v1=="true", 1, 0)  #Transforma os valores da vari�vel v1 em um valor num�rico de 0 ou 1, 1 para "true" e 0 para "false"
treinoIono$v2    <- ifelse(treinoIono$v2=="true", 1, 0)  #Transforma os valores da vari�vel v2 em um valor num�rico de 0 ou 1, 1 para "true" e 0 para "false"

testeIono$label <- ifelse(testeIono$label== "g", 1, 0) #Transforma os valores da vari�vel alvo em um valor num�rico de 0 ou 1, 1 para g e 0 para b, "g" significando que � um sinal bom e "b" significando que � um sinal ruim
testeIono$v1    <- ifelse(testeIono$v1=="true", 1, 0)  #Transforma os valores da vari�vel v1 em um valor num�rico de 0 ou 1, 1 para "true" e 0 para "false"
testeIono$v2    <- ifelse(testeIono$v2=="true", 1, 0)  #Transforma os valores da vari�vel v2 em um valor num�rico de 0 ou 1, 1 para "true" e 0 para "false"


levels(factor(treinoIono$label)) #Comando para ver se os valores da vari�vel alvo do conjunto de treino mudaram de "g" ou "b" para 0 ou 1
levels(factor(treinoIono$v1))    #Comando para ver se os valores da vari�vel v1 do conjunto de treino mudaram de "true" ou "false" para 0 ou 1
levels(factor(treinoIono$v2))    #Comando para ver se os valores da vari�vel v2 do conjunto de treino mudaram de "true" ou "false" para 0 ou 1

levels(factor(testeIono$label)) #Comando para ver se os valores da vari�vel alvo do conjunto de teste mudaram de "g" ou "b" para 0 ou 1
levels(factor(testeIono$v1))    #Comando para ver se os valores da vari�vel v1 do conjunto de teste mudaram de "true" ou "false" para 0 ou 1
levels(factor(testeIono$v2))    #Comando para ver se os valores da vari�vel v2 do conjunto de teste mudaram de "true" ou "false" para 0 ou 1


redeNeural <- neuralnet(label~v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13+v14+v15+v16+v17+v18+v19+v20+v21+v22+v23+v24+v25+v26+v27+v28+v29+v30+v31+v32+v33+v34, #Vari�vel alvo e vari�veis utilizadas como neur�nio de entrada
                               hidden = c(30, 20, 10), #Quantidade de neur�nios na camada oculta
                               data = treinoIono,      #Conjunto de testes
                               err.fct = "ce",         #Entropia cruzada/cross entropy para classifica��o
                               linear.output = FALSE,  #FALSE para classifica��o
                               lifesign = "minimal",   #Basicamente o n�vel de "debug" da rede neural
                               stepmax = 1e6,          #N�mero m�ximo de passos, n�mero de vezes que os pesos ser�o atualizados, 1e6 � o n�mero m�ximo que o pacote neuralnet aceita
                               rep = 15,               #N�mero de �pocas da rede neural
                               learningrate = 0.08)    #Taxa de aprendizado


avaliarTreino <- predict(redeNeural, treinoIono, rep <- 5) # Aqui o rep ser� substituido com a melhor �poca
predicaoTreino <- ifelse(avaliarTreino>0.5, 1, 0)
matrizConfusaoTreino <- confusionMatrix(factor(predicaoTreino), factor(treinoIono$label))

avaliarTeste <- predict(redeNeural, testeIono, rep <- 5) # Aqui o rep ser� substituido com a melhor �poca
predicaoTeste <- ifelse(avaliarTeste>0.5, 1, 0)
matrizConfusaoTeste <- confusionMatrix(factor(predicaoTeste), factor(testeIono$label))


matrizConfusaoTreino #Visualiza��o da matriz de confus�o do conjunto de treino feita pelo pacote caret

matrizConfusaoTeste #Visualiza��o da matriz de confus�o do conjunto de teste feita pelo pacote caret







