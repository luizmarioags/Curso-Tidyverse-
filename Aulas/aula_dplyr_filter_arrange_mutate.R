#Instalando as bibliotecas necessárias 
install.packages("gapminder") #A biblioteca Gapminder coleta indicadores e dados
install.packages("tidyverse") #Tidyverse contém o dplyr (manipulação de dados) 
                              #e o dplyr
#A instalação também pode ser feita no console do R

#Carregando as bibliotecas 
library(gapminder)
library(dplyr) 
#Vizualizando o gapminder
gapminder
#Como podemos ver na saída do console, a tabela possui 1704 linhas e 6 colunas

#Filtrando dados

#Filtrar dados significa pegar somente a parte que nos interessa dentro da base
#Para filtrar dados com o dplyr usamos o filter 
#Para que essa função funcione, usamos o pipe para dar o comando %>% 
#O pipe é colocado sempre ao lado do dataset que vamos trabalhar
#Usamos também operadores lógicos.
#Operadores Lógicos: == -> Igual; != -> Diferente, >= -> Maior ou igual
#                    <= -> Menor ou igual, > -> Maior, < -> Menor,
#                    !x -> Não é x, x|y -> x ou y, x & y -> x e y

#Exemplo 1: Filtre o gapminder para o ano de 2007 
gapminder %>% 
  filter(year == 2007) #Veremos somente observações do ano de 2007 

#Exemplo 2: Filtre para mostrar apenas dados dos Estados Unidos
gapminder %>% 
  filter(country == 'United States')
#Exemplo 3: Mostre somente dados dos Estados Unidos no ano de 2007 
gapminder %>% 
  filter(year == 2007, country == 'United States')

#Arrange 
#O arrange ordena e organiza as informações da base de dados
#Ele pode organizar de forma crescente ou decrescente
#Essa organização pode ser feita com base em uma ou mais variáveis 
#Exemplo 1 - Organize com base no Pib per Capita 
gapminder %>% 
  arrange(gdpPercap) #Dentro do parênteses colocamos a variável
                     #Ele sempre fará isso em ordem crescente (Padrão)
#Exemplo 2 - Organize o dataset na forma decrescente com base no Pip per Capita 
gapminder %>% 
  arrange(desc(gdpPercap))
#Exemplo 3 - Mostre dados organizados em ordem decrescente do PiB per Capita em 2007
gapminder %>% 
  #Usamos multiplas condições
  filter(year == 2007) %>%  #Sempre com o Pipe
  arrange(desc(gdpPercap)) 

#Mutate 

#O comando mutate muda o dataset, adicionando, removendo ou alterando as variáveis
#Exemplo 1 - Altere os dados de população para população por milhão 
gapminder %>% 
  mutate(pop = pop/1000000) #Este comando substitui os valores da coluna pop

#Exemplo 2 - Calcule o PIB, usando o mutate 
gapminder %>% 
  mutate(gdp = gdpPercap*pop) #Criamos uma nova coluna gdp 


