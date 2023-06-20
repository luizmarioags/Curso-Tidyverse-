#Chamando as bibliotecas que vamos usar
library(gapminder)
library(dplyr)
#Summarize

#o summarize() transforma varias linhas em uma única
#Existem funções embutidas do R que podem ajudar a compreender certos dados
#uma delas é a mean() que retorna a média e a sum() que soma todos os elementos 
#median() retorna a mediana 
#min() retorna o valor minimo, analogamente max() retorna o máximo 

#Exemplo 1 - Faça a média da expectativa de vida no ano de 2007
gapminder %>% #comando pipe para iniciar a analise
  #filtrando incialmente para pegar só os dados do ano de 2007
  filter(year == 2007) %>% 
  #fazendo o summarize
  summarize(media_expec_vida = mean(lifeExp)) #salva na coluna media_expec_vida

#Exemplo 2 - Faça a media da expectativa de vida e encontre o maior PIB per Capita (gdpPerCap) para o ano de 2007
gapminder %>% 
  filter(year == 2007) %>% 
  #fazendo o summarize para as duas variáveis
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

#Group_By

#o group_by resume os dados em grupos especificos e combina os resultados

#Exemplo 1 - Qual é a média da expectativa de vida e a população total em cada continente no ano de 2007 ?
gapminder %>% 
  filter(year == 2007) %>% #filtra para 2007
  group_by(continent) %>% #agrupar por continente 
  summarize(media_expec_vida = mean(lifeExp),
            pop_total = sum(pop)) #soma a populacao

#Exemplo 2 - Como esses dados se alteraram no decorrer do tempo ? 
# Para fazer isso iremos agrupar por continente e ano 
gapminder %>% 
  group_by(year,continent) %>% #agrupando por continente e ano 
  summarize(pop_total = sum(pop),
            media_expec_vida = mean(lifeExp))
#Vizualizando dados agrupados 
library(ggplot2) #Chamando a biblioteca para os gráficos
#Definindo um novo data set por ano 
por_ano <- gapminder %>%
  group_by(year) %>%
  summarize(total_pop = sum(pop))
View(por_ano)
#Fazendo o grafico 
ggplot(por_ano, aes(x = year, y = total_pop)) + 
  geom_point() + 
  expand_limits(y = 0) #Colocando o grafico para começar no eixo y

#Fazendo o gráfico para vários continentes 
por_ano_continente <- gapminder %>% 
    group_by(year,continent) %>% #agrupando por continente e ano 
    summarize(pop_total = sum(pop),
              media_expec_vida = mean(lifeExp))
ggplot(por_ano_continente, aes(x = year, y = pop_total, color = continent)) + 
  geom_point() + 
  expand_limits(y = 0)
