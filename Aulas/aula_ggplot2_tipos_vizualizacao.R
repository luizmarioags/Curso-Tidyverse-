#Chamando as bibliotecas
library(ggplot2)
library(dplyr)
library(gapminder)
#Gráficos de Linha
#Criando Dataframe
por_ano_continente <- gapminder %>%
  group_by(continent,year) %>%
  summarize(meanLifeExp = mean(lifeExp))
#Para fazer um gráfico de linha adicionamos geom_line()
#Fazemos essa adição no lugar de geom_point()
ggplot(por_ano_continente,aes(x = year, y = meanLifeExp, color = continent)) + 
  geom_line() + #Especifica como gráfico de linha
  expand_limits(y = 0)

#Gráfico de Barras
#Criando Dataframe
by_continent <- gapminder %>% 
  filter(year == 2007) %>% #filtra para 2007
  group_by(continent) %>% #agrupar por continente 
  summarize(meanLifeExp = mean(lifeExp),
            pop_total = sum(pop)) #soma a populacao

#Para fazer um gráfico de barra adicionamos geom_col()
ggplot(by_continent, aes(x = continent, y = meanLifeExp)) +
  geom_col() #Especifica como gráfico de barra

#Em gráficos de barras o nosso x sempre é a variável categórica

#Histogramas

#Histogramas mostram a distribuição de uma variável 
#Cada barra do histograma representa um compartimento da variável 
#A altura da barra representa quantas observaçoes existem dentro desse compartimento 
#Para fazer histogramas usamos o geom_histogram()
#O compartimento pode ser alterado usando o bin dentro do geom_histogram()
#Exemplo
gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Histograma da população
ggplot(gapminder_1952, aes(x=pop_by_mil)) + 
  geom_histogram()

#Gráfico de Boxplot 
#Exemplo 
#Crie o data set filtrando para o ano de 1952
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Faça o boxplot comparando o PIB per Capita por continente
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) + 
  geom_boxplot() + 
  scale_y_log10() #colocando o eixo y na escala log

#Adicionando título no gráfico
# Para adicionar título usamos o labs(title ='')
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  labs(title = 'Comparando o PIB per Capita por continente') #Adiciona título

