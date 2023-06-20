#Chamando as bibliotecas
library(gapminder)
library(ggplot2)
library(dplyr)

# Criando um novo dataset
gapminder_2007 <- gapminder %>% # o <- pode ser substituido por =
  filter(year == 2007) #filtrando para o ano de 2007
View(gapminder_2007)

# Gerando gráfico com o ggplot2
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth()
#Grafico 2 
ggplot(gapminder_2007, aes(x = pop, y = gdpPercap)) + 
  geom_point() +
  geom_smooth()
#Escalas Log no eixo x
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()+
  geom_smooth() +
  scale_x_log10() #log no eixo x
#Escalas Log nos dois eixos
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()+
  geom_smooth() +
  scale_x_log10()+ #log no eixo x
  scale_y_log10()  #log no eixo y
#Grafico com filtros para cor e para tamanho dos pontos
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent,
                           size = pop)) + #o comando color distingue a cor por continente
                                          # o comando size destingue o tamanho do ponto de acordo com a magnitude de uma variável
  geom_point()+
  scale_x_log10()
#Subplots 
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()+
  scale_x_log10()+ 
  facet_wrap(~continent) #divide os dados em relacao a uma variavel 

