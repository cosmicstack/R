library(ggplot2)
summary(mpg)

ggplot(data=mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, size=cty), color='steelblue') +
  #geom_histogram(mapping = aes(x=displ), binwidth = 0.05, color='black', fill='transparent')
  geom_smooth(aes(displ, cty), method = 'lm')