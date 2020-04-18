#Ejer de Grammar


data("faithful")
# Basic scatterplot
ggplot(data = faithful, 
       mapping = aes(x = eruptions, y = waiting)) + 
  geom_point()
