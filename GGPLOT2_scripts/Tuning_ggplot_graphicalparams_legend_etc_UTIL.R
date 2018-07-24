library(dplyr)
library(ggplot2)
library(grid)
library(tidyr)
subpr <- c(83746.4,77552.2,82426.4,81346.4,80026.4,81272.2,79952.2,78872.2,77552.2)
master <- c(74000,77585.2 ,77596.2 ,77629.2 ,77640.2,77673.2 ,77684.2,77717.2,77717.2)
invest <- c(0,264,33,44,77,88,121,132,165)
x<-rep(1:9)
a <- subpr+invest
df <- data.frame(Iterations = x,Master = master, SubP_InvC = a)
plot1 <- df %>%
  ggplot() +
  geom_line(aes(x = Iterations, y = master), size = 1, alpha = 0.75) +
  ylab("Master Problem") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot2 <- df %>%
  ggplot() +
  geom_line(aes(x = Iterations, y = a), size = 1, alpha = 0.75) +
  ylab("Subproblema + Invest Costs") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

df %>% gather(tipo,valor,c(2:3)) %>% ggplot(aes(x=Iterations,y=valor,colour=tipo)) + geom_line(size=1)+
  labs(title = "Benders Iterations\n", x = "Iterations", y = "Objective values", color = "Problem Type\n") +
  scale_color_manual(labels = c("Master", "Subproblem + InvC"), values = c("blue", "red")) + theme_bw()
