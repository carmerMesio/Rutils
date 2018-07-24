##FAST PCA ANALISIS usin PRCOMP
soldat$y<-as.factor(soldat$y)
soldat$y<-mapvalues(soldat$y, from = c("-1", "1"), to = c("minone", "one"))
#detach("package:plyr", unload=TRUE)

resp<-soldat$y

knnFit2 <- preProcess(soldat[,-73], method =c("center","scale","knnImpute"))

soldatcent <- predict(knnFit2,soldat[,-73])

soldatcent <- cbind(soldatcent,resp)

correlations <- cor(soldat[,-73])
correlations <- melt(correlations)
correlations %>% filter(abs(value) > 0.5 & abs(value)<1) %>% distinct(value,.keep_all = TRUE) %>% group_by(Var1) %>% tally() %>% arrange(desc(n))

filter_df <- correlations %>% filter(abs(value)>0.5 & abs(value)<1) %>% distinct(value,.keep_all = TRUE) %>% select(Var1) %>% distinct(Var1,.keep_all = TRUE)

selvars <- data.frame(soldatcent) %>% select(resp,one_of(as.character(filter_df$Var1)))

corrgram::corrgram(selvars,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")

selvars <- selvars %>% select(-c(resp,x62,x61,x28))
pca = prcomp(selvars, scale. = F, center = F)
plot(pca, type="l")

sca=summary(pca)
plot(sca$importance[3,],type = 'o')

pca_df <- data.frame(pca$x[,1:20])

pca_df <- cbind(pca_df,resp)

PCbiplot <- function(PC, x="PC1", y="PC2") {
  data <- data.frame( PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y))
  datapc <- data.frame(varnames=row.names(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 3, vjust=1, color="darkred")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="black")
  plot
}

PCbiplot(pca)
