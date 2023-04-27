plot_bar_qual <- function(df, variables = NULL){
  # for(i in variables){
  #   if (class(df[,i]) == 'numeric'){
  #     stop("One of the variables is not a quality variable.")
  #   }
  # }
  k <- 1
  dl <- length(variables)
  for(i in 1:(dl-1)){
    if(dl%% i == 0){
      k <- i
    } else{
      k <- k
    }
  }
  frame <- as.data.frame(df %>% 
    select(variables))
  cols <- colnames(frame)
  p <- list()
  for(i in 1:dl){
    p[[i]] <- frame %>% 
      ggplot(aes(x = frame[,i]))+
      geom_bar(fill = 'royalblue1')+
      labs(x = cols[i], 
           y = 'Count',
           title = paste("Distribution of a column", cols[i]))
  }
  wrap_plots(p,nrow = k,ncol = dl/k)
}
library(dplyr)
library(ggplot2)
library(patchwork)
plot_bar_qual(mtcars,c(1,2,4,5,6,7))
mtcars
variables <- c(1,2,4,5)
frame <- iris %>% 
  select(c(1,2,4,5))

df <- iris
print(as.data.frame(frame) %>% 
        ggplot(aes(x = frame[,4]))+
        geom_bar(fill = 'royalblue1')+
        labs(x = cols[4], 
             y = 'Count',
             title = paste("Distribution of a column", cols[4])))
