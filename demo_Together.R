library(superml)
library(tidyverse)
library(caret)
library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Metrics)
library(recosystem)
library(scales)
library(arrow)
library(recommenderlab)
library(cluster)
library(FactoMineR)
library(kknn)
library(caTools)
library(proxy)
library(stylo)
library(FNN)
`%notin%` <- Negate(`%in%`)


Combined = as.data.frame(read_parquet("Combined.parquet"))
simil = as.data.frame(read_parquet("simil.parquet"))
recommend = as.data.frame(read_parquet("recommend.parquet"))
content_rec = function(name) {
  idx = simil |>
    filter(simil$data2_name == name) |>
    as.data.frame() |>
    transpose()|>
    setNames( "similar_val")|>
    na.omit()
  
  # Sort the dataframe by 'similar_val' in descending order
  idx = idx |>
    cbind(colnames(simil))|>
    arrange(desc(similar_val))
  colnames(idx) = c("values", "name")
  
  return(as.data.frame(idx)[-c(1,2),])
}
#Recommedation
#head(content_rec(name = "Pacific Rim"),20)


collab_rec = function(name) {
  recommend2 = recommend|>
    filter(Movie1 == name | Movie2 == name | Movie3 == name | 
             Movie4 == name | Movie5 == name | Movie6 == name |
             Movie7 == name | Movie8 == name | Movie9 == name | Movie10 == name
    )
  scaled_recommendations = recommend2|>
    unlist(recommend2)|>
    as.data.frame()
  colnames(scaled_recommendations) = "name"
  scaled_recommendations =  scaled_recommendations|>
    group_by(name)|>
    summarise(number = n())|>
    arrange(desc(number))
  scaled_recommendations$number = scaled_recommendations$number/(dim(recommend2)[1])
  #print(dim(recommend2)[1])
  return(scaled_recommendations[-1,])
}
#head(collab_rec(name = "Pacific Rim"),20)


hybrid_rec = function(cont, collab){
  
  Hybrid = full_join(cont,collab,by="name")|>
    as.data.frame()|>
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  Hybrid$values = as.numeric(Hybrid$values)
  Hybrid$number = as.numeric(Hybrid$number)
  #Hybrid = Hybrid %>% dplyr::filter(!is.na(values))
  #ybrid = Hybrid %>% dplyr::filter(!is.na(number))
  Hybrid = Hybrid|>
    #values come from content based filtering, and number comes from collaborative filtering
    #collaborative filtering is weighted heavier than content due to it being 
    mutate(Combined_values = (.35*values + .65*number))|>
    arrange(desc(Combined_values))
  return(Hybrid)
}
#head(hybrid_rec(content_rec("Pacific Rim"), collab_rec("Pacific Rim")),20)




content_rec2 = function(multiplenames){
  together = content_rec(multiplenames[1]) 
  for(i in multiplenames[-1]){
    together = together |> mutate(values = as.numeric(values))
    together = bind_rows(together, content_rec(i) |> mutate(values = as.numeric(values)))|>
      group_by(name)|>
      summarise(values = sum(values))|>
      ungroup()|>
      arrange(desc(values))
  }
  return(together)#return(together[-which(together$name %in% multiplenames ),])
}
#head(content_rec2(list("Pacific Rim", "Superbad")))


collab_rec2 = function(multiplenames){
  together = collab_rec(multiplenames[1]) 
  for(i in multiplenames[-1]){
    together = bind_rows(together, collab_rec(i))|>
      group_by(name)|>
      mutate(number = as.numeric(number))|>
      summarise(number = sum(number))|>
      ungroup()|>
      arrange(desc(number))
  }
  return(together)#return(together[-which(together$name %in% multiplenames ),])
}
#head(collab_rec2(list("Pacific Rim", "Superbad")))


hybrid_rec2 = function(multiplenames){
  together = hybrid_rec(content_rec(multiplenames[1]),collab_rec(multiplenames[1])) 
  if(length(multiplenames) == 1){
    together = together %>% dplyr::filter(name %notin% multiplenames)
    return(together[,-c(1,3)])
  }
  for(i in multiplenames[-1]){
    together = bind_rows(together, hybrid_rec(content_rec(i),collab_rec(i)))|>
      group_by(name)|>
      mutate(Combined_values = as.numeric(Combined_values))|>
      summarise(Combined_values = sum(Combined_values))|>
      ungroup()|>
      arrange(desc(Combined_values))
  }
  return(together[-which(together$name %in% multiplenames ),]) 
}
names_of_movies = as.data.frame(colnames(simil))
#head(hybrid_rec2(list("Lady Bird", "Brothers", "Black Swan")), 20)
#head(hybrid_rec2(list("Avengers: Age of Ultron")), 20)

multiplenames = list("Men in Black")




#head(hybrid_rec2(multiplenames), 20)

