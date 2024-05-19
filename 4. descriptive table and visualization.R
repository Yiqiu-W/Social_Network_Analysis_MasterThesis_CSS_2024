setwd("mypath")

library(sna)
library(tidyverse)
library(kableExtra)
library(ggplot2)
# Hamming distance
Hamming <- function(net1,net2) {
  tbl <- table(c(0,0,1,1,net1),c(0,1,0,1,net2))-1
  return(tbl[1,2]+tbl[2,1])
}

# Jaccard stability index
Jaccard <- function(net1,net2) {
  tbl <- table(c(0,0,1,1,net1),c(0,1,0,1,net2))-1
  return(tbl[2,2]/(tbl[1,2]+tbl[2,1]+tbl[2,2]))
}


### Friendship network
load("fr_variables_12100.Rdata")
v_12100 <- variablelist


net_desc <- function(class,network1,network2,network3,network4){
  result_df <- data.frame(
    # wave
  wave = c("wave 1","wave 2","wave 3","wave 4"),
    # node number
  N = c(nrow(network1),nrow(network2),nrow(network3),nrow(network4)),
    # ties
  E = c(sum(network1,na.rm=TRUE),sum(network2,na.rm=TRUE),sum(network3,na.rm=TRUE),sum(network4,na.rm=TRUE)),
    # density in percentage
  dens_per = c(round(gden(network1)*100,1),round(gden(network2)*100,1),round(gden(network3)*100,1),round(gden(network4)*100,1)),
    # average degree
  ave_deg = c(round(mean(degree(network1)),1),round(mean(degree(network2)),1),round(mean(degree(network3)),1),round(mean(degree(network4)),1)),
    # reciprocity in percentage
  recip_per = c(round(grecip(network1,measure='edgewise')*100,1),round(grecip(network2,measure='edgewise')*100,1),round(grecip(network3,measure='edgewise')*100,1),round(grecip(network4,measure='edgewise')*100,1)),
    # transitivity in percentage
  trans_per = c(round(gtrans(network1)*100,1),round(gtrans(network2)*100,1),round(gtrans(network3)*100,1),round(gtrans(network4)*100,1)),
    # number of isolate
  iso = c(length(isolates(network1)),length(isolates(network2)),length(isolates(network3)),length(isolates(network4))),
    # female percentage
  fem_per = rep((round(sum(class$gender[,2] == 0) / length(class$gender[,2]) * 100,1)),4),
    # roma percentage
  roma_per = rep((round(sum(class$roma[,2] == 1) / length(class$roma[,2]) *100,1)),4),
    # missing data(NA) in roma
  roma_Na = rep(sum(is.na(class$roma[,2])),4),
    # Hamming
  Hamm = c(" ",
            round(Hamming(network1,network2),1),
            round(Hamming(network2,network3),1),
            round(Hamming(network3,network4),1)),
    # Jaccard
  Jacc = c(" ",
            round(Jaccard(network1,network2),1),
            round(Jaccard(network2,network3),1),
            round(Jaccard(network3,network4),1))
  )
  return(result_df)
  }

df_1 <- net_desc(v_12100,v_12100$friendship[[1]],v_12100$friendship[[2]],v_12100$friendship[[3]],v_12100$friendship[[4]])

# apply the function to the rest 10 classrooms
# 14100
rm(variablelist,v_12100) # in case of carelessness

load("fr_variables_14100.Rdata")
v_14100 <- variablelist
df_2 <- net_desc(v_14100,v_14100$friendship[[1]],v_14100$friendship[[2]],v_14100$friendship[[3]],v_14100$friendship[[4]])

# 21200
rm(variablelist,v_14100)
load("fr_variables_21200.Rdata")
v_21200 <- variablelist
df_3 <- net_desc(v_21200,v_21200$friendship[[1]],v_21200$friendship[[2]],v_21200$friendship[[3]],v_21200$friendship[[4]])

# 23100
rm(variablelist,v_21200)
load("fr_variables_23100.Rdata")
v_23100 <- variablelist
df_4 <- net_desc(v_23100,v_23100$friendship[[1]],v_23100$friendship[[2]],v_23100$friendship[[3]],v_23100$friendship[[4]])

# 31100
rm(variablelist,v_23100)
load("fr_variables_31100.Rdata")
v_31100 <- variablelist
df_5 <- net_desc(v_31100,v_31100$friendship[[1]],v_31100$friendship[[2]],v_31100$friendship[[3]],v_31100$friendship[[4]])

# 31200
rm(variablelist,v_31100)
load("fr_variables_31200.Rdata")
v_31200 <- variablelist
df_6 <- net_desc(v_31200,v_31200$friendship[[1]],v_31200$friendship[[2]],v_31200$friendship[[3]],v_31200$friendship[[4]])

# 43200
rm(variablelist,v_31200)
load("fr_variables_43200.Rdata")
v_43200 <- variablelist
df_7 <- net_desc(v_43200,v_43200$friendship[[1]],v_43200$friendship[[2]],v_43200$friendship[[3]],v_43200$friendship[[4]])

# 55200
rm(variablelist,v_43200)
load("fr_variables_55200.Rdata")
v_55200 <- variablelist
df_8 <- net_desc(v_55200,v_55200$friendship[[1]],v_55200$friendship[[2]],v_55200$friendship[[3]],v_55200$friendship[[4]])

# 57100
rm(variablelist,v_55200)
load("fr_variables_57100.Rdata")
v_57100 <- variablelist
df_9 <- net_desc(v_57100,v_57100$friendship[[1]],v_57100$friendship[[2]],v_57100$friendship[[3]],v_57100$friendship[[4]])

# 63100
rm(variablelist,v_57100)
load("fr_variables_63100.Rdata")
v_63100 <- variablelist
df_10 <- net_desc(v_63100,v_63100$friendship[[1]],v_63100$friendship[[2]],v_63100$friendship[[3]],v_63100$friendship[[4]])

# 67100
rm(variablelist,v_63100)
load("fr_variables_67100.Rdata")
v_67100 <- variablelist
df_11 <- net_desc(v_67100,v_67100$friendship[[1]],v_67100$friendship[[2]],v_67100$friendship[[3]],v_67100$friendship[[4]])



final_fr_df <- rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10,df_11)
names(final_fr_df) <- c("","N","E","Density
                        (%)","Average 
                        degree","Reciprocity
                        (%)","Transitivity
                        (%)","Isolates","Female
                        (%)","Roma
                        (%)","Roma_NA","Hamming","Jaccard")

kbl(final_fr_df, caption = "Table 2: Descriptives Statistics: Friendship Networks", booktabs = T) %>% 
  kable_styling(font_size = 10, htmltable_class = "lightable-classic", html_font = "Times New Roman",full_width = F) %>%  
  pack_rows(group_label = "class 12100", 1,4) %>% 
  pack_rows("class 14100", 5,8) %>% 
  pack_rows("class 21200", 9,12) %>% 
  pack_rows("class 23100",13,16) %>% 
  pack_rows("class 31100",17,20) %>% 
  pack_rows("class 31200",21,24)%>% 
  pack_rows("class 43200",25,28)%>% 
  pack_rows("class 55200",29,32)%>% 
  pack_rows("class 57100",33,36) %>% 
  pack_rows("class 63100",37,40) %>%
  pack_rows("class 67100",41,44) %>%
  footnote("Data Source:RECENS (2017), N = 184
           Roma_NA indicates the number of students who did not provide ethnictity related information throughout the four waves.") %>% 
  save_kable("network_descr_fr.html", bs_theme = "flatly")

# do the same for dislike and gossip perception


# Plot 
# Wave 1
library(igraph)
# Convert adjacency matrices to graph objects
graph1 <- graph_from_adjacency_matrix(v_12100$friendship[[1]], mode = "directed")
graph2 <- graph_from_adjacency_matrix(v_12100$gossip[[1]], mode = "directed")
gender_colors <- ifelse(v_12100$gender[, 2] == 1, "blue", "pink")

# Make layouts for plotting
layout_coords <- layout.fruchterman.reingold(graph1+graph2)
# Plot the first network
png(filename = "12100_W1.png",600,600)
plot(graph1, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "black", edge.arrow.size = 0.6)

# Add edges from the second network to the plot with a different color
plot(graph2, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "red", edge.arrow.size = 0.6, add = TRUE)

while (!is.null(dev.list()))  dev.off()


# Wave 2
graph3 <- graph_from_adjacency_matrix(v_12100$friendship[[2]], mode = "directed")
graph4 <- graph_from_adjacency_matrix(v_12100$gossip[[2]], mode = "directed")
png(filename = "12100_W2.png",600,600)
plot(graph3, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "black", edge.arrow.size = 0.6)
plot(graph4, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "red", edge.arrow.size = 0.6, add = TRUE)
while (!is.null(dev.list()))  dev.off()

# Wave 3
graph5 <- graph_from_adjacency_matrix(v_12100$friendship[[3]], mode = "directed")
graph6 <- graph_from_adjacency_matrix(v_12100$gossip[[3]], mode = "directed")
png(filename = "12100_W3.png",600,600)
plot(graph5, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "black", edge.arrow.size = 0.6)
plot(graph6, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "red", edge.arrow.size = 0.6, add = TRUE)
while (!is.null(dev.list()))  dev.off()

# Wave 4
graph7 <- graph_from_adjacency_matrix(v_12100$friendship[[4]], mode = "directed")
graph8 <- graph_from_adjacency_matrix(v_12100$gossip[[4]], mode = "directed")
png(filename = "12100_W4.png",600,600)
plot(graph7, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "black", edge.arrow.size = 0.6)
plot(graph8, layout = layout_coords, vertex.size = 10, vertex.color = gender_colors, vertex.label = NA, edge.color = "red", edge.arrow.size = 0.6, add = TRUE)
while (!is.null(dev.list()))  dev.off()

# do the same for all the other classrooms and for the dislike network & gossip perception network
# remember to change the name of png file to distinguish friendship & gp and dislike & gp
