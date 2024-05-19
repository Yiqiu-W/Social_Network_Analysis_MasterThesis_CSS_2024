library(tidyverse)
library(sna)
library(RSiena)
classroom <-12100   # change to relevant school id
path <- "mypath"
wave_code_fr <- c("14_1_1","16_1_2","18_1_3","20_1_4") # wave code for friendship
wave_code_gp <- c("29_3_1","30_3_2","32_1_3","36_3_4") # wave code for gossip perception

## function preparation
# load my network based on path made of type(friendship or gp), classroom(e.g. 12100) and wave_code
load_my_net <- function(type, classroom, wave_code){
  net <- read.csv(paste0(path,type, '/',classroom, '_', wave_code,'.csv' ))
  colnames(net) <- rownames(net)
  net <- as.matrix(net)
  return(net)
} 

# imputation function
replace_NA_with_previous <- function(net_1, net_2) {
  # Iterate over rows and columns of net_2
  for(i in 1:nrow(net_2)) {
    for(j in 1:ncol(net_2)) {
      # If cell in net_2 is NA, replace it with corresponding cell from net_1
      if(is.na(net_2[i, j])) {
        net_2[i, j] <- net_1[i, j]
      }
    }
  }
  return(net_2)
}

# Jaccard stability index
Jaccard <- function(net1,net2) {
  tbl <- table(c(0,0,1,1,net1),c(0,1,0,1,net2))-1
  return(tbl[2,2]/(tbl[1,2]+tbl[2,1]+tbl[2,2]))
}




# wave 1
fr_1 <- load_my_net("friendship", classroom, wave_code_fr[1])
gp_1 <- load_my_net("gp", classroom, wave_code_gp[1])

# wave 2
fr_2 <- load_my_net("friendship", classroom, wave_code_fr[2])
gp_2 <- load_my_net("gp", classroom, wave_code_gp[2])

# wave 3
fr_3 <- load_my_net("friendship", classroom, wave_code_fr[3])
gp_3 <- load_my_net("gp", classroom, wave_code_gp[3])

# wave 4
fr_4 <- load_my_net("friendship", classroom, wave_code_fr[4])
gp_4 <- load_my_net("gp", classroom, wave_code_gp[4])

# student who were in this classroom throughout the four waves
common_students <- Reduce(intersect, list(rownames(fr_1), rownames(fr_2), rownames(fr_3), rownames(fr_4),
                                          rownames(gp_1), rownames(gp_2), rownames(gp_3)))
# we have 16 students. Good, the networks are not too small
# keep only those students in our networks
fr_1 <- fr_1[common_students,common_students]
fr_2 <- fr_2[common_students,common_students]
fr_3 <- fr_3[common_students,common_students]
fr_4 <- fr_4[common_students,common_students]

gp_1 <- gp_1[common_students,common_students]
gp_2 <- gp_2[common_students,common_students]
gp_3 <- gp_3[common_students,common_students]
gp_4 <- gp_4[common_students,common_students]

# it is not intuitive to let students give rating to themselves
# make the diagonal 3 for friendship and 0 for gossip perception first
diag(fr_1) <- 3
diag(fr_2) <- 3
diag(fr_3) <- 3
diag(fr_4) <- 3

diag(gp_1) <- 0
diag(gp_2) <- 0
diag(gp_3) <- 0
diag(gp_4) <- 0

# all attributes for the students left
gender <- read.csv("mypath/the_attribute_file") %>% 
          filter(idcode %in% common_students) %>% 
          select(idcode,gender)
roma <- read.csv("mypath/the_attribute_file") %>% 
  filter(idcode %in% common_students) %>% 
  select(idcode,roma)
# good, there are no missing values


## check if there are too many NAs in the original network
# for classroom 12100, it appears that student 12109 did not reveal his/her attitude toward other classmate
# either in friendship network(all NAs) or gossip perception network(only 1 reponse)
# but he/she did get comments from other classmates
# However, we also have 0 in some responses in friendship network which should also be seen as 0
fr_1[fr_1 == 0] <- NA
fr_2[fr_2 == 0] <- NA
fr_3[fr_3 == 0] <- NA
fr_4[fr_4 == 0] <- NA
# how many NAs in the network
sum(is.na(fr_1))
sum(is.na(fr_2))
sum(is.na(fr_3)) 
sum(is.na(fr_4))

sum(is.na(gp_1)) 
sum(is.na(gp_2)) 
sum(is.na(gp_3)) 
sum(is.na(gp_4)) 

# percentage of NAs
sum(is.na(fr_1)) / length(fr_1) *100   
sum(is.na(fr_2)) / length(fr_2) *100  
sum(is.na(fr_3)) / length(fr_3) *100  
sum(is.na(fr_4)) / length(fr_4) *100 

sum(is.na(gp_1)) / length(gp_1) *100
sum(is.na(gp_2)) / length(gp_2) *100 
sum(is.na(gp_3)) / length(gp_3) *100 
sum(is.na(gp_4)) / length(gp_4) *100 
# acceptable if all are lower than 20%


## now we impute the missing data and create our friendship and disliking networks

## Friendship and Disliking
# first, we look at the first wave of friendship and gossip perception
# in friendship network, we here see NA as being neutral(3) OR give value 0 directly
# wave 1
fr_1[is.na(fr_1)] <- 3


# imputation
# replace NAs in wave 2 to wave 4 based on their previous wave 
fr_2 <- replace_NA_with_previous(fr_1,fr_2)
fr_3 <- replace_NA_with_previous(fr_2,fr_3)
fr_4 <- replace_NA_with_previous(fr_3,fr_4)


## dichotomizing

# friendship
# a tie is considered existing when the value in the original friendship network is 4 or 5
friendship_1 <- fr_1
friendship_1[friendship_1 %in% 1:3] <- 0
friendship_1[friendship_1 %in% 4:5] <- 1

friendship_2 <- fr_2
friendship_2[friendship_2 %in% 1:3] <- 0
friendship_2[friendship_2 %in% 4:5] <- 1

friendship_3 <- fr_3
friendship_3[friendship_3 %in% 1:3] <- 0
friendship_3[friendship_3 %in% 4:5] <- 1

friendship_4 <- fr_4
friendship_4[friendship_4 %in% 1:3] <- 0
friendship_4[friendship_4 %in% 4:5] <- 1

# disliking
# a tie is considered existing when the value in the original friendship network is 1 or 2
dislike_1 <- fr_1
dislike_1[dislike_1 %in% 3:5] <- 0
dislike_1[dislike_1 %in% 1:2] <- 1

dislike_2 <- fr_2
dislike_2[dislike_2 %in% 3:5] <- 0
dislike_2[dislike_2 %in% 1:2] <- 1

dislike_3 <- fr_3
dislike_3[dislike_3 %in% 3:5] <- 0
dislike_3[dislike_3 %in% 1:2] <- 1

dislike_4 <- fr_4
dislike_4[dislike_4 %in% 3:5] <- 0
dislike_4[dislike_4 %in% 1:2] <- 1

## GP
gp_1[is.na(gp_1)] <- 0

# impute
# replace NAs in wave 2 to wave 4 based on their previous wave 
gp_2 <- replace_NA_with_previous(gp_1,gp_2)
gp_3 <- replace_NA_with_previous(gp_2,gp_3)
gp_4 <- replace_NA_with_previous(gp_3,gp_4)


## Jaccard index and density check
Jaccard(friendship_1, friendship_2)  
Jaccard(friendship_2, friendship_3)  
Jaccard(friendship_3, friendship_4)   # nice, all stable enough(>0.3)
gden(friendship_1) 
gden(friendship_2) 
gden(friendship_3)
gden(friendship_4) 

Jaccard(dislike_1, dislike_2) 
Jaccard(dislike_2, dislike_3) 
Jaccard(dislike_3, dislike_4)   # close to 0.3 or above # acceptable
gden(dislike_1) 
gden(dislike_2) 
gden(dislike_3) 
gden(dislike_4) 

Jaccard(gp_1, gp_2) 
Jaccard(gp_2, gp_3) 
Jaccard(gp_3, gp_4)
gden(gp_1) 
gden(gp_2) 
gden(gp_3) 
gden(gp_4) # okay

# save networks and variables for descriptive table later
# friendship
variablelist <-list(list(friendship_1,friendship_2,friendship_3,friendship_4),list(gp_1,gp_2,gp_3,gp_4),gender,roma)

names(variablelist) <- c("friendship","gossip","gender","roma")

setwd("mywd")
save(variablelist, file = paste0("fr_variables_",classroom,".Rdata"))

# dislike
variablelist <-list(list(dislike_1,dislike_2,dislike_3,dislike_4),list(gp_1,gp_2,gp_3,gp_4),gender,roma)
names(variablelist) <- c("dislike","gossip","gender","roma")
save(variablelist, file = paste0("dl_variables_",classroom,".Rdata"))

#### make siena object
# the number of students
numberActors <- nrow(friendship_1)

# dependent networks
friendship <- sienaDependent(
  array(c(friendship_1,friendship_2,friendship_3,friendship_4),
  dim = c(numberActors,numberActors,4)),allowOnly = F)

dislike <- sienaDependent(
  array(c(dislike_1,dislike_2,dislike_3,dislike_4),
        dim = c(numberActors,numberActors,4)),allowOnly = F)

# independent networks
gp <- varDyadCovar(array(c(gp_1,gp_2,gp_3),
                   dim=c(numberActors,numberActors,3)))

# gender
gender <- coCovar(gender[,2])
# roma
roma <- coCovar(roma[,2])

### save the object
setwd("thepath")
# friendship as dependent
netDynamics_fr <- sienaDataCreate(friendship, gp, gender, roma)
save(netDynamics_fr, file=paste0('netDynamics_fr_gp',classroom,'.Rdata'))
# dislike as dependent
netDynamics_dl <- sienaDataCreate(dislike, gp, gender, roma)
save(netDynamics_dl, file=paste0('netDynamics_dl_gp',classroom,'.Rdata'))
