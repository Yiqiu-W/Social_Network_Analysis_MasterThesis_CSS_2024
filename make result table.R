library(kableExtra)
library(RSiena)
library(tidyverse)


setwd("mypath")
load("Meta.Parfull.Rdata")
load("overalls08.full.Rdata") # just to check effect names

parameters_df <- as.data.frame(parameters.08)


colnames(parameters_df)[1:2] <- c("mu_hat", "mu_se")

parameters.08.new <- parameters_df %>% 
  mutate(p_value = 2 * (1 - pnorm(abs(mu_hat / mu_se)))) %>% 
  mutate(star = ifelse(p_value < 0.001,"***",ifelse(p_value < 0.01,"**",ifelse(p_value < 0.05,"*",ifelse(p_value < 0.1,"+"," "))))) %>% 
  select(mu_hat,mu_se,star,N) %>% 
  mutate(mu_hat = round(mu_hat,3),
         mu_se = round(mu_se,3))

parameters.08.new$mu_se <- paste0("(",parameters.08.new$mu_se,")")

parameters.08.new <- parameters.08.new[c(1,2,3,4,5,10,11,12,13,6,7,8,9),]

parameters.08.new$eff <- c("Outdegree (Density)","Reciprocity","Transitive Triplets","Indegree - popularity (sqrt)",
                           "Outdegree - activity (sqrt)","Gender Alter","Gender Ego","Same Gender","Same Roma","GP","GP x Reciprocity",
                           "WW=>X Shared Outgoing GP","WX=>X Closure of GP")

parameters.08.new <- parameters.08.new %>% 
  select(eff,mu_hat,mu_se,star,N)
####
rm(parameters.08,parameters_df)
setwd("mypath")
load("Meta.Par1.Rdata")

parameters_df <- as.data.frame(parameters.08)

colnames(parameters_df)[1:2] <- c("mu_hat_b", "mu_se_b")

parameters.08.basic <- parameters_df %>% 
  mutate(p_value = 2 * (1 - pnorm(abs(mu_hat_b / mu_se_b)))) %>% 
  mutate(star_b = ifelse(p_value < 0.001,"***",ifelse(p_value < 0.01,"**",ifelse(p_value < 0.05,"*",ifelse(p_value < 0.1,"+"," "))))) %>% 
  mutate(mu_hat_b = round(mu_hat_b,3),
         mu_se_b = round(mu_se_b,3),
         N_b = N) %>% 
  select(mu_hat_b,mu_se_b,star_b,N_b)

parameters.08.basic$mu_se_b <- paste0("(",parameters.08.basic$mu_se_b,")")

parameters.08.basic$eff <- c("Outdegree (Density)","Reciprocity","Transitive Triplets","Indegree - popularity (sqrt)",
                           "Outdegree - activity (sqrt)","Gender Alter","Gender Ego","Same Gender","Same Roma")

parameters.08.basic <- parameters.08.basic %>% 
  select(eff,mu_hat_b,mu_se_b,star_b,N_b)



df <- merge(parameters.08.basic,parameters.08.new,by="eff", all=TRUE)
df <- df[c(7,8,11,5,6,1,2,9,10,3,4,12,13),]


df[is.na(df)] <- ""
rownames(df) <- NULL

setwd("mypath")
kbl(df, caption = "Table 7: Results of Meta-Analysis(Friendship Network)", booktabs = T,col.names = c("Effect","Est.","(s.e)."," ","C","Est.","(s.e.)"," ","C")) %>% 
  kable_styling(font_size = 12, htmltable_class = "lightable-classic", html_font = "Times New Roman",full_width = F) %>% 
  add_header_above(c(" " = 1, "Basic Model" = 4, "Full Model" = 4)) %>% 
  footnote("Data Source:RECENS (2017), N = 184.
           C indicates the number of classrooms used.
           *** = p < 0.001, ** = p < 0.01, * = p < 0.05, + = p < 0.1
") %>% 
  save_kable("meta.html", bs_theme = "flatly")
  

