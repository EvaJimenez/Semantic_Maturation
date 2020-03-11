###################################################
## 4 # STUDY 2: LATE TALKERS VS TYPICAL TALKERS ###
###################################################

## 1 ## Load Word and Sentences CDI data ####

setwd('XXX') 
load("Nouns_Verbs_df.RData") # Create in R file 2
load("CHILDES_matrix.RData")
load("childbg.RData")
load("CDI_SENTENCES.RData")

length(unique(CDI_SENTENCES$data_id))## 5520 unique children 
# see 'subjectIDs_Study2.csv' for the id of the children's vocabularies downloaded from TALKBANK

## 2 ## Count number of words produced ####

# Create column ready for whether word is produced
CDI_SENTENCES$value[is.na(CDI_SENTENCES$value)] <- ""
CDI_SENTENCES$wordproduced <- FALSE
CDI_SENTENCES$wordproduced <- (CDI_SENTENCES$value == "produces" & CDI_SENTENCES$type  == "word")


# Create column ready for whether word can be matched with words selected for analysis
CDI_SENTENCES$For_analysis <- FALSE
CDI_SENTENCES$For_analysis <- ( CDI_SENTENCES$definition  %in%  Nouns_Verbs_df$Words_CDI )

# Create column ready for : 'For_analysis' and 'wordproduced'
CDI_SENTENCES$For_analysis_And_Produced <- FALSE
CDI_SENTENCES$For_analysis_And_Produced <- (CDI_SENTENCES$wordproduced == TRUE & CDI_SENTENCES$For_analysis == TRUE)

CDI_SENTENCES[1:10,]

# Create column in 'childbg' ready for count of words produced
childbg$count_produced <- 0.0
childbg$count_produced_analysis <- 0.0

# Find the sum of (TRUE/FALSE) to get the number of words produced per child
childbg$count_produced         <- aggregate(CDI_SENTENCES$wordproduced, by=list(Category=CDI_SENTENCES$data_id), FUN=sum)[,2]
childbg$count_produced_analysis <- aggregate(CDI_SENTENCES$For_analysis_And_Produced,     by=list(Category=CDI_SENTENCES$data_id), FUN=sum)[,2]

# Find the % loss in words produced reported when we consider subset (wordsw matching with CHILDES CoOc)
childbg$percentage_produced_loss <- 100 - (100 / childbg$count_produced * childbg$count_produced_analysis)

# Insert word class in 'CDI_SENTENCES'
CDI_SENTENCES$Class <- NA
for (i in 1:length(Nouns_CDI_original)) {
  CDI_SENTENCES$Class[which(CDI_SENTENCES$definition==Nouns_CDI_original[i])] <- "Noun"
}

for (i in 1:length(Verbs_CDI)) {
  CDI_SENTENCES$Class[which(CDI_SENTENCES$definition==Verbs_CDI[i])] <- "Verb"
}

# Number of nouns and verbs produced
childbg$count_produced_analysis_NOUNS <- aggregate(CDI_SENTENCES$For_analysis_And_Produced[CDI_SENTENCES$Class=="Noun"],   
                                                   by=list(Category=CDI_SENTENCES$data_id[CDI_SENTENCES$Class=="Noun"]), FUN=sum)[,2]

childbg$count_produced_analysis_VERBS <- aggregate(CDI_SENTENCES$For_analysis_And_Produced[CDI_SENTENCES$Class=="Verb"],   
                                                   by=list(Category=CDI_SENTENCES$data_id[CDI_SENTENCES$Class=="Verb"]), FUN=sum)[,2]

childbg$count_produced_analysis_NOUNS_VERBS <- aggregate(CDI_SENTENCES$For_analysis_And_Produced[CDI_SENTENCES$Class=="Verb" |CDI_SENTENCES$Class=="Noun"],   
                                                         by=list(Category=CDI_SENTENCES$data_id[CDI_SENTENCES$Class=="Verb"|CDI_SENTENCES$Class=="Noun"]), FUN=sum)[,2]
## 3 ## Find the childs percentile based on total number of words known and identify late talkers ####
CDI_vocabulary_norm_WS <- read.csv(file = "CDI_vocabulary_norm_WS.csv", header = T)

## Function to get Childs percentile given age and number of words known

childsPercentile <- function(age, nrofWords) {
  
  searchspace <- CDI_vocabulary_norm_WS[ match( age, CDI_vocabulary_norm_WS$age) , 6:15]
  
  # find the value that is closest to x
  maxless <- max(searchspace[searchspace <= nrofWords], 0)
  # find out which value that is
  retvalue <- which(searchspace == maxless)
  if (is.integer0(retvalue)) { 
    retvalue <- 1 
  } else {
    retvalue <- retvalue + 1
  }
  return(retvalue / 10)
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

# Bring in the normal distributed data/
CDI_vocabulary_norm_WS$X1 <- 999.99
childbg$percentile_NORM <- 0.0

# Use the above function to calculate each childs percentile
for (childiter in 1:length(childbg$data_id)) { 
  childbg$percentile_NORM[childiter] <- childsPercentile( childbg$age[childiter] ,
                                                          childbg$count_produced[childiter] )
}

childbg$type_talker <- NA

for (i in 1:nrow(childbg)) {
  if(childbg$percentile_NORM[i]<=0.2){
    childbg$type_talker[i] <- "late_talker"
  }
  else{
    childbg$type_talker[i] <- "typical_talker"
  }
}

## 4 ## Create a matrix with the words produced by children  ####

childWord_matrix <- matrix(FALSE, length(childbg$data_id), length(Nouns_Verbs_df$Words_CDI) )
rownames(childWord_matrix) <- childbg$data_id
colnames(childWord_matrix) <- Nouns_Verbs_df$Words_CDI
childWord_matrix[1:10, 1:10]

# iterate through children 
for(i in 1:length(childbg$data_id)) {
  
  print(i)
  wordsknownbythischild <-as.character(CDI_SENTENCES$definition[CDI_SENTENCES$data_id == childbg$data_id[i] &
                                                                  CDI_SENTENCES$For_analysis_And_Produced == TRUE])
  childWord_matrix[ i,   match( wordsknownbythischild , colnames(childWord_matrix)) ] <- TRUE
}

# Number of LTs and TTs in total 

dim(childbg)
table(childbg$type_talker) # LT: 1117, TT: 4403
# Minimum one word produced
childbg <- childbg[childbg$count_produced_analysis>0,]
table(childbg$type_talker) # LT: 1094, TT: 4403

#Percentage of late talkers in the sample
table(childbg$type_talker)[1] * 100 / table(childbg$type_talker)[2]

###################### NOUNS ###################### 
## 1 ## Selection of children to analyse for nouns ####
# Criteria:
# Minimun of 10 nouns
# Maximun of words is the maximun of words produced by the late talker group, excluding outliers

# Minimun and maximum of nouns produced 
min(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="late_talker"]) # 0 # 
max(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="late_talker"]) # 229 # 

min(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="typical_talker"]) # 5 #
max(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="typical_talker"]) # 286 # 

hist(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="late_talker"])
hist(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="typical_talker"])

table(childbg$count_produced_analysis_NOUNS[childbg$type_talker=="late_talker"]) # three outliers
childbg_NOUNS <- childbg[childbg$count_produced_analysis_NOUNS>=10 & childbg$count_produced_analysis_NOUNS<= 190,] # minimun 10 words

# Number of LTs and TTs for the nouns analysis
dim(childbg_NOUNS)
table(childbg_NOUNS$type_talker) # TTs: 2585  , LTs:  626

## 2 ## Adapt dataset to contain only nouns ####

# In child Directed Speech: CHILDES_matrix_NOUNS
CHILDES_matrix_NOUNS <- CHILDES_matrix[match(tolower(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]),tolower(rownames(CHILDES_matrix))),
                                           match(tolower(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]),tolower(colnames(CHILDES_matrix)))]
dim(CHILDES_matrix_NOUNS) # 286 nouns


# In children's vocabularies: childWord_matrix_NOUNS
childWord_matrix_NOUNS <- childWord_matrix[match(childbg_NOUNS$data_id,rownames(childWord_matrix))
                                           ,match(tolower(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]),tolower(colnames(childWord_matrix)))]
dim(childWord_matrix_NOUNS)
dim(childbg_NOUNS)

# Contextual_diversity
## 3 ## Compute contextual diversity average for each child ####

childbg_NOUNS$contextual_diversity_NOUNS_ave <- NA

for (i in 1:nrow(childWord_matrix_NOUNS)) {
  id <- rownames(childWord_matrix_NOUNS)[i]
  child <- childWord_matrix_NOUNS[i,]
  words_prod <- names(child[child==TRUE])
  a <- match(words_prod,as.character(Nouns_Verbs_df$Words_CDI))
  a <- a[!is.na(a)]
  a <- Nouns_Verbs_df$contextual_diversity[a]
  a <- a[!is.na(a)]
  childbg_NOUNS$contextual_diversity_NOUNS_ave[childbg_NOUNS$data_id==id] <- mean(a)
  
}



## 4 ## Compute network statistics for each child ####

childbg_NOUNS$cc_local <- NA
childbg_NOUNS$ave_path_length <- NA

# Sort "CHILDES_matrix_NOUNS" alphabetically and make sure it is a matrix
#CHILDES_matrix_NOUNS <- as.data.frame(CHILDES_matrix_NOUNS)
CHILDES_matrix_NOUNS <- CHILDES_matrix_NOUNS[,order(colnames(CHILDES_matrix_NOUNS))]
CHILDES_matrix_NOUNS <- CHILDES_matrix_NOUNS[order(rownames(CHILDES_matrix_NOUNS)),]
CHILDES_matrix_NOUNS <- as.matrix(CHILDES_matrix_NOUNS)
CHILDES_matrix_NOUNS[1:10,1:10]

# Sort columns in "childWord_matrix_NOUNS" alphabetically
childWord_matrix_NOUNS <- childWord_matrix_NOUNS[,order(colnames(childWord_matrix_NOUNS))]
childWord_matrix_NOUNS[1:10,1:10]
dim(childWord_matrix_NOUNS)

# Test that columns in  "CHILDES_matrix_NOUNS" and  "childWord_matrix_NOUNS" match in order
test_match_order2 <- function(x,y) {
  
  if (isTRUE(all.equal(x,y))) print('Perfect match in same order')
  
  if (!isTRUE(all.equal(x,y)) && isTRUE(all.equal(sort(x),sort(y)))) print('Perfect match in wrong order')
  
  if (!isTRUE(all.equal(x,y)) && !isTRUE(all.equal(sort(x),sort(y)))) print('No match')
}

test_match_order2(colnames(childWord_matrix_NOUNS),colnames(CHILDES_matrix_NOUNS))


# Compute network properties
library(igraph)
for(child_iter in 1:length(childbg_NOUNS$data_id) ) {
  
  print(child_iter)
  
  
  if(childbg_NOUNS$count_produced_analysis_NOUNS[child_iter]  > 1) {
    child_indiv_matrix <- CHILDES_matrix_NOUNS[ childWord_matrix_NOUNS[child_iter,] , childWord_matrix_NOUNS[child_iter,] ]
    
    
    child_indiv_network = graph.adjacency(child_indiv_matrix , mode="undirected",   weighted=NULL, diag=FALSE)   
    
    childbg_NOUNS[child_iter, c("cc_local") ]   <- transitivity(child_indiv_network, type = c("localaverage"))
    childbg_NOUNS[child_iter, c("ave_path_length") ] <- average.path.length(child_indiv_network)
    
  }
  
}

childbg_NOUNS[1:10,]

## 5 ## Description of the sample ####

# Unique children in both analysis #

all_part <- c(unique(childbg_NOUNS$data_id),unique(childbg_VERBS$data_id))
all_part <- unique(all_part)
length(all_part)
#Number of verba and late_talker
table(childbg_NOUNS$type_talker)
dim(childbg_NOUNS)

# Age differece? 
wilcox.test(childbg_NOUNS$age[childbg_NOUNS$type_talker=="late_talker"],
            childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"],  conf.int = TRUE, conf.level = 0.95)
median(childbg_GEST_typical_talker$age)
median(childbg_GEST_late_talker$age)


# Percentage of late talkers 
table(childbg_NOUNS$type_talker)[1] * 100 / table(childbg_NOUNS$type_talker)[2]

# Percentage of female 
table(childbg_NOUNS$sex)[1] * 100 / table(childbg_NOUNS$sex)[2]

# Percentage of late talkers - female 
table(childbg_NOUNS$sex[childbg_NOUNS$type_talke=="late_talker"])[1] * 100 / table(childbg_NOUNS$sex[childbg_NOUNS$type_talke=="late_talker"])[2]

# Percentage of typical talkers - female 

table(childbg_NOUNS$sex[childbg_NOUNS$type_talke=="typical_talker"])[1] * 100 / table(childbg_NOUNS$sex[childbg_NOUNS$type_talke=="typical_talker"])[2]



#Vocab sizes distribution
childbg_NOUNS$vocab_bins_20 <-as.character(cut(childbg_NOUNS$count_produced_analysis_NOUNS,breaks=seq(9,190,20)))

table(childbg_NOUNS$vocab_bins_20)
View(childbg_NOUNS)

library(plyr)
childbg_NOUNS$vocab_bins_20 <- revalue(childbg_NOUNS$vocab_bins_20, c("(169,189]"="(169,190]"))
childbg_NOUNS$vocab_bins_20[childbg_NOUNS$count_produced_analysis_NOUNS==190] <- "(169,190]"

# late_talker
summary(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="late_talker"]) 
max(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="late_talker"])
min(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="late_talker"])
mean(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="late_talker"])
sd(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="late_talker"])
hist(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="late_talker"])




#type_talker
summary(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"]) 
hist(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"])
max(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"])
min(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"])
mean(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"])
sd(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"])
hist(childbg_NOUNS$count_produced_analysis_NOUNS[childbg_NOUNS$type_talker=="typical_talker"])

#BOTH

table(childbg_NOUNS$vocab_bins_20)

# Age
summary(childbg_NOUNS$age[childbg_NOUNS$type_talker=="late_talker"]) # All words checklist
hist(childbg_NOUNS$age[childbg_NOUNS$type_talker=="late_talker"])
mean(childbg_NOUNS$age[childbg_NOUNS$type_talker=="late_talker"])
sd(childbg_NOUNS$age[childbg_NOUNS$type_talker=="late_talker"])

summary(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"]) # All words checklist
hist(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"])
mean(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"])
sd(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"])

library(dplyr)

distribution_late_talker <- childbg_NOUNS[childbg_NOUNS$type_talker=="late_talker",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

distribution_typical_talker <- childbg_NOUNS[childbg_NOUNS$type_talker=="typical_talker",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

View(distribution_typical_talker)
View(distribution_late_talker)
write.csv(distribution_typical_talker,file = "distribution_typical_talker.csv")
write.csv(distribution_late_talker,file = "distribution_late_talker.csv")

summary(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"]) # All words checklist
hist(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"])
mean(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"])
sd(childbg_NOUNS$age[childbg_NOUNS$type_talker=="typical_talker"])
childbg_NOUNS[childbg_NOUNS$type_talker=="typical_talker",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

## Both talkers TOTAL
table(childbg_NOUNS$vocab_bins_20)

# words

length(Nouns_Verbs_df$Words_CDI) #257 Total
length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]) #286 Total
length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]) #596 Total
wilcox.test(Nouns_Verbs_df$contextual_diversity[Nouns_Verbs_df$Class=="Noun"],
            Nouns_Verbs_df$contextual_diversity[Nouns_Verbs_df$Class=="Verb"], conf.int = TRUE, conf.level = 0.95)

median(Nouns_Verbs_df$contextual_diversity[Nouns_Verbs_df$Class=="Noun"])
median(Nouns_Verbs_df$contextual_diversity[Nouns_Verbs_df$Class=="Verb"])



## 6 ## Analysis and plots for contextual diversity ####


# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_NOUNS, aes(x=count_produced_analysis_NOUNS,y=contextual_diversity_NOUNS_ave, colour=type_talker ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_NOUNS$type_talker <- as.ordered(childbg_NOUNS$type_talker)
contrasts(childbg_NOUNS$type_talker) <- "contr.treatment"

model_1 <- bam(contextual_diversity_NOUNS_ave ~ 1 +
                 s(count_produced_analysis_NOUNS) ,
               data=childbg_NOUNS, method="ML")

model_2 <-bam(contextual_diversity_NOUNS_ave ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")


compareML(model_1,model_2)  ## adding type of talker makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)

beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "type_talkertypical_talker")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals


# GAM plot 
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')

## 7 ## Analysis and plots for network statistics ####
## Clustering coefficient ####

# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_NOUNS, aes(x=count_produced_analysis_NOUNS,y=cc_local, colour=type_talker ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_NOUNS$type_talker <- as.ordered(childbg_NOUNS$type_talker)
contrasts(childbg_NOUNS$type_talker) <- "contr.treatment"

model_1 <- bam(cc_local ~ 1 +
                 s(count_produced_analysis_NOUNS) ,
               data=childbg_NOUNS, method="ML")

model_2 <-bam(cc_local ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")


compareML(model_1,model_2)  ## adding type of talker makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)

beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "type_talkertypical_talker")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals


# GAM plot 
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Clustering Coefficient in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')

## Average path length ####


# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_NOUNS, aes(x=count_produced_analysis_NOUNS,y=ave_path_length, colour=type_talker ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_NOUNS$type_talker <- as.ordered(childbg_NOUNS$type_talker)
contrasts(childbg_NOUNS$type_talker) <- "contr.treatment"

model_1 <- bam(ave_path_length ~ 1 +
                 s(count_produced_analysis_NOUNS) ,
               data=childbg_NOUNS, method="ML")

model_2 <-bam(ave_path_length ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")

  
compareML(model_1,model_2)  ## adding type of talker makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)

beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "type_talkertypical_talker")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals


# GAM plot 
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            main = "PANEL A \n Average Path Length in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomright', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')

## 8 ## Save data ####
save(childbg_NOUNS, file = "childbg_NOUNS.RData")

###################### VERBS ###################### 
## 1 ## Selection of children to analyse for Verbs ####
# Criteria:
# Minimun of 10 Verbs
# Maximun of words is the maximun of words produced by the late talker group, excluding outliers

# Minimun and maximum of Verbs produced 
min(childbg$count_produced_analysis_VERBS[childbg$type_talker=="late_talker"]) # 0 # 
max(childbg$count_produced_analysis_VERBS[childbg$type_talker=="late_talker"]) # 89 # 

min(childbg$count_produced_analysis_VERBS[childbg$type_talker=="typical_talker"]) # 0 #
max(childbg$count_produced_analysis_VERBS[childbg$type_talker=="typical_talker"]) # 96 # 

hist(childbg$count_produced_analysis_VERBS[childbg$type_talker=="late_talker"])
hist(childbg$count_produced_analysis_VERBS[childbg$type_talker=="typical_talker"])

table(childbg$count_produced_analysis_VERBS[childbg$type_talker=="late_talker"]) # There are two outliers. set max to 70
childbg_VERBS <- childbg[childbg$count_produced_analysis_VERBS>=10 & childbg$count_produced_analysis_VERBS<= 70,] # minimun 10 words

# Number of LTs and TTs for the Verbss analysis
dim(childbg_VERBS)
table(childbg_VERBS$type_talker) # TTs: 1766  , LTs:  183

## 2 ## Adapt dataset to contain only Verbs ####

# In child Directed Speech: CHILDES_matrix_VERBS
CHILDES_matrix_VERBS <- CHILDES_matrix[match(tolower(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]),tolower(rownames(CHILDES_matrix))),
                                       match(tolower(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]),tolower(colnames(CHILDES_matrix)))]
dim(CHILDES_matrix_VERBS) # 96 Verbs


# In children's vocabularies: childWord_matrix_VERBS
childWord_matrix_VERBS <- childWord_matrix[match(childbg_VERBS$data_id,rownames(childWord_matrix))
                                           ,match(tolower(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]),tolower(colnames(childWord_matrix)))]
dim(childWord_matrix_VERBS)
dim(childbg_VERBS)

# Contextual_diversity
## 3 ## Compute contextual diversity average for each child ####

childbg_VERBS$contextual_diversity_VERBS_ave <- NA

for (i in 1:nrow(childWord_matrix_VERBS)) {
  id <- rownames(childWord_matrix_VERBS)[i]
  child <- childWord_matrix_VERBS[i,]
  words_prod <- names(child[child==TRUE])
  a <- match(words_prod,as.character(Nouns_Verbs_df$Words_CDI))
  a <- a[!is.na(a)]
  a <- Nouns_Verbs_df$contextual_diversity[a]
  a <- a[!is.na(a)]
  childbg_VERBS$contextual_diversity_VERBS_ave[childbg_VERBS$data_id==id] <- mean(a)
  
}



## 4 ## Compute network statistics for each child ####

childbg_VERBS$cc_local <- NA
childbg_VERBS$ave_path_length <- NA

# Sort "CHILDES_matrix_VERBS" alphabetically and make sure it is a matrix
CHILDES_matrix_VERBS <- CHILDES_matrix_VERBS[,order(colnames(CHILDES_matrix_VERBS))]
CHILDES_matrix_VERBS <- CHILDES_matrix_VERBS[order(rownames(CHILDES_matrix_VERBS)),]
CHILDES_matrix_VERBS <- as.matrix(CHILDES_matrix_VERBS)
CHILDES_matrix_VERBS[1:10,1:10]

# Sort columns in "childWord_matrix_VERBS" alphabetically
childWord_matrix_VERBS <- childWord_matrix_VERBS[,order(colnames(childWord_matrix_VERBS))]
childWord_matrix_VERBS[1:10,1:10]
dim(childWord_matrix_VERBS)
dim(childWord_matrix_VERBS)

# Test that columns in  "CHILDES_matrix_VERBS" and  "childWord_matrix_VERBS" match in order
test_match_order2 <- function(x,y) {
  
  if (isTRUE(all.equal(x,y))) print('Perfect match in same order')
  
  if (!isTRUE(all.equal(x,y)) && isTRUE(all.equal(sort(x),sort(y)))) print('Perfect match in wrong order')
  
  if (!isTRUE(all.equal(x,y)) && !isTRUE(all.equal(sort(x),sort(y)))) print('No match')
}

test_match_order2(colnames(childWord_matrix_VERBS),colnames(CHILDES_matrix_VERBS))


# Compute network porperties
library(igraph)
for(child_iter in 1:length(childbg_VERBS$data_id) ) {
  
  print(child_iter)
  
  
  if(childbg_VERBS$count_produced_analysis_VERBS[child_iter]  > 1) {
    child_indiv_matrix <- CHILDES_matrix_VERBS[ childWord_matrix_VERBS[child_iter,] , childWord_matrix_VERBS[child_iter,] ]
    
    
    child_indiv_network = graph.adjacency(child_indiv_matrix , mode="undirected",   weighted=NULL, diag=FALSE)   
    
    childbg_VERBS[child_iter, c("cc_local") ]   <- transitivity(child_indiv_network, type = c("localaverage"))
    childbg_VERBS[child_iter, c("ave_path_length") ] <- average.path.length(child_indiv_network)
    
  }
  
}

childbg_VERBS[1:10,]





## 5 ## Description of the sample ####
# Overlap ##

sum(!is.na(match(childbg_NOUNS$data_id,childbg_VERBS$data_id)))

#Number of verba and late_talker
table(childbg_VERBS$type_talker)
dim(childbg_VERBS)

# Age differece? 
wilcox.test(childbg_VERBS$age[childbg_VERBS$type_talker=="late_talker"],
            childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"],  conf.int = TRUE, conf.level = 0.95)
median(childbg_VERBS$age[childbg_VERBS$type_talker=="late_talker"])
median(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])


# Precentage of late talkers 
table(childbg_VERBS$type_talker)[1] * 100 / table(childbg_VERBS$type_talker)[2]

#Vocab sizes distribution
childbg_VERBS$vocab_bins_20 <-as.character(cut(childbg_VERBS$count_produced_analysis_VERBS,breaks=seq(9,70,20)))

table(childbg_VERBS$vocab_bins_20)
View(childbg_VERBS)

library(plyr)
childbg_VERBS$vocab_bins_20 <- revalue(childbg_VERBS$vocab_bins_20, c("(49,69]"="(49,70]"))
childbg_VERBS$vocab_bins_20[childbg_VERBS$count_produced_analysis_VERBS==70] <- "(49,70]"

# late_talker
summary(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="late_talker"]) 
max(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="late_talker"])
min(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="late_talker"])
mean(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="late_talker"])
sd(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="late_talker"])
hist(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="late_talker"])




#type_talker
summary(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"]) 
hist(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"])
max(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"])
min(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"])
mean(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"])
sd(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"])
hist(childbg_VERBS$count_produced_analysis_VERBS[childbg_VERBS$type_talker=="typical_talker"])

#BOTH

table(childbg_VERBS$vocab_bins_20)

# Age
summary(childbg_VERBS$age[childbg_VERBS$type_talker=="late_talker"]) # All words checklist
hist(childbg_VERBS$age[childbg_VERBS$type_talker=="late_talker"])
mean(childbg_VERBS$age[childbg_VERBS$type_talker=="late_talker"])
sd(childbg_VERBS$age[childbg_VERBS$type_talker=="late_talker"])

summary(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"]) # All words checklist
hist(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])
mean(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])
sd(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])

library(dplyr)

distribution_late_talker <- childbg_VERBS[childbg_VERBS$type_talker=="late_talker",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

distribution_typical_talker <- childbg_VERBS[childbg_VERBS$type_talker=="typical_talker",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

View(distribution_typical_talker)
View(distribution_late_talker)
write.csv(distribution_typical_talker,file = "distribution_typical_talker_VERBS.csv")
write.csv(distribution_late_talker,file = "distribution_late_talker_VERBS.csv")

summary(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"]) # All words checklist
hist(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])
mean(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])
sd(childbg_VERBS$age[childbg_VERBS$type_talker=="typical_talker"])
childbg_VERBS[childbg_VERBS$type_talker=="typical_talker",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

## Both talkers TOTAL
table(childbg_VERBS$vocab_bins_20)

# words

length(VERBS_Verbs_df$Words_CDI) #257 Total
length(VERBS_Verbs_df$Words_CDI[VERBS_Verbs_df$Class=="Noun"]) #286 Total
length(VERBS_Verbs_df$Words_CDI[VERBS_Verbs_df$Class=="Verb"]) #596 Total
wilcox.test(VERBS_Verbs_df$contextual_diversity[VERBS_Verbs_df$Class=="Noun"],
            VERBS_Verbs_df$contextual_diversity[VERBS_Verbs_df$Class=="Verb"], conf.int = TRUE, conf.level = 0.95)

median(VERBS_Verbs_df$contextual_diversity[VERBS_Verbs_df$Class=="Noun"])
median(VERBS_Verbs_df$contextual_diversity[VERBS_Verbs_df$Class=="Verb"])

## 6 ## Analysis and plots for contextual diversity ####


# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_VERBS, aes(x=count_produced_analysis_VERBS,y=contextual_diversity_VERBS_ave, colour=type_talker ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_VERBS$type_talker <- as.ordered(childbg_VERBS$type_talker)
contrasts(childbg_VERBS$type_talker) <- "contr.treatment"

model_1 <- bam(contextual_diversity_VERBS_ave ~ 1 +
                 s(count_produced_analysis_VERBS) ,
               data=childbg_VERBS, method="ML")

model_2 <-bam(contextual_diversity_VERBS_ave ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")


compareML(model_1,model_2)  ## adding type of talker makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)

beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "type_talkertypical_talker")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals


# GAM plot 
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="late_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Verb Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            ylim=c(270,300),xlog=FALSE,xaxp=c(10,70,6),yaxp=c(270,300,6),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="typical_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')

## 7 ## Analysis and plots for network statistics ####
## Clustering coefficient ####


# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_VERBS, aes(x=count_produced_analysis_VERBS,y=cc_local, colour=type_talker ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_VERBS$type_talker <- as.ordered(childbg_VERBS$type_talker)
contrasts(childbg_VERBS$type_talker) <- "contr.treatment"

model_1 <- bam(cc_local ~ 1 +
                 s(count_produced_analysis_VERBS) ,
               data=childbg_VERBS, method="ML")

model_2 <-bam(cc_local ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")


compareML(model_1,model_2)  ## adding type of talker makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)

beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "type_talkertypical_talker")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals


## Average path length ####


# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_VERBS, aes(x=count_produced_analysis_VERBS,y=ave_path_length, colour=type_talker ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_VERBS$type_talker <- as.ordered(childbg_VERBS$type_talker)
contrasts(childbg_VERBS$type_talker) <- "contr.treatment"

model_1 <- bam(ave_path_length ~ 1 +
                 s(count_produced_analysis_VERBS) ,
               data=childbg_VERBS, method="ML")

model_2 <-bam(ave_path_length ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")


compareML(model_1,model_2)  ## adding type of talker makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)

beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "type_talkertypical_talker")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals


## 8 ## Save data ####
save(childbg_VERBS, file = "childbg_VERBS.RData")

#################### NOUNS & VERBS ###############
## 1 ## Plot ####

model_N <-bam(contextual_diversity_NOUNS_ave ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")
model_V <-bam(contextual_diversity_VERBS_ave ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")

setWidth = (130*0.039370 )
pdf(file='Study_2_LT_TT.pdf',width=setWidth,height=8, fonts = "sans")
par(mfrow=c(2,1), ps = 10,cex.lab = 1,cex.axis = 0.9)
plot_smooth(model_N, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
             xlab =" Noun vocabulary size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2) 
plot_smooth(model_N, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('topright', 
       legend=c("Late talkers","Typical talkers"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n',cex=0.9)
plot_smooth(model_V, view="count_produced_analysis_VERBS", cond=list(type_talker="late_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
             xlab =" Verb vocabulary Size", ylab =  "Average Contextual Diversity",
            ylim=c(270,300),xlog=FALSE,xaxp=c(10,70,6),yaxp=c(270,300,6),hide.labe=TRUE,lwd=3,lty=2) 
plot_smooth(model_V, view="count_produced_analysis_VERBS", cond=list(type_talker="typical_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)

dev.off()
