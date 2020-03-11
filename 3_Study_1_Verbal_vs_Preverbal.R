################################################
## 3 # STUDY 1: VERBAL VS PREVERBAL CHILDREN ###
################################################
setwd('XXX')

## 1 ## Load data: CDI_GESTURES ####

# Read in raw data
CDI_GESTURES <- read.csv("instrument_data_GESTURES.csv")

#Create basic child background info data.frame - One child per row
childbg_GEST <-CDI_GESTURES[ with(CDI_GESTURES, order(data_id)) ,c(1:4)]
childbg_GEST_keeprows <- match(unique(childbg_GEST$data_id) , childbg_GEST$data_id)
childbg_GEST <- childbg_GEST[childbg_GEST_keeprows, ]

# Count the number of unique children and unique words
length(unique(childbg_GEST$data_id))## 2435 unique children 
# see 'subjectIDs_Study1.csv' for the id of the children's vocabularies downloaded from TALKBANK
length(unique(CDI_GESTURES$definition[CDI_GESTURES$type=="word"]))  ## 396 Number of words in CDI W&G



## 2 ## Extract CDI words and select CDI categories for syntactic analysis ####
as.character(unique(CDI_GESTURES$category[CDI_GESTURES$type=="word"])) 
sort(as.character(unique(CDI_GESTURES$definition[CDI_GESTURES$type=="word"])) )

## Nouns: "animals", "vehicles", "toys","food_drink","clothing","body_parts","household","furniture_rooms","places","people"
## Verbs: "action_words"

Nouns_CDI_GEST <- as.character(unique(CDI_GESTURES$definition[CDI_GESTURES$type=="word" & CDI_GESTURES$category=="animals"|
                                                                CDI_GESTURES$category=="vehicles" | CDI_GESTURES$category=="toys" |
                                                                CDI_GESTURES$category=="food_drink" | CDI_GESTURES$category=="clothing" |
                                                                CDI_GESTURES$category=="body_parts" | CDI_GESTURES$category=="household" |
                                                                CDI_GESTURES$category=="furniture_rooms" | CDI_GESTURES$category=="outside" |
                                                                CDI_GESTURES$category=="people" ]))

Nouns_CDI_GEST <- Nouns_CDI_GEST[!is.na(Nouns_CDI_GEST)]
Verbs_CDI_GEST <-  as.character(unique(CDI_GESTURES$definition[CDI_GESTURES$type=="word" & CDI_GESTURES$category=="action_words"]))

length(Nouns_CDI_GEST) #229
length(Verbs_CDI_GEST) #55

# Remove 2-word nouns
# Remove words that are verb and noun
library(stringr)
Nouns_CDI_GEST <- Nouns_CDI_GEST[-(which(str_count(Nouns_CDI_GEST, " ")==1))]
Nouns_CDI_GEST <- Nouns_CDI_GEST[-(which(str_count(Nouns_CDI_GEST, " ")==2))]
Verbs_CDI_GEST <- Verbs_CDI_GEST[-(which(str_count(Verbs_CDI_GEST, " ")==1))]
length(Nouns_CDI_GEST) #206
length(Verbs_CDI_GEST) #51

# Remove *s
# Keep first word when /
Nouns_CDI_GEST_original <- Nouns_CDI_GEST # Keep originals

a <- Nouns_CDI_GEST[which(str_count(Nouns_CDI_GEST, "\\*")==1)]
Nouns_CDI_GEST <- Nouns_CDI_GEST[- (match(a,Nouns_CDI_GEST))]
a <-  str_split(a, "\\*")
a <-  unlist(lapply(a, `[[`, 1))
Nouns_CDI_GEST <- c(a,Nouns_CDI_GEST)

Nouns_CDI_GEST_original_df <- data.frame(Original= sort(Nouns_CDI_GEST_original),Adapted=sort(Nouns_CDI_GEST)) 

Nouns_Verbs_df_GEST <- data.frame(Words_CHILDES= c(as.character(Nouns_CDI_GEST_original_df$Adapted) ,Verbs_CDI_GEST),
                                  Words_CDI= c(as.character(Nouns_CDI_GEST_original_df$Original),Verbs_CDI_GEST),
                                  Class= c(rep("Noun",length(Nouns_CDI_GEST_original_df$Adapted)), rep("Verb",length(Verbs_CDI_GEST))))

## 3 ## Insert contextual diversity into 'Nouns_Verbs_df_GEST' ####
# Insert contextual diversity into

for (i in 1:nrow(Nouns_Verbs_df_GEST)) {
  if(!is.na(match(Nouns_Verbs_df_GEST$Words_CDI[i],Nouns_Verbs_df$Words_CDI))){
    Nouns_Verbs_df_GEST$contextual_diversity[i] <-  Nouns_Verbs_df$contextual_diversity[match(Nouns_Verbs_df_GEST$Words_CDI[i],Nouns_Verbs_df$Words_CDI)]
  }
  
}


## 4 ## Count number of words produced  ####

# Create column ready for whether word is produced
CDI_GESTURES$value[is.na(CDI_GESTURES$value)] <- ""
CDI_GESTURES$wordproduced <- FALSE
CDI_GESTURES$wordproduced <- (CDI_GESTURES$value == "produces" & CDI_GESTURES$type  == "word")


# Create column ready for whether word can be matched with words selected for analysis
CDI_GESTURES$For_analysis <- FALSE
CDI_GESTURES$For_analysis <- ( CDI_GESTURES$definition  %in%  Nouns_Verbs_df_GEST$Words_CDI )

# Create column ready for : 'For_analysis' and 'wordproduced'
CDI_GESTURES$For_analysis_And_Produced <- FALSE
CDI_GESTURES$For_analysis_And_Produced <- (CDI_GESTURES$wordproduced == TRUE & CDI_GESTURES$For_analysis == TRUE)

CDI_GESTURES[1:10,]

# Create column in 'childbg_GEST' ready for count of words produced
childbg_GEST$count_produced <- 0.0
childbg_GEST$count_produced_analysis <- 0.0

# Find the sum of (TRUE/FALSE) to get the number of words produced per child
childbg_GEST$count_produced         <- aggregate(CDI_GESTURES$wordproduced, by=list(Category=CDI_GESTURES$data_id), FUN=sum)[,2]
childbg_GEST$count_produced_analysis <- aggregate(CDI_GESTURES$For_analysis_And_Produced,     by=list(Category=CDI_GESTURES$data_id), FUN=sum)[,2]

# Find the % loss in words produced reported when we consider subset (wordsw matching with CHILDES CoOc)
childbg_GEST$percentage_produced_loss <- 100 - (100 / childbg_GEST$count_produced * childbg_GEST$count_produced_analysis)

# Insert word class in 'CDI_GESTURES'
CDI_GESTURES$Class <- NA
for (i in 1:length(Nouns_CDI_GEST_original)) {
  CDI_GESTURES$Class[which(CDI_GESTURES$definition==Nouns_CDI_GEST_original[i])] <- "Noun"
}

for (i in 1:length(Verbs_CDI_GEST)) {
  CDI_GESTURES$Class[which(CDI_GESTURES$definition==Verbs_CDI_GEST[i])] <- "Verb"
}

# Number of nouns and verbs produced
childbg_GEST$count_produced_analysis_NOUNS <- aggregate(CDI_GESTURES$For_analysis_And_Produced[CDI_GESTURES$Class=="Noun"],   
                                                        by=list(Category=CDI_GESTURES$data_id[CDI_GESTURES$Class=="Noun"]), FUN=sum)[,2]

childbg_GEST$count_produced_analysis_VERBS <- aggregate(CDI_GESTURES$For_analysis_And_Produced[CDI_GESTURES$Class=="Verb"],   
                                                        by=list(Category=CDI_GESTURES$data_id[CDI_GESTURES$Class=="Verb"]), FUN=sum)[,2]

childbg_GEST$count_produced_analysis_NOUNS_VERBS <- aggregate(CDI_GESTURES$For_analysis_And_Produced[CDI_GESTURES$Class=="Verb" |CDI_GESTURES$Class=="Noun"],   
                                                              by=list(Category=CDI_GESTURES$data_id[CDI_GESTURES$Class=="Verb"|CDI_GESTURES$Class=="Noun"]), FUN=sum)[,2]

## 5 ## Count number of words understood ####

# Create column ready for whether word is understood
CDI_GESTURES$value[is.na(CDI_GESTURES$value)] <- ""
CDI_GESTURES$wordunderstood <- FALSE
CDI_GESTURES$wordunderstood <- ( CDI_GESTURES$type  == "word" & CDI_GESTURES$value == "understands" |CDI_GESTURES$value == "produces")


# Create column ready for : 'For_analysis' and 'wordunderstood'
CDI_GESTURES$For_analysis_And_Understood <- FALSE
CDI_GESTURES$For_analysis_And_Understood <- (CDI_GESTURES$wordunderstood == TRUE & CDI_GESTURES$For_analysis == TRUE)

CDI_GESTURES[1:10,]

# Create column in 'childbg_GEST' ready for count of words Understood
childbg_GEST$count_Understood <- 0.0
childbg_GEST$count_Understood_analysis <- 0.0

# Find the sum of (TRUE/FALSE) to get the number of words Understood per child
childbg_GEST$count_Understood         <- aggregate(CDI_GESTURES$wordunderstood, by=list(Category=CDI_GESTURES$data_id), FUN=sum)[,2]
childbg_GEST$count_Understood_analysis <- aggregate(CDI_GESTURES$For_analysis_And_Understood,     by=list(Category=CDI_GESTURES$data_id), FUN=sum)[,2]

# Find the % loss in words Understood reported when we consider subset (wordsw matching with CHILDES CoOc)
childbg_GEST$percentage_Understood_loss <- 100 - (100 / childbg_GEST$count_Understood * childbg_GEST$count_Understood_analysis)

# Number of nouns and verbs Understood
childbg_GEST$count_Understood_analysis_NOUNS <- aggregate(CDI_GESTURES$For_analysis_And_Understood[CDI_GESTURES$Class=="Noun"],   
                                                          by=list(Category=CDI_GESTURES$data_id[CDI_GESTURES$Class=="Noun"]), FUN=sum)[,2]

childbg_GEST$count_Understood_analysis_VERBS <- aggregate(CDI_GESTURES$For_analysis_And_Understood[CDI_GESTURES$Class=="Verb"],   
                                                          by=list(Category=CDI_GESTURES$data_id[CDI_GESTURES$Class=="Verb"]), FUN=sum)[,2]

childbg_GEST$count_Understood_analysis_NOUNS_VERBS <- aggregate(CDI_GESTURES$For_analysis_And_Understood[CDI_GESTURES$Class=="Verb" |CDI_GESTURES$Class=="Noun"],   
                                                                by=list(Category=CDI_GESTURES$data_id[CDI_GESTURES$Class=="Verb"|CDI_GESTURES$Class=="Noun"]), FUN=sum)[,2]

## 6 ## Create a matrix with the words understood by children and compute contextual diversity average ####

childWord_matrix_GEST_und <- matrix(FALSE, length(childbg_GEST$data_id), length(Nouns_Verbs_df_GEST$Words_CDI) )
rownames(childWord_matrix_GEST_und) <- childbg_GEST$data_id
colnames(childWord_matrix_GEST_und) <- Nouns_Verbs_df_GEST$Words_CDI
childWord_matrix_GEST_und[1:10, 1:10]

# iterate through children 
for(i in 1:length(childbg_GEST$data_id)) {
  
  print(i)
  wordsknownbythischild <-as.character(CDI_GESTURES$definition[CDI_GESTURES$data_id == childbg_GEST$data_id[i] &
                                                                 CDI_GESTURES$For_analysis_And_Understood == TRUE])
  childWord_matrix_GEST_und[ i,   match( wordsknownbythischild , colnames(childWord_matrix_GEST_und)) ] <- TRUE
}

# Compute averages of contextual diversity for each child for COMPREHENSION

childWord_matrix_GEST_und_words <- childWord_matrix_GEST_und
childWord_matrix_GEST_und <- as.data.frame(childWord_matrix_GEST_und)
childWord_matrix_GEST_und$contextual_diversity_ave_UND <-NA

for (i in 1:nrow(childWord_matrix_GEST_und_words)) {
  child <- childWord_matrix_GEST_und_words[i,]
  words_und <- names(child[child==TRUE])
  a <- match(words_und,as.character(Nouns_Verbs_df_GEST$Words_CDI))
  a <- a[!is.na(a)]
  a <- Nouns_Verbs_df_GEST$contextual_diversity[a]
  a <- a[!is.na(a)]
  childWord_matrix_GEST_und$contextual_diversity_ave_UND[i] <- mean(a)
  
}

## Insert contextual diversity in 'childbg_GEST'
childbg_GEST$contextual_diversity_ave_UND <- NA

for (i in 1:nrow(childbg_GEST) ){
  if(!is.na(match(childbg_GEST$data_id[i],rownames(childWord_matrix_GEST_und)))){
    childbg_GEST$contextual_diversity_ave_UND[i]<- childWord_matrix_GEST_und$contextual_diversity_ave_UND[match(childbg_GEST$data_id[i],rownames(childWord_matrix_GEST_und))]
    
  }
  
}


## 7 ## Create a matrix with the words produced by children and compute contextual diversity average ####

childWord_matrix_GEST_prod <- matrix(FALSE, length(childbg_GEST$data_id), length(Nouns_Verbs_df_GEST$Words_CDI) )
rownames(childWord_matrix_GEST_prod) <- childbg_GEST$data_id
colnames(childWord_matrix_GEST_prod) <- Nouns_Verbs_df_GEST$Words_CDI
childWord_matrix_GEST_prod[1:10, 1:10]

# iterate through children 
for(i in 1:length(childbg_GEST$data_id)) {
  
  print(i)
  wordsknownbythischild <-as.character(CDI_GESTURES$definition[CDI_GESTURES$data_id == childbg_GEST$data_id[i] &
                                                                 CDI_GESTURES$For_analysis_And_Produced == TRUE])
  childWord_matrix_GEST_prod[ i,   match( wordsknownbythischild , colnames(childWord_matrix_GEST_prod)) ] <- TRUE
}

# Compute averages of contextual diversity for each child for PRODUCTION 

childWord_matrix_GEST_prod_words <- childWord_matrix_GEST_prod
childWord_matrix_GEST_prod <- as.data.frame(childWord_matrix_GEST_prod)
childWord_matrix_GEST_prod$contextual_diversity_ave_PROD <-NA

for (i in 1:nrow(childWord_matrix_GEST_prod_words)) {
  child <- childWord_matrix_GEST_prod_words[i,]
  words_und <- names(child[child==TRUE])
  a <- match(words_und,as.character(Nouns_Verbs_df_GEST$Words_CDI))
  a <- a[!is.na(a)]
  a <- Nouns_Verbs_df_GEST$contextual_diversity[a]
  a <- a[!is.na(a)]
  childWord_matrix_GEST_prod$contextual_diversity_ave_PROD[i] <- mean(a)
  
}

## Insert contextual diversity in 'childbg_GEST'
childbg_GEST$contextual_diversity_ave_PROD <- NA

for (i in 1:nrow(childbg_GEST) ){
  if(!is.na(match(childbg_GEST$data_id[i],rownames(childWord_matrix_GEST_prod)))){
    childbg_GEST$contextual_diversity_ave_PROD[i]<- childWord_matrix_GEST_prod$contextual_diversity_ave_PROD[match(childbg_GEST$data_id[i],rownames(childWord_matrix_GEST_prod))]
    
  }
  
}



## 8 ## Find the childs percentile based on total number of words known and identify late talkers ####
CDI_vocabulary_norm_WG <- read.csv(file = "CDI_vocabulary_norm_WG.csv", header = T)

# Function to get Childs percentile given age and number of words known

childsPercentile <- function(age, nrofWords) {
  
  searchspace <- CDI_vocabulary_norm_WG[ match( age, CDI_vocabulary_norm_WG$age) , 6:15]
  
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
CDI_vocabulary_norm_WG$X1 <- 999.99
childbg_GEST$percentile_NORM <- 0.0

# Use the above function to calculate each childs percentile

for (childiter in 1:length(childbg_GEST$data_id)) { 
  if(length(childsPercentile( childbg_GEST$age[childiter],childbg_GEST$count_produced[childiter] ))==0){
    childbg_GEST$percentile_NORM[childiter] <- childsPercentile( childbg_GEST$age[childiter] ,
                                                                 childbg_GEST$count_produced[childiter] )
  }
  if(length(childsPercentile( childbg_GEST$age[childiter],childbg_GEST$count_produced[childiter] ))>0){
    a <- childsPercentile( childbg_GEST$age[childiter] ,childbg_GEST$count_produced[childiter] )
    childbg_GEST$percentile_NORM[childiter] <- max(a)
  }
}

childbg_GEST$type_talker <- NA

for (i in 1:nrow(childbg_GEST)) {
  if(childbg_GEST$percentile_NORM[i]<=0.2){
    childbg_GEST$type_talker[i] <- "late_talker"
  }
  else{
    childbg_GEST$type_talker[i] <- "typical_talker"
  }
}





## Number of LTs and TTs 

table(childbg_GEST$type_talker) # late_talker: 305, typical_talker: 2130
## 9 ## Identify preverbal and verbal children ####

# Calculating comprehension-expression gap 
childbg_GEST$compr_exp_gap_NV <- childbg_GEST$count_Understood_analysis_NOUNS_VERBS - childbg_GEST$count_produced_analysis_NOUNS_VERBS

# Verbal abd preverbal children
childbg_GEST_PREVERBAL <- childbg_GEST[childbg_GEST$count_produced_analysis_NOUNS_VERBS==0 & 
                                         childbg_GEST$count_Understood_analysis_NOUNS_VERBS>=10,] #minimun 10 words understood

## Max and min words understood by preverbal children 
max(childbg_GEST_PREVERBAL$count_Understood_analysis_NOUNS_VERBS)  # 230
min(childbg_GEST_PREVERBAL$count_Understood_analysis_NOUNS_VERBS)  # 10
hist(childbg_GEST_PREVERBAL$count_Understood_analysis_NOUNS_VERBS)
table(childbg_GEST_PREVERBAL$count_Understood_analysis_NOUNS_VERBS)

childbg_GEST_VERBAL <- childbg_GEST[childbg_GEST$count_produced_analysis_NOUNS_VERBS>0 & # Minimun one word produced
                                      childbg_GEST$compr_exp_gap_NV >0&# making sure that comprehension has been assessed
                                      childbg_GEST$count_Understood_analysis_NOUNS_VERBS>=10 & #minimun 10 words understood
                                      childbg_GEST$count_Understood_analysis_NOUNS_VERBS<=230,] # no more than the maximun of words preverbal children understand

max(childbg_GEST_VERBAL$count_Understood_analysis_NOUNS_VERBS)  
min(childbg_GEST_VERBAL$count_Understood_analysis_NOUNS_VERBS) 
hist(childbg_GEST_VERBAL$count_produced_analysis_NOUNS_VERBS)

# Join dfs 
childbg_GEST_PREVERBAL$verbal_prev <- "preverbal"
childbg_GEST_VERBAL$verbal_prev <- "verbal"
childbg_GEST_VERBAL_PREVERBAL <- rbind(childbg_GEST_PREVERBAL,childbg_GEST_VERBAL)



## 10 ## Sample description ####


#Number of verba and preverbal
table(childbg_GEST_VERBAL_PREVERBAL$verbal_prev)
dim(childbg_GEST_VERBAL_PREVERBAL)

# Age differece? 
wilcox.test(childbg_GEST_VERBAL$age,childbg_GEST_PREVERBAL$age,  conf.int = TRUE, conf.level = 0.95)
median(childbg_GEST_VERBAL$age)
median(childbg_GEST_PREVERBAL$age)

# Precentage of female 
table(childbg_GEST_PREVERBAL$sex)[1] * 100 / table(childbg_GEST_PREVERBAL$sex)[2]

# Precentage of late talkers in preverbal and verbal groups
table(childbg_GEST_PREVERBAL$type_talker)[1] * 100 / table(childbg_GEST_PREVERBAL$type_talker)[2]
table(childbg_GEST_VERBAL$type_talker)[1] * 100 / table(childbg_GEST_VERBAL$type_talker)[2]

#Vocab sizes distribution
childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20 <-as.character(cut(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS,breaks=seq(9,230,20)))

table(childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20)

library(plyr)
childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20 <- revalue(childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20, c("(209,229]"="(219,230]"))
childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20[childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS==230] <- "(209,230]"

# preverbal
summary(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"]) 
max(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
min(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
mean(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
sd(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
hist(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])




#verbal
summary(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"]) 
hist(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
max(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
min(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
mean(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
sd(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
hist(childbg_GEST_VERBAL_PREVERBAL$count_Understood_analysis_NOUNS_VERBS[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])

#BOTH

table(childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20)

# Age
summary(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"]) # All words checklist
hist(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
mean(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
sd(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal"])
library(dplyr)

distribution_preverbal <- childbg_GEST_VERBAL_PREVERBAL[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="preverbal",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

distribution_verbal <- childbg_GEST_VERBAL_PREVERBAL[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

View(distribution_verbal)
View(distribution_preverbal)
write.csv()

summary(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"]) # All words checklist
hist(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
mean(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
sd(childbg_GEST_VERBAL_PREVERBAL$age[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal"])
childbg_GEST_VERBAL_PREVERBAL[childbg_GEST_VERBAL_PREVERBAL$verbal_prev=="verbal",] %>%
  group_by(vocab_bins_20) %>% 
  summarise_each(funs(n(), Mean=mean(.)), age)

## Both talkers TOTAL
table(childbg_GEST_VERBAL_PREVERBAL$vocab_bins_20)

# words

length(Nouns_Verbs_df_GEST$Words_CDI) #257 Total
length(Nouns_Verbs_df_GEST$Words_CDI[Nouns_Verbs_df_GEST$Class=="Noun"]) #206 Total
length(Nouns_Verbs_df_GEST$Words_CDI[Nouns_Verbs_df_GEST$Class=="Verb"]) #51 Total
wilcox.test(Nouns_Verbs_df_GEST$contextual_diversity[Nouns_Verbs_df_GEST$Class=="Noun"],
            Nouns_Verbs_df_GEST$contextual_diversity[Nouns_Verbs_df_GEST$Class=="Verb"], conf.int = TRUE, conf.level = 0.95)
            
median(Nouns_Verbs_df_GEST$contextual_diversity[Nouns_Verbs_df_GEST$Class=="Noun"])
median(Nouns_Verbs_df_GEST$contextual_diversity[Nouns_Verbs_df_GEST$Class=="Verb"])

## 11 ## Analysis and plots ####

# Quick data exploration 
library(ggplot2)
ggplot(data = childbg_GEST_VERBAL_PREVERBAL, aes(x=count_Understood_analysis_NOUNS_VERBS,y=contextual_diversity_ave_UND, colour=verbal_prev ))+
  geom_point()+
  geom_smooth(method = "loess")

# Genaralized Additive Moldel 
library(mgcv)
library(itsadug)

childbg_GEST_VERBAL_PREVERBAL$verbal_prev <- as.ordered(childbg_GEST_VERBAL_PREVERBAL$verbal_prev)
contrasts(childbg_GEST_VERBAL_PREVERBAL$verbal_prev) <- "contr.treatment"

model_1 <- bam(contextual_diversity_ave_UND ~ 1 +
                        s(count_Understood_analysis_NOUNS_VERBS) ,
                        data=childbg_GEST_VERBAL_PREVERBAL, method="ML")

model_2 <-bam(contextual_diversity_ave_UND ~ verbal_prev +
                         s(count_Understood_analysis_NOUNS_VERBS) ,
                       data=childbg_GEST_VERBAL_PREVERBAL, method="ML")


compareML(model_1,model_2)  ## adding verbla/preverbal makes it  more predictive
gam.check(model_2) ## assumptions
summary(model_2) # model results
anova(model_2)
df.residual(model_2)


beta <- coef(model_2)
Vb <- vcov(model_2, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "verbal_prevverbal")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals




# GAM plot 
setWidth = (130*0.039370 )
pdf(file='Study_1_verbal_preverbal.pdf',width=setWidth,height=4, fonts = "sans")
par(ps = 10,cex.lab = 1,cex.axis = 0.9)
plot_smooth(model_2, view="count_Understood_analysis_NOUNS_VERBS", cond=list(verbal_prev="preverbal"), rm.ranef=F, rug=FALSE, col="grey",
            xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,230),xlog=FALSE,xaxp=c(10,230,11),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_Understood_analysis_NOUNS_VERBS", cond=list(verbal_prev="verbal"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
par(ps = 9,cex=1)
legend('topright', 
       legend=c("Preverbal","Verbal"),
        col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')

dev.off()


# GAM controlling for language delay

model_2_verbs_LT <-bam(contextual_diversity_ave_UND ~ verbal_prev + type_talker +
                         s(count_Understood_analysis_NOUNS_VERBS) ,
                       data=childbg_GEST_VERBAL_PREVERBAL, method="ML")

compareML(model_2_verbs,model_2_verbs_LT) ## adding LT/TT doesn't makes it  more predictive
gam.check(model_2_verbs_LT) 
summary(model_2_verbs_LT)



# GAM controlling for verbs produced

model_2_verbs <-bam(contextual_diversity_ave_UND ~ verbal_prev + count_Understood_analysis_VERBS+
                      s(count_Understood_analysis_NOUNS_VERBS) ,
                    data=childbg_GEST_VERBAL_PREVERBAL, method="ML")

compareML(model_2,model_2_verbs) ## controlling for number of verbs  makes it more predictive
gam.check(model_2_verbs) 
summary(model_2_verbs)
anova(model_2_verbs)
df.residual(model_2_verbs)

beta <- coef(model_2_verbs)
Vb <- vcov(model_2_verbs, unconditional = TRUE)
se <- sqrt(diag(Vb))
i <- which(names(beta) == "verbal_prevverbal")
beta[i] + (c(-1,1) * (2 * se[i])) # Confidence Intervals




