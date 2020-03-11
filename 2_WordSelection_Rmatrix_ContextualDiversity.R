#################################################################################################
## 2 # WORD SELECTION, REDUCE MATRIX OF CO-OCCURRENCES AND COMPUTE CONTEXTUAL DIVERSITY VALUES ###
#################################################################################################
setwd('XXX')


## 1 ## Load our data with the Words & Sentences data to extract the CDI words ####
CDI_SENTENCES <- read.csv("instrument_data_SENTENCES.csv")

#Create basic child background info data.frame - One child per row
childbg <-CDI_SENTENCES[ with(CDI_SENTENCES, order(data_id)) ,c(1:4)]
childbg <- childbg[match(unique(childbg$data_id) , childbg$data_id), ]

# Count the number of unique words
length(unique(CDI_SENTENCES$definition[CDI_SENTENCES$type=="word"]))  ## 680 Number of words in CDI W&S


## 2 ## Extract CDI words and select CDI categories for syntactic analysis ####
as.character(unique(CDI_SENTENCES$category[CDI_SENTENCES$type=="word"])) # Semantic categories

# Our criteria:
# Nouns: "animals", "vehicles", "toys","food_drink","clothing","body_parts","household","furniture_rooms","places","people"
# Verbs: "action_words"

Nouns_CDI <- as.character(unique(CDI_SENTENCES$definition[CDI_SENTENCES$type=="word" & CDI_SENTENCES$category=="animals"|
                                                            CDI_SENTENCES$category=="vehicles" | CDI_SENTENCES$category=="toys" |
                                                            CDI_SENTENCES$category=="food_drink" | CDI_SENTENCES$category=="clothing" |
                                                            CDI_SENTENCES$category=="body_parts" | CDI_SENTENCES$category=="household" |
                                                            CDI_SENTENCES$category=="furniture_rooms" | CDI_SENTENCES$category=="places" |
                                                            CDI_SENTENCES$category=="people" ]))


Nouns_CDI <- Nouns_CDI[!is.na(Nouns_CDI)]
Verbs_CDI <-  as.character(unique(CDI_SENTENCES$definition[CDI_SENTENCES$type=="word" & CDI_SENTENCES$category=="action_words"]))

length(Nouns_CDI) #332 
length(Verbs_CDI) #103

# Remove 2-word nouns
# Remove words that are verb and noun
library(stringr)
Nouns_CDI <- Nouns_CDI[-(which(str_count(Nouns_CDI, " ")==1))]
Nouns_CDI <- Nouns_CDI[-(which(str_count(Nouns_CDI, " ")==2))]
Verbs_CDI <- Verbs_CDI[-(which(str_count(Verbs_CDI, " ")==1))]
length(Nouns_CDI) #303
length(Verbs_CDI) #96

# Remove *s
# Keep first word when /
Nouns_CDI_original <- Nouns_CDI # Keep originals

a <- Nouns_CDI[which(str_count(Nouns_CDI, "\\*")==1)]
Nouns_CDI <- Nouns_CDI[- (match(a,Nouns_CDI))]
a <-  str_split(a, "\\*")
a <-  unlist(lapply(a, `[[`, 1))
Nouns_CDI <- c(a,Nouns_CDI)

a <- Nouns_CDI[which(str_count(Nouns_CDI, "\\/")==1)]
Nouns_CDI <- Nouns_CDI[- (match(a,Nouns_CDI))]
a <-  str_split(a, "\\/")
a <-  unlist(lapply(a, `[[`, 1))
Nouns_CDI <- c(a,Nouns_CDI)

Nouns_CDI_original_df <- data.frame(Original= sort(Nouns_CDI_original),Adapted=sort(Nouns_CDI)) ################### checkear si esto es necesario

## 3 ## Identify which selected CDI words are in CHILDES ####

# Load data 
load("CHILDES_matrix_all.Rdata")
load("allWordsUnique.Rdata")

# Extract all words from CHILDES data
words_CHILDES <- allWordsUnique 

# Nouns
sum(!is.na(match(tolower(Nouns_CDI),tolower(words_CHILDES)))) #  286
Nouns_CDI <- Nouns_CDI[which(!is.na(match(tolower(Nouns_CDI),tolower(words_CHILDES))))]
length(Nouns_CDI) #  286
Nouns_CDI_original <-  as.character(Nouns_CDI_original_df$Original[which(!is.na(match(tolower(Nouns_CDI_original_df$Adapted),tolower(words_CHILDES))))]) #####
length(Nouns_CDI_original) #  286


# Verbs 
length(Verbs_CDI) #  96
sum(!is.na(match(Verbs_CDI,words_CHILDES))) #  96

# Reduce and join in a common dataframe

Nouns_CDI_original_df <- Nouns_CDI_original_df[match(Nouns_CDI_original,Nouns_CDI_original_df$Original),]
Nouns_Verbs_df <- data.frame(Words_CHILDES= c(as.character(Nouns_CDI_original_df$Adapted) ,Verbs_CDI),
                             Words_CDI= c(as.character(Nouns_CDI_original_df$Original),Verbs_CDI),
                             Class= c(rep("Noun",length(Nouns_CDI_original_df$Adapted)), rep("Verb",length(Verbs_CDI))))



## 4 ## Reduce matrix of co-occurrences and compute contextual diversity ####

n_col <- match(tolower(Nouns_Verbs_df$Words_CHILDES),tolower(colnames(CHILDES_matrix_all)))
n_row <- match(tolower(Nouns_Verbs_df$Words_CHILDES),tolower(rownames(CHILDES_matrix_all)))

CHILDES_matrix <- CHILDES_matrix_all[n_row,n_col]
CHILDES_matrix[CHILDES_matrix>0] <- 1 # Make it binary

# Change column names accordingly
for (i in 1:ncol(CHILDES_matrix)) { 
  if(!is.na(match( tolower(colnames(CHILDES_matrix)[i]),tolower(Nouns_Verbs_df$Words_CHILDES)))){
    colnames(CHILDES_matrix)[i] <-as.character(Nouns_Verbs_df$Words_CDI[match( tolower(colnames(CHILDES_matrix)[i]),tolower(Nouns_Verbs_df$Words_CHILDES))])
  }
  
}

for (i in 1:nrow(CHILDES_matrix)) { 
  if(!is.na(match( tolower(rownames(CHILDES_matrix)[i]),tolower(Nouns_Verbs_df$Words_CHILDES)))){
    rownames(CHILDES_matrix)[i] <-as.character(Nouns_Verbs_df$Words_CDI[match( tolower(rownames(CHILDES_matrix)[i]),tolower(Nouns_Verbs_df$Words_CHILDES))])
  }
  
}

dim(CHILDES_matrix)
contextual_diversity<-rowSums(data.frame(r=rowSums(CHILDES_matrix), c=colSums(CHILDES_matrix)) )

# Insert contextual diversity in 'Nouns_Verbs_df'

Nouns_Verbs_df$contextual_diversity <- NA
for (i in 1:nrow(Nouns_Verbs_df)) {
  if(!is.na(match(Nouns_Verbs_df$Words_CDI[i],names(contextual_diversity)))){
    
    Nouns_Verbs_df$contextual_diversity[i] <-  as.numeric(contextual_diversity[match(Nouns_Verbs_df$Words_CDI[i],names(contextual_diversity))])
  }
}

## 5 ## Save data ####
save(Nouns_Verbs_df, file = "Nouns_Verbs_df.RData")
save(CHILDES_matrix, file = "CHILDES_matrix.RData")
save(childbg, file = "childbg.RData")
save(CDI_SENTENCES, file = "CDI_SENTENCES.RData")


