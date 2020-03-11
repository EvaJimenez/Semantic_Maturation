##################################################
## 5 # STUDY 3: THE THREE COMPUTATIONAL MODELS ###
##################################################
## 1 ## Load data ####
load("Nouns_Verbs_df.RData") 
load("childbg_VERBS.RData") 
load("childbg_NOUNS.RData") 

# Create dfs separate for LTs and TTs

childbg_NOUNS_LT <- childbg_NOUNS[childbg_NOUNS$type_talker=="late_talker",]
childbg_NOUNS_TT <- childbg_NOUNS[childbg_NOUNS$type_talker=="typical_talker",]

childbg_VERBS_LT <- childbg_VERBS[childbg_VERBS$type_talker=="late_talker",]
childbg_VERBS_TT <- childbg_VERBS[childbg_VERBS$type_talker=="typical_talker",]

####### MODEL 1 : PREFERENTIAL ACQUISITION MODEL # PA ####### 
## 1 ## Parameter search #### 

# Parameter set values for grid search: 
beta_attention_1_set=seq(0,2,0.01) 
Nrep=500 # Number of repetitions

# Function to go parallel
PA_function <- function(){
  
  TTerr_NOUNS <- vector() 
  LTerr_NOUNS <- vector()
  TTerr_VERBS <- vector() 
  LTerr_VERBS <- vector()
  for (S in 1:length(beta_attention_1_set)){
    beta_attention_1<- beta_attention_1_set[S]  
    
    
    ## Create empty vectors/df
    word <- vector()
    words = vector()
    position_word <- 1
    p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                          Class=Nouns_Verbs_df$Class,
                          comprehension=0,
                          contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
    
    while (nrow(p_wi_df)>1) {
      ## chose word by freq to fill comprehension vector
      num <- p_wi_df$contextual_diversity^beta_attention_1
      den <- sum(p_wi_df$contextual_diversity^beta_attention_1)
      prob <-  num/den
      names(prob) <- as.character(p_wi_df$Word)
      word_prod <- sample(names(prob),1,prob = prob)
      words[position_word] <- word_prod
      #Eliminate learned word
      p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
      position_word <- position_word+1
      
    }
    ##### vocabulary completed here
    
    words_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
    words_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
    
    words_N <- Nouns_Verbs_df$contextual_diversity[match(words_N,Nouns_Verbs_df$Words_CDI)]
    
    vocab_GROWTH_N <- vector()
    for (e in 1:length(words_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
      vocab_GROWTH_N[e]  <- mean(words_N[1:e])
    }
    
    words_V <- Nouns_Verbs_df$contextual_diversity[match(words_V,Nouns_Verbs_df$Words_CDI)]
    vocab_GROWTH_V <- vector()
    for (e in 1:length(words_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
      vocab_GROWTH_V[e]  <- mean(words_V[1:e])
    }
    
    ## MSE per talker and typer of word
    
    TTerr_NOUNS[S] <- mean((childbg_NOUNS_TT$contextual_diversity_NOUNS_ave
                            -vocab_GROWTH_N[childbg_NOUNS_TT$count_produced_analysis_NOUNS])^2,na.rm=T) 
    LTerr_NOUNS[S] <- mean((childbg_NOUNS_LT$contextual_diversity_NOUNS_ave
                            -vocab_GROWTH_N[childbg_NOUNS_LT$count_produced_analysis_NOUNS])^2,na.rm=T) 
    TTerr_VERBS[S] <- mean((childbg_VERBS_TT$contextual_diversity_VERBS_ave
                            -vocab_GROWTH_V[childbg_VERBS_TT$count_produced_analysis_VERBS])^2,na.rm=T) 
    LTerr_VERBS[S] <- mean((childbg_VERBS_LT$contextual_diversity_VERBS_ave
                            -vocab_GROWTH_V[childbg_VERBS_LT$count_produced_analysis_VERBS])^2,na.rm=T) 
  }
  err_NOUNS_VERBS <- list()
  err_NOUNS_VERBS[[1]] <- TTerr_NOUNS
  err_NOUNS_VERBS[[2]] <- LTerr_NOUNS
  err_NOUNS_VERBS[[3]] <- TTerr_VERBS
  err_NOUNS_VERBS[[4]] <- LTerr_VERBS
  err_NOUNS_VERBS
  
  
}

library(parallel)

no_cores <- detectCores() - 1 # Number of CPUs in your computer, leaving one free to keep operating (optional)
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","PA_function","childbg_VERBS_LT","childbg_VERBS_TT",
                    "childbg_NOUNS_LT","childbg_NOUNS_TT","beta_attention_1_set")) # data needed in the funtion
clusterEvalQ(cl,library(BBmisc))

set.seed(1313)
ptm <- proc.time()
PA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  PA_function()
})

proc.time() - ptm
stopCluster(cl)

 saveRDS(PA_function_RESULTS, "PA_function_RESULTS_PA_1.rds") # Save your results for each search

# Adapt Results

vocab_df_ERROR_N_TT <- data.frame(beta_attention_1_set=beta_attention_1_set, MSE=NA)
vocab_df_ERROR_N_LT <- vocab_df_ERROR_N_TT
vocab_df_ERROR_V_TT <- vocab_df_ERROR_N_TT
vocab_df_ERROR_V_LT <- vocab_df_ERROR_N_TT

position_w <- 1

for (z in 1:length(beta_attention_1_set)) {
  vocab_df_ERROR_N_TT$MSE[z] <-mean(unlist(lapply(lapply(PA_function_RESULTS, "[[",1),"[[",z))) ### SIGUE AQUI
} 

for (z in 1:length(beta_attention_1_set)) {
  vocab_df_ERROR_N_LT$MSE[z] <-mean(unlist(lapply(lapply(PA_function_RESULTS, "[[",2),"[[",z))) ### SIGUE AQUI
} 

for (z in 1:length(beta_attention_1_set)) {
  vocab_df_ERROR_V_TT$MSE[z] <-mean(unlist(lapply(lapply(PA_function_RESULTS, "[[",3),"[[",z))) ### SIGUE AQUI
} 

for (z in 1:length(beta_attention_1_set)) {
  vocab_df_ERROR_V_LT$MSE[z] <-mean(unlist(lapply(lapply(PA_function_RESULTS, "[[",4),"[[",z))) ### SIGUE AQUI
} 




## Average MSE of nouns and verbs
vocab_df_ERROR_LT <- vocab_df_ERROR_V_LT
vocab_df_ERROR_LT$MSE <- (vocab_df_ERROR_V_LT$MSE + vocab_df_ERROR_N_LT$MSE)/2

vocab_df_ERROR_TT <- vocab_df_ERROR_V_TT
vocab_df_ERROR_TT$MSE <- (vocab_df_ERROR_V_TT$MSE + vocab_df_ERROR_N_TT$MSE)/2


## Plot MSE per talker ###

plot(vocab_df_ERROR_LT$MSE)
plot(vocab_df_ERROR_TT$MSE)


## Best parameter values
vocab_df_ERROR_LT$beta_attention_1_set[which(vocab_df_ERROR_LT$MSE==min(vocab_df_ERROR_LT$MSE))]
vocab_df_ERROR_TT$beta_attention_1_set[which(vocab_df_ERROR_TT$MSE==min(vocab_df_ERROR_TT$MSE))]

View(vocab_df_ERROR_LT)
View(vocab_df_ERROR_TT)






## 2 ## Create model's networks ####
## Late Talker ####
beta_attention_1= 2.02
Nrep=500
LT_PA_function <- function(){
  
  ## Create empty vectors/df
  word <- vector()
  words_LT = vector()
  position_word <- 1
  p_wi_LT_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                           Class=Nouns_Verbs_df$Class,
                           comprehension=0,
                           contextual_diversity= Nouns_Verbs_df$contextual_diversity)   
  
  # First vocabulary formation
  while (nrow(p_wi_LT_df)>0) {
    ## chose word by freq to fill comprehension vector
    num <- p_wi_LT_df$contextual_diversity^beta_attention_1
    den <- sum(p_wi_LT_df$contextual_diversity^beta_attention_1)
    prob <-  num/den 
    names(prob) <- as.character(p_wi_LT_df$Word)
    cumprob <- cumsum(prob) 
    r <- runif(1) 
    word_prod <- names(cumprob[cumprob>r])[1]
    
    position_word <- position_word + 1
    # Insert the word into the production vector
    words_LT[position_word] <- word_prod
    #Eliminate learned word
    p_wi_LT_df <- p_wi_LT_df[-(which(p_wi_LT_df$Word==word_prod)),]
    
    
  }
  
  
  words_LT_N <- words_LT[which(!is.na( match(words_LT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
  words_LT_V <- words_LT[which(!is.na( match(words_LT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
  
  words_LT_N <- Nouns_Verbs_df$contextual_diversity[match(words_LT_N,Nouns_Verbs_df$Words_CDI)]
  
  LT_vocab_GROWTH_N <- vector()
  for (e in 1:length(words_LT_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
    LT_vocab_GROWTH_N[e]  <- mean(words_LT_N[1:e])
  }
  
  
  words_LT_V <- Nouns_Verbs_df$contextual_diversity[match(words_LT_V,Nouns_Verbs_df$Words_CDI)]
  LT_vocab_GROWTH_V <- vector()
  for (e in 1:length(words_LT_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
    LT_vocab_GROWTH_V[e]  <- mean(words_LT_V[1:e])
    
  }
  LT_vocab_GROWTH_N_V <- list()
  LT_vocab_GROWTH_N_V[[1]] <- LT_vocab_GROWTH_N
  LT_vocab_GROWTH_N_V[[2]] <- LT_vocab_GROWTH_V
  
  LT_vocab_GROWTH_N_V
}

library(parallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","LT_PA_function","beta_attention_1"))
clusterEvalQ(cl,library(BBmisc))

set.seed(1313)
ptm <- proc.time()
LT_PA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  LT_PA_function()
})

proc.time() - ptm
stopCluster(cl)

## Adapt Results 

LT_vocab_df_GROWTH_N <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]), Nrep))
LT_vocab_df_GROWTH_V <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]), Nrep))


for (k in 1:Nrep) {
  LT_vocab_df_GROWTH_N[,k] <-unlist(lapply(LT_PA_function_RESULTS, "[[",1)[k])
  
}

for (k in 1:Nrep) {
  LT_vocab_df_GROWTH_V[,k] <-unlist(lapply(LT_PA_function_RESULTS, "[[",2)[k])
  
}


LT_vocab_df_GROWTH_N$contextual_diversity_ave <- NA

for (i in 1:nrow(LT_vocab_df_GROWTH_N)) {
  LT_vocab_df_GROWTH_N$contextual_diversity_ave[i] <- mean(as.numeric(LT_vocab_df_GROWTH_N[i,1:(ncol(LT_vocab_df_GROWTH_N)-1)]))
}

LT_vocab_df_GROWTH_V$contextual_diversity_ave <- NA

for (i in 1:nrow(LT_vocab_df_GROWTH_V)) {
  LT_vocab_df_GROWTH_V$contextual_diversity_ave[i] <- mean(as.numeric(LT_vocab_df_GROWTH_V[i,1:(ncol(LT_vocab_df_GROWTH_V)-1)]))
}


## Typical Talker ####
beta_attention_1= 1.69
Nrep=500
TT_PA_function <- function(){
  
  ## Create empty vectors/df
  word <- vector()
  words_TT = vector()
  position_word <- 1
  p_wi_TT_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                           Class=Nouns_Verbs_df$Class,
                           comprehension=0,
                           contextual_diversity= Nouns_Verbs_df$contextual_diversity)   
  
  # First vocabulary formation
  while (nrow(p_wi_TT_df)>0) {
    ## chose word by freq to fill comprehension vector
    num <- p_wi_TT_df$contextual_diversity^beta_attention_1
    den <- sum(p_wi_TT_df$contextual_diversity^beta_attention_1)
    prob <-  num/den 
    names(prob) <- as.character(p_wi_TT_df$Word)
    cumprob <- cumsum(prob) 
    r <- runif(1) 
    word_prod <- names(cumprob[cumprob>r])[1]
    
    position_word <- position_word + 1
    # Insert the word into the production vector
    words_TT[position_word] <- word_prod
    #Eliminate learned word
    p_wi_TT_df <- p_wi_TT_df[-(which(p_wi_TT_df$Word==word_prod)),]
    
    
  }
  
  
  words_TT_N <- words_TT[which(!is.na( match(words_TT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
  words_TT_V <- words_TT[which(!is.na( match(words_TT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
  
  words_TT_N <- Nouns_Verbs_df$contextual_diversity[match(words_TT_N,Nouns_Verbs_df$Words_CDI)]
  
  TT_vocab_GROWTH_N <- vector()
  for (e in 1:length(words_TT_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
    TT_vocab_GROWTH_N[e]  <- mean(words_TT_N[1:e])
  }
  
  
  words_TT_V <- Nouns_Verbs_df$contextual_diversity[match(words_TT_V,Nouns_Verbs_df$Words_CDI)]
  TT_vocab_GROWTH_V <- vector()
  for (e in 1:length(words_TT_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
    TT_vocab_GROWTH_V[e]  <- mean(words_TT_V[1:e])
    
  }
  TT_vocab_GROWTH_N_V <- list()
  TT_vocab_GROWTH_N_V[[1]] <- TT_vocab_GROWTH_N
  TT_vocab_GROWTH_N_V[[2]] <- TT_vocab_GROWTH_V
  
  TT_vocab_GROWTH_N_V
}

library(parallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","TT_PA_function","beta_attention_1"))
clusterEvalQ(cl,library(BBmisc))

set.seed(1313)
ptm <- proc.time()
TT_PA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  TT_PA_function()
})

proc.time() - ptm
stopCluster(cl)

## Adapt Results 

TT_vocab_df_GROWTH_N <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]), Nrep))
TT_vocab_df_GROWTH_V <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]), Nrep))


for (k in 1:Nrep) {
  TT_vocab_df_GROWTH_N[,k] <-unlist(lapply(TT_PA_function_RESULTS, "[[",1)[k])
  
}

for (k in 1:Nrep) {
  TT_vocab_df_GROWTH_V[,k] <-unlist(lapply(TT_PA_function_RESULTS, "[[",2)[k])
  
}


TT_vocab_df_GROWTH_N$contextual_diversity_ave <- NA

for (i in 1:nrow(TT_vocab_df_GROWTH_N)) {
  TT_vocab_df_GROWTH_N$contextual_diversity_ave[i] <- mean(as.numeric(TT_vocab_df_GROWTH_N[i,1:(ncol(TT_vocab_df_GROWTH_N)-1)]))
}

TT_vocab_df_GROWTH_V$contextual_diversity_ave <- NA

for (i in 1:nrow(TT_vocab_df_GROWTH_V)) {
  TT_vocab_df_GROWTH_V$contextual_diversity_ave[i] <- mean(as.numeric(TT_vocab_df_GROWTH_V[i,1:(ncol(TT_vocab_df_GROWTH_V)-1)]))
}



## Save/load simulations ####

#TT_vocab_df_GROWTH_N_PA <- TT_vocab_df_GROWTH_N
#LT_vocab_df_GROWTH_N_PA <- LT_vocab_df_GROWTH_N
#TT_vocab_df_GROWTH_V_PA <- TT_vocab_df_GROWTH_V
#LT_vocab_df_GROWTH_V_PA <- LT_vocab_df_GROWTH_V

save(TT_vocab_df_GROWTH_N, file = "TT_vocab_df_GROWTH_N_PA.RData")
save(LT_vocab_df_GROWTH_N, file = "LT_vocab_df_GROWTH_N_PA.RData")
save(TT_vocab_df_GROWTH_V, file = "TT_vocab_df_GROWTH_V_PA.RData")
save(LT_vocab_df_GROWTH_V, file = "LT_vocab_df_GROWTH_V_PA.RData")

#load("TT_vocab_df_GROWTH_N_PA.RData")
#load("LT_vocab_df_GROWTH_N_PA.RData")
#load("TT_vocab_df_GROWTH_V_PA.RData")
#load("LT_vocab_df_GROWTH_V_PA.RData")

#TT_vocab_df_GROWTH_N <-  TT_vocab_df_GROWTH_N_PA
#LT_vocab_df_GROWTH_N <-  LT_vocab_df_GROWTH_N_PA
#TT_vocab_df_GROWTH_V <-  TT_vocab_df_GROWTH_V_PA
#LT_vocab_df_GROWTH_V <-  LT_vocab_df_GROWTH_V_PA

## 3 ## Plot nouns networks ####
library(itsadug)

# TT
TT <- TT_vocab_df_GROWTH_N[,1:Nrep]
TT =as.vector( unlist(c(TT)))

vocab_df_GROWTH_N_MODEL_PLOTS <- data.frame(matrix(TT,length(TT),1))
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size <- rep(seq(1:nrow(TT_vocab_df_GROWTH_N)),Nrep)
head(vocab_df_GROWTH_N_MODEL_PLOTS)
colnames(vocab_df_GROWTH_N_MODEL_PLOTS)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10 <- cut(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size,  breaks=seq(9,190,10))
library(plyr)
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10 <- revalue(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10, c("(179,189]"="(179,190]"))
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10[vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size==190] <- "(179,190]"
vocab_df_GROWTH_N_MODEL_PLOTS <- vocab_df_GROWTH_N_MODEL_PLOTS[!is.na(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10),]
vocab_df_GROWTH_N_MODEL_PLOTS$type_talker <- "Typical talker MODEL"

## LT
LT <- LT_vocab_df_GROWTH_N[,1:Nrep]
LT =as.vector( unlist(c(LT)))

vocab_df_GROWTH_N_MODEL_PLOTS_b <- data.frame(matrix(LT,length(LT),1))
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size <- rep(seq(1:nrow(LT_vocab_df_GROWTH_N)),Nrep)
head(vocab_df_GROWTH_N_MODEL_PLOTS_b)
colnames(vocab_df_GROWTH_N_MODEL_PLOTS_b)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10 <- cut(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size,  breaks=seq(9,190,10))
library(plyr)
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10 <- revalue(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10, c("(179,189]"="(179,190]"))
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10[vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size==190] <- "(179,190]"
vocab_df_GROWTH_N_MODEL_PLOTS_b <- vocab_df_GROWTH_N_MODEL_PLOTS_b[!is.na(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10),]
vocab_df_GROWTH_N_MODEL_PLOTS_b$type_talker <- "Late talker MODEL"

vocab_df_GROWTH_N_MODEL_PLOTS <- rbind(vocab_df_GROWTH_N_MODEL_PLOTS,vocab_df_GROWTH_N_MODEL_PLOTS_b)

# Observed data
par(mfrow=c(1,2))

model_2 <-bam(contextual_diversity_NOUNS_ave ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')


# Model's data

vocab_df_GROWTH_N_MODEL_PLOTS$type_talker <- as.factor(vocab_df_GROWTH_N_MODEL_PLOTS$type_talker)

model_2_PA_model <-bam(contextual_diversity_ave ~ type_talker +
                                  s(vocab_size) ,
                                data=vocab_df_GROWTH_N_MODEL_PLOTS, method="ML")

plot_smooth(model_2_PA_model, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PA_model, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=1,lwd=3,se=0)


## 4 ## Plot verbs networks ####
library(itsadug)

# TT
TT <- TT_vocab_df_GROWTH_V[,1:Nrep]
TT =as.vector( unlist(c(TT)))


vocab_df_GROWTH_V_MODEL_PLOTS <- data.frame(matrix(TT,length(TT),1))
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size <- rep(seq(1:nrow(TT_vocab_df_GROWTH_V)),Nrep)
head(vocab_df_GROWTH_V_MODEL_PLOTS)
colnames(vocab_df_GROWTH_V_MODEL_PLOTS)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10 <- cut(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size,  breaks=seq(9,70,10))
library(plyr)
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10 <- revalue(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10, c("(59,69]"="(59,70]"))
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10[vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size==70] <- "(59,70]"
vocab_df_GROWTH_V_MODEL_PLOTS <- vocab_df_GROWTH_V_MODEL_PLOTS[!is.na(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10),]
vocab_df_GROWTH_V_MODEL_PLOTS$type_talker <- "Typical talker MODEL"


## LT

LT <- LT_vocab_df_GROWTH_V[,1:Nrep]
LT =as.vector( unlist(c(LT)))

vocab_df_GROWTH_V_MODEL_PLOTS_b <- data.frame(matrix(LT,length(LT),1))
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size <- rep(seq(1:nrow(LT_vocab_df_GROWTH_V)),Nrep)
head(vocab_df_GROWTH_V_MODEL_PLOTS_b)
colnames(vocab_df_GROWTH_V_MODEL_PLOTS_b)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10 <- cut(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size,  breaks=seq(9,70,10))
library(plyr)
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10 <- revalue(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10, c("(59,69]"="(59,70]"))
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10[vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size==70] <- "(59,70]"
vocab_df_GROWTH_V_MODEL_PLOTS_b <- vocab_df_GROWTH_V_MODEL_PLOTS_b[!is.na(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10),]
vocab_df_GROWTH_V_MODEL_PLOTS_b$type_talker <- "Late talker MODEL"


vocab_df_GROWTH_V_MODEL_PLOTS <- rbind(vocab_df_GROWTH_V_MODEL_PLOTS,vocab_df_GROWTH_V_MODEL_PLOTS_b)

# Observed data
par(mfrow=c(1,2))

model_2 <-bam(contextual_diversity_VERBS_ave ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Verb Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            ylim=c(270,300),xlog=FALSE,xaxp=c(10,70,6),yaxp=c(270,300,6),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')


# Model's data

vocab_df_GROWTH_V_MODEL_PLOTS$type_talker <- as.factor(vocab_df_GROWTH_V_MODEL_PLOTS$type_talker)

model_2_PA_model_V <-bam(contextual_diversity_ave ~ type_talker +
                           s(vocab_size) ,
                         data=vocab_df_GROWTH_V_MODEL_PLOTS, method="ML")

plot_smooth(model_2_PA_model_V, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PA_model_V, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=1,lwd=3,se=0)


####### MODEL 2 : PROGRESSIVE PREFERENTIAL ACQUISITION # PPA ####### 
## 1a ## Parameter search # Ranges of values #### 
# Parameter set values for grid search: 
boost_set=c(0.5)
beta_attention_set_1=seq(0,1,0.25)
beta_attention_set_2=seq(0,1,0.25)

tau_NOUNS_set= seq(10,50,10) 
tau_VERBS_set= seq(10,50,10) 

length(boost_set)*length(beta_attention_set_1)*length(beta_attention_set_2)*length(tau_NOUNS_set)*length(tau_VERBS_set) # Number of combinations

Nrep=5 # Number of repetitions

# Function to go parallel
PPA_function <- function(){
  
  TTerr_NOUNS_ALL_1 <- list() 
  LTerr_NOUNS_ALL_1 <- list()
  TTerr_VERBS_ALL_1 <- list() 
  LTerr_VERBS_ALL_1 <- list()
  
  TTerr_NOUNS_ALL_2 <- list() 
  LTerr_NOUNS_ALL_2 <- list()
  TTerr_VERBS_ALL_2 <- list() 
  LTerr_VERBS_ALL_2 <- list()
  
  TTerr_NOUNS_ALL_3 <- list() 
  LTerr_NOUNS_ALL_3 <- list()
  TTerr_VERBS_ALL_3 <- list() 
  LTerr_VERBS_ALL_3 <- list()
  
  TTerr_NOUNS_ALL_4 <- list() 
  LTerr_NOUNS_ALL_4 <- list()
  TTerr_VERBS_ALL_4 <- list() 
  LTerr_VERBS_ALL_4 <- list()
  
  
  for (V in 1:length(beta_attention_set_1)){
    beta_attention_1<- beta_attention_set_1[V]  
    TTerr_NOUNS <- vector() 
    LTerr_NOUNS <- vector()
    TTerr_VERBS <- vector() 
    LTerr_VERBS <- vector()
    
    for (H in 1:length(beta_attention_set_2)){
      beta_attention_2<- beta_attention_set_2[H]
      
      for (Y in 1:length(boost_set)){
        boost<- boost_set[Y]
        
        for (W in 1:length(tau_NOUNS_set)){
          tau_NOUNS<- tau_NOUNS_set[W]
          
          for(S in  1:length(tau_VERBS_set) ){
            tau_VERBS <- tau_VERBS_set[S]
            ## Create empty vectors/df
            word <- vector()
            words = vector()
            position_word <- 1
            p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                                  Class=Nouns_Verbs_df$Class,
                                  comprehension=0,
                                  contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
            
            while (nrow(p_wi_df)>381) {
              ## chose word by freq to fill comprehension vector
              num <- p_wi_df$contextual_diversity^beta_attention_1
              den <- sum(p_wi_df$contextual_diversity^beta_attention_1)
              prob <-  num/den
              names(prob) <- as.character(p_wi_df$Word)
              word <- sample(names(prob),1,prob = prob)
              b <- match(word,p_wi_df$Word)
              p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
              
              ## Pass the threshold?
              
              if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
                
                words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
                words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
                
                if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
                  words_passed_t <- words_passed_t_N
                }
                if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
                  words_passed_t <- words_passed_t_V
                }
                
  
                # Insert the word into the production vector
                words[position_word] <- words_passed_t
                #Eliminate learned word
                p_wi_df <- p_wi_df[-(which(p_wi_df$Word==words_passed_t)),]
                
                
                
              }
            }
            while (nrow(p_wi_df)>0) {
              ## chose word by freq to fill comprehension vector
              
              num <- p_wi_df$contextual_diversity^beta_attention_2
              den <- sum(p_wi_df$contextual_diversity^beta_attention_2)
              prob <-  num/den
              names(prob) <- as.character(p_wi_df$Word)
              word <- sample(names(prob),1,prob = prob)
              b <- match(word,p_wi_df$Word)
              p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
              
              ## Pass the threshold?
              
              if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
                
                words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
                words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
                
                if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
                  words_passed_t <- words_passed_t_N
                }
                if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
                  words_passed_t <- words_passed_t_V
                }
                
                
                position_word <- position_word + 1
                
                # Insert the word into the production vector
                words[position_word] <- words_passed_t
                #Eliminate learned word
                p_wi_df <- p_wi_df[-(which(p_wi_df$Word==words_passed_t)),]
                
                
                
              }
              
            }
            ##### HERE YOU HAVE COMPLETED VOCABULARIES
            
            
            words_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
            words_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
            
            words_N <- Nouns_Verbs_df$contextual_diversity[match(words_N,Nouns_Verbs_df$Words_CDI)]
            
            vocab_GROWTH_N <- vector()
            for (e in 1:length(words_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
              vocab_GROWTH_N[e]  <- mean(words_N[1:e])
            }
            
            
            words_V <- Nouns_Verbs_df$contextual_diversity[match(words_V,Nouns_Verbs_df$Words_CDI)]
            vocab_GROWTH_V <- vector()
            for (e in 1:length(words_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
              vocab_GROWTH_V[e]  <- mean(words_V[1:e])
            }
            
            
            TTerr_NOUNS[S] <- mean((childbg_NOUNS_TT$contextual_diversity_NOUNS_ave
                                    -vocab_GROWTH_N[childbg_NOUNS_TT$count_produced_analysis_NOUNS])^2,na.rm=T) 
            LTerr_NOUNS[S] <- mean((childbg_NOUNS_LT$contextual_diversity_NOUNS_ave
                                    -vocab_GROWTH_N[childbg_NOUNS_LT$count_produced_analysis_NOUNS])^2,na.rm=T) 
            TTerr_VERBS[S] <- mean((childbg_VERBS_TT$contextual_diversity_VERBS_ave
                                    -vocab_GROWTH_V[childbg_VERBS_TT$count_produced_analysis_VERBS])^2,na.rm=T) 
            LTerr_VERBS[S] <- mean((childbg_VERBS_LT$contextual_diversity_VERBS_ave
                                    -vocab_GROWTH_V[childbg_VERBS_LT$count_produced_analysis_VERBS])^2,na.rm=T) 
          }
          TTerr_NOUNS_ALL_1[[W]] <- TTerr_NOUNS 
          LTerr_NOUNS_ALL_1[[W]] <-LTerr_NOUNS
          TTerr_VERBS_ALL_1[[W]] <- TTerr_VERBS
          LTerr_VERBS_ALL_1[[W]] <- LTerr_VERBS
        }
        
        TTerr_NOUNS_ALL_2[[Y]] <- TTerr_NOUNS_ALL_1 
        LTerr_NOUNS_ALL_2[[Y]] <-LTerr_NOUNS_ALL_1
        TTerr_VERBS_ALL_2[[Y]] <- TTerr_VERBS_ALL_1
        LTerr_VERBS_ALL_2[[Y]] <- LTerr_VERBS_ALL_1
        
      }
      
      TTerr_NOUNS_ALL_3[[H]] <- TTerr_NOUNS_ALL_2 
      LTerr_NOUNS_ALL_3[[H]] <-LTerr_NOUNS_ALL_2
      TTerr_VERBS_ALL_3[[H]] <- TTerr_VERBS_ALL_2
      LTerr_VERBS_ALL_3[[H]] <- LTerr_VERBS_ALL_2
      
    }
    
    TTerr_NOUNS_ALL_4[[V]] <- TTerr_NOUNS_ALL_3 
    LTerr_NOUNS_ALL_4[[V]] <-LTerr_NOUNS_ALL_3
    TTerr_VERBS_ALL_4[[V]] <- TTerr_VERBS_ALL_3
    LTerr_VERBS_ALL_4[[V]] <- LTerr_VERBS_ALL_3
    
  }
  
  
  err_NOUNS_VERBS <- list()
  err_NOUNS_VERBS[[1]] <- TTerr_NOUNS_ALL_4
  err_NOUNS_VERBS[[2]] <- LTerr_NOUNS_ALL_4
  err_NOUNS_VERBS[[3]] <- TTerr_VERBS_ALL_4
  err_NOUNS_VERBS[[4]] <- LTerr_VERBS_ALL_4
  err_NOUNS_VERBS
  
  
}

library(parallel)

no_cores <- detectCores() - 1

cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","PPA_function","boost_set","childbg_VERBS_LT","childbg_VERBS_TT",
                    "childbg_NOUNS_LT","childbg_NOUNS_TT","tau_VERBS_set","tau_NOUNS_set","beta_attention_set_1","beta_attention_set_2"))
clusterEvalQ(cl,library(BBmisc))

set.seed(1313) 
ptm <- proc.time()
PPA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  PPA_function()
})

proc.time() - ptm
stopCluster(cl)

saveRDS(PPA_function_RESULTS, "PPA_function_RESULTS_phase_1_TEST.rds")

# Adapt Results 

vocab_df_ERROR_N_TT <- data.frame(beta_attention_set_1=rep(beta_attention_set_1,times=c(rep(length(beta_attention_set_2)*length(boost_set)*length(tau_NOUNS_set)*length(tau_VERBS_set),
                                                                                            length(beta_attention_set_1)))),
                                  beta_attention_set_2=rep(rep(beta_attention_set_2,times=c(rep(length(boost_set)*length(tau_NOUNS_set)*length(tau_VERBS_set),
                                                                                                length(beta_attention_set_2)))),length(beta_attention_set_1)),
                                  boost_set=rep(rep(rep(boost_set,times=c(rep(length(tau_NOUNS_set)*length(tau_VERBS_set),
                                                                              length(boost_set)))),length(beta_attention_set_2)),length(beta_attention_set_1)),
                                  tau_NOUNS_set=rep(rep(rep(rep(tau_NOUNS_set,times=c(rep(length(tau_VERBS_set),
                                                                                          length(tau_NOUNS_set)))),length(boost_set)),length(beta_attention_set_2)),length(beta_attention_set_1)),
                                  
                                  tau_VERBS_set=rep(rep(rep(rep(rep(tau_VERBS_set,times=c(rep(1,length(tau_VERBS_set)))),length(tau_NOUNS_set)),length(boost_set)),length(beta_attention_set_2)),length(beta_attention_set_1)),
                                  MSE=NA)


vocab_df_ERROR_N_LT <- vocab_df_ERROR_N_TT

vocab_df_ERROR_V_TT <- vocab_df_ERROR_N_TT

vocab_df_ERROR_V_LT <- vocab_df_ERROR_N_TT


position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_N_TT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PPA_function_RESULTS, "[[",1), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}


position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_N_LT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PPA_function_RESULTS, "[[",2), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}

position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_V_TT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PPA_function_RESULTS, "[[",3), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}


position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_V_LT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PPA_function_RESULTS, "[[",4), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}




## Average MSE of nouns and verbs
vocab_df_ERROR_LT <- vocab_df_ERROR_V_LT
vocab_df_ERROR_LT$MSE <- (vocab_df_ERROR_V_LT$MSE + vocab_df_ERROR_N_LT$MSE)/2

vocab_df_ERROR_TT <- vocab_df_ERROR_V_TT
vocab_df_ERROR_TT$MSE <- (vocab_df_ERROR_V_TT$MSE + vocab_df_ERROR_N_TT$MSE)/2



## Plot MSE per talker ###
plot(vocab_df_ERROR_LT$MSE)
plot(vocab_df_ERROR_TT$MSE)
View(vocab_df_ERROR_LT)
View(vocab_df_ERROR_TT)


## Best set of parameter values
View(vocab_df_ERROR_LT)
View(vocab_df_ERROR_TT)

## 1b ## Parameter search # Test specific set of values #### 

# Parameter set values for grid search from previous search

## Select x best parameter sets 

top_x <- 2
vocab_df_ERROR_LT <- vocab_df_ERROR_LT[order(vocab_df_ERROR_LT$MSE),]
vocab_df_ERROR_LT_R <- vocab_df_ERROR_LT[1:top_x,1:5]
vocab_df_ERROR_TT <- vocab_df_ERROR_TT[order(vocab_df_ERROR_TT$MSE),]
vocab_df_ERROR_TT_R <- vocab_df_ERROR_TT[1:top_x,1:5]

vocab_df_ERROR_R <- rbind(vocab_df_ERROR_LT_R,vocab_df_ERROR_TT_R)
dim(vocab_df_ERROR_R)
vocab_df_ERROR_R <- unique(vocab_df_ERROR_R)
dim(vocab_df_ERROR_R)

## fine-grained search taus per unique set of betas and boost:

vocab_df_ERROR_temp <- as.data.frame(matrix(nrow = nrow(vocab_df_ERROR_R)*(11*11), ncol = ncol(vocab_df_ERROR_LT))) ##  (+- 5)
colnames(vocab_df_ERROR_temp) <- colnames(vocab_df_ERROR_R)
dim(vocab_df_ERROR_temp)

vocab_df_ERROR_temp[,1] <- rep(as.matrix(vocab_df_ERROR_R$beta_attention_set_1),each=(11*11))
vocab_df_ERROR_temp[,2] <- rep(as.matrix(vocab_df_ERROR_R$beta_attention_set_2),each=(11*11))
vocab_df_ERROR_temp[,3] <- rep(as.matrix(vocab_df_ERROR_R$boost_set),each=(11*11))

block_start <- 1
block_end <- (11*11)
for (i in 1:top_x) {
  tau_NOUNS_set <- (vocab_df_ERROR_R$tau_NOUNS_set[i]-5):(vocab_df_ERROR_R$tau_NOUNS_set[i]+5)
  tau_VERBS_set <- (vocab_df_ERROR_R$tau_VERBS_set[i]-5):(vocab_df_ERROR_R$tau_VERBS_set[i]+5)
  vocab_df_ERROR_temp$tau_NOUNS_set[block_start:block_end] <-as.vector(unlist(expand.grid(tau_NOUNS_set,tau_VERBS_set)[1]))
  vocab_df_ERROR_temp$tau_VERBS_set[block_start:block_end] <-as.vector(unlist(expand.grid(tau_NOUNS_set,tau_VERBS_set)[2]))
  
  block_start <- block_start+(11*11)
  block_end <- block_start + ((11*11)-1)
}

dim(vocab_df_ERROR_temp)
vocab_df_ERROR_temp <- unique(vocab_df_ERROR_temp)
dim(vocab_df_ERROR_temp)
vocab_df_ERROR_R <- vocab_df_ERROR_temp

#saveRDS(vocab_df_ERROR_R, "vocab_df_ERROR_R.rds")

# Function to go parallel
PPA_function_2 <- function(){
  
  
  TTerr_NOUNS <- vector() 
  LTerr_NOUNS <- vector()
  TTerr_VERBS <- vector() 
  LTerr_VERBS <- vector()
  for (V in 1:nrow(vocab_df_ERROR_R)){
    
    parameter_set <-as.numeric(vocab_df_ERROR_R[V,])
    beta_attention_1<-  parameter_set[1] 
    beta_attention_2<- parameter_set[2]
    boost<- parameter_set[3]
    tau_NOUNS<- parameter_set[4]
    tau_VERBS <- parameter_set[5]
    
    
    ## Create empty vectors/df
    word <- vector()
    words = vector()
    position_word <- 1
    p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                          Class=Nouns_Verbs_df$Class,
                          comprehension=0,
                          contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
    
    while (nrow(p_wi_df)>381) {
      ## chose word by freq to fill comprehension vector
      num <- p_wi_df$contextual_diversity^beta_attention_1
      den <- sum(p_wi_df$contextual_diversity^beta_attention_1)
      prob <-  num/den
      names(prob) <- as.character(p_wi_df$Word)
      word <- sample(names(prob),1,prob = prob)
      b <- match(word,p_wi_df$Word)
      p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
      
      ## Pass the threshold?
      
      if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
        
        words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
        words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
        
        if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
          words_passed_t <- words_passed_t_N
        }
        if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
          words_passed_t <- words_passed_t_V
        }
        
        
        position_word <- position_word + 1
        
        # Insert the word into the production vector
        words[position_word] <- words_passed_t
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==words_passed_t)),]
        
        
        
      }
    }
    while (nrow(p_wi_df)>0) {
      num <- p_wi_df$contextual_diversity^beta_attention_2
      den <- sum(p_wi_df$contextual_diversity^beta_attention_2)
      prob <-  num/den
      names(prob) <- as.character(p_wi_df$Word)
      word <- sample(names(prob),1,prob = prob)
      b <- match(word,p_wi_df$Word)
      p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
      
      ## Pass the threshold?
      
      if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
        
        words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
        words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
        
        if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
          words_passed_t <- words_passed_t_N
        }
        if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
          words_passed_t <- words_passed_t_V
        }
        
        
        position_word <- position_word + 1
        
        # Insert the word into the production vector
        words[position_word] <- words_passed_t
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==words_passed_t)),]
        
      }
      
    }
    
    
    words_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
    words_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
    
    words_N <- Nouns_Verbs_df$contextual_diversity[match(words_N,Nouns_Verbs_df$Words_CDI)]
    
    vocab_GROWTH_N <- vector()
    for (e in 1:length(words_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
      vocab_GROWTH_N[e]  <- mean(words_N[1:e])
    }
    
    
    words_V <- Nouns_Verbs_df$contextual_diversity[match(words_V,Nouns_Verbs_df$Words_CDI)]
    vocab_GROWTH_V <- vector()
    for (e in 1:length(words_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
      vocab_GROWTH_V[e]  <- mean(words_V[1:e])
    }
    
    
    TTerr_NOUNS[V] <- mean((childbg_NOUNS_TT$contextual_diversity_NOUNS_ave
                            -vocab_GROWTH_N[childbg_NOUNS_TT$count_produced_analysis_NOUNS])^2,na.rm=T) 
    LTerr_NOUNS[V] <- mean((childbg_NOUNS_LT$contextual_diversity_NOUNS_ave
                            -vocab_GROWTH_N[childbg_NOUNS_LT$count_produced_analysis_NOUNS])^2,na.rm=T) 
    TTerr_VERBS[V] <- mean((childbg_VERBS_TT$contextual_diversity_VERBS_ave
                            -vocab_GROWTH_V[childbg_VERBS_TT$count_produced_analysis_VERBS])^2,na.rm=T) 
    LTerr_VERBS[V] <- mean((childbg_VERBS_LT$contextual_diversity_VERBS_ave
                            -vocab_GROWTH_V[childbg_VERBS_LT$count_produced_analysis_VERBS])^2,na.rm=T) 
  }
  
  
  err_NOUNS_VERBS <- list()
  err_NOUNS_VERBS[[1]] <- TTerr_NOUNS
  err_NOUNS_VERBS[[2]] <- LTerr_NOUNS
  err_NOUNS_VERBS[[3]] <- TTerr_VERBS
  err_NOUNS_VERBS[[4]] <- LTerr_VERBS
  err_NOUNS_VERBS
  
  
}

Nrep=500

library(parallel)

no_cores <- detectCores() - 1

cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","PPA_function_2","childbg_VERBS_LT","childbg_VERBS_TT",
                    "childbg_NOUNS_LT","childbg_NOUNS_TT","vocab_df_ERROR_R"))
clusterEvalQ(cl,library(BBmisc))

set.seed(1313)
ptm <- proc.time()
VOCAB_PPA_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  PPA_function_2()
})

proc.time() - ptm
stopCluster(cl)

saveRDS(VOCAB_PPA_RESULTS, "VOCAB_PPA_RESULTS_1.rds")

# Adapt results 
vocab_df_ERROR_N_LT <- data.frame(beta_attention_set_1=vocab_df_ERROR_LT_R$beta_attention_set_1,
                                  beta_attention_set_2=vocab_df_ERROR_LT_R$beta_attention_set_2,
                                  boost_set=vocab_df_ERROR_LT_R$boost_set,
                                  tau_NOUNS_set=vocab_df_ERROR_LT_R$tau_NOUNS_set,
                                  tau_VERBS_set=vocab_df_ERROR_LT_R$tau_VERBS_set,
                                  MSE=NA)


vocab_df_ERROR_V_LT <- vocab_df_ERROR_N_LT
vocab_df_ERROR_N_TT <- vocab_df_ERROR_N_LT
vocab_df_ERROR_V_TT <- vocab_df_ERROR_N_LT

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_N_LT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PPA_RESULTS, "[[",2), "[[",z) ))
} 

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_V_LT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PPA_RESULTS, "[[",4), "[[",z) )) 
} 

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_N_TT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PPA_RESULTS, "[[",1), "[[",z) )) 
} 

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_V_TT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PPA_RESULTS, "[[",3), "[[",z) )) 
} 





## Average MSE of nouns and verbs
vocab_df_ERROR_LT <- vocab_df_ERROR_V_LT
vocab_df_ERROR_LT$MSE <- (vocab_df_ERROR_V_LT$MSE + vocab_df_ERROR_N_LT$MSE)/2

vocab_df_ERROR_TT <- vocab_df_ERROR_V_TT
vocab_df_ERROR_TT$MSE <- (vocab_df_ERROR_V_TT$MSE + vocab_df_ERROR_N_TT$MSE)/2



## Plot MSE per talker 
plot(vocab_df_ERROR_LT$MSE)
plot(vocab_df_ERROR_TT$MSE)

View(vocab_df_ERROR_TT)
View(vocab_df_ERROR_LT)



## 2 ## Create model's networks ####
Nrep=500
library(itsadug)

Nrep=500

boost_LT=0.5
tau_NOUNS_LT=6.5
tau_VERBS_LT=10
beta_attention_LT=0.75
beta_attention_LT_2=0


boost_TT=0.5
tau_NOUNS_TT=7
tau_VERBS_TT=18
beta_attention_TT=0.75
beta_attention_TT_2=0


## Late Talker ####
set.seed(1313)

LT_PPA_function <- function(){
  
  ## Create empty vectors/df
  word <- vector()
  words_LT = vector()
  position_word <- 1
  p_wi_LT_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                           Class=Nouns_Verbs_df$Class,
                           comprehension=0,
                           contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
  
  # First vocabulary formation
  while (nrow(p_wi_LT_df)>381) {
    ## chose word by freq to fill comprehension vector
    num <- p_wi_LT_df$contextual_diversity^beta_attention_LT
    den <- sum(p_wi_LT_df$contextual_diversity^beta_attention_LT)
    prob <-  num/den
    names(prob) <- as.character(p_wi_LT_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_LT_df$Word)
    p_wi_LT_df$comprehension[b] <- p_wi_LT_df$comprehension[b] + boost_LT 
    
    ## Pass the threshold?
    
    if(sum(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Noun"]>tau_NOUNS_LT)>0 |sum(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Verb"]>tau_VERBS_LT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_LT_df$Word[p_wi_LT_df$Class=="Noun"][which(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Noun"]>tau_NOUNS_LT)])
      words_passed_t_V <- as.character(p_wi_LT_df$Word[p_wi_LT_df$Class=="Verb"][which(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Verb"]>tau_VERBS_LT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      position_word <- position_word + 1
      
      # Insert the word into the production vector
      words_LT[position_word] <- words_passed_t
      #Eliminate learned word
      p_wi_LT_df <- p_wi_LT_df[-(which(p_wi_LT_df$Word==words_passed_t)),]
      
      
      
    }
  }
  while (nrow(p_wi_LT_df)>0) {
    num <- p_wi_LT_df$contextual_diversity^beta_attention_LT_2
    den <- sum(p_wi_LT_df$contextual_diversity^beta_attention_LT_2)
    prob <-  num/den
    names(prob) <- as.character(p_wi_LT_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_LT_df$Word)
    p_wi_LT_df$comprehension[b] <- p_wi_LT_df$comprehension[b] + boost_LT 
    
    ## Pass the threshold?
    
    if(sum(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Noun"]>tau_NOUNS_LT)>0 |sum(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Verb"]>tau_VERBS_LT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_LT_df$Word[p_wi_LT_df$Class=="Noun"][which(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Noun"]>tau_NOUNS_LT)])
      words_passed_t_V <- as.character(p_wi_LT_df$Word[p_wi_LT_df$Class=="Verb"][which(p_wi_LT_df$comprehension[p_wi_LT_df$Class=="Verb"]>tau_VERBS_LT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      position_word <- position_word + 1
      
      # Insert the word into the production vector
      words_LT[position_word] <- words_passed_t
      #Eliminate learned word
      p_wi_LT_df <- p_wi_LT_df[-(which(p_wi_LT_df$Word==words_passed_t)),]
      
      
    }
    
  }
  ##### HERE YOU HAVE COMPLETED VOCABULARIES
  
  
  words_LT_N <- words_LT[which(!is.na( match(words_LT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
  words_LT_V <- words_LT[which(!is.na( match(words_LT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
  
  words_LT_N <- Nouns_Verbs_df$contextual_diversity[match(words_LT_N,Nouns_Verbs_df$Words_CDI)]
  
  LT_vocab_GROWTH_N <- vector()
  for (e in 1:length(words_LT_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
    LT_vocab_GROWTH_N[e]  <- mean(words_LT_N[1:e])
  }
  
  
  words_LT_V <- Nouns_Verbs_df$contextual_diversity[match(words_LT_V,Nouns_Verbs_df$Words_CDI)]
  LT_vocab_GROWTH_V <- vector()
  for (e in 1:length(words_LT_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
    LT_vocab_GROWTH_V[e]  <- mean(words_LT_V[1:e])
    
  }
  LT_vocab_GROWTH_N_V <- list()
  LT_vocab_GROWTH_N_V[[1]] <- LT_vocab_GROWTH_N
  LT_vocab_GROWTH_N_V[[2]] <- LT_vocab_GROWTH_V
  
  LT_vocab_GROWTH_N_V
}

library(parallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","LT_PPA_function","tau_NOUNS_LT","tau_VERBS_LT","boost_LT","beta_attention_LT","beta_attention_LT_2"))
clusterEvalQ(cl,library(BBmisc))


ptm <- proc.time()
LT_PPA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  LT_PPA_function()
})

proc.time() - ptm
stopCluster(cl)

# Adapt Results 

LT_vocab_df_GROWTH_N <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]), Nrep))
LT_vocab_df_GROWTH_V <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]), Nrep))


for (k in 1:Nrep) {
  LT_vocab_df_GROWTH_N[,k] <-unlist(lapply(LT_PPA_function_RESULTS, "[[",1)[k])
  
}

for (k in 1:Nrep) {
  LT_vocab_df_GROWTH_V[,k] <-unlist(lapply(LT_PPA_function_RESULTS, "[[",2)[k])
  
}


LT_vocab_df_GROWTH_N$contextual_diversity_ave <- NA

for (i in 1:nrow(LT_vocab_df_GROWTH_N)) {
  LT_vocab_df_GROWTH_N$contextual_diversity_ave[i] <- mean(as.numeric(LT_vocab_df_GROWTH_N[i,1:(ncol(LT_vocab_df_GROWTH_N)-1)]))
}

LT_vocab_df_GROWTH_V$contextual_diversity_ave <- NA

for (i in 1:nrow(LT_vocab_df_GROWTH_V)) {
  LT_vocab_df_GROWTH_V$contextual_diversity_ave[i] <- mean(as.numeric(LT_vocab_df_GROWTH_V[i,1:(ncol(LT_vocab_df_GROWTH_V)-1)]))
}


## Typical Talker ####
set.seed(1313)

TT_PPA_function <- function(){
  
  ## Create empty vectors/df
  word <- vector()
  words_TT = vector()
  position_word <- 1
  p_wi_TT_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                           Class=Nouns_Verbs_df$Class,
                           comprehension=0,
                           contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
  
  # First vocabulary formation
  while (nrow(p_wi_TT_df)>381) {
    ## chose word by freq to fill comprehension vector
    num <- p_wi_TT_df$contextual_diversity^beta_attention_TT
    den <- sum(p_wi_TT_df$contextual_diversity^beta_attention_TT)
    prob <-  num/den
    names(prob) <- as.character(p_wi_TT_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_TT_df$Word)
    p_wi_TT_df$comprehension[b] <- p_wi_TT_df$comprehension[b] + boost_TT 
    
    ## Pass the threshold?
    
    if(sum(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Noun"]>tau_NOUNS_TT)>0 |sum(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Verb"]>tau_VERBS_TT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_TT_df$Word[p_wi_TT_df$Class=="Noun"][which(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Noun"]>tau_NOUNS_TT)])
      words_passed_t_V <- as.character(p_wi_TT_df$Word[p_wi_TT_df$Class=="Verb"][which(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Verb"]>tau_VERBS_TT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      position_word <- position_word + 1
      
      # Insert the word into the production vector
      words_TT[position_word] <- words_passed_t
      #Eliminate learned word
      p_wi_TT_df <- p_wi_TT_df[-(which(p_wi_TT_df$Word==words_passed_t)),]
      
      
      
    }
  }
  while (nrow(p_wi_TT_df)>0) {
    num <- p_wi_TT_df$contextual_diversity^beta_attention_TT_2
    den <- sum(p_wi_TT_df$contextual_diversity^beta_attention_TT_2)
    prob <-  num/den
    names(prob) <- as.character(p_wi_TT_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_TT_df$Word)
    p_wi_TT_df$comprehension[b] <- p_wi_TT_df$comprehension[b] + boost_TT 
    
    ## Pass the threshold?
    
    if(sum(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Noun"]>tau_NOUNS_TT)>0 |sum(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Verb"]>tau_VERBS_TT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_TT_df$Word[p_wi_TT_df$Class=="Noun"][which(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Noun"]>tau_NOUNS_TT)])
      words_passed_t_V <- as.character(p_wi_TT_df$Word[p_wi_TT_df$Class=="Verb"][which(p_wi_TT_df$comprehension[p_wi_TT_df$Class=="Verb"]>tau_VERBS_TT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      position_word <- position_word + 1
      
      # Insert the word into the production vector
      words_TT[position_word] <- words_passed_t
      #Eliminate learned word
      p_wi_TT_df <- p_wi_TT_df[-(which(p_wi_TT_df$Word==words_passed_t)),]
      
      
    }
    
  }
  
  words_TT_N <- words_TT[which(!is.na( match(words_TT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
  words_TT_V <- words_TT[which(!is.na( match(words_TT,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
  
  words_TT_N <- Nouns_Verbs_df$contextual_diversity[match(words_TT_N,Nouns_Verbs_df$Words_CDI)]
  
  TT_vocab_GROWTH_N <- vector()
  for (e in 1:length(words_TT_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
    TT_vocab_GROWTH_N[e]  <- mean(words_TT_N[1:e])
  }
  
  
  words_TT_V <- Nouns_Verbs_df$contextual_diversity[match(words_TT_V,Nouns_Verbs_df$Words_CDI)]
  TT_vocab_GROWTH_V <- vector()
  for (e in 1:length(words_TT_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
    TT_vocab_GROWTH_V[e]  <- mean(words_TT_V[1:e])
    
  }
  TT_vocab_GROWTH_N_V <- list()
  TT_vocab_GROWTH_N_V[[1]] <- TT_vocab_GROWTH_N
  TT_vocab_GROWTH_N_V[[2]] <- TT_vocab_GROWTH_V
  
  TT_vocab_GROWTH_N_V
}

library(parallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","TT_PPA_function","tau_NOUNS_TT","tau_VERBS_TT","boost_TT","beta_attention_TT","beta_attention_TT_2"))
clusterEvalQ(cl,library(BBmisc))


ptm <- proc.time()
TT_PPA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  TT_PPA_function()
})

proc.time() - ptm
stopCluster(cl)

# Adapt Results 

TT_vocab_df_GROWTH_N <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]), Nrep))
TT_vocab_df_GROWTH_V <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]), Nrep))


for (k in 1:Nrep) {
  TT_vocab_df_GROWTH_N[,k] <-unlist(lapply(TT_PPA_function_RESULTS, "[[",1)[k])
  
}

for (k in 1:Nrep) {
  TT_vocab_df_GROWTH_V[,k] <-unlist(lapply(TT_PPA_function_RESULTS, "[[",2)[k])
  
}


TT_vocab_df_GROWTH_N$contextual_diversity_ave <- NA

for (i in 1:nrow(TT_vocab_df_GROWTH_N)) {
  TT_vocab_df_GROWTH_N$contextual_diversity_ave[i] <- mean(as.numeric(TT_vocab_df_GROWTH_N[i,1:(ncol(TT_vocab_df_GROWTH_N)-1)]))
}

TT_vocab_df_GROWTH_V$contextual_diversity_ave <- NA

for (i in 1:nrow(TT_vocab_df_GROWTH_V)) {
  TT_vocab_df_GROWTH_V$contextual_diversity_ave[i] <- mean(as.numeric(TT_vocab_df_GROWTH_V[i,1:(ncol(TT_vocab_df_GROWTH_V)-1)]))
}


## Save/load simulations ####

#TT_vocab_df_GROWTH_N_PPA <- TT_vocab_df_GROWTH_N
#LT_vocab_df_GROWTH_N_PPA <- LT_vocab_df_GROWTH_N
#TT_vocab_df_GROWTH_V_PPA <- TT_vocab_df_GROWTH_V
#LT_vocab_df_GROWTH_V_PPA <- LT_vocab_df_GROWTH_V

save(TT_vocab_df_GROWTH_N, file = "TT_vocab_df_GROWTH_N_PPA.RData")
save(LT_vocab_df_GROWTH_N, file = "LT_vocab_df_GROWTH_N_PPA.RData")
save(TT_vocab_df_GROWTH_V, file = "TT_vocab_df_GROWTH_V_PPA.RData")
save(LT_vocab_df_GROWTH_V, file = "LT_vocab_df_GROWTH_V_PPA.RData")

load("TT_vocab_df_GROWTH_N_PPA.RData")
load("LT_vocab_df_GROWTH_N_PPA.RData")
load("TT_vocab_df_GROWTH_V_PPA.RData")
load("LT_vocab_df_GROWTH_V_PPA.RData")

#TT_vocab_df_GROWTH_N <-  TT_vocab_df_GROWTH_N_PPA
#LT_vocab_df_GROWTH_N <-  LT_vocab_df_GROWTH_N_PPA
#TT_vocab_df_GROWTH_V <-  TT_vocab_df_GROWTH_V_PPA
#LT_vocab_df_GROWTH_V <-  LT_vocab_df_GROWTH_V_PPA

## 3 ## Plot nouns networks ####
library(itsadug)

# TT
TT <- TT_vocab_df_GROWTH_N[,1:Nrep]
TT =as.vector( unlist(c(TT)))

vocab_df_GROWTH_N_MODEL_PLOTS <- data.frame(matrix(TT,length(TT),1))
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size <- rep(seq(1:nrow(TT_vocab_df_GROWTH_N)),Nrep)
head(vocab_df_GROWTH_N_MODEL_PLOTS)
colnames(vocab_df_GROWTH_N_MODEL_PLOTS)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10 <- cut(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size,  breaks=seq(9,190,10))
library(plyr)
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10 <- revalue(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10, c("(179,189]"="(179,190]"))
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10[vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size==190] <- "(179,190]"
vocab_df_GROWTH_N_MODEL_PLOTS <- vocab_df_GROWTH_N_MODEL_PLOTS[!is.na(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10),]
vocab_df_GROWTH_N_MODEL_PLOTS$type_talker <- "Typical talker MODEL"

## LT
LT <- LT_vocab_df_GROWTH_N[,1:Nrep]
LT =as.vector( unlist(c(LT)))

vocab_df_GROWTH_N_MODEL_PLOTS_b <- data.frame(matrix(LT,length(LT),1))
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size <- rep(seq(1:nrow(LT_vocab_df_GROWTH_N)),Nrep)
head(vocab_df_GROWTH_N_MODEL_PLOTS_b)
colnames(vocab_df_GROWTH_N_MODEL_PLOTS_b)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10 <- cut(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size,  breaks=seq(9,190,10))
library(plyr)
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10 <- revalue(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10, c("(179,189]"="(179,190]"))
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10[vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size==190] <- "(179,190]"
vocab_df_GROWTH_N_MODEL_PLOTS_b <- vocab_df_GROWTH_N_MODEL_PLOTS_b[!is.na(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10),]
vocab_df_GROWTH_N_MODEL_PLOTS_b$type_talker <- "Late talker MODEL"

vocab_df_GROWTH_N_MODEL_PLOTS <- rbind(vocab_df_GROWTH_N_MODEL_PLOTS,vocab_df_GROWTH_N_MODEL_PLOTS_b)

# Observed data
par(mfrow=c(1,2))

model_2 <-bam(contextual_diversity_NOUNS_ave ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')


# Model's data

vocab_df_GROWTH_N_MODEL_PLOTS$type_talker <- as.factor(vocab_df_GROWTH_N_MODEL_PLOTS$type_talker)

model_2_PPA_model <-bam(contextual_diversity_ave ~ type_talker +
                         s(vocab_size) ,
                       data=vocab_df_GROWTH_N_MODEL_PLOTS, method="ML")

plot_smooth(model_2_PPA_model, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PPA_model, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=1,lwd=3,se=0)


## 4 ## Plot verbs networks ####
library(itsadug)

# TT
TT <- TT_vocab_df_GROWTH_V[,1:Nrep]
TT =as.vector( unlist(c(TT)))


vocab_df_GROWTH_V_MODEL_PLOTS <- data.frame(matrix(TT,length(TT),1))
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size <- rep(seq(1:nrow(TT_vocab_df_GROWTH_V)),Nrep)
head(vocab_df_GROWTH_V_MODEL_PLOTS)
colnames(vocab_df_GROWTH_V_MODEL_PLOTS)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10 <- cut(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size,  breaks=seq(9,70,10))
library(plyr)
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10 <- revalue(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10, c("(59,69]"="(59,70]"))
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10[vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size==70] <- "(59,70]"
vocab_df_GROWTH_V_MODEL_PLOTS <- vocab_df_GROWTH_V_MODEL_PLOTS[!is.na(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10),]
vocab_df_GROWTH_V_MODEL_PLOTS$type_talker <- "Typical talker MODEL"


## LT

LT <- LT_vocab_df_GROWTH_V[,1:Nrep]
LT =as.vector( unlist(c(LT)))

vocab_df_GROWTH_V_MODEL_PLOTS_b <- data.frame(matrix(LT,length(LT),1))
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size <- rep(seq(1:nrow(LT_vocab_df_GROWTH_V)),Nrep)
head(vocab_df_GROWTH_V_MODEL_PLOTS_b)
colnames(vocab_df_GROWTH_V_MODEL_PLOTS_b)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10 <- cut(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size,  breaks=seq(9,70,10))
library(plyr)
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10 <- revalue(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10, c("(59,69]"="(59,70]"))
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10[vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size==70] <- "(59,70]"
vocab_df_GROWTH_V_MODEL_PLOTS_b <- vocab_df_GROWTH_V_MODEL_PLOTS_b[!is.na(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10),]
vocab_df_GROWTH_V_MODEL_PLOTS_b$type_talker <- "Late talker MODEL"


vocab_df_GROWTH_V_MODEL_PLOTS <- rbind(vocab_df_GROWTH_V_MODEL_PLOTS,vocab_df_GROWTH_V_MODEL_PLOTS_b)

# Observed data
par(mfrow=c(1,2))

model_2 <-bam(contextual_diversity_VERBS_ave ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Verb Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            ylim=c(270,300),xlog=FALSE,xaxp=c(10,70,6),yaxp=c(270,300,6),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')


# Model's data

vocab_df_GROWTH_V_MODEL_PLOTS$type_talker <- as.factor(vocab_df_GROWTH_V_MODEL_PLOTS$type_talker)

model_2_PPA_model_V <-bam(contextual_diversity_ave ~ type_talker +
                           s(vocab_size) ,
                         data=vocab_df_GROWTH_V_MODEL_PLOTS, method="ML")

plot_smooth(model_2_PPA_model_V, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PPA_model_V, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=1,lwd=3,se=0)


####### MODEL 3 : PROGRESSIVE LURE OF ASSOCIATES # PLA ####### 
## 1a ## Parameter search # Ranges of values #### 
# Parameter set values for grid search: 
boost_set=c(0.5)
beta_attention_set_1=seq(0,1,0.25)
beta_attention_set_2=seq(0,1,0.25)

tau_NOUNS_set= seq(10,50,10) 
tau_VERBS_set= seq(10,50,10) 

length(boost_set)*length(beta_attention_set_1)*length(beta_attention_set_2)*length(tau_NOUNS_set)*length(tau_VERBS_set) # Number of combinations

Nrep=2 # Number of repetitions

# Function to go parallel
PLA_function <- function(){
  
  TTerr_NOUNS_ALL_1 <- list() 
  LTerr_NOUNS_ALL_1 <- list()
  TTerr_VERBS_ALL_1 <- list() 
  LTerr_VERBS_ALL_1 <- list()
  
  TTerr_NOUNS_ALL_2 <- list() 
  LTerr_NOUNS_ALL_2 <- list()
  TTerr_VERBS_ALL_2 <- list() 
  LTerr_VERBS_ALL_2 <- list()
  
  TTerr_NOUNS_ALL_3 <- list() 
  LTerr_NOUNS_ALL_3 <- list()
  TTerr_VERBS_ALL_3 <- list() 
  LTerr_VERBS_ALL_3 <- list()
  
  TTerr_NOUNS_ALL_4 <- list() 
  LTerr_NOUNS_ALL_4 <- list()
  TTerr_VERBS_ALL_4 <- list() 
  LTerr_VERBS_ALL_4 <- list()
  
  
  for (V in 1:length(beta_attention_set_1)){
    beta_attention_1<- beta_attention_set_1[V]  
    TTerr_NOUNS <- vector() 
    LTerr_NOUNS <- vector()
    TTerr_VERBS <- vector() 
    LTerr_VERBS <- vector()
    
    for (H in 1:length(beta_attention_set_2)){
      beta_attention_2<- beta_attention_set_2[H]
      
      for (Y in 1:length(boost_set)){
        boost<- boost_set[Y]
        
        for (W in 1:length(tau_NOUNS_set)){
          tau_NOUNS<- tau_NOUNS_set[W]
          
          for(S in  1:length(tau_VERBS_set) ){
            tau_VERBS <- tau_VERBS_set[S]
            ## Create empty vectors/df
            word <- vector()
            words = vector()
            position_word <- 0
            p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                                  Class=Nouns_Verbs_df$Class,
                                  comprehension=0,
                                  contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
            
            while (nrow(p_wi_df)>381) {
              ## chose word by freq to fill comprehension vector
              num <- p_wi_df$contextual_diversity^beta_attention_1
              den <- sum(p_wi_df$contextual_diversity^beta_attention_1)
              prob <-  num/den
              names(prob) <- as.character(p_wi_df$Word)
              word <- sample(names(prob),1,prob = prob)
              b <- match(word,p_wi_df$Word)
              p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
              
              ## Pass the threshold?
              
              if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
                
                words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
                words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
                if(length(words_passed_t_N)>0 & length(words_passed_t_V)>0){
                  words_passed_t <- c(words_passed_t_V,words_passed_t_N)
                }
                if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
                  words_passed_t <- words_passed_t_N
                }
                if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
                  words_passed_t <- words_passed_t_V
                }
                
                
                for (u in 1:length(words_passed_t)) {
                  position_word <- position_word + 1
                  #word_prod <- sample(words_passed_t,1)
                  p_wi_df_R <- p_wi_df[match(words_passed_t,p_wi_df$Word),]
                  num <- p_wi_df_R$comprehension^1.4
                  den <- sum(p_wi_df_R$comprehension^1.4)
                  prob_2 <-  num/den
                  names(prob_2) <- as.character(p_wi_df_R$Word)
                  word_prod <- sample(names(prob_2),1,prob = prob_2)
                  
                  words_passed_t <- words_passed_t[-match(word_prod,words_passed_t)]
                  # Insert the word into the production vector
                  words[position_word] <- word_prod
                  #Eliminate learned word
                  p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
                  ## Change comprehension for those words that are connected the new learned word
                  new_cont_div <- CHILDES_matrix[match(word_prod,rownames(CHILDES_matrix)),]
                  new_cont_div[2,] <- CHILDES_matrix[,match(word_prod,colnames(CHILDES_matrix))]
                  new_cont_div <- colSums(new_cont_div)
                  new_cont_div <-names(new_cont_div)[new_cont_div>0]
                  
                  a <- match(new_cont_div,p_wi_df$Word)
                  a <- a[!is.na(a)]
                  
                  p_wi_df$comprehension[a] <- p_wi_df$comprehension[a] + boost
                  
                }
              }
            }
            while (nrow(p_wi_df)>0) {
              
              num <- p_wi_df$contextual_diversity^beta_attention_2
              den <- sum(p_wi_df$contextual_diversity^beta_attention_2)
              prob <-  num/den
              names(prob) <- as.character(p_wi_df$Word)
              word <- sample(names(prob),1,prob = prob)
              b <- match(word,p_wi_df$Word)
              p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost #
              
              ## Pass the threshold?
              
              if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
                
                words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
                words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
                if(length(words_passed_t_N)>0 & length(words_passed_t_V)>0){
                  words_passed_t <- c(words_passed_t_V,words_passed_t_N)
                }
                if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
                  words_passed_t <- words_passed_t_N
                }
                if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
                  words_passed_t <- words_passed_t_V
                }
                
                
                for (u in 1:length(words_passed_t)) {
                  position_word <- position_word + 1
                  p_wi_df_R <- p_wi_df[match(words_passed_t,p_wi_df$Word),]
                  num <- p_wi_df_R$comprehension^1.4
                  den <- sum(p_wi_df_R$comprehension^1.4)
                  prob_2 <-  num/den
                  names(prob_2) <- as.character(p_wi_df_R$Word)
                  word_prod <- sample(names(prob_2),1,prob = prob_2)
                  
                  words_passed_t <- words_passed_t[-match(word_prod,words_passed_t)]
                  # Insert the word into the production vector
                  words[position_word] <- word_prod
                  #Eliminate learned word
                  p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
                  ## Change comprehension for those words that are connected the new learned word
                  new_cont_div <- CHILDES_matrix[match(word_prod,rownames(CHILDES_matrix)),]
                  new_cont_div[2,] <- CHILDES_matrix[,match(word_prod,colnames(CHILDES_matrix))]
                  new_cont_div <- colSums(new_cont_div)
                  new_cont_div <-names(new_cont_div)[new_cont_div>0]
                  
                  a <- match(new_cont_div,p_wi_df$Word)
                  a <- a[!is.na(a)]
                  
                  p_wi_df$comprehension[a] <- p_wi_df$comprehension[a] + boost
                  
                }
                
              }
            }
            
            
            
            words_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
            words_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
            
            words_N <- Nouns_Verbs_df$contextual_diversity[match(words_N,Nouns_Verbs_df$Words_CDI)]
            
            vocab_GROWTH_N <- vector()
            for (e in 1:length(words_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
              vocab_GROWTH_N[e]  <- mean(words_N[1:e])
            }
            
            
            words_V <- Nouns_Verbs_df$contextual_diversity[match(words_V,Nouns_Verbs_df$Words_CDI)]
            vocab_GROWTH_V <- vector()
            for (e in 1:length(words_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
              vocab_GROWTH_V[e]  <- mean(words_V[1:e])
            }
            
            
            TTerr_NOUNS[S] <- mean((childbg_NOUNS_TT$contextual_diversity_NOUNS_ave
                                    -vocab_GROWTH_N[childbg_NOUNS_TT$count_produced_analysis_NOUNS])^2,na.rm=T) 
            LTerr_NOUNS[S] <- mean((childbg_NOUNS_LT$contextual_diversity_NOUNS_ave
                                    -vocab_GROWTH_N[childbg_NOUNS_LT$count_produced_analysis_NOUNS])^2,na.rm=T) 
            TTerr_VERBS[S] <- mean((childbg_VERBS_TT$contextual_diversity_VERBS_ave
                                    -vocab_GROWTH_V[childbg_VERBS_TT$count_produced_analysis_VERBS])^2,na.rm=T) 
            LTerr_VERBS[S] <- mean((childbg_VERBS_LT$contextual_diversity_VERBS_ave
                                    -vocab_GROWTH_V[childbg_VERBS_LT$count_produced_analysis_VERBS])^2,na.rm=T) 
          }
          TTerr_NOUNS_ALL_1[[W]] <- TTerr_NOUNS 
          LTerr_NOUNS_ALL_1[[W]] <-LTerr_NOUNS
          TTerr_VERBS_ALL_1[[W]] <- TTerr_VERBS
          LTerr_VERBS_ALL_1[[W]] <- LTerr_VERBS
        }
        
        TTerr_NOUNS_ALL_2[[Y]] <- TTerr_NOUNS_ALL_1 
        LTerr_NOUNS_ALL_2[[Y]] <-LTerr_NOUNS_ALL_1
        TTerr_VERBS_ALL_2[[Y]] <- TTerr_VERBS_ALL_1
        LTerr_VERBS_ALL_2[[Y]] <- LTerr_VERBS_ALL_1
        
      }
      
      TTerr_NOUNS_ALL_3[[H]] <- TTerr_NOUNS_ALL_2 
      LTerr_NOUNS_ALL_3[[H]] <-LTerr_NOUNS_ALL_2
      TTerr_VERBS_ALL_3[[H]] <- TTerr_VERBS_ALL_2
      LTerr_VERBS_ALL_3[[H]] <- LTerr_VERBS_ALL_2
      
    }
    
    TTerr_NOUNS_ALL_4[[V]] <- TTerr_NOUNS_ALL_3 
    LTerr_NOUNS_ALL_4[[V]] <-LTerr_NOUNS_ALL_3
    TTerr_VERBS_ALL_4[[V]] <- TTerr_VERBS_ALL_3
    LTerr_VERBS_ALL_4[[V]] <- LTerr_VERBS_ALL_3
    
  }
  
  
  err_NOUNS_VERBS <- list()
  err_NOUNS_VERBS[[1]] <- TTerr_NOUNS_ALL_4
  err_NOUNS_VERBS[[2]] <- LTerr_NOUNS_ALL_4
  err_NOUNS_VERBS[[3]] <- TTerr_VERBS_ALL_4
  err_NOUNS_VERBS[[4]] <- LTerr_VERBS_ALL_4
  err_NOUNS_VERBS
  
  
}



library(parallel)

no_cores <- detectCores() - 1

cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","PLA_function", "CHILDES_matrix","boost_set","childbg_VERBS_LT","childbg_VERBS_TT",
                    "childbg_NOUNS_LT","childbg_NOUNS_TT","tau_VERBS_set","tau_NOUNS_set","beta_attention_set_1","beta_attention_set_2"))
clusterEvalQ(cl,library(BBmisc))

set.seed(1313)
ptm <- proc.time()
PLA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  PLA_function()
})

saveRDS(VOCAB_function_RESULTS, "PPA_function_RESULTS_phase_1_TEST.rds")

# Adapt Results 

vocab_df_ERROR_N_TT <- data.frame(beta_attention_set_1=rep(beta_attention_set_1,times=c(rep(length(beta_attention_set_2)*length(boost_set)*length(tau_NOUNS_set)*length(tau_VERBS_set),
                                                                                            length(beta_attention_set_1)))),
                                  beta_attention_set_2=rep(rep(beta_attention_set_2,times=c(rep(length(boost_set)*length(tau_NOUNS_set)*length(tau_VERBS_set),
                                                                                                length(beta_attention_set_2)))),length(beta_attention_set_1)),
                                  boost_set=rep(rep(rep(boost_set,times=c(rep(length(tau_NOUNS_set)*length(tau_VERBS_set),
                                                                              length(boost_set)))),length(beta_attention_set_2)),length(beta_attention_set_1)),
                                  tau_NOUNS_set=rep(rep(rep(rep(tau_NOUNS_set,times=c(rep(length(tau_VERBS_set),
                                                                                          length(tau_NOUNS_set)))),length(boost_set)),length(beta_attention_set_2)),length(beta_attention_set_1)),
                                  
                                  tau_VERBS_set=rep(rep(rep(rep(rep(tau_VERBS_set,times=c(rep(1,length(tau_VERBS_set)))),length(tau_NOUNS_set)),length(boost_set)),length(beta_attention_set_2)),length(beta_attention_set_1)),
                                  MSE=NA)


vocab_df_ERROR_N_LT <- vocab_df_ERROR_N_TT

vocab_df_ERROR_V_TT <- vocab_df_ERROR_N_TT

vocab_df_ERROR_V_LT <- vocab_df_ERROR_N_TT


position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_N_TT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PLA_function_RESULTS, "[[",1), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}


position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_N_LT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PLA_function_RESULTS, "[[",2), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}

position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_V_TT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PLA_function_RESULTS, "[[",3), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}


position_w <- 1

for (z in 1:length(beta_attention_set_1)) {
  for (d in 1:length(beta_attention_set_2)) {
    for (x in 1:length(boost_set)) {
      for (k in 1:length(tau_NOUNS_set)) {
        for (y in 1:length(tau_VERBS_set)) {
          vocab_df_ERROR_V_LT$MSE[position_w] <-mean(unlist( lapply(lapply(lapply(lapply(lapply(lapply(PLA_function_RESULTS, "[[",4), "[[",z) ,"[[",d),"[[",x),"[[",k),"[[",y))) ### SIGUE AQUI
          position_w <- position_w +1
        } 
      }
    }
  }
}




## Average MSE of nouns and verbs
vocab_df_ERROR_LT <- vocab_df_ERROR_V_LT
vocab_df_ERROR_LT$MSE <- (vocab_df_ERROR_V_LT$MSE + vocab_df_ERROR_N_LT$MSE)/2

vocab_df_ERROR_TT <- vocab_df_ERROR_V_TT
vocab_df_ERROR_TT$MSE <- (vocab_df_ERROR_V_TT$MSE + vocab_df_ERROR_N_TT$MSE)/2

vocab_df_ERROR_LT_PPA_1 <- vocab_df_ERROR_LT
vocab_df_ERROR_TT_PPA_1 <- vocab_df_ERROR_TT

## Plot MSE per talker ###
plot(vocab_df_ERROR_LT$MSE)
plot(vocab_df_ERROR_TT$MSE)
View(vocab_df_ERROR_LT)
View(vocab_df_ERROR_TT)


## Best set of parameter values

View(vocab_df_ERROR_LT)
View(vocab_df_ERROR_TT)

## 1b ## Parameter search # Test specific set of values after testing ranges #### 

# Parameter set values for grid search from previous search

## Select x best parameter sets 
top_x <- 100
vocab_df_ERROR_LT <- vocab_df_ERROR_LT[order(vocab_df_ERROR_LT$MSE),]
vocab_df_ERROR_LT_R <- vocab_df_ERROR_LT[1:top_x,1:5]
vocab_df_ERROR_TT <- vocab_df_ERROR_TT[order(vocab_df_ERROR_TT$MSE),]
vocab_df_ERROR_TT_R <- vocab_df_ERROR_TT[1:top_x,1:5]

vocab_df_ERROR_R <- rbind(vocab_df_ERROR_LT_R,vocab_df_ERROR_TT_R)
dim(vocab_df_ERROR_R)
vocab_df_ERROR_R <- unique(vocab_df_ERROR_R)
dim(vocab_df_ERROR_R)

## fine-grained search taus:

vocab_df_ERROR_temp <- as.data.frame(matrix(nrow = nrow(vocab_df_ERROR_R)*(11*11), ncol = ncol(vocab_df_ERROR_LT))) ##  (+- 5)
colnames(vocab_df_ERROR_temp) <- colnames(vocab_df_ERROR_R)
dim(vocab_df_ERROR_temp)

vocab_df_ERROR_temp[,1] <- rep(as.matrix(vocab_df_ERROR_R$beta_attention_set_1),each=(11*11))
vocab_df_ERROR_temp[,2] <- rep(as.matrix(vocab_df_ERROR_R$beta_attention_set_2),each=(11*11))
vocab_df_ERROR_temp[,3] <- rep(as.matrix(vocab_df_ERROR_R$boost_set),each=(11*11))
i=1
i=2
block_start <- 1
block_end <- (11*11)
for (i in 1:top_x) {
  tau_NOUNS_set <- (vocab_df_ERROR_R$tau_NOUNS_set[i]-5):(vocab_df_ERROR_R$tau_NOUNS_set[i]+5)
  tau_VERBS_set <- (vocab_df_ERROR_R$tau_VERBS_set[i]-5):(vocab_df_ERROR_R$tau_VERBS_set[i]+5)
  vocab_df_ERROR_temp$tau_NOUNS_set[block_start:block_end] <-as.vector(unlist(expand.grid(tau_NOUNS_set,tau_VERBS_set)[1]))
  vocab_df_ERROR_temp$tau_VERBS_set[block_start:block_end] <-as.vector(unlist(expand.grid(tau_NOUNS_set,tau_VERBS_set)[2]))
  
  block_start <- block_start+(11*11)
  block_end <- block_start + ((11*11)-1)
}

dim(vocab_df_ERROR_temp)
vocab_df_ERROR_temp <- unique(vocab_df_ERROR_temp)
dim(vocab_df_ERROR_temp)
vocab_df_ERROR_R <- vocab_df_ERROR_temp

saveRDS(vocab_df_ERROR_R, "vocab_df_ERROR_R.rds")


# Function to go parallel
PLA_function_2 <- function(){
  
  
  TTerr_NOUNS <- vector() 
  LTerr_NOUNS <- vector()
  TTerr_VERBS <- vector() 
  LTerr_VERBS <- vector()
  for (V in 1:nrow(vocab_df_ERROR_R)){
    
    parameter_set <-as.numeric(vocab_df_ERROR_R[V,])
    beta_attention_1<-  parameter_set[1] 
    beta_attention_2<- parameter_set[2]
    boost<- parameter_set[3]
    tau_NOUNS<- parameter_set[4]
    tau_VERBS <- parameter_set[5]
    
    
    ## Create empty vectors/df
    word <- vector()
    words = vector()
    position_word <- 0
    p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                          Class=Nouns_Verbs_df$Class,
                          comprehension=0,
                          contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
    
    while (nrow(p_wi_df)>381) {
      ## chose word by freq to fill comprehension vector
      num <- p_wi_df$contextual_diversity^beta_attention_1
      den <- sum(p_wi_df$contextual_diversity^beta_attention_1)
      prob <-  num/den
      names(prob) <- as.character(p_wi_df$Word)
      word <- sample(names(prob),1,prob = prob)
      b <- match(word,p_wi_df$Word)
      p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
      
      ## Pass the threshold?
      
      if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
        
        words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
        words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
        
        if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
          words_passed_t <- words_passed_t_N
        }
        if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
          words_passed_t <- words_passed_t_V
        }
        
        
        position_word <- position_word + 1
        
        # Insert the word into the production vector
        words[position_word] <- words_passed_t
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==words_passed_t)),]
        
        
        
      }
    }
    while (nrow(p_wi_df)>0) {
      ## chose word by freq to fill comprehension vector
      
      num <- p_wi_df$contextual_diversity^beta_attention_2
      den <- sum(p_wi_df$contextual_diversity^beta_attention_2)
      prob <-  num/den
      names(prob) <- as.character(p_wi_df$Word)
      word <- sample(names(prob),1,prob = prob)
      b <- match(word,p_wi_df$Word)
      p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost # add x to selected word
      
      ## Pass the threshold?
      
      if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)>0 ){ # If YES, it will produce this word, if NO, back to the top
        
        words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS)])
        words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS)])
        
        if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
          words_passed_t <- words_passed_t_N
        }
        if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
          words_passed_t <- words_passed_t_V
        }
        
        
        position_word <- position_word + 1
        
        # Insert the word into the production vector
        words[position_word] <- words_passed_t
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==words_passed_t)),]
        
        
        
      }
      
    }
    ##### HERE YOU HAVE COMPLETED VOCABULARIES
    
    
    words_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
    words_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
    
    words_N <- Nouns_Verbs_df$contextual_diversity[match(words_N,Nouns_Verbs_df$Words_CDI)]
    
    vocab_GROWTH_N <- vector()
    for (e in 1:length(words_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
      vocab_GROWTH_N[e]  <- mean(words_N[1:e])
    }
    
    
    words_V <- Nouns_Verbs_df$contextual_diversity[match(words_V,Nouns_Verbs_df$Words_CDI)]
    vocab_GROWTH_V <- vector()
    for (e in 1:length(words_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
      vocab_GROWTH_V[e]  <- mean(words_V[1:e])
    }
    
    
    TTerr_NOUNS[V] <- mean((childbg_NOUNS_TT$contextual_diversity_NOUNS_ave
                            -vocab_GROWTH_N[childbg_NOUNS_TT$count_produced_analysis_NOUNS])^2,na.rm=T) 
    LTerr_NOUNS[V] <- mean((childbg_NOUNS_LT$contextual_diversity_NOUNS_ave
                            -vocab_GROWTH_N[childbg_NOUNS_LT$count_produced_analysis_NOUNS])^2,na.rm=T) 
    TTerr_VERBS[V] <- mean((childbg_VERBS_TT$contextual_diversity_VERBS_ave
                            -vocab_GROWTH_V[childbg_VERBS_TT$count_produced_analysis_VERBS])^2,na.rm=T) 
    LTerr_VERBS[V] <- mean((childbg_VERBS_LT$contextual_diversity_VERBS_ave
                            -vocab_GROWTH_V[childbg_VERBS_LT$count_produced_analysis_VERBS])^2,na.rm=T) 
  }
  
  
  err_NOUNS_VERBS <- list()
  err_NOUNS_VERBS[[1]] <- TTerr_NOUNS
  err_NOUNS_VERBS[[2]] <- LTerr_NOUNS
  err_NOUNS_VERBS[[3]] <- TTerr_VERBS
  err_NOUNS_VERBS[[4]] <- LTerr_VERBS
  err_NOUNS_VERBS
  
  
}

Nrep=500

library(parallel)

no_cores <- detectCores() - 1

cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","PLA_function_2","childbg_VERBS_LT","childbg_VERBS_TT","CHILDES_matrix",
                    "childbg_NOUNS_LT","childbg_NOUNS_TT","vocab_df_ERROR_R"))

clusterEvalQ(cl,library(BBmisc))

set.seed(1313)
ptm <- proc.time()
VOCAB_PLA_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  PLA_function_2()
})

proc.time() - ptm
stopCluster(cl)

saveRDS(VOCAB_PLA_RESULTS, "VOCAB_PLA_RESULTS_1.rds")

# Adapt results 
vocab_df_ERROR_N_LT <- data.frame(beta_attention_set_1=vocab_df_ERROR_LT_R$beta_attention_set_1,
                                  beta_attention_set_2=vocab_df_ERROR_LT_R$beta_attention_set_2,
                                  boost_set=vocab_df_ERROR_LT_R$boost_set,
                                  tau_NOUNS_set=vocab_df_ERROR_LT_R$tau_NOUNS_set,
                                  tau_VERBS_set=vocab_df_ERROR_LT_R$tau_VERBS_set,
                                  MSE=NA)


vocab_df_ERROR_V_LT <- vocab_df_ERROR_N_LT
vocab_df_ERROR_N_TT <- vocab_df_ERROR_N_LT
vocab_df_ERROR_V_TT <- vocab_df_ERROR_N_LT

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_N_LT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PLA_RESULTS, "[[",2), "[[",z) ))
} 

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_V_LT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PLA_RESULTS, "[[",4), "[[",z) )) 
} 

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_N_TT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PLA_RESULTS, "[[",1), "[[",z) )) 
} 

for (z in 1:nrow(vocab_df_ERROR_R)) {
  vocab_df_ERROR_V_TT$MSE[z] <-mean(unlist( lapply(lapply(VOCAB_PLA_RESULTS, "[[",3), "[[",z) )) 
} 





## Average MSE of nouns and verbs
vocab_df_ERROR_LT <- vocab_df_ERROR_V_LT
vocab_df_ERROR_LT$MSE <- (vocab_df_ERROR_V_LT$MSE + vocab_df_ERROR_N_LT$MSE)/2

vocab_df_ERROR_TT <- vocab_df_ERROR_V_TT
vocab_df_ERROR_TT$MSE <- (vocab_df_ERROR_V_TT$MSE + vocab_df_ERROR_N_TT$MSE)/2



## Plot MSE per talker 
plot(vocab_df_ERROR_LT$MSE)
plot(vocab_df_ERROR_TT$MSE)

View(vocab_df_ERROR_TT)
View(vocab_df_ERROR_LT)



## 2 ## Create model's networks ####
Nrep=500
library(itsadug)

Nrep=500

boost_LT=1
tau_NOUNS_LT=10
tau_VERBS_LT=14
beta_attention_LT=0.6
beta_attention_LT_2=0.2


boost_TT=1
tau_NOUNS_TT=11
tau_VERBS_TT=15
beta_attention_TT=0.6
beta_attention_TT_2=0.2

## Late Talker ####


set.seed(1313)

LT_PLA_function <- function(){
  
  ## Create empty vectors/df
  word <- vector()
  words = vector()
  position_word <- 0
  p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                        Class=Nouns_Verbs_df$Class,
                        comprehension=0,
                        contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
  
  while (nrow(p_wi_df)>381) {
    ## chose word by freq to fill comprehension vector
    num <- p_wi_df$contextual_diversity^beta_attention_LT
    den <- sum(p_wi_df$contextual_diversity^beta_attention_LT)
    prob <-  num/den
    names(prob) <- as.character(p_wi_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_df$Word)
    p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost_LT # add x to selected word
    
    ## Pass the threshold?
    
    if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_LT)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_LT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_LT)])
      words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_LT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)>0){
        words_passed_t <- c(words_passed_t_V,words_passed_t_N)
      }
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      
      for (u in 1:length(words_passed_t)) {
        position_word <- position_word + 1
        #word_prod <- sample(words_passed_t,1)
        p_wi_df_R <- p_wi_df[match(words_passed_t,p_wi_df$Word),]
        num <- p_wi_df_R$comprehension^1 # if more than one word reaches the threshold at the same time 
        den <- sum(p_wi_df_R$comprehension^1)
        prob_2 <-  num/den
        names(prob_2) <- as.character(p_wi_df_R$Word)
        word_prod <- sample(names(prob_2),1,prob = prob_2)
        
        words_passed_t <- words_passed_t[-match(word_prod,words_passed_t)]
        # Insert the word into the production vector
        words[position_word] <- word_prod
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
        ## Change comprehension for those words that are connected the new learned word
        new_cont_div <- CHILDES_matrix[match(word_prod,rownames(CHILDES_matrix)),]
        new_cont_div[2,] <- CHILDES_matrix[,match(word_prod,colnames(CHILDES_matrix))]
        new_cont_div <- colSums(new_cont_div)
        new_cont_div <-names(new_cont_div)[new_cont_div>0]
        
        a <- match(new_cont_div,p_wi_df$Word)
        a <- a[!is.na(a)]
        
        p_wi_df$comprehension[a] <- p_wi_df$comprehension[a] + boost_LT
        
      }
    }
  }
  while (nrow(p_wi_df)>0) {
    
    num <- p_wi_df$contextual_diversity^beta_attention_LT_2
    den <- sum(p_wi_df$contextual_diversity^beta_attention_LT_2)
    prob <-  num/den
    names(prob) <- as.character(p_wi_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_df$Word)
    p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost_LT #
    
    ## Pass the threshold?
    
    if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_LT)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_LT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_LT)])
      words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_LT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)>0){
        words_passed_t <- c(words_passed_t_V,words_passed_t_N)
      }
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      
      for (u in 1:length(words_passed_t)) {
        position_word <- position_word + 1
        p_wi_df_R <- p_wi_df[match(words_passed_t,p_wi_df$Word),]
        num <- p_wi_df_R$comprehension^1
        den <- sum(p_wi_df_R$comprehension^1)
        prob_2 <-  num/den
        names(prob_2) <- as.character(p_wi_df_R$Word)
        word_prod <- sample(names(prob_2),1,prob = prob_2)
        
        words_passed_t <- words_passed_t[-match(word_prod,words_passed_t)]
        # Insert the word into the production vector
        words[position_word] <- word_prod
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
        ## Change comprehension for those words that are connected the new learned word
        new_cont_div <- CHILDES_matrix[match(word_prod,rownames(CHILDES_matrix)),]
        new_cont_div[2,] <- CHILDES_matrix[,match(word_prod,colnames(CHILDES_matrix))]
        new_cont_div <- colSums(new_cont_div)
        new_cont_div <-names(new_cont_div)[new_cont_div>0]
        
        a <- match(new_cont_div,p_wi_df$Word)
        a <- a[!is.na(a)]
        
        p_wi_df$comprehension[a] <- p_wi_df$comprehension[a] + boost_LT
        
      }
      
    }
  }
  
  
  words_LT_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
  words_LT_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
  
  words_LT_N <- Nouns_Verbs_df$contextual_diversity[match(words_LT_N,Nouns_Verbs_df$Words_CDI)]
  
  LT_vocab_GROWTH_N <- vector()
  for (e in 1:length(words_LT_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
    LT_vocab_GROWTH_N[e]  <- mean(words_LT_N[1:e])
  }
  
  
  words_LT_V <- Nouns_Verbs_df$contextual_diversity[match(words_LT_V,Nouns_Verbs_df$Words_CDI)]
  LT_vocab_GROWTH_V <- vector()
  for (e in 1:length(words_LT_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
    LT_vocab_GROWTH_V[e]  <- mean(words_LT_V[1:e])
    
  }
  LT_vocab_GROWTH_N_V <- list()
  LT_vocab_GROWTH_N_V[[1]] <- LT_vocab_GROWTH_N
  LT_vocab_GROWTH_N_V[[2]] <- LT_vocab_GROWTH_V
  
  LT_vocab_GROWTH_N_V
}


library(parallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","LT_PLA_function", "CHILDES_matrix","tau_NOUNS_LT","tau_VERBS_LT","boost_LT","beta_attention_LT", "beta_attention_LT_2"))
clusterEvalQ(cl,library(BBmisc))


ptm <- proc.time()
LT_PLA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  LT_PLA_function()
})

proc.time() - ptm
stopCluster(cl)

# Adapt Results 

LT_vocab_df_GROWTH_N <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]), Nrep))
LT_vocab_df_GROWTH_V <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]), Nrep))


for (k in 1:Nrep) {
  LT_vocab_df_GROWTH_N[,k] <-unlist(lapply(LT_PLA_function_RESULTS, "[[",1)[k])
  
}

for (k in 1:Nrep) {
  LT_vocab_df_GROWTH_V[,k] <-unlist(lapply(LT_PLA_function_RESULTS, "[[",2)[k])
  
}


LT_vocab_df_GROWTH_N$contextual_diversity_ave <- NA

for (i in 1:nrow(LT_vocab_df_GROWTH_N)) {
  LT_vocab_df_GROWTH_N$contextual_diversity_ave[i] <- mean(as.numeric(LT_vocab_df_GROWTH_N[i,1:(ncol(LT_vocab_df_GROWTH_N)-1)]))
}

LT_vocab_df_GROWTH_V$contextual_diversity_ave <- NA

for (i in 1:nrow(LT_vocab_df_GROWTH_V)) {
  LT_vocab_df_GROWTH_V$contextual_diversity_ave[i] <- mean(as.numeric(LT_vocab_df_GROWTH_V[i,1:(ncol(LT_vocab_df_GROWTH_V)-1)]))
}


## Typical Talker ####

set.seed(1313)

TT_PLA_function <- function(){
  
  ## Create empty vectors/df
  word <- vector()
  words = vector()
  position_word <- 0
  p_wi_df <- data.frame(Word =Nouns_Verbs_df$Words_CDI, 
                        Class=Nouns_Verbs_df$Class,
                        comprehension=0,
                        contextual_diversity=Nouns_Verbs_df$contextual_diversity)   
  
  while (nrow(p_wi_df)>381) {
    ## chose word by freq to fill comprehension vector
    num <- p_wi_df$contextual_diversity^beta_attention_TT
    den <- sum(p_wi_df$contextual_diversity^beta_attention_TT)
    prob <-  num/den
    names(prob) <- as.character(p_wi_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_df$Word)
    p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost_TT # add x to selected word
    
    ## Pass the threshold?
    
    if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_TT)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_TT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_TT)])
      words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_TT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)>0){
        words_passed_t <- c(words_passed_t_V,words_passed_t_N)
      }
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      
      for (u in 1:length(words_passed_t)) {
        position_word <- position_word + 1
        #word_prod <- sample(words_passed_t,1)
        p_wi_df_R <- p_wi_df[match(words_passed_t,p_wi_df$Word),]
        num <- p_wi_df_R$comprehension^1 # if more than one word reaches the threshold at the same time 
        den <- sum(p_wi_df_R$comprehension^1)
        prob_2 <-  num/den
        names(prob_2) <- as.character(p_wi_df_R$Word)
        word_prod <- sample(names(prob_2),1,prob = prob_2)
        
        words_passed_t <- words_passed_t[-match(word_prod,words_passed_t)]
        # Insert the word into the production vector
        words[position_word] <- word_prod
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
        ## Change comprehension for those words that are connected the new learned word
        new_cont_div <- CHILDES_matrix[match(word_prod,rownames(CHILDES_matrix)),]
        new_cont_div[2,] <- CHILDES_matrix[,match(word_prod,colnames(CHILDES_matrix))]
        new_cont_div <- colSums(new_cont_div)
        new_cont_div <-names(new_cont_div)[new_cont_div>0]
        
        a <- match(new_cont_div,p_wi_df$Word)
        a <- a[!is.na(a)]
        
        p_wi_df$comprehension[a] <- p_wi_df$comprehension[a] + boost_TT
        
      }
    }
  }
  while (nrow(p_wi_df)>0) {
    
    num <- p_wi_df$contextual_diversity^beta_attention_TT_2
    den <- sum(p_wi_df$contextual_diversity^beta_attention_TT_2)
    prob <-  num/den
    names(prob) <- as.character(p_wi_df$Word)
    word <- sample(names(prob),1,prob = prob)
    b <- match(word,p_wi_df$Word)
    p_wi_df$comprehension[b] <- p_wi_df$comprehension[b] + boost_TT #
    
    ## Pass the threshold?
    
    if(sum(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_TT)>0 |sum(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_TT)>0 ){ # If YES, it will produce this word, if NO, back to the top
      
      words_passed_t_N <- as.character(p_wi_df$Word[p_wi_df$Class=="Noun"][which(p_wi_df$comprehension[p_wi_df$Class=="Noun"]>tau_NOUNS_TT)])
      words_passed_t_V <- as.character(p_wi_df$Word[p_wi_df$Class=="Verb"][which(p_wi_df$comprehension[p_wi_df$Class=="Verb"]>tau_VERBS_TT)])
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)>0){
        words_passed_t <- c(words_passed_t_V,words_passed_t_N)
      }
      if(length(words_passed_t_N)>0 & length(words_passed_t_V)==0){
        words_passed_t <- words_passed_t_N
      }
      if(length(words_passed_t_N)==0 & length(words_passed_t_V)>0){
        words_passed_t <- words_passed_t_V
      }
      
      
      for (u in 1:length(words_passed_t)) {
        position_word <- position_word + 1
        p_wi_df_R <- p_wi_df[match(words_passed_t,p_wi_df$Word),]
        num <- p_wi_df_R$comprehension^1
        den <- sum(p_wi_df_R$comprehension^1)
        prob_2 <-  num/den
        names(prob_2) <- as.character(p_wi_df_R$Word)
        word_prod <- sample(names(prob_2),1,prob = prob_2)
        
        words_passed_t <- words_passed_t[-match(word_prod,words_passed_t)]
        # Insert the word into the production vector
        words[position_word] <- word_prod
        #Eliminate learned word
        p_wi_df <- p_wi_df[-(which(p_wi_df$Word==word_prod)),]
        ## Change comprehension for those words that are connected the new learned word
        new_cont_div <- CHILDES_matrix[match(word_prod,rownames(CHILDES_matrix)),]
        new_cont_div[2,] <- CHILDES_matrix[,match(word_prod,colnames(CHILDES_matrix))]
        new_cont_div <- colSums(new_cont_div)
        new_cont_div <-names(new_cont_div)[new_cont_div>0]
        
        a <- match(new_cont_div,p_wi_df$Word)
        a <- a[!is.na(a)]
        
        p_wi_df$comprehension[a] <- p_wi_df$comprehension[a] + boost_TT
        
      }
      
    }
  }
  
  
  words_TT_N <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"])))]
  words_TT_V <- words[which(!is.na( match(words,Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"])))]
  
  words_TT_N <- Nouns_Verbs_df$contextual_diversity[match(words_TT_N,Nouns_Verbs_df$Words_CDI)]
  
  TT_vocab_GROWTH_N <- vector()
  for (e in 1:length(words_TT_N)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : NOUNS
    TT_vocab_GROWTH_N[e]  <- mean(words_TT_N[1:e])
  }
  
  
  words_TT_V <- Nouns_Verbs_df$contextual_diversity[match(words_TT_V,Nouns_Verbs_df$Words_CDI)]
  TT_vocab_GROWTH_V <- vector()
  for (e in 1:length(words_TT_V)) { ## COMPUTES AVERAGE OF CONTEXTUAL DIVERSITY AS THE VOCAB GROWS : VERBS
    TT_vocab_GROWTH_V[e]  <- mean(words_TT_V[1:e])
    
  }
  TT_vocab_GROWTH_N_V <- list()
  TT_vocab_GROWTH_N_V[[1]] <- TT_vocab_GROWTH_N
  TT_vocab_GROWTH_N_V[[2]] <- TT_vocab_GROWTH_V
  
  TT_vocab_GROWTH_N_V
}


library(parallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
clusterExport(cl, c("Nouns_Verbs_df","TT_PLA_function", "CHILDES_matrix","tau_NOUNS_TT","tau_VERBS_TT","boost_TT","beta_attention_TT", "beta_attention_TT_2"))
clusterEvalQ(cl,library(BBmisc))


ptm <- proc.time()
TT_PLA_function_RESULTS <- parLapply(cl,1:Nrep,  function (m) {
  TT_PLA_function()
})

proc.time() - ptm
stopCluster(cl)

# Adapt RESULTS 

TT_vocab_df_GROWTH_N <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Noun"]), Nrep))
TT_vocab_df_GROWTH_V <- data.frame(matrix(vector(), length(Nouns_Verbs_df$Words_CDI[Nouns_Verbs_df$Class=="Verb"]), Nrep))


for (k in 1:Nrep) {
  TT_vocab_df_GROWTH_N[,k] <-unlist(lapply(TT_PLA_function_RESULTS, "[[",1)[k])
  
}

for (k in 1:Nrep) {
  TT_vocab_df_GROWTH_V[,k] <-unlist(lapply(TT_PLA_function_RESULTS, "[[",2)[k])
  
}


TT_vocab_df_GROWTH_N$contextual_diversity_ave <- NA

for (i in 1:nrow(TT_vocab_df_GROWTH_N)) {
  TT_vocab_df_GROWTH_N$contextual_diversity_ave[i] <- mean(as.numeric(TT_vocab_df_GROWTH_N[i,1:(ncol(TT_vocab_df_GROWTH_N)-1)]))
}

TT_vocab_df_GROWTH_V$contextual_diversity_ave <- NA

for (i in 1:nrow(TT_vocab_df_GROWTH_V)) {
  TT_vocab_df_GROWTH_V$contextual_diversity_ave[i] <- mean(as.numeric(TT_vocab_df_GROWTH_V[i,1:(ncol(TT_vocab_df_GROWTH_V)-1)]))
}



## Save/load simulations ####

#TT_vocab_df_GROWTH_N_PLA <- TT_vocab_df_GROWTH_N
#LT_vocab_df_GROWTH_N_PLA <- LT_vocab_df_GROWTH_N
#TT_vocab_df_GROWTH_V_PLA <- TT_vocab_df_GROWTH_V
#LT_vocab_df_GROWTH_V_PLA <- LT_vocab_df_GROWTH_V

#save(TT_vocab_df_GROWTH_N, file = "TT_vocab_df_GROWTH_N_PLA.RData")
#save(LT_vocab_df_GROWTH_N, file = "LT_vocab_df_GROWTH_N_PLA.RData")
#save(TT_vocab_df_GROWTH_V, file = "TT_vocab_df_GROWTH_V_PLA.RData")
#save(LT_vocab_df_GROWTH_V, file = "LT_vocab_df_GROWTH_V_PLA.RData")

load("TT_vocab_df_GROWTH_N_PLA.RData")
load("LT_vocab_df_GROWTH_N_PLA.RData")
load("TT_vocab_df_GROWTH_V_PLA.RData")
load("LT_vocab_df_GROWTH_V_PLA.RData")

TT_vocab_df_GROWTH_N <-  TT_vocab_df_GROWTH_N_PLA
LT_vocab_df_GROWTH_N <-  LT_vocab_df_GROWTH_N_PLA
TT_vocab_df_GROWTH_V <-  TT_vocab_df_GROWTH_V_PLA
LT_vocab_df_GROWTH_V <-  LT_vocab_df_GROWTH_V_PLA

## 3 ## Plot nouns networks ####
library(itsadug)

# TT
TT <- TT_vocab_df_GROWTH_N[,1:Nrep]
TT =as.vector( unlist(c(TT)))

vocab_df_GROWTH_N_MODEL_PLOTS <- data.frame(matrix(TT,length(TT),1))
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size <- rep(seq(1:nrow(TT_vocab_df_GROWTH_N)),Nrep)
head(vocab_df_GROWTH_N_MODEL_PLOTS)
colnames(vocab_df_GROWTH_N_MODEL_PLOTS)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10 <- cut(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size,  breaks=seq(9,190,10))
library(plyr)
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10 <- revalue(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10, c("(179,189]"="(179,190]"))
vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10[vocab_df_GROWTH_N_MODEL_PLOTS$vocab_size==190] <- "(179,190]"
vocab_df_GROWTH_N_MODEL_PLOTS <- vocab_df_GROWTH_N_MODEL_PLOTS[!is.na(vocab_df_GROWTH_N_MODEL_PLOTS$vocab_bins_10),]
vocab_df_GROWTH_N_MODEL_PLOTS$type_talker <- "Typical talker MODEL"

## LT
LT <- LT_vocab_df_GROWTH_N[,1:Nrep]
LT =as.vector( unlist(c(LT)))

vocab_df_GROWTH_N_MODEL_PLOTS_b <- data.frame(matrix(LT,length(LT),1))
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size <- rep(seq(1:nrow(LT_vocab_df_GROWTH_N)),Nrep)
head(vocab_df_GROWTH_N_MODEL_PLOTS_b)
colnames(vocab_df_GROWTH_N_MODEL_PLOTS_b)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10 <- cut(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size,  breaks=seq(9,190,10))
library(plyr)
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10 <- revalue(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10, c("(179,189]"="(179,190]"))
vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10[vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_size==190] <- "(179,190]"
vocab_df_GROWTH_N_MODEL_PLOTS_b <- vocab_df_GROWTH_N_MODEL_PLOTS_b[!is.na(vocab_df_GROWTH_N_MODEL_PLOTS_b$vocab_bins_10),]
vocab_df_GROWTH_N_MODEL_PLOTS_b$type_talker <- "Late talker MODEL"

vocab_df_GROWTH_N_MODEL_PLOTS <- rbind(vocab_df_GROWTH_N_MODEL_PLOTS,vocab_df_GROWTH_N_MODEL_PLOTS_b)

# Observed data
par(mfrow=c(1,2))

model_2 <-bam(contextual_diversity_NOUNS_ave ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,270,9),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=TRUE, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')


# Model's data

vocab_df_GROWTH_N_MODEL_PLOTS$type_talker <- as.factor(vocab_df_GROWTH_N_MODEL_PLOTS$type_talker)

model_2_PLA_model <-bam(contextual_diversity_ave ~ type_talker +
                         s(vocab_size) ,
                       data=vocab_df_GROWTH_N_MODEL_PLOTS, method="ML")

plot_smooth(model_2_PLA_model, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="black",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PLA_model, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="black",
            add=TRUE,lty=1,lwd=3,se=0)


## 4 ## Plot verbs networks ####
library(itsadug)

# TT
TT <- TT_vocab_df_GROWTH_V[,1:Nrep]
TT =as.vector( unlist(c(TT)))


vocab_df_GROWTH_V_MODEL_PLOTS <- data.frame(matrix(TT,length(TT),1))
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size <- rep(seq(1:nrow(TT_vocab_df_GROWTH_V)),Nrep)
head(vocab_df_GROWTH_V_MODEL_PLOTS)
colnames(vocab_df_GROWTH_V_MODEL_PLOTS)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10 <- cut(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size,  breaks=seq(9,70,10))
library(plyr)
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10 <- revalue(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10, c("(59,69]"="(59,70]"))
vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10[vocab_df_GROWTH_V_MODEL_PLOTS$vocab_size==70] <- "(59,70]"
vocab_df_GROWTH_V_MODEL_PLOTS <- vocab_df_GROWTH_V_MODEL_PLOTS[!is.na(vocab_df_GROWTH_V_MODEL_PLOTS$vocab_bins_10),]
vocab_df_GROWTH_V_MODEL_PLOTS$type_talker <- "Typical talker MODEL"


## LT

LT <- LT_vocab_df_GROWTH_V[,1:Nrep]
LT =as.vector( unlist(c(LT)))

vocab_df_GROWTH_V_MODEL_PLOTS_b <- data.frame(matrix(LT,length(LT),1))
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size <- rep(seq(1:nrow(LT_vocab_df_GROWTH_V)),Nrep)
head(vocab_df_GROWTH_V_MODEL_PLOTS_b)
colnames(vocab_df_GROWTH_V_MODEL_PLOTS_b)[1] <-"contextual_diversity_ave"
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10 <- cut(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size,  breaks=seq(9,70,10))
library(plyr)
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10 <- revalue(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10, c("(59,69]"="(59,70]"))
vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10[vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_size==70] <- "(59,70]"
vocab_df_GROWTH_V_MODEL_PLOTS_b <- vocab_df_GROWTH_V_MODEL_PLOTS_b[!is.na(vocab_df_GROWTH_V_MODEL_PLOTS_b$vocab_bins_10),]
vocab_df_GROWTH_V_MODEL_PLOTS_b$type_talker <- "Late talker MODEL"


vocab_df_GROWTH_V_MODEL_PLOTS <- rbind(vocab_df_GROWTH_V_MODEL_PLOTS,vocab_df_GROWTH_V_MODEL_PLOTS_b)

# Observed data
par(mfrow=c(1,2))

model_2 <-bam(contextual_diversity_VERBS_ave ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="grey",
            main = "PANEL A \n Average Contextual Diversity in Noun Vocabularies", xlab =" Vocabulary Size", ylab =  "Average Contextual Diversity",
            ylim=c(270,300),xlog=FALSE,xaxp=c(10,70,6),yaxp=c(270,300,6),hide.labe=TRUE,lwd=3,lty=2, cex.lab=1.5,cex.main= 1.5) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_2, view="count_produced_analysis_VERBS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="grey",
            add=TRUE,lwd=3,lty=1)
legend('bottomleft', 
       legend=c("Late talker","Typical talker"),
       col=c("grey", "grey"), lwd=3,lty=c(2,1), bty='n')


# Model's data

vocab_df_GROWTH_V_MODEL_PLOTS$type_talker <- as.factor(vocab_df_GROWTH_V_MODEL_PLOTS$type_talker)

model_2_PLA_model_V <-bam(contextual_diversity_ave ~ type_talker +
                           s(vocab_size) ,
                         data=vocab_df_GROWTH_V_MODEL_PLOTS, method="ML")

plot_smooth(model_2_PLA_model_V, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PLA_model_V, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,lty=1,lwd=3,se=0)


### PLOT ALL MODELS #########

model_N <-bam(contextual_diversity_NOUNS_ave ~ type_talker +
                s(count_produced_analysis_NOUNS) ,
              data=childbg_NOUNS, method="ML")

model_V <-bam(contextual_diversity_VERBS_ave ~ type_talker +
                s(count_produced_analysis_VERBS) ,
              data=childbg_VERBS, method="ML")
##### Nouns 

setWidth = (130*0.039370 )
pdf(file='Study_3_PA_PPA_PLA.pdf',width=setWidth,height=9, fonts = "sans")
par(mfrow=c(2,1),ps = 10,cex.lab = 1,cex.axis = 0.9)



plot_smooth(model_N, view="count_produced_analysis_NOUNS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="black",
             xlab =" Noun Vocabulary Size", ylab =  "Average Contextual Diversity",
            xlim=c(10,190),xlog=FALSE,xaxp=c(10,190,18),yaxp=c(180,280,10),hide.labe=TRUE,se=0,lwd=3,lty=2) ## QUITA se=0 para que salga los confident intervals
plot_smooth(model_N, view="count_produced_analysis_NOUNS", cond=list(type_talker="typical_talker"), rm.ranef=TRUE, rug=FALSE, col="black",
            add=TRUE,se=0,lwd=3,lty=1)
legend('topright', 
       legend=c("Late talkers","Typical talkers","Observed data", "PA model","PPA model","PLA model"),cex=0.9,
       col=c("grey", "grey","black","blue","orange","yellow"), lwd=3,lty=c(2,1,1,1,1,1), bty='n')

### PA

plot_smooth(model_2_PA_model, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="blue",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PA_model, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="blue",
            add=TRUE,lty=1,lwd=3,se=0)


### PPA

plot_smooth(model_2_PPA_model, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="orange",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PPA_model, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="orange",
            add=TRUE,lty=1,lwd=3,se=0)

### PPA

plot_smooth(model_2_PLA_model, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="yellow",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PLA_model, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=TRUE, rug=FALSE, col="yellow",
            add=TRUE,lty=1,lwd=3,se=0)

## 2 ## Verbs #####


plot_smooth(model_V, view="count_produced_analysis_VERBS", cond=list(type_talker="late_talker"), rm.ranef=F, rug=FALSE, col="black",
            xlab =" Verb Vocabulary Size", ylab =  "Average Contextual Diversity",
            ylim=c(270,300),xlog=FALSE,xaxp=c(10,70,6),yaxp=c(270,300,6),hide.labe=TRUE,lwd=3,lty=2,se=0) 
plot_smooth(model_V, view="count_produced_analysis_VERBS", cond=list(type_talker="typical_talker"), rm.ranef=F, rug=FALSE, col="black",
            add=TRUE,se=0,lwd=3,lty=1)


### PA

plot_smooth(model_2_PA_model_V, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="blue",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PA_model_V, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="blue",
            add=TRUE,lty=1,lwd=3,se=0)


### PPA

plot_smooth(model_2_PPA_model_V, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="orange",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PPA_model_V, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="orange",
            add=TRUE,lty=1,lwd=3,se=0)

### PLA

plot_smooth(model_2_PLA_model_V, view="vocab_size", cond=list(type_talker="Late talker MODEL"), rm.ranef=F, rug=FALSE, col="yellow",
            add=TRUE,lty=2,lwd=3,se=0)
plot_smooth(model_2_PLA_model_V, view="vocab_size", cond=list(type_talker="Typical talker MODEL"), rm.ranef=F, rug=FALSE, col="yellow",
            add=TRUE,lty=1,lwd=3,se=0)

dev.off()



