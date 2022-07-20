# set working directory
setwd("C:/Users/ntina/Desktop")

# load xml library
library(xml2)

# import xml file for the speech corpus
speech_Corpus <- read_xml("C:/Users/ntina/Desktop/Cclusters/finalspeechcorpus-consonant_cl.xml")

# extract tags annotated words using xpath
speech_tags <- xml_find_all(speech_Corpus, "//segment")
words <- trimws(xml_text(speech_tags))


# load tidyverse for data wrangling
library(tidyverse)

# extract only the features
speech_features <- xml_find_all(speech_Corpus, "//segment") %>% xml_attr("features")

# create a data frame for the speech corpus
speech_df <- data.frame(speech_features, speech_words)
head(speech_df, n = 50)

# rename column of attributes
names(speech_df)[1] <- "consonant_clusters"

# rename attributes
speech_df$consonant_clusters[speech_df$consonant_clusters == "consonant_cl;not_learned"] <- "not_learned"
speech_df$consonant_clusters[speech_df$consonant_clusters == "consonant_cl;learned"] <- "learned"


# convert features column to a factor with two levels
speech_df$consonant_clusters <- as.factor(speech_df$consonant_clusters)
print(levels(speech_df$consonant_clusters))

# correct a mistake
speech_df$speech_words[255] <- "επισκεφθούμε"

# turn all characters to lowercase
speech_df$words=tolower(speech_df$words)

# create a ggplot to show the distribution of the two types of consonant clusters
ggplot(speech_df) +
  aes(x = consonant_clusters, fill = consonant_clusters) +
  geom_bar() +
  ggtitle("Speech corpus") + theme_minimal()

# turn all characters to lowercase
speech_df$speech_words=tolower(speech_df$speech_words)

# assign to a variable a vector of the xml paths that we are going to import and process for the written corpus
xml_paths <- c("C:/Users/ntina/Desktop/Cclusters/Populismus_Data_01-consonant_cl.xml", "C:/Users/ntina/Desktop/Cclusters/written_corpus(economic)-consonant_cl.xml", 
               "C:/Users/ntina/Desktop/Cclusters/written_tanea-consonant_cl.xml", "C:/Users/ntina/Desktop/Cclusters/written_history-consonant_cl.xml")


# create a function that opens and processes multiple xml files
process_xml <- function(xml_paths) {
  rdxml <- read_xml(xml_paths)
  extr_seg <- xml_find_all(rdxml, "//segment") 
  vls <- trimws(xml_text(extr_seg))
  extr_feats <- xml_find_all(rdxml, "//segment") %>% xml_attr("features")
               df<- data.frame(features= extr_feats, words = vls)
}


# apply this function to each element of the vector we have created for the xml files of the written corpus
dflist <- lapply(xml_paths, function(x){process_xml(x)})


# the lapply returns a list of the results of the function (dataframes) and the do.call combines them all into one df
written_df <- do.call(rbind, dflist)

# rename column
names(written_df)[1] <- "consonant_clusters"

# change names
written_df$consonant_clusters[written_df$consonant_clusters == "consonant_cl;not_learned"] <- "not_learned"
written_df$consonant_clusters[written_df$consonant_clusters == "consonant_cl;learned"] <- "learned"

# correct the name of a value
written_df$consonant_clusters[40] <-"learned"



#turn all characters to lowercase
written_df$words=tolower(written_df$words)

ggplot(written_df) +
  aes(x = consonant_clusters, fill = consonant_clusters) +
  geom_bar() +
  ggtitle("Written corpus")

# create a dataframe of the frequency of each word
freq_speech_df <- as.data.frame(table(speech_df$speech_words))
names(freq_speech_df)[1] <- "words"

#plot words whose occurences exceed 9 times in the speech corpus
plot(droplevels(freq_speech_df[freq_speech_df$Freq > 9, ]), main = "Most frequent words in Speech Corpus") 

# df with most frequent words in speech corpus
mostfreq_sdf <- freq_speech_df %>% filter(Freq >9)
frq_plot_speech <-ggplot(mostfreq_sdf, aes(x = words, y = (Freq/nrow(speech_df))*100)) + ylim(0,20) +geom_bar(stat= "identity", fill = c("#9f79ee", "#ab82ff", "#9370db", "#c71585", "#7a378b","#7b68ee","#9370db",  "#d15fee", "#ba55d3")) + ggtitle("Most frequent words in Speech Corpus in %")


# create a df of the frequency of words in written corpus
freq_written_df <- as.data.frame(table(written_df$words))
names(freq_written_df)[1] <- "words"

#plot words whose occurences exceed 10 times in written corpus
plot(droplevels(freq_written_df[freq_written_df$Freq > 10, ]), main = "Most frequent words in Written Corpus") 

mostfreq_wdf <- freq_written_df %>% filter(Freq >9)
frq_plot_written <-ggplot(mostfreq_wdf, aes(x = words, y = (Freq/nrow(written_df))*100)) + ylim(0,20)+ geom_bar(stat = "identity", fill= c("#8d5524","#ffdbac","#c68642","#e0ac69", "#ffdbac", "#e0ac69", "#c68642", "#e0ac69", "#ffdbac", "#e0ac69")) +ggtitle("Most frequent words in Written Corpus in %")

names(speech_df)[names(speech_df) == "speech_words"] <- "words"
speech_df <- speech_df %>% mutate(register = "speech")
written_df <- written_df %>% mutate(register= "written")
whole_df <- rbind(speech_df, written_df)
whole_df$register <- as.factor(whole_df$register)

# barplot showing occurences of learned and not learned cclusters in the two registers
ggplot(whole_df, aes(fill = consonant_clusters,x = register)) +
  geom_bar(position = "stack") +
  theme_minimal()


vowels <- c("εί","ει","οι","οί","ου","ού","αι","αί","ε",'έ','α',"ά","η","ή","ω",
            "ώ","ι","ί","υ","ύ","ο","ό")

# we create an empty data frame with 4 columns
all <- data.frame(speech_df$consonant_clusters,
                  Words=character(296),
                  Before=character(296),
                  After=character(296))

# function that iterates through every word of the consonant_cluster column and extract the vowels that come before and after the ccluster ("φτ" or "φθ") and places them in cols of the df we created
createdf <- function(df,no){
  all <- data.frame(df$consonant_clusters,
                    Words=character(no),
                    Before=character(no),
                    After=character(no))
  cnt <- 1
  for(i in df$words){
    if(grepl("φτ",i,fixed=TRUE)){
      all$Words[cnt] <- i
      w <- strsplit(i,"φτ")[[1]]
      w1 <- strsplit(w[1],"")[[1]]
      w1 <- paste(w1[length(w1)-1],w1[length(w1)],sep = "")
      for(j in vowels){
        if(!(is_empty(w1))){
          if(grepl(j,w1,fixed=TRUE)){
            all$Before[cnt] <- j
            break
          } 
        }
        else{
          all$Before[cnt] <- "Onset"
        }
      }
    }
    else{
      all$Words[cnt] <- i
      w <- strsplit(i,"φθ")[[1]]
      w1 <- strsplit(w[1],"")[[1]]
      w1 <- paste(w1[length(w1)-1],w1[length(w1)],sep = "")
      for(j in vowels){
        if(!(is_empty(w1))){
          if(grepl(j,w1,fixed=TRUE)){
            all$Before[cnt] <- j
            break
          } 
        }
        else{
          all$Before[cnt] <- "Onset"
        }
      }
    }
    cnt <- cnt + 1
  }
  
  cnt <- 1
  for(i in df$words){
    if(grepl("φτ",i,fixed=TRUE)){
      all$Words[cnt] <- i
      w <- strsplit(i,"φτ")[[1]]
      w2 <- strsplit(w[2],"")[[1]]
      w2 <- paste(w2[1],w2[2],sep = "")
      for(j in vowels){
        if(!(is_empty(w2))){
          if(grepl(j,w2,fixed=TRUE)){
            all$After[cnt] <- j
            break
          } 
        }
        else{
          all$After[cnt] <- "Onset"
        }
      }
    }
    else{
      all$Words[cnt] <- i
      w <- strsplit(i,"φθ")[[1]]
      w2 <- strsplit(w[2],"")[[1]]
      w2 <- paste(w2[1],w2[2],sep = "")
      for(j in vowels){
        if(!(is_empty(w2))){
          if(grepl(j,w2,fixed=TRUE)){
            all$After[cnt] <- j
            break
          } 
        }
        else{
          all$After[cnt] <- "Onset"
        }
      }
    }
    cnt <- cnt + 1
  }
  all
}

# apply the function in the two dfs for the written and speech corpus
speech_split <- createdf(speech_df,296)
written_split <- createdf(written_df,300)

# it helps us to compare two plots by having them side by side
par(mfrow= c(1,2))
# turn characters to factorts
s <- as.factor(as.character(speech_split$Before))
# we create a barplot of the table for cols "df.consonant_clusters" (learned/not_learned) and the vowels that come before cclusters of the speech corpus which shows the occurences of the learned/not_learned consonant clusters (/ft/ - /fθ/) based on what comes before.
p1 <- barplot(table(speech_split$df.consonant_clusters,speech_split$Before),main="Vowels that come before cclusters in Speech corpus",ylim = c(0,200),col = c("#576afb","#ae48e2"),legend=TRUE)
# show occurences of each vowel
text(p1, y = table(s), label = table(s), pos = 3, cex = 0.8, col = "red")

# char to factors
w <- as.factor(as.character(written_split$Before))
# barplot of the table of the cols "df.consonant_clusters" (learned/not_learned) and "Before" of the written_split dataframe of the written corpus
p2 <- barplot(table(written_split$df.consonant_clusters, written_split$Before),main="Vowels that come before cclusters in Written corpus",ylim=c(0,180),col = c("#18afe2","#FF4000","#25a5e2","#254586","#aefcde","#a25f4e"), legend = TRUE)
# show occurences of each vowel
text(p2, y = table(w), label = table(w), pos = 3, cex = 0.8, col = "dark green")


par(mfrow=c(1,1))
par(mfrow=c(1,2))
s2 <- as.factor(as.character(speech_split$After))
p3 <-barplot(table(speech_split$df.consonant_clusters, speech_split$After),main="Vowels that come after cclusters in Speech corpus",ylim = c(0,150), col = c("yellow", "dark red", "coral", "red", "yellow", "coral"), legend= TRUE)
text(p3, y = table(s2), label = table(s), pos = 3, cex= 0.8, col = "red")

w2 <- as.factor(as.character(written_split$After))
p4 <- barplot(table(written_split$df.consonant_clusters,written_split$After),main="Vowels that come after cclusters in Written corpus",ylim = c(0,150), col = c("dark blue", "blue", "light green", "blue", "light green", "light green", "green", "light green"), legend= TRUE)
text(p4,y= table(w2), label = table(w2), pos = 3, cex= 0.8, col = "dark green")


# create a table wit the register and the occurences of cclusters
t <- table(whole_df$register, whole_df$consonant_clusters)

#barplot of the table 
barplot(t, beside = T, legend= T, col = c("yellow", "blue"))



# perform chi square test
ch_test<-chisq.test(table(whole_df$corpus, whole_df$consonant_clusters))
ch_test

# check out the expected values
ch_test$expected


# we also perform fisher's test which is more suitable for smaller samples
fisher.test(t)


#Pearson's residuals
ch_test$residuals
# association plot showing the positive and negative residuals (red is negative - black is positive)
assocplot(t) 



