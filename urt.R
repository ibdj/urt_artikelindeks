library(googlesheets4)
library(tidyverse)
library(tm) # to process the text
library(wordcloud) # to make the wordcloud
library(RColorBrewer) # to make the word cloud
library(stopwords)


urt <- read_sheet("https://docs.google.com/spreadsheets/d/1c3ecR6iBXZtTZydkzK8V8OVq-Q-_Bv0W0egmGovvjDQ/edit?gid=0#gid=0", sheet = 'artikelindeks', col_names = T) |> 
  mutate(sider_pr_forfatter = round(`antal sider`/`antal forfattere`,2) )

urt_longer <- urt |> 
  pivot_longer(cols = forfatter1:forfatter14, names_to = "level", values_to = "forfatter_navn") |> 
  drop_na(forfatter_navn)

urt_forfattere <- urt_longer |> 
  group_by(forfatter_navn) |>
  summarise(sider_total = sum(sider_pr_forfatter),
            row_count = n())
  
#### Word cloud ####

combined_text <- urt %>%
  pull(titel) %>%
  paste(collapse = " ")

# Create a corpus
corpus <- Corpus(VectorSource(combined_text))

danish_stopwords <- stopwords("da", source = "snowball")
custom_stopwords <- c("Boganmeldelse", "–", "-", "–","”","”","”","”","’")

# Text cleaning
corpus <- corpus %>%
  #tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, danish_stopwords) %>%
  tm_map(removeWords, custom_stopwords) %>%
  tm_map(stripWhitespace)

# Create a document-term matrix
dtm <- TermDocumentMatrix(corpus, control = list(tolower = FALSE))
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)
df <- df[df$word != "–", ]

df$word <- gsub("”", "", df$word)
df$word <- gsub('’', "", df$word)
df <- df[df$word != "", ]

# Aggregate frequencies
df_agg <- aggregate(freq ~ tolower(word), data = df, sum)

# Find most common original case
df_case <- df %>%
  group_by(tolower(word)) %>%
  summarise(original_word = names(which.max(table(word))))

# Merge aggregated frequencies with original case
df_final <- merge(df_agg, df_case, by = "tolower(word)")



# Clean up the final dataframe
df_final <- df_final[, c("original_word", "freq")]
colnames(df_final) <- c("word", "freq")

set.seed(1234) # for reproducibility

wordcloud(words = df_final$word, 
          freq = df_final$freq, 
          min.freq = 1,
          max.words = 120, 
          random.order = FALSE, 
          rot.per = 0.5, 
          colors = brewer.pal(8, "Set1"))

brewer.pal(n = 8, name = "Dark2")
