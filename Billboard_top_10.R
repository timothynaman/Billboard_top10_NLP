
###########################
######### created by Timothy Naman
######### MBAN2 HULT 2021
######### Date: 12.5.2021
######### version 0.1
###########################
 library(tidytuesdayR)
 library(dplyr)
 library(tidytext)
 library(janeaustenr)
 library(igraph)
 library(ggraph)
 library(scales)
 library(tidyr)
 library(stringr)
 library(ggplot2)
library(readr)
library(readxl)
 
 #preparing data
 my_billboard_data<-read_excel("/Users/Timothy/Downloads/BILLBOARD top10 1960-2020.xlsx")
 my_billboard_data$Year<- as.character(my_billboard_data$Year)
 colnames(my_billboard_data)[5] <- "text"
 my_billboard_data<-as.data.frame(my_billboard_data)
 tidy_billboard <- my_billboard_data %>%
   unnest_tokens(word, text)
 view(tidy_billboard)
 
 #removing stop words
 data(stop_words)
billboard_no_stop <- tidy_billboard %>%
   anti_join(stop_words)
 view(billboard_no_stop)
 #viewing the count frequencies for each token without stop words
 billboard_no_stop %>%
   count(word, sort=TRUE)
 
 ###MOST COMMON QUADRO-GRAM
 billboard_quadrograms <- my_billboard_data %>%
   unnest_tokens(quadrogram, text, token = "ngrams", n=4)
 
 billboard_quadrogram_separated <- billboard_quadrograms %>%
   separate(quadrogram, c("word1", "word2","word3", "word4"), sep = " ")
 
 quadrogram_filtered <- billboard_quadrogram_separated %>%
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>%
   filter(!word3 %in% stop_words$word) %>%
   filter(!word4 %in% stop_words$word) 
 
 quadrogram_counts <-  billboard_quadrogram_separated %>%
   count(word1, word2,word3,word4, sort = TRUE)
 
 quadrogram_counts

 #quadro-gram network
 quadrogram_graph <- quadrogram_counts %>%
   filter(n>7) %>%
   graph_from_data_frame()
 
 quadrogram_graph
 #plotting the quadro-gram
 ggraph(quadrogram_graph, layout = "fr") +
   geom_edge_link()+
   geom_node_point()+
   geom_node_text(aes(label=name), vjust =1, hjust=1)

 
 
 
 
 
 
 
###CORRELATION and CORRELOGRAMS BASED ON DECADE
 
# creating a tidy format for 1970
sixties <- my_billboard_data%>%
   filter(Year == "1960")
 
 tidy_1960 <- sixties %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_1960)
 
# creating a tidy format for 1970
seventies <- my_billboard_data %>%
   filter(Year == "1970")
 
 tidy_1970 <- seventies %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_1970)
 
# creating a tidy format for 1980
 eighties <- my_billboard_data %>%
   filter(Year == "1980")
 
 tidy_1980 <- eighties %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_1980)
 
# creating a tidy format for 1990
 nineties <- my_billboard_data %>%
   filter(Year == "1990")
 
 tidy_1990 <- nineties %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_1990)
 
#creating a tidy format for 2000
 twothousands <- my_billboard_data %>%
   filter(Year == "2000")
 
 tidy_2000 <-  twothousands %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_2000)
 
# creating a tidy format for 2010
 tens <- my_billboard_data %>%
   filter(Year == "2010")
 
 tidy_2010 <- tens %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_2010)

# creating a tidy format for 2020
twenties <- my_billboard_data %>%
   filter(Year == "2020")
 
 tidy_2020 <- twenties %>%
   unnest_tokens(word, text) %>%
   anti_join(stop_words)
 view(tidy_2020)
 
 #Correlogram
 library(tidyr)
 frequency1 <- bind_rows(mutate(tidy_1960, Decade= "sixties"),
                        mutate(tidy_1970, Decade= "seventies"),
                        mutate(tidy_1980, Decade="eighties"),
                        mutate(tidy_1990, Decade="nineties"),
                        mutate(tidy_2000, Decade="twothousands"),
                        mutate(tidy_2010, Decade="tens"),
                        mutate(tidy_2020, Decade="twenties")
 )%>%#closing bind_rows
   mutate(word=str_extract(word, "[a-z']+")) %>%
   count(Decade, word) %>%
   group_by(Decade) %>%
   mutate(proportion = n/sum(n))%>%
   select(-n) %>%
   spread(Decade, proportion) %>%
   gather(Decade, proportion,`sixties`, `seventies`,`eighties`,`nineties`,`twothousands`,`tens`)
 
 #let's plot the correlograms:
 
 ggplot(frequency1, aes(x=proportion, y = `twenties`, 
                       color = abs(`twenties`- proportion)))+
   geom_abline(color="grey40", lty=2)+
   geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
   geom_text(aes(label=word), check_overlap = TRUE, vjust=1) +
   scale_x_log10(labels = percent_format())+
   scale_y_log10(labels= percent_format())+
   scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
   facet_wrap(~Decade, ncol=3)+
   theme(legend.position = "none")+
   labs(y= "twenties", x=NULL)
 
 #doing the cor.test()

cor.test(data=frequency1[frequency1$Decade == "seventies",],
          ~proportion + `twenties`)
 
cor.test(data=frequency1[frequency1$Decade == "twothousands",],
          ~proportion + `twenties`)




# SENTIMENT ANALYSIS OF MY_BILLBOARD

#sentiment for 1960
sixties <- tidy_billboard %>%
  filter(Year == "1960")

afinn <- sixties %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  sixties%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  sixties %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words

bing_counts <- sixties %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



#sentiment for 1970
seventies <- tidy_billboard %>%
  filter(Year == "1970")

afinn1 <- seventies %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc1 <- bind_rows(
  seventies%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  seventies %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn1, bing_and_nrc1) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words

bing_counts1 <- seventies %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts1

bing_counts1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


#sentiment for 1980
eighties <- tidy_billboard %>%
  filter(Year == "1980")

afinn2 <- eighties %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc2 <- bind_rows(
  eighties%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  eighties %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn2, bing_and_nrc2) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words 
bing_counts2 <- eighties %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts2

bing_counts2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#sentiment for 1990

nineties <- tidy_billboard %>%
  filter(Year == "1990")

afinn3 <- nineties %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc3 <- bind_rows(
  nineties%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  nineties %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn3, bing_and_nrc3) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words 

bing_counts3 <- nineties %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts3

bing_counts3 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


#sentiment for 2000

twothousands <- tidy_billboard %>%
  filter(Year == "2000")

affin4 <- twothousands %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc4 <- bind_rows(
  twothousands%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  twothousands %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(affin4, bing_and_nrc4) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for 2000

bing_counts4 <- twothousands %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts4

bing_counts4 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()




#sentiment for 2010

tens <- tidy_billboard %>%
  filter(Year == "2010")

affin5 <- tens %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc5 <- bind_rows(
  tens%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  tens %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(affin5, bing_and_nrc5) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words for 2010

bing_counts5 <- tens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts5

bing_counts5 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



#sentiment for 2020

twenties <- tidy_billboard %>%
  filter(Year == "2020")

affin6 <- twenties %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc6 <- bind_rows(
  twenties%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  twenties %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(affin6, bing_and_nrc6) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

# Most common positive and negative words 

bing_counts6 <- twenties %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts6

bing_counts6 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



###ZIPF's LAW and TF-IDF By Year

billboard_token <- my_billboard_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(Year, word, sort=TRUE) %>%
  ungroup()

total_words <- billboard_token %>%
  group_by(Year) %>%
  summarise(total=sum(n))

billboard_words <- left_join(billboard_token, total_words)%>%
  filter(Year %in% c("1960","1970","1980","1990","2000","2010","2020"))

view(billboard_words)

# ZIPF's law 

freq_by_rank1 <- billboard_words %>%
  group_by(Year) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank1

# ZIPF's Law plot
freq_by_rank1 %>%
  ggplot(aes(rank, `term frequency`, color=Year))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

# TF_IDF 

Year_words <- billboard_words %>%
  bind_tf_idf(word, Year, n)

Year_words # we get all the zeors because we are looking at stop words ... too common

arranged_idf<-Year_words %>%
  arrange(desc(tf_idf))
 arranged_idf

# TF_IDF :
Year_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(Year) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Year))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~Year, ncol=4, scales="free")+
  coord_flip()




