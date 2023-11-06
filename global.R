my_data <- read.csv("data/AppleStore.csv")
my_data <- subset(my_data, select = -c(1))


my_data_genre <- unique(my_data$prime_genre)
my_data_genre

library(dplyr)
library(data.table)

infobox_data <- filter(my_data, prime_genre == "Games")




############################################################################# TRENDS WITH PARAMETERS ########################################################################################################

c1 <- c("size_bytes","price","user_rating","user_rating_ver","rating_count_tot","rating_count_ver")
########################################################################################################
agg_t_size <- aggregate(round((my_data$size_bytes)*0.000001,digits = 2),
                 by=list(my_data$prime_genre), FUN=mean)

colnames(agg_t_size) <- c('prime_genre', 'size_bytes')
########################################################################################################
agg_t_price <- aggregate(round((my_data$price),digits = 2),
                 by=list(my_data$prime_genre), FUN=mean)

colnames(agg_t_price) <- c('prime_genre', 'price')
########################################################################################################
agg_t_user_rating <- aggregate(round((my_data$user_rating),digits = 2),
                 by=list(my_data$prime_genre), FUN=mean)

colnames(agg_t_user_rating) <- c('prime_genre', 'user_rating')
########################################################################################################
agg_t_user_rating_ver <- aggregate(round((my_data$user_rating_ver),digits = 2),
                               by=list(my_data$prime_genre), FUN=mean)

colnames(agg_t_user_rating_ver) <- c('prime_genre', 'user_rating_ver')
########################################################################################################
agg_t_rating_count_tot <- aggregate(round((my_data$rating_count_tot),digits = 2),
                 by=list(my_data$prime_genre), FUN=mean)

colnames(agg_t_rating_count_tot) <- c('prime_genre', 'rating_count_tot')
#######################################################################################################
agg_t_rating_count_ver <- aggregate(round((my_data$rating_count_ver),digits = 2),
                                    by=list(my_data$prime_genre), FUN=mean)

colnames(agg_t_rating_count_ver) <- c('prime_genre', 'rating_count_ver')
######################################################################################################################################################################################


############################################################################# PRICE TRENDS #########################################################################################################

c2 <- c("size_bytes","user_rating","user_rating_ver","cont_rating")
########################################################################################################
df_fil <- filter(my_data, price <50 )  # filtering data as there are many outlier
########################################################################################################
agg_p_cont <- aggregate(round((df_fil$price),digits = 2),
                 by=list(df_fil$cont_rating), FUN=mean)

colnames(agg_p_cont) <- c('cont_rating', 'price')
######################################################################################################################################################################################


############################################################################### FREE vs PAID #########################################################################################
df_fil$cat <- ifelse(df_fil$price == 0, "Free",
                     "Paid")

df_fil_p <- filter(df_fil, cat == "Paid")

df_fil_f <- filter(df_fil, cat == "Free")

agg_p <- aggregate(df_fil_p$prime_genre, by = list(df_fil_p$prime_genre), FUN = length)
colnames(agg_p) <- c('prime_genre', 'Count')

agg_f <- aggregate(df_fil_f$prime_genre, by = list(df_fil_f$prime_genre), FUN = length)
colnames(agg_f) <- c('prime_genre', 'Count')


colnames(agg_f) <- c('prime_genre','Count_f')
colnames(agg_p) <- c('prime_genre','Count_p')

df_fin <- cbind(agg_p, Count_f = agg_f$Count_f)
transpose(df_fin)
df_1 <- transpose(df_fin)
rownames(df_1) <- colnames(df_fin)
colnames(df_1) <- df_fin$prime_genre
df_1 <- df_1[-1,]





















