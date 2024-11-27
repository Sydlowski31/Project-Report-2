library(readr) # Let's Us Import File
library(ggplot2) # Graphing
library(caret) # Train-Test Split
library(corrplot) # Visualize Correlation Matrix
library(class) # Classification - K-Nearest Neighbors

bus = read_csv("/Users/sydlo/Downloads/OA 11.7 - yelp_academic_dataset_business.json.csv")
user = read_csv("/Users/sydlo/Downloads/OA 11.6 - yelp_academic_dataset_user.json.csv")

# Business Data

print(bus[1:5])
print(user[1:5])

ggplot(bus) + geom_bar(aes(x=state))

cont_table_stars = table(bus$stars)
pie(cont_table_stars)


review = subset(bus, review_count <= 1750)

ggplot(review, aes(x=stars, y=review_count, group=stars)) + 
  geom_boxplot(show.legend = FALSE) + labs(x="Star Rating", 
                                           y = "Review Count")

stars1 = subset(bus, stars==1)
stars5 = subset(bus, stars==5)
stars15 = rbind(stars1, stars5)
View(stars15)

chi_test = chisq.test(stars15$stars, stars15$review_count)
print(chi_test)

# User Data

print(colnames(user))

cool_funny = cor(user$cool_votes, user$funny_votes)
cool_useful = cor(user$cool_votes, user$useful_votes)
funny_useful = cor(user$funny_votes, user$useful_votes)
cat("Correlation Score for Cool and Funny Votes: ", cool_funny)
cat("Correlation Score for Cool and Useful Votes: ", cool_useful)
cat("Correlation Score for Funny and Useful Votes: ", funny_useful)

linear_model = lm(user$funny_votes ~ user$cool_votes)
print(linear_model)

coefs = coef(linear_model)
print(coefs)
lm_y_intercept = coefs[1]
lm_slope = coefs[2]

cat("Slope:", lm_slope, "Y-int:", lm_y_intercept)

ggplot(user) + geom_point(aes(x = cool_votes, y = funny_votes)) + 
  geom_smooth(aes(x = cool_votes, y = funny_votes), method="lm", se=F) + 
  labs(x="Cool Votes", y="Funny Votes", title="Funny Votes vs. Cool Votes Correlation")

View(user)

review_fans = cor(user$review_count, user$fans)
print(review_fans)
linear_model_rf = lm(user$fans ~ user$review_count)
print(linear_model_rf)

coefs_rf = coef(linear_model_rf)
print(coefs_rf)
lm_y_int_rf = coefs_rf[1]
lm_slope_rf = coefs_rf[2]

cat("Slope:", lm_slope_rf, "Y-int:", lm_y_int_rf)

ggplot(user) + geom_point(aes(x = review_count, y = fans)) + 
  geom_smooth(aes(x = review_count, y = fans), method="lm", se=F) + 
  labs(x="Number of Reviews", y="Fans", title="Does the Amount of Reviews Affect the Number of Fans?")

avg_fans = cor(user$average_stars, user$fans)

linear_model_af = lm(user$fans ~ user$average_stars)


coefs_af = coef(linear_model_af)

lm_y_int_af = coefs_af[1]
lm_slope_af = coefs_af[2]

ggplot(user) + geom_point(aes(x = average_stars, y = fans)) + 
  geom_smooth(aes(x = average_stars, y = fans), method="lm", se=F) + 
  labs(x="Average Stars", y="Fans", title="Do the Average Star Ratings Affect the Number of Fans?")


user_cluster = kmeans(user[1:11], 2)  
print(table(user_cluster$cluster, user$review_count)) 

user$cluster = user_cluster$cluster
View(user)


ggplot(user) + geom_point(aes(x=review_count,y=fans,color=user$cluster))


