library(tidyverse)
library(factoextra)
library(lubridate)
library(ggpubr)

# Please change to your own path.
df <- read.csv(".../marketing_campaign.csv",
               sep='\t')
# -------------------------------------------------------------------------------
# Preliminary process
colSums(is.na(df))
df <- na.omit(df)
summary(df) # Summary Statistics
colnames(df)

# Combine/Calculate 
df['Age']= 2022 - df$Year_Birth
df['total_spent'] = df$MntMeatProducts + df$MntFishProducts + df$MntWines + 
  df$MntFruits + df$MntSweetProducts + df$MntGoldProds
df['Kid'] = df$Kidhome + df$Teenhome
df['accepted'] = df$AcceptedCmp1 + df$AcceptedCmp2 + df$AcceptedCmp3 + 
  df$AcceptedCmp4 + df$AcceptedCmp5
# df$Kid <- factor(df$Kid)
# -------------------------------------------------------------------------------
# Data Visualization
inc <- df %>% 
  filter(Income < 600000) %>% # delete the outliers
  ggplot(aes(x=Income)) + 
  geom_histogram(color="white") + 
  labs(title="Customer Income") +
  xlab("Income($)")

ag <- df %>% 
  ggplot(aes(x=Age)) +
  geom_histogram(color="white") + 
  labs(title="Customer Age")

ts <- df %>% 
  ggplot(aes(x=total_spent)) +
  geom_histogram(color="white") + 
  labs(title="Cutomer Total Spent") +
  xlab("Total Spent($)")

ed <- df %>% 
  ggplot(aes(x=Education)) + 
  geom_bar() + 
  labs(title="Cutomer Education")

ma <- df %>% 
  ggplot(aes(x=Marital_Status)) + 
  geom_bar() + 
  labs(title="Customer Marital Status") +
  xlab("Marital Status")

kd <- df %>% 
  ggplot(aes(x=Kid)) + 
  geom_bar() + 
  labs(title="Kid")

in_t <- df %>% 
  filter(Income < 600000) %>% 
  ggplot(aes(x=Income, y=total_spent)) +
  geom_point(alpha=0.5, color="blue") +
  xlab("Income($)") +
  ylab("Total Spent($)")

ag_t <- df %>% 
  filter(Income < 600000) %>% 
  ggplot(aes(x=Age, y=total_spent)) +
  geom_point(alpha=0.5, color="red") +
  ylab("Total Spent($)")

kd_t <- df %>%
  ggplot(aes(x=Kid, y=total_spent)) +
  geom_boxplot(width=.3, fill="black") +
  stat_summary(fun=median, geom="point", fill="white", shape=21, size=2.5) +
  ylab("Total Spent($)")

df %>%
  filter(Income < 600000) %>% 
  ggplot(aes(x=Education, y=Income)) +
  geom_boxplot(width=.1, fill="black") +
  stat_summary(fun=median, geom="point", fill="white", shape=21, size=2.5)

ed_t <- df %>%
  ggplot(aes(x=Education, y=total_spent)) +
  geom_boxplot(width=.3, fill="black") +
  stat_summary(fun=median, geom="point", fill="white", shape=21, size=2.5) +
  ylab("Total Spent($)")

df %>%
  filter(Income < 600000) %>% 
  ggplot(aes(x=Marital_Status, y=Income)) +
  geom_boxplot(width=.3, fill="black") +
  stat_summary(fun=median, geom="point", fill="white", shape=21, size=2.5)

mar_t <- df %>%
  ggplot(aes(x=Marital_Status, y=total_spent)) +
  geom_boxplot(width=.3, fill="black") +
  stat_summary(fun=median, geom="point", fill="white", shape=21, size=2.5) +
  xlab("Marital Status") + 
  ylab("Total Spent($)")

df %>% 
  ggplot(aes(x=NumDealsPurchases)) + 
  geom_bar() + 
  labs(title="Number of Deals Purchases") +
  xlab("Deal Purchases")

n.web <- df %>% 
  ggplot(aes(x=NumWebPurchases)) + 
  geom_bar() + 
  labs(title="Number of Web Purchases") +
  xlab("Web Purchases")

n.cat <- df %>% 
  ggplot(aes(x=NumCatalogPurchases)) + 
  geom_bar() + 
  labs(title="Number of Catalogue Purchases") +
  xlab("Catalogue Purchases")

n.store <- df %>% 
  ggplot(aes(x=NumStorePurchases)) + 
  geom_bar() + 
  labs(title="Number of Store Purchases") +
  xlab("Store Purchases")

n.webV <- df %>% 
  ggplot(aes(x=NumWebVisitsMonth)) + 
  geom_bar() + 
  labs(title="Number of Website Visits Per Month") +
  xlab("Website Visits Per Month")

df %>% 
  ggplot(aes(x=Response)) + 
  geom_bar() + 
  labs(title="Response")

df %>% 
  ggplot(aes(x=accepted)) + 
  geom_bar() + 
  labs(title="Accepted")

ggarrange(ag, inc, kd, nrow=1)
ggarrange(ed, ma)

cor(df$Age, y=df$total_spent)
cor(df$Income, y=df$total_spent)
ggarrange(ag_t, in_t)

ggarrange(kd_t, ed_t, mar_t, nrow=1)

ggarrange(n.web, n.cat, n.store, n.webV)

# -------------------------------------------------------------------------------
education.type <- factor(df$Education, order=FALSE)
levels(education.type)
marital.type <- factor(df$Marital_Status, order=FALSE)
levels(marital.type)
df$Education <- as.numeric(factor(df$Education, order=FALSE)) - 1
df$Marital_Status <- as.numeric(factor(df$Marital_Status, order=FALSE)) - 1
df$Dt_Customer <- as.numeric(as.POSIXct(df$Dt_Customer, format="%d-%m-%Y"))

colnames(df)
df_kc1 <- df[, c(3:5, 8, 9, 16:20, 30:32)]
df_kc2 <- df[, c(3, 4, 16:20, 30:33)]
colnames(df_kc1)
colnames(df_kc2)
df_hc<-df[,-c(1:11,17,18,20:22)]

# -------------------------------------------------------------------------------
# clustering 1
set.seed(362)
WSS1 <- rep(0, 10)
for (k in 1:10) {
  # extract the total within-group sum of squared errors
  WSS1[k] = kmeans(x=scale(df_kc1),
                  centers=k, nstart=25)$tot.withinss
}

# find the optimal K value
ggplot(mapping=aes(x=1:10, y=WSS1)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept=4) +
  scale_x_discrete(name="k", limits=factor(1:10)) +
  labs(title = "Elbow Method For Clustering 1")

# Looking for K-means Clustering
cust_kmeans1 <- kmeans(x = scale(df_kc1),
                      centers = 4, nstart = 50)
fviz_cluster(cust_kmeans1, data = df_kc1, geom = "point",ellipse.type = "norm",
             repel = TRUE)

df_kc1['cluster'] = as.factor(cust_kmeans1$cluster)
head(df_kc1)

# -------------------------------------------------------------------------------
# clustering 2
WSS2 <- rep(0, 10)
for (k in 1:10) {
  # extract the total within-group sum of squared errors
  WSS2[k] = kmeans(x=scale(df_kc2),
                  centers=k, nstart=25)$tot.withinss
}

# find the optimal K value
ggplot(mapping=aes(x=1:10, y=WSS2)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept=4) +
  scale_x_discrete(name="k", limits=factor(1:10)) +
  labs(title = "Elbow Method For Clustering 2")

# Looking for K-means Clustering
cust_kmeans2 <- kmeans(x = scale(df_kc2),
                      centers = 4, nstart = 50)
fviz_cluster(cust_kmeans2,  data = df_kc2, geom = "point",ellipse.type = "norm",
             repel = TRUE)

df_kc2['cluster'] = as.factor(cust_kmeans2$cluster)
head(df_kc2)

# -------------------------------------------------------------------------------
# Cluster 1 Plot

# cluster/total spent
p.sp <- ggplot(df_kc1, aes(x=cluster,y=total_spent,fill=cluster)) +
  geom_boxplot() +
  labs(title="Total Spent") +
  ylab("Total Spent($)")

ggplot(df_kc1, aes(x=cluster,y=total_spent,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 2500))

# cluster/age （no significant connection）
p.age <- ggplot(df_kc1, aes(x=cluster,y=Age,fill=cluster)) +
  geom_boxplot()+
  labs(title="Age")

# cluster/income
p.in <- df_kc1 %>% 
  filter(Income < 600000) %>% 
  ggplot(aes(x=cluster,y=Income,fill=cluster)) +
  geom_boxplot(outlier.color=NA) +
  coord_cartesian(ylim=c(0, 100000)) +
  labs(title="Income") + 
  ylab("Income($)")

# （no significant connection）
p.kid <- ggplot(df_kc1, aes(x=cluster,y=Kid,fill=cluster)) +
  geom_boxplot() +
  labs(title="Kid")

p.re <- ggplot(df_kc1, aes(x=cluster,y=Recency,fill=cluster)) +
  geom_boxplot() +
  labs(title="Recency")

p.cat <- ggplot(df_kc1, aes(x=cluster,y=NumCatalogPurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 12)) +
  labs(title="Number of Catalogue Purchase ") +
  ylab("Number of catalogue purchases")

p.web <- ggplot(df_kc1, aes(x=cluster,y=NumWebPurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 12)) +
  labs(title="Number of Website Purchases") +
  ylab("Number of website purchases")

p.store <- ggplot(df_kc1, aes(x=cluster,y=NumStorePurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  labs(title="Number of Store Purchases") +
  ylab("Number of store purchases")

p.webV <- ggplot(df_kc1, aes(x=cluster,y=NumWebVisitsMonth,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 12)) +
  labs(title="Number of Website Visits Per Month") +
  ylab("Website Visits Per Month")

p.deal <- ggplot(df_kc1, aes(x=cluster,y=NumDealsPurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  labs(title="Number of Discount Purchases") +
  ylab("Number of discount purchases")

p.ed <- ggplot(df_kc1, aes(x=cluster, y=Education, fill=cluster)) +
  geom_boxplot() +
  labs(title="Education")

p.mar <- ggplot(df_kc1, aes(x=cluster, y=Marital_Status, fill=cluster)) +
  geom_boxplot() +
  labs(title="Marital Status") +
  ylab("Marital Status")

ggplot(df_kc1, aes(x=cluster, y=Dt_Customer, fill=cluster)) +
  geom_boxplot() + 
  labs(title="Date of cutomer's enrollment")

ggarrange(p.sp, p.age, p.in, p.kid, p.re, p.ed, common.legend=T, legend="bottom")
ggarrange(p.sp, p.cat, p.web, p.store, p.deal, p.webV, common.legend=T, 
          legend="bottom")

Distance <- dist(df_hc, method = 'euclidean')
hplot<-hclust(Distance, method='complete')
plot(hplot, hang = -1, cex = 0.6)
# -------------------------------------------------------------------------------
# Cluster 1 data range

cluster1 <- filter(df_kc1, cluster==1)
cluster2 <- filter(df_kc1, cluster==2)
cluster3 <- filter(df_kc1, cluster==3)
cluster4 <- filter(df_kc1, cluster==4)

#################################################################################
# find outliers of income
out_income1 <- boxplot.stats(cluster1$Income)$out
out_income2 <- boxplot.stats(cluster2$Income)$out
out_income3 <- boxplot.stats(cluster3$Income)$out
out_income4 <- boxplot.stats(cluster4$Income)$out

out_index_income1 <- which(cluster1$Income %in% c(out_income1))
out_index_income2 <- which(cluster2$Income %in% c(out_income2))
out_index_income3 <- which(cluster3$Income %in% c(out_income3))
out_index_income4 <- which(cluster4$Income %in% c(out_income4))

cluster1.in <- cluster1[c(-out_index_income1), ]
cluster2.in <- cluster2[c(-out_index_income2), ]
cluster3.in <- cluster3[c(-out_index_income3), ]
cluster4.in <- cluster4[c(-out_index_income4), ]
#################################################################################

income_range <- data.frame(
  cluster = c(1:4),
  income.min = c(min(cluster1.in$Income), min(cluster2.in$Income), 
                 min(cluster3.in$Income), min(cluster4.in$Income)),
  income.max = c(max(cluster1.in$Income), max(cluster2.in$Income), 
                 max(cluster3.in$Income), max(cluster4.in$Income)),
  mean = c(mean(cluster1.in$Income), mean(cluster2.in$Income), 
           mean(cluster3.in$Income), mean(cluster4.in$Income))
)

#################################################################################
# find outliers of total spent.
out_spent1 <- boxplot.stats(cluster1$total_spent)$out
out_spent2 <- boxplot.stats(cluster2$total_spent)$out
out_spent3 <- boxplot.stats(cluster3$total_spent)$out
out_spent4 <- boxplot.stats(cluster4$total_spent)$out

out_index_spent1 <- which(cluster1$total_spent %in% c(out_spent1))
out_index_spent2 <- which(cluster2$total_spent %in% c(out_spent2))
out_index_spent3 <- which(cluster3$total_spent %in% c(out_spent3))
out_index_spent4 <- which(cluster4$total_spent %in% c(out_spent4))

cluster1.sp <- cluster1[c(-out_index_spent1), ]
cluster2.sp <- cluster2[c(-out_index_spent2), ]
cluster3.sp <- cluster3[c(-out_index_spent3), ]
cluster4.sp <- cluster4[c(-out_index_spent4), ]

#################################################################################

total_spent_range <- data.frame(
  cluster = c(1:4),
  total_spent.min = c(min(cluster1.sp$total_spent), min(cluster2.sp$total_spent), 
                      min(cluster3.sp$total_spent), min(cluster4.sp$total_spent)),
  total_spent.max = c(max(cluster1.sp$total_spent), max(cluster2.sp$total_spent), 
                      max(cluster3.sp$total_spent), max(cluster4.sp$total_spent)),
  mean = c(mean(cluster1.sp$total_spent), mean(cluster2.sp$total_spent), 
           mean(cluster3.sp$total_spent), mean(cluster4.sp$total_spent))
)

education <- data.frame(
  cluster = c(1:4),
  Sceond.n.cycle = c(nrow(filter(cluster1, Education==0)),
                     nrow(filter(cluster2, Education==0)),
                     nrow(filter(cluster3, Education==0)),
                     nrow(filter(cluster4, Education==0))),
  Basic = c(nrow(filter(cluster1, Education==1)),
            nrow(filter(cluster2, Education==1)),
            nrow(filter(cluster3, Education==1)),
            nrow(filter(cluster4, Education==1))),
  Graduation = c(nrow(filter(cluster1, Education==2)),
                 nrow(filter(cluster2, Education==2)),
                 nrow(filter(cluster3, Education==2)),
                 nrow(filter(cluster4, Education==2))),
  Master = c(nrow(filter(cluster1, Education==3)),
             nrow(filter(cluster2, Education==3)),
             nrow(filter(cluster3, Education==3)),
             nrow(filter(cluster4, Education==3))),
  PhD = c(nrow(filter(cluster1, Education==4)),
          nrow(filter(cluster2, Education==4)),
          nrow(filter(cluster3, Education==4)),
          nrow(filter(cluster4, Education==4)))
)

#################################################################################
# find outliers of age

out_age1 <- boxplot.stats(cluster1$Age)$out
out_age2 <- boxplot.stats(cluster2$Age)$out
out_age3 <- boxplot.stats(cluster3$Age)$out
out_age4 <- boxplot.stats(cluster4$Age)$out

out_index_age1 <- which(cluster1$Age %in% c(out_age1))
out_index_age2 <- which(cluster2$Age %in% c(out_age2))
out_index_age3 <- which(cluster3$Age %in% c(out_age3))
out_index_age4 <- which(cluster4$Age %in% c(out_age4))

cluster1.age <- cluster1[c(-out_index_age1), ]
cluster2.age <- cluster2[c(-out_index_age2), ]
cluster3.age <- cluster3[c(-out_index_age3), ]
cluster4.age <- cluster4[c(-out_index_age4), ]

#################################################################################

age_range <- data.frame(
  cluster = c(1:4),
  age.min = c(min(cluster1.age$Age), min(cluster2.age$Age), 
              min(cluster3.age$Age), min(cluster4.age$Age)),
  age.max = c(max(cluster1.age$Age), max(cluster2.age$Age), 
              max(cluster3.age$Age), max(cluster4.age$Age)),
  mean = c(mean(cluster1.age$Age), mean(cluster2.age$Age), 
           mean(cluster3.age$Age), mean(cluster4.age$Age))
)

kid <- data.frame(
  cluster = c(1:4),
  one.kid = c(nrow(filter(cluster1, Kid==1)),
              nrow(filter(cluster2, Kid==1)),
              nrow(filter(cluster3, Kid==1)),
              nrow(filter(cluster4, Kid==1))),
  two.kids = c(nrow(filter(cluster1, Kid==2)),
               nrow(filter(cluster2, Kid==2)),
               nrow(filter(cluster3, Kid==2)),
               nrow(filter(cluster4, Kid==2))),
  three.kids = c(nrow(filter(cluster1, Kid==3)),
                 nrow(filter(cluster2, Kid==3)),
                 nrow(filter(cluster3, Kid==3)),
                 nrow(filter(cluster4, Kid==3)))
)

recency_range <- data.frame(
  cluster = c(1:4),
  recency.min = c(min(cluster1$Recency), min(cluster2$Recency), 
                  min(cluster3$Recency), min(cluster4$Recency)),
  recency.max = c(max(cluster1$Recency), max(cluster2$Recency), 
                  max(cluster3$Recency), max(cluster4$Recency)),
  mean = c(mean(cluster1$Recency), mean(cluster2$Recency), 
           mean(cluster3$Recency), mean(cluster4$Recency))
)

# -------------------------------------------------------------------------------
# Table Visualization (Cluster 1)
View(income_range)
View(total_spent_range)
View(education)
View(age_range)
View(kid)
View(recency_range)

# -------------------------------------------------------------------------------
# Cluster 2 Plot
ggplot(df_kc2, aes(x=cluster,y=NumCatalogPurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 12))

ggplot(df_kc2, aes(x=cluster,y=NumWebPurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 12))

ggplot(df_kc2, aes(x=cluster,y=NumStorePurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA)

ggplot(df_kc2, aes(x=cluster,y=NumWebVisitsMonth,fill=cluster)) +
  geom_boxplot(outlier.colour=NA) +
  coord_cartesian(ylim=c(0, 12))

ggplot(df_kc2, aes(x=cluster,y=NumDealsPurchases,fill=cluster)) +
  geom_boxplot(outlier.colour=NA)

ggplot(df_kc2, aes(x=cluster,y=total_spent,fill=cluster)) +
  geom_boxplot()

ggplot(df_kc2, aes(x=cluster,y=Age,fill=cluster)) +
  geom_boxplot()

ggplot(df_kc2, aes(x=cluster,y=Kid,fill=cluster)) +
  geom_boxplot()

# -------------------------------------------------------------------------------
# Cluster 2 data range
colnames(df_kc2)

cluster2.1 <- filter(df_kc2, cluster==1)
cluster2.2 <- filter(df_kc2, cluster==2)
cluster2.3 <- filter(df_kc2, cluster==3)
cluster2.4 <- filter(df_kc2, cluster==4)

web <- data.frame(
  cluster = c(1:4),
  web.min = c(min(cluster2.1$NumWebPurchases), min(cluster2.2$NumWebPurchases), 
              min(cluster2.3$NumWebPurchases), min(cluster2.4$NumWebPurchases)),
  web.max = c(max(cluster2.1$NumWebPurchases), max(cluster2.2$NumWebPurchases), 
              max(cluster2.3$NumWebPurchases), max(cluster2.4$NumWebPurchases)),
  mean = c(mean(cluster2.1$NumWebPurchases), mean(cluster2.2$NumWebPurchases), 
           mean(cluster2.3$NumWebPurchases), mean(cluster2.4$NumWebPurchases))
)

category <- data.frame(
  cluster = c(1:4),
  cat.min = c(min(cluster2.1$NumCatalogPurchases), 
              min(cluster2.2$NumCatalogPurchases), 
              min(cluster2.3$NumCatalogPurchases), 
              min(cluster2.4$NumCatalogPurchases)),
  cat.max = c(max(cluster2.1$NumCatalogPurchases), 
              max(cluster2.2$NumCatalogPurchases), 
              max(cluster2.3$NumCatalogPurchases), 
              max(cluster2.4$NumCatalogPurchases)),
  mean = c(mean(cluster2.1$NumCatalogPurchases), 
           mean(cluster2.2$NumCatalogPurchases), 
           mean(cluster2.3$NumCatalogPurchases), 
           mean(cluster2.4$NumCatalogPurchases))
)

store <- data.frame(
  cluster = c(1:4),
  store.min = c(min(cluster2.1$NumStorePurchases), 
               min(cluster2.2$NumStorePurchases), 
               min(cluster2.3$NumStorePurchases), 
               min(cluster2.4$NumStorePurchases)),
  store.max = c(max(cluster2.1$NumStorePurchases), 
               max(cluster2.2$NumStorePurchases), 
               max(cluster2.3$NumStorePurchases), 
               max(cluster2.4$NumStorePurchases)),
  mean = c(mean(cluster2.1$NumStorePurchases), 
           mean(cluster2.2$NumStorePurchases), 
           mean(cluster2.3$NumStorePurchases), 
           mean(cluster2.4$NumStorePurchases))
)

deals <- data.frame(
  cluster = c(1:4),
  deals.min = c(min(cluster2.1$NumDealsPurchases), 
                min(cluster2.2$NumDealsPurchases), 
                min(cluster2.3$NumDealsPurchases), 
                min(cluster2.4$NumDealsPurchases)),
  deals.max = c(max(cluster2.1$NumDealsPurchases), 
                max(cluster2.2$NumDealsPurchases), 
                max(cluster2.3$NumDealsPurchases), 
                max(cluster2.4$NumDealsPurchases)),
  mean = c(mean(cluster2.1$NumDealsPurchases), 
           mean(cluster2.2$NumDealsPurchases), 
           mean(cluster2.3$NumDealsPurchases), 
           mean(cluster2.4$NumDealsPurchases))
)

web.visit <- data.frame(
  cluster = c(1:4),
  webVisit.min = c(min(cluster2.1$NumWebVisitsMonth), 
                   min(cluster2.2$NumWebVisitsMonth), 
                   min(cluster2.3$NumWebVisitsMonth), 
                   min(cluster2.4$NumWebVisitsMonth)),
  webVisit.max = c(max(cluster2.1$NumWebVisitsMonth), 
                   max(cluster2.2$NumWebVisitsMonth), 
                   max(cluster2.3$NumWebVisitsMonth), 
                   max(cluster2.4$NumWebVisitsMonth)),
  mean = c(mean(cluster2.1$NumWebVisitsMonth), 
           mean(cluster2.2$NumWebVisitsMonth), 
           mean(cluster2.3$NumWebVisitsMonth), 
           mean(cluster2.4$NumWebVisitsMonth))
)

# -------------------------------------------------------------------------------
# Table Visualization (Cluster 1)
View(web)
View(category)
View(store)
View(deals)
View(web.visit)

















