#STA6704 - Final project R-code
# Load required libraries
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(BioStatR)
library(corrplot)
library(qgraph)
library("factoextra")
library(psych)
library(dendextend)
library(cluster)
library(gridExtra)
#library(Hmisc)
#library("FactoMineR")

# Load the dataset
country_data <- read.csv(file = "C:\\Users\\divya\\Desktop\\Country-data.csv" , header = TRUE)
head(country_data)
dim(country_data)
var_definition <- read.csv(file = "C:\\Users\\divya\\Desktop\\data-dictionary.csv" , header = TRUE)
var_definition

# Standard statistical measures
summary(country_data)
str(country_data)

# Missing values
sum(is.na(country_data))
plot_missing(country_data, ggtheme = theme_bw(), title = "Missing Values in Country Dataset") +
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

# Univariate analysis: Histogram
numeric_data <-country_data[, sapply(country_data, is.numeric)]
hist(numeric_data)
title("Histogram of variables", line = -1, outer = TRUE, font.main=4)

# Univariate analysis: Boxplot
boxplot_data <- country_data %>% gather(Attributes, values, c(2:10))
box_plot <- ggplot(boxplot_data, aes(x=Attributes, y=values)) + geom_boxplot(aes(fill=Attributes)) + ggtitle("Boxplot of Varibles")
box_plot + facet_wrap( ~ Attributes, scales="free") +   theme_bw() +guides(fill = "none") + theme(
  plot.title = element_text(hjust = 0.5, face="bold.italic"))

# Boxplot - comparing variables:
data_compare <- country_data %>% gather(Attributes, values, c(2:5, 7:9)) 
ggplot(data_compare, aes(x=reorder(Attributes, values, FUN=median), y=values,  
                         fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Boxplots of Variables - Comparison") +
  theme_bw() +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(), plot.title = element_text(hjust = 0.5, face="bold.italic")) +
  ylim(-10, 220)  +  coord_flip()

# scatter plot - bivariate:
upper.panel <- function(x, y){
  points(x,y, pch=21, col ="blue")
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.8, txt)
}
pairs(country_data[,2:10], lower.panel = NULL, upper.panel = upper.panel, diag.panel =  panel.hist , main = "Bivariate distribution - Country Data" , font.main=4)

# Correlation Matrix:
country_corr <- cor(country_data[,2:10])
corrplot(country_corr, type="upper",  tl.col = "black", tl.srt = 45, addCoef.col ='black', number.cex = 0.8)
title("Correlation Matrix - Country Data", line = -1, outer = TRUE, font.main=4)

qgraph(country_corr,  labels = colnames(country_data[,2:10]), 
       layout = "spring", 
       label.cex = 1, normalize=TRUE, edge.width=0.65 )
title("Correlation graph - Country Data", line = -2, outer = TRUE, font.main=4)

# Scale dataset:
country_df <- as.data.frame(country_data)
country_scaled <- scale(country_df[,2:10])
rownames(country_scaled) <- country_df[,1] 

# PCA using prcomp
country_pca <- prcomp(country_scaled)
country_pca
summary(country_pca)
country_eigen <- get_eigenvalue(country_pca)
country_eigen

factoextra::fviz_eig(country_pca, addlabels = TRUE) + theme_classic() + labs(title = "Variances - PCA",  x = "Principal Components", title.size = 4) +theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))
#screeplot(country_pca)

factoextra::fviz_pca_var( X = country_pca,  col.var = "contrib",  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE, title = "Variables Contribution to Principal Components") + theme_classic() + theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

barplot(country_pca$rotation[,"PC1"], main = "Barplot of PCA - PC1", font.main = 4)
barplot(country_pca$rotation[,"PC2"], main = "Barplot of PCA - PC2", font.main = 4)

#fviz_contrib(country_pca, choice = "var", axes = 1)
#fviz_contrib(country_pca, choice = "var", axes = 2)

#PCA using princomp 
country_prin1 <- principal(country_scaled, nfactors=2, rotate="none")
country_prin1

country_prin_var1 <- loadings(country_prin1)
country_prin_var1

qgraph(country_prin_var1, labels = colnames(country_data[,2:10]), label.cex = 1, normalize=TRUE, edge.width=0.65 )
title("Variables contribution to principal components, PC1 & PC2", line = -3, outer = TRUE, font.main=4)

country_prin2 <- principal(country_scaled, nfactors=2, rotate="varimax")
country_prin2

country_prin_var2 <- loadings(country_prin2)
country_prin_var2 

qgraph(country_prin_var2, labels = colnames(country_data[,2:10]), label.cex = 1, normalize=TRUE, edge.width=0.65 )
title("Variables contribution to principal components(rotated), PC1 & PC2", line = -3, outer = TRUE, font.main=4)

# Hierarchical clustering:
distance_country <- dist(country_scaled) # default euclidean
hiercluster_comp <- hclust(distance_country, method = "complete")

dend_country <- as.dendrogram(hiercluster_comp)
plot(dend_country, main = "Dendogram - Hierarchical Clustering(euclidean, complete)", font.main = 4)

dendogram_3 <- color_branches(dend_country, k =3)
plot(dendogram_3, main = "Dendogram - Hierarchical Clustering (3 clusters color)", font.main = 4)

cutree_hierclust <- cutree( tree = hiercluster_comp, k = 3)

prin_comp_hier <- data.frame(prcomp(x = country_scaled, center = FALSE,scale. = FALSE )$x[,1:2],  Name = rownames(country_scaled), Cluster = as.character(cutree_hierclust), stringsAsFactors = FALSE)

ggplot(prin_comp_hier) +
  aes(x = PC1,y = PC2,color = Cluster,fill = Cluster,label = Name ,group = Cluster) + 
  geom_point() + 
  ggrepel::geom_text_repel(color = "black",size = 3) + 
  ggtitle("Scatter plot - hierarchical cluster (euclidean, complete)") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

dist_country_canberra <- dist(country_scaled, "canberra")
hiercluster_wardd <- hclust(distance_country, method = "ward.D")
dend_ward_country <- as.dendrogram(hiercluster_wardd)
plot(dend_ward_country, main = "Dendogram - Hierarchical Clustering(canberra, ward.D)", font.main = 4)

dendogram_ward_3 <- color_branches(dend_ward_country, k =3)
plot(dendogram_ward_3, main = "Dendogram - Hierarchical Clustering (3 clusters color)", font.main = 4)

cutree_hierclust_ward <- cutree( tree = hiercluster_wardd, k = 3)

princomp_hier_ward <- data.frame(prcomp(x = country_scaled, center = FALSE,scale. = FALSE )$x[,1:2],  Name = rownames(country_scaled),  Cluster = as.character(cutree_hierclust_ward), stringsAsFactors = FALSE)

ggplot(princomp_hier_ward) +
  aes(x = PC1,y = PC2,color = Cluster,fill = Cluster,label = Name ,group = Cluster) + 
  geom_point() + 
  ggrepel::geom_text_repel(color = "black",size = 3) + 
  ggtitle("Scatter plot - hierarchical cluster (Canberra, ward.D)") + 
  theme_bw() + 
  #theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

cutree_hierclust_ward

# Kmeans Clustering:
kmeans2_country <- kmeans(country_scaled, centers = 2)
factoextra::fviz_cluster( object = kmeans2_country, data = country_scaled, main = "Cluster Plot (Kmeans - 2 clusters)") + theme_classic() + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) 

kmeans3_country <- kmeans(country_scaled, centers = 3)
factoextra::fviz_cluster( object = kmeans3_country, data = country_scaled, main = "Cluster Plot (Kmeans - 3 clusters)") + theme_classic() + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) 

kmeans4_country <- kmeans(country_scaled, centers = 4)
factoextra::fviz_cluster( object = kmeans4_country, data = country_scaled, main = "Cluster Plot (Kmeans - 4 clusters)") + theme_classic() + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) 

kmeans5_country <- kmeans(country_scaled, centers = 5)
factoextra::fviz_cluster( object = kmeans5_country, data = country_scaled, main = "Cluster Plot (Kmeans - 5 clusters)") + theme_classic() + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) 

set.seed(423)
factoextra::fviz_nbclust(x = country_scaled,  FUNcluster = kmeans, method = "wss") + labs(title = "Elbow Method") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

set.seed(423)
factoextra::fviz_nbclust(x = country_scaled,  FUNcluster = kmeans, method = "silhouette") + labs(title = "Silhouette Method") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

set.seed(423)
factoextra::fviz_nbclust(x = country_scaled,  FUNcluster = kmeans, method = "gap_stat") + labs(title = "Gap Statistics") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic"))

par(mfrow = c(2, 2))
plot(silhouette(kmeans2_country$cluster, daisy(country_scaled)), 
     col = c("#cc3300", "#006600"), main = "  ", font.main = 4)
plot(silhouette(kmeans3_country$cluster, daisy(country_scaled)), 
     col = c("#cc3300", "#006600", "#0099FF"), main = " ", font.main = 4)
plot(silhouette(kmeans4_country$cluster, daisy(country_scaled)), 
     col = c("#cc3300", "#006600", "#0099FF","#AA4371"), main = " ", font.main = 4)
plot(silhouette(kmeans5_country$cluster, daisy(country_scaled)), 
     col = c("#cc3300", "#006600", "#0099FF","#AA4371", "#2C728EFF"), main = " ", font.main = 4)
title("Silhouette plot - kmeans Clusters", line = -1.5, outer = TRUE)

country_df["kmeans3"] = kmeans3_country$cluster

# Visualizing clusters in boxplots
par(mfrow = c(3, 3))
boxplot(child_mort~kmeans3,data=country_df, col = rainbow(3), xlab = "", main = "Child Mortality Rate")
boxplot(exports~kmeans3,data=country_df, col = rainbow(3) , xlab = "", main = "Exports")
boxplot(health~kmeans3,data=country_df, col = rainbow(3) , xlab = "", main = "Health")
boxplot(imports~kmeans3,data= country_df, col = rainbow(3), xlab = "", main = "Imports")
boxplot(income~kmeans3,data=country_df, col = rainbow(3), xlab = "", main = "Income")
boxplot(inflation~kmeans3,data=country_df, col = rainbow(3), xlab = "", main = "Inflation")
boxplot(life_expec~kmeans3,data=country_df, col = rainbow(3), xlab = "", main = "Life Expectancy")
boxplot(total_fer~kmeans3,data=country_df, col = rainbow(3),xlab = "", main = "Total Fertility")
boxplot(gdpp~kmeans3,data=country_df, col = rainbow(3), xlab = "", main = "GDP per capita")
title("Kmeans 3 Clusters Vs Input variables", line = -1, outer = TRUE)
#par(mfrow=c(1,1))
dev.off()

mean_kmeans3 <- country_df %>%
  group_by(kmeans3) %>% 
  summarise(n = n(), 
            child_mort = mean(child_mort),
            exports = mean(exports),
            health = mean(health),
            imports = mean(imports),
            income = mean(income),
            inflation = mean(inflation),
            life_expec = mean(life_expec),
            total_fer = mean(total_fer),
            gdpp = mean(gdpp))

mean_kmeans3

country_df["country_category"] = NA
country_df$country_category[country_df$kmeans3 == 1] <- "Under-Developed"
country_df$country_category[country_df$kmeans3 == 2] <- "Developing"
country_df$country_category[country_df$kmeans3 == 3] <- "Developed"

#Barplot of input variables and clusters: 
ggplot(data = country_df, aes(x = reorder(country,-child_mort), y = child_mort, fill = country_category)) + geom_bar(stat="identity") + xlab("Country") + ylab("Child Mortality") + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_discrete(name = "Countries Clusters") + ggtitle("Barplot of Child Mortality - Clusters") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) + theme(legend.position = c(0.9, 0.9))

ggplot(data = country_df, aes(x = reorder(country,-gdpp), y = gdpp, fill = country_category)) + geom_bar(stat="identity") + xlab("Country") + ylab("GDP") + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_discrete(name = "Countries Clusters") + ggtitle("Barplot of GDP per capita - Clusters") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) + theme(legend.position = c(0.9, 0.9))

ggplot(data = country_df, aes(x = reorder(country,-income), y = income, fill = country_category)) + geom_bar(stat="identity") + xlab("Country") + ylab("Income") + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_discrete(name = "Countries Clusters") + ggtitle("Barplot of Income - Clusters") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) + theme(legend.position = c(0.9, 0.9))

ggplot(data = country_df, aes(x = reorder(country,-life_expec), y = life_expec, fill = country_category)) + geom_bar(stat="identity") + xlab("Country") + ylab("Life expectancy") + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_discrete(name = "Countries Clusters") + ggtitle("Barplot of Life Expectancy - Clusters") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) + theme(legend.position = c(0.9, 0.9))

#Scatter plot of input variables and clusters: 

p1 <- ggplot(data = country_df, aes(x = child_mort, y = life_expec, color = country_category)) + geom_point() + xlab("Child Mortality") + ylab("Life Expectancy") + theme_bw() + theme( axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_discrete(name = "Countries Clusters") + ggtitle("Child Mortality Vs Life Expectancy - Clusters") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) +  theme(legend.position = c(0.85, 0.895))

p2 <- ggplot(data = country_df, aes(x = income, y = gdpp, color = country_category)) + geom_point() + xlab("Income") + ylab("GDP") + theme_bw() + theme( axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_discrete(name = "Countries Clusters") + ggtitle("Income Vs GDP - Clusters") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic")) +  theme(legend.position = c(0.85, 0.895))

grid.arrange(p1, p2, ncol = 2)

# Countries list
display <- ttheme_minimal( core = list(bg_params = list(fill = blues9[1:4], col=NA), fg_params = list(fontface=3)), colhead = list(fg_params=list(col="navyblue", fontface=4L)),  rowhead = list(fg_params = list(col = "blue", fontface=4L)))
dev <- subset(country_df, kmeans3 == 3)$country
dim(dev) <- c(6,6)
plot.new()
title("Developed Countries", line = -8, outer = TRUE, font.main=4)
grid.table(dev, theme = display, rows = NULL)

developing <- subset(country_df, kmeans3 == 2)$country
dim(developing) <- c(14,6)
plot.new()
title("Developing Countries", line = -2, outer = TRUE, font.main=4)
grid.table(developing, theme = display, rows = NULL)

under_dev <- subset(country_df, kmeans3 == 1)$country
under_dev <- under_dev[1:48]
dim(under_dev) <- c(8,6)
plot.new()
title("Under-Developed Countries", line = -2, outer = TRUE, font.main=4)
grid.table(under_dev, theme = display, rows = NULL)


#Visualizing clusters in map
world_map <- map_data('world')
colnames(world_map)[5] <- "country"
#data_merger <- left_join(country_df, world_map, by = "country")

#data_merger %>% filter(is.na(long))
#subset(country_data, country %in% (unique(subset(data_merger,kmeans4 == 4)$country)))

world_map["country"][world_map["country"] == "Barbuda"] <- "Antigua and Barbuda"
world_map["country"][world_map["country"] == "USA"] <- "United States"
world_map["country"][world_map["country"] == "UK"] <- "United Kingdom"
world_map["country"][world_map["country"] == "Republic of Congo"] <- "Congo, Rep."
world_map["country"][world_map["country"] == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
world_map["country"][world_map["country"] == "Laos"] <- "Lao"
world_map["country"][world_map["country"] == "Ivory Coast"] <- "Cote d'Ivoire"
world_map["country"][world_map["country"] == "Kyrgyzstan"] <- "Kyrgyz Republic"
world_map["country"][world_map["country"] == "Macedonia"] <- "Macedonia, FYR"
world_map["country"][world_map["country"] == "Micronesia"] <- "Micronesia, Fed. Sts."
world_map["country"][world_map["country"] == "Slovakia"] <- "Slovak Republic"
world_map["country"][world_map["country"] == "Saint Vincent"] <- "St. Vincent and the Grenadines"
world_map["country"][world_map["country"] == "Grenadines"] <- "St. Vincent and the Grenadines"

data_merger <- left_join(world_map, country_df, by = "country")

ggplot(data_merger, aes(x = long, y = lat, group = group, fill = factor(kmeans3))) + geom_polygon(color = 'gray') + scale_fill_manual(values = c("#1F968BFF", "#FDE725FF","#481B6DFF"), limits = c(1,2,3), labels = c("Under-Developed","Developing","Developed"), name = "Countries Clusters") + theme_bw() + ggtitle("Kmeans Country Clusters on World Map") + theme(plot.title = element_text(hjust = 0.5, face="bold.italic"), legend.position = c(.9,.1))

