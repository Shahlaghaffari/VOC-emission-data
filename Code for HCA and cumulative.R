install.packages(tidyverse)
install.packages("ggrepel")
library(tidyverse)      # Data manipulation & visualization
library(factoextra)     # PCA & clustering visualization
library(ggplot2)        # Custom plots
library(cluster)        # Clustering functions
library(ggrepel)



data <- data.frame(
  Sample = c("Spruce lasert", "Spruce Beis", "Spruce Untreated", "Aspen", "Oak", "Pine untreated", "Pine lasert", "Pine Bies", "Pine painted"),
  Formaldehyde = c(4.9, 17.1, 2.6, 23.4, 9.4, 4.8, 2.2, 407.5, 673.9),
  Methanol = c(262.0, 644.9, 224.5, 162.2, 3279.1, 303.0, 232.4, 310.0, 127.3),
  Methyl_fluoride = c(0.5, 1.1, 0.4, 0.3, 5.9, 0.5, 0.4, 0.5, 0.3),
  Cyclopropenylidene = c(16.9, 50.0, 11.7, 36.2, 6.3, 5.3, 5.9, 7.4, 7.5),
  Cyanomethylene = c(0.7, 2.0, 0.5, 1.5, 0.2, 0.2, 0.2, 0.4, 0.4),
  Acetone_fragment_25 = c(130.4, 498.2, 101.1, 152.6, 46.9, 37.9, 47.7, 57.3, 54.9),
  Isocyano_methane = c(4.6, 13.4, 3.0, 2.7, 2.8, 1.5, 1.2, 2.5, 4.2),
  Ketene_8 = c(146.3, 390.4, 193.7, 70.3, 117.4, 72.3, 71.4, 171.0, 1423.4),
  Isocyanic_acid = c(5.9, 12.9, 4.9, 3.3, 3.7, 3.2, 1.9, 5.6, 27.8),
  Acetaldehyde = c(57.6, 215.4, 37.3, 77.7, 74.4, 64.1, 34.0, 42.8, 46.6),
  Formamide = c(2.2, 7.4, 1.3, 1.0, 2.7, 2.3, 1.2, 1.7, 2.3),
  Formic_acid = c(27.5, 205.5, 94.2, -2.1, 14.2, 3.2, -2.4, 67.1, 30.7),
  Butenyne = c(6.0, 12.4, 2.8, 48.1, 1.3, 1.3, 0.9, 2.2, 2.4),
  Methyl_1_propenyl = c(0.0, 0.0, 0.0, 0.9, 0.0, 0.0, 0.0, 0.0, 0.0),
  Alkyl_fragment_8 = c(278.8, 814.6, 164.3, 86.8, 85.9, 87.6, 63.7, 193.1, 27.0),
  Butyn_3_yl = c(12.7, 34.4, 7.8, 3.9, 4.2, 4.3, 3.1, 9.1, 1.6),
  Butene_7 = c(56.3, 202.5, 54.6, 102.5, 29.1, 21.2, 20.5, 27.1, 29.6),
  Propanoyl = c(3.5, 12.3, 2.9, 4.3, 1.6, 1.3, 1.1, 2.0, 1.4),
  Acetone_6 = c(369.6, 1411.3, 96.7, 714.9, 254.2, 362.0, 88.6, 77.5, 77.1),
  Methyl_ethoxy_radical = c(16.1, 62.1, 4.5, 14.0, 11.3, 15.6, 4.1, 2.3, 3.0),
  Nitromethane = c(4.5, 5.0, 5.5, 1.6, 3.4, 2.9, 1.9, 6.8, 56.7),
  Alkyl_fragment = c(1.0, 2.9, 1.0, 6.6, 0.4, 0.1, 0.0, 0.5, 0.7),
  Cyclopentadienyl_radical = c(0.6, 1.0, 0.3, 6.7, 0.1, 0.1, 0.1, 0.1, 0.5),
  Butanol_fragment = c(27.4, 49.4, 11.3, 320.8, 3.3, 2.8, 2.3, 6.4, 15.1),
  Pyrrole = c(1.9, 3.5, 0.9, 15.4, 0.3, 0.3, 0.2, 0.6, 1.2),
  Isoprene = c(182.9, 733.5, 91.9, 123.0, 42.1, 52.1, 29.3, 82.7, 21.2),
  Pyrroline = c(9.7, 35.6, 5.5, 6.2, 2.4, 2.9, 1.6, 4.6, 1.3),
  Cyclopentane = c(23.5, 101.5, 31.4, 10.7, 7.3, 7.1, 3.6, 20.2, 4.5),
  Dimethylformamide = c(0.2, 20.4, 1.8, -0.2, 0.7, 0.0, 4.3, 1.0, 3.6),
  Propanethiol = c(2.4, 5.6, 0.2, 33.4, 0.0, 0.0, 0.0, 0.0, 0.9),
  Propanoic_acid = c(6.4, 14.7, 19.3, 2.3, 55.0, 5.0, 3.7, 13.3, 12.6),
  Butanal = c(18.3, 554.0, 44.4, 6.0, 23.9, 14.1, 119.1, 9.9, 51.0),
  Acetic_acid = c(133.9, 157.7, 165.6, 79.1, 101.1, 85.1, 51.7, 347.8, 2944.3),
  Hexadiyn = c(0.5, 1.1, 0.2, 3.9, 0.1, 0.0, 0.0, 0.1, 0.3),
  Ethylbenzene_fragment = c(14.0, 22.2, 7.4, 351.8, 2.6, 1.7, 1.5, 3.1, 8.8),
  Pyridine = c(4.9, 7.5, 1.7, 38.1, 0.4, 0.3, 0.3, 0.7, 3.2),
  Hexenal_fragment = c(1911.7, 2834.2, 647.3, 5971.6, 159.3, 124.7, 119.0, 164.9, 1221.0),
  Cyclopentenyl_carbenium = c(95.4, 154.2, 32.9, 3165.1, 9.2, 7.3, 6.9, 9.3, 61.2),
  Cyclohexene = c(490.8, 1337.1, 275.6, 203.9, 142.9, 134.3, 102.9, 352.8, 33.6),
  Dimethyl_propyne_amine = c(26.4, 68.9, 15.6, 6.8, 8.6, 8.1, 6.2, 19.1, 2.2),
  Pentenal = c(10.1, 38.4, 7.2, 0.1, 1.9, 1.1, 0.3, 9.9, 0.3),
  Pentanal = c(18.9, 79.0, 11.1, 2.2, 6.5, 6.2, 3.3, 5.7, 2.5),
  Ethyl_acetamide = c(0.6, 3.7, 0.2, -0.1, 0.0, 0.0, -0.2, 0.3, 0.0),
  Ethyl_acetate = c(5.0, 14.3, 5.9, 0.9, 3.1, 3.1, 2.2, 5.9, 13.6),
  Diethyl_sulphide = c(2.0, 6.5, 1.4, 37.7, 0.0, 0.0, 0.0, 0.0, 0.0),
  Benzyl_radical = c(14.5, 24.1, 6.2, 251.3, 1.5, 0.9, 1.0, 1.5, 9.8),
  Toluene = c(155.0, 370.2, 40.8, 2597.5, 17.0, 10.6, 7.4, 14.2, 45.9),
  Methyl_pyridine = c(10.7, 24.6, 2.0, 263.0, 0.1, -0.7, -0.9, 0.2, 3.2),
  Trepenes_fragment = c(100.8, 175.0, 39.7, 3502.5, 14.1, 9.6, 7.4, 16.6, 61.1),
  Furfural_2 = c(3.2, 5.3, 1.4, 218.3, 0.5, 0.3, 0.2, 1.4, 5.4),
  Alkyl_fragment_3 = c(9.3, 32.6, 12.0, 1.9, 4.6, 2.6, 1.3, 4.5, 51.9),
  E_2_hexenal_9 = c(2.2, 9.2, 2.2, 0.4, 0.5, 0.5, 0.1, 1.8, 1.2),
  Hexanal_9 = c(20.7, 64.9, 15.0, 2.3, 7.8, 6.5, 4.8, 9.8, 1.5),
  Styrene_5 = c(1.6, 3.7, 1.2, 11.9, 2.0, 1.2, 0.6, 2.1, 1.5),
  Benzaldehyde_2 = c(25.8, 29.8, 23.0, 125.0, 2.2, 1.1, 2.0, 4.8, 9.4),
  Dimethyl_pyridine_11 = c(4.0, 4.8, 3.1, 44.4, 0.4, 0.2, 0.4, 0.5, 1.6),
  Terpene_fragment_2 = c(22.6, 43.1, 18.5, 91.1, 3.0, 1.7, 1.3, 4.4, 5.9),
  Cyclohexanecarbonitrile = c(0.9, 1.7, 0.7, 5.0, 0.1, 0.1, 0.1, 0.4, 0.7),
  Cyclopent_trione = c(4.0, 14.3, 7.2, 1.3, 1.1, 0.8, 0.4, 2.1, 1.9),
  Heptanonitrile_3 = c(0.2, 0.7, 0.3, 0.2, 0.1, 0.0, 0.0, 0.2, 0.2),
  Heptenal_3 = c(6.2, 20.2, 6.7, 0.2, 0.8, 0.7, 0.3, 2.1, 0.2),
  Heptanal_4 = c(10.2, 69.8, 18.2, 1.0, 2.4, 1.9, 1.1, 6.4, 1.5),
  Butyl_acetate_8 = c(4.8, 21.6, 5.4, 0.8, 1.8, -0.2, -0.5, 1.1, 0.4),
  Propyl_sulfide_2 = c(0.0, 0.7, 0.8, 8.4, 0.0, 0.0, 0.0, 0.0, 0.0),
  Terpenes_6 = c(7.7, 11.6, 2.8, 444.3, 2.0, 0.0, 0.0, 1.4, 9.9),
  Dimethyl_benzenamine_6 = c(1.6, 2.3, 0.6, 45.5, 0.4, 0.0, 0.0, 0.2, 1.2),
  Sesquiterpene_fragment_2 = c(3.1, 6.8, 2.2, 43.6, 0.5, 0.3, 0.2, 0.8, 1.2),
  p_Cymenene_13 = c(7.2, 10.0, 1.8, 12.3, 0.3, 0.0, 0.0, 0.0, 1.6),
  Butylbenzene_7 = c(17.4, 34.5, 5.2, 416.4, 1.3, 0.5, 0.4, 1.7, 8.2),
  Amphetamine_7 = c(8.0, 14.2, 2.1, 294.6, 0.4, 0.2, 0.1, 0.5, 4.0),
  d_Limonene_16 = c(2097.0, 2590.3, 732.6, 4268.9, 148.3, 119.5, 117.8, 237.0, 1606.1),
  Methyl_t_butylpyrazole_4 = c(24.2, 40.3, 11.9, 755.5, 3.2, 3.0, 2.1, 9.7, 9.0),
  Dimethylhistamine = c(0.7, 1.2, 0.4, 8.0, 0.1, 0.1, 0.1, 0.8, 0.4)
)

rownames(data) <- data$Sample
data <- data[, -1]  # Remove the "Sample" column

head(data)

scaled_data <- scale(data)


# Compute the distance matrix
dist_matrix <- dist(scaled_data, method = "euclidean")

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", cex = 0.9)

# Cut the dendrogram into 3 clusters
clusters <- cutree(hclust_result, k = 4)

# Add cluster labels to the original data
data$Cluster <- clusters
print(data)

# Perform k-means clustering (e.g., k = 4)
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(scaled_data, centers = 5, nstart = 25)

# Add cluster labels to the original data
data$Cluster <- kmeans_result$cluster
print(data)

# Perform PCA for visualization
pca_result <- prcomp(scaled_data, scale = TRUE)
scores <- pca_result$x






######################################################################################################

# Extract loadings (rotation matrix)
loadings <- pca_result$rotation

# Calculate the percentage contribution of each compound to PC1
pc1_loadings <- loadings[, 1]  # Loadings for PC1
pc1_contribution <- (pc1_loadings^2 / sum(pc1_loadings^2)) * 100  # Percentage contribution

# Create a data frame for visualization
pc1_contribution_df <- data.frame(
  Compound = names(pc1_contribution),
  Contribution_PC1 = round(pc1_contribution, 2)
)

# Sort the data frame by contribution in descending order
pc1_contribution_df <- pc1_contribution_df[order(-pc1_contribution_df$Contribution_PC1), ]

# Print the contribution of each compound to PC1
print(pc1_contribution_df)

# Optional: Plot the contributions
library(ggplot2)

ggplot(pc1_contribution_df, aes(x = reorder(Compound, -Contribution_PC1), y = Contribution_PC1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Percentage Contribution of Compounds to PC1",
       x = "Compound",
       y = "Contribution to PC1 (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










######################################################################################################






# Calculate the variance explained by each PC
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Calculate the cumulative variance explained
cumulative_variance_explained <- cumsum(variance_explained)

# Print the cumulative variance explained
print(cumulative_variance_explained)

# Plot the cumulative variance explained
plot(cumulative_variance_explained, type = "b", pch = 19, col = "blue",
     xlab = "Principal Component", ylab = "Cumulative Variance Explained",
     main = "Cumulative Variance Explained by Principal Components")
abline(h = 0.95, col = "red", lty = 2)  # Optional: Add a line at 95% variance explained

# Add labels to points
text(1:length(cumulative_variance_explained), cumulative_variance_explained, 
     labels = round(cumulative_variance_explained, 2), pos = 3, cex = 0.8, col = "red")
##########################################################################################################################

# Plot the clusters
library(ggplot2)
library(ggrepel)




# Plot the clusters

par(mar = c(5, 10,10, 5))
library(ggplot2)
ggplot(data.frame(scores), aes(x = PC1, y = PC2, color = as.factor(kmeans_result$cluster))) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = rownames(scores)), box.padding = 0.5, max.overlaps = Inf) +
  labs(title = "k-means Clustering (k = 5)", x = "PC1", y = "PC2", color = "Cluster") +
    theme_minimal()+
  xlim(-7,10)   # Adjust limits dynamically
ylim(-10,10) 

# Analyze PCA loadings to determine which VOCs contribute most to the separation
loadings <- pca_result$rotation

# Print the loadings for the first two principal components (PC1 and PC2)
print(loadings[, 1:2])

# Visualize the loadings for PC1 and PC2
library(ggplot2)
par(mar = c(5, 6,5, 5))
loadings_df <- data.frame(VOC = rownames(loadings), PC1 = loadings[, 1], PC2 = loadings[, 2])



ggplot(loadings_df, aes(x = PC1, y = PC2, label = VOC)) +
  geom_point(size = 2, color = "blue") +
  geom_text(vjust = 2, hjust =2, color = "red") +
  labs(title = "PCA Loadings for PC1 and PC2", x = "PC1 Loading", y = "PC2 Loading") +
  theme_minimal()+
xlim(-0.3,0.4) +  # Adjust limits dynamically
ylim(-0.4,0.4) 

# Identify the VOCs with the highest absolute loadings for PC1 and PC2
top_vocs_pc1 <- loadings_df[order(abs(loadings_df$PC1), decreasing = TRUE), ]
top_vocs_pc2 <- loadings_df[order(abs(loadings_df$PC2), decreasing = TRUE), ]

print("Top VOCs contributing to PC1:")
print(head(top_vocs_pc1))

print("Top VOCs contributing to PC2:")
print(head(top_vocs_pc2))





############################################################################################################################


#     FILTERATION


# Original data
data <- data.frame(
  Sample = c("Pine lasert", "Pine Bies", "Spruce lasert", "Spruce Untreated", 
             "Spruce Beis", "Pine painted", "Pine untreated", "Aspen", "Oak"),
  Formald. = c(0.0, 17.1, 9.4, 0.0, 0.0, 0.0, 23.4, 407.5, 1136.6),
  Methanol = c(262.0, 644.9, 3279.1, 303.0, 232.4, 224.5, 162.2, 310.0, 214.7),
  Alkyl_frag = c(130.4, 498.2, 46.9, 37.9, 47.7, 101.1, 152.6, 57.3, 92.6),
  Ketene = c(146.3, 390.4, 117.4, 72.3, 71.4, 193.7, 70.3, 171.0, 2400.9),
  Acetaldehyde = c(57.6, 215.4, 74.4, 64.1, 34.0, 37.3, 77.7, 42.8, 78.6),
  Formic_acid = c(27.5, 205.5, 14.2, 0.0, 0.0, 94.2, 0.0, 67.1, 51.7),
  Aldehydes_frag = c(278.8, 814.6, 85.9, 87.6, 63.7, 164.3, 86.8, 193.1, 45.5),
  C4H8_Alkyl_frag = c(56.3, 202.5, 29.1, 21.2, 20.5, 54.6, 102.5, 27.1, 49.9),
  Acetone = c(369.6, 1411.3, 254.2, 362.0, 88.6, 96.7, 714.9, 77.5, 130.1),
  Nitromethane = c(0.0, 0.0, 0.0, 0.0, 0.0, 5.5, 0.0, 6.8, 95.6),
  Terpene_frag = c(27.4, 49.4, 0.0, 0.0, 0.0, 11.3, 320.8, 6.4, 25.4),
  Isoprene = c(182.9, 733.5, 42.1, 52.1, 29.3, 91.9, 123.0, 82.7, 35.8),
  Alcohol_frag = c(23.5, 101.5, 7.3, 7.1, 3.6, 31.4, 10.7, 20.2, 7.7)
)

# Remove Pine lasert, Pine Bies, and Pine painted
data_filtered <- subset(data, !Sample %in% c("Pine lasert", "Pine Bies", "Pine painted"))

# View the filtered data
print(data_filtered)

rownames(data_filtered) <- data_filtered$Sample
data <- data_filtered[, -1]  # Remove the "Sample" column

head(data)

scaled_data <- scale(data)


# Compute the distance matrix
dist_matrix <- dist(scaled_data, method = "euclidean")

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", cex = 0.9)

# Cut the dendrogram into 3 clusters
clusters <- cutree(hclust_result, k = 4)

# Add cluster labels to the original data
data$Cluster <- clusters
print(data)

# Perform k-means clustering (e.g., k = 3)
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Add cluster labels to the original data
data$Cluster <- kmeans_result$cluster
print(data)

# Perform PCA for visualization
pca_result <- prcomp(scaled_data, scale = TRUE)
scores <- pca_result$x

# Plot the clusters
library(ggplot2)
ggplot(data.frame(scores), aes(x = PC1, y = PC2, color = as.factor(kmeans_result$cluster))) +
  geom_point(size = 4) +
  geom_text(aes(label = rownames(data)), vjust = 1.5, hjust = 1.5) +
  labs(title = "k-means Clustering (k = 3)", x = "PC1", y = "PC2", color = "Cluster") +
  theme_minimal()

