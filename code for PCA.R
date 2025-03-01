
# Load Libraries
library(ggplot2)
library(reshape2)
install.packages("ggrepel")
library(ggrepel)


pca_scores <- data.frame(
  PC1 = c(1, 2, 3, 4),
  PC2 = c(4, 3, 2, 1)
)


rm(data) 

data <- data.frame(
  Sample = c("Lacquered_pine", "Stained_pine", "Painted_pine", "Untreated_pine"),
  
  C4H8O_Butanone = c(18.31923522, 554.0460831, 44.35414821, 5.982075425),
  C2H4O2_Acetates = c(133.8685411, 157.7260908, 165.5886617, 79.1275609),
  C6H6_Benzene = c(14.00425686, 22.16172088, 7.373436265, 351.7986485),
  C6H8_Terpens_frag = c(1911.720196, 2834.223056, 647.3072506, 5971.594731),
  C6H9_Cyclopentenyl_carbenium = c(95.36106748, 154.1794285, 32.87144994, 3165.064871),
  C6H10_hexenol_frag = c(490.8239552, 1337.094821, 275.5614211, 203.9406235),
  C5H10O_Pentanal = c(18.85740168, 79.0308639, 11.06155408, 2.225191457),
  C7H8_Toluene = c(155.0401178, 370.219307, 40.83984233, 2597.525851),
  C7H10_Terpene_frag = c(100.8189271, 174.9925001, 39.68340496, 3502.505917),
  C7H12_Alkyl_fragment = c(9.270378343, 32.60513013, 12.00510434, 1.862552145),
  C8H8_Styrene = c(1.618060549, 3.663968102, 1.197669213, 11.85448104),
  C7H14O_Heptanal = c(10.19633641, 69.75153936, 18.20099546, 1.030714621),
  C9H12_Sesquiterepene = c(7.65642904, 11.62410603, 2.797394129, 444.2913686),
  C10H14_p_Cymene = c(17.367172, 34.48663705, 5.243249607, 416.4229628),
  C10H16_Monotrepens = c(2097.0209, 2590.273584, 732.5861951, 4268.863068),
  C9H14O_2_pentylfuran = c(24.16924329, 40.31452121, 11.90334193, 755.490491)
)


# Set row names as the sample names
rownames(data) <- data$Sample

# Remove the "Sample" column
data <- data[, -1]

# Perform PCA
pca_result <- prcomp(data, scale = TRUE)

# Summary of PCA
summary(pca_result)

# Extract the coordinates of the scores (observations)
scores <- pca_result$x[, 1:2]

# Extract the coordinates of the loadings (variables)
loadings <- pca_result$rotation[, 1:2]

pca_scores <- as.data.frame(scores)
pca_loadings <- as.data.frame(loadings)


scaling_factor <- 7  # Adjust this value to increase or decrease the arrow length

ggplot() +
  geom_point(data = pca_scores, aes(PC1, PC2), color = "black") +
  geom_text_repel(
    data = pca_scores, 
    aes(PC1, PC2, label = rownames(pca_scores)), 
    color = "black",
    max.overlaps = 20  # Increase the max.overlaps value
  ) +
  geom_segment(
    data = pca_loadings, 
    aes(x = 0, y = 0, xend = scaling_factor * PC1, yend = scaling_factor * PC2), 
    arrow = arrow(length = unit(0.2, "cm")), 
    color = "red"
  ) +
  geom_text_repel(
    data = pca_loadings, 
    aes(scaling_factor * PC1, scaling_factor * PC2, label = rownames(pca_loadings)), 
    color = "red",
    max.overlaps = 20  # Increase the max.overlaps value
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),  # Adjust text size
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 12)  # Adjust axis text size
  )









