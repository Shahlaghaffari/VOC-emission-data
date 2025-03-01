library(metRology)
library(ggplot2)

Satterthwaite<-function(sdX,sdY,nuX,nuY) 
{
  fX<-sdX^2/(nuX+1)
  fY<-sdY^2/(nuY+1)
  en<-(fX+fY)^2
  gX<-fX^2/nuX
  gY<-fY^2/nuY
  dn<-gX+gY
  nZ<-en/dn
  floor(nZ)
  nZ
}

# This method (ref 14.3.1 in "The Bayesian Way") is based on Bayesian posterior 
# analysis, which works as long as the number of samples is 2 or greater, rather 
# than "3 or greater" in other popular methods. It creates a posterior probability
# distribution for the means for each of the two data sets, and from that a 
# distribution for the difference between them, which gives CI and p-values.

differences = function(data1,data2,alpha=0.05) {
  n1 = length(data1)
  n2 = length(data2)
  m1 = mean(data1)
  m2 = mean(data2)
  s1 = sd(data1)
  s2 = sd(data2)
  sm1 = s1*sqrt(1/n1)
  sm2 = s2*sqrt(1/n2)
  v1 = n1 - 1
  v2 = n2 - 1
  # µ1 ~ t(m1, sm1, v1) -- Posterior for mean for data1
  # µ2 ~ t(m2, sm2, v2) -- Posterior for mean for data2
  # dµ = µ1 - µ2  -- distribution of difference
  m = m1 - m2
  s = sqrt(sm1^2+sm2^2)
  v = Satterthwaite(sm1,sm2,v1,v2)
  # dµ = t(m,s,v)
  lower = qt.scaled(alpha/2,v,m,s)
  upper = qt.scaled(1-alpha/2,v,m,s)
  pVal = 2*pt.scaled(-abs(m),v,0,s)
  report = c(m,lower,upper,pVal)
  heading = c("mean","lower","upper","p value")
  rbind(heading,report)
}

# Example of TVVOc
Pu = c(1576.841128,	2480.690944)
PL= c(1357.79488,	1954.332289)
Pb= c(5385.389872, 5747.841678)
Pp= c(1157.932105,	1073.334974)
Su= c(1369.702033,	746.8294671)
Sl= c(4222.43599,	3796.285088)
Sb= c(619.9854265,	662.556)



# Combine all data into a single data frame
data <- data.frame(
  Sample = c(rep("Untreated pine", length(Pu)), rep("Lacquered pine", length(PL)), rep("Stained pine", length(Pb)), 
             rep("Painted pine", length(Pp)), rep("Untreated spruce", length(Su)), rep("Lacquered spruce", length(Sl)), 
             rep("Stained spruce", length(Sb))),
  Value = c(Pu, PL, Pb, Pp, Su, Sl, Sb)
)
# Set the order of the Sample factor
data$Sample <- factor(data$Sample, levels = c("Untreated pine", "Lacquered pine", "Stained pine",
                                              "Painted pine", "Untreated spruce", "Lacquered spruce",
                                              "Stained spruce"))
# Create box plots
ggplot(data, aes(x = Sample, y = Value, fill = Sample)) +
  geom_boxplot() +
  labs( y = "µg/m3"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  legend.position = "none")
















differences(Pu,Su)
differences(Pu,As)
differences(Pu,oak)
differences(oak,Su)
differences(As,Su)
differences(As,oak)
#treated pine
differences(Pu,PL)
differences(Pu,Pb)
differences(Pu,Pp)
differences(Pb,PL)
differences(Pp,PL)
differences(Pp,Pb)

#treated spruce
differences(Su,Sl)
differences(Su,Sb)
differences(Sb,Sl)



# Example of TVOc
Pu = c(33142.27042,	38650.89782)
PL= c(4133.109227,	6906.684739)
Pb= c(9648.67539,	8915.232068)
Pp= c(2267.247666,	2627.263603)
Su= c(735.7646083,	383.6450552)
Sl= c(806.8420177,	682.5868306)
Sb= c(571.3510751,	884.7398106)
As= c(1288.248496,	1269.792791)
oak= c(6306.691253,	5830.013617)

# Combine all data into a single data frame
data <- data.frame(
  Sample = c(rep("Untreated pine", length(Pu)), rep("Lacquered pine", length(PL)), rep("Stained pine", length(Pb)), 
             rep("Painted pine", length(Pp)), rep("Untreated spruce", length(Su)), rep("Lacquered spruce", length(Sl)), 
             rep("Stained spruce", length(Sb))),
  Value = c(Pu, PL, Pb, Pp, Su, Sl, Sb)
)
# Set the order of the Sample factor
data$Sample <- factor(data$Sample, levels = c("Untreated pine", "Lacquered pine", "Stained pine",
                                              "Painted pine", "Untreated spruce", "Lacquered spruce",
                                              "Stained spruce"))
# Create box plots
ggplot(data, aes(x = Sample, y = Value, fill = Sample)) +
  geom_boxplot() +
  labs( title = "TVOC Emissions", y = "µg/m3") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Bold and centered title
        plot.title.position = "panel",                                    # ✅ Force center alignment relative to panel
        axis.title.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5),  legend.position = "none", 
    panel.border = element_rect(colour = "black", fill = NA, size = 1))



differences(Pu,Su)
differences(Pu,As)
differences(Pu,oak)
differences(oak,Su)
differences(As,Su)
differences(As,oak)
#treated pine
differences(Pu,PL)
differences(Pu,Pb)
differences(Pu,Pp)
differences(Pb,PL)
differences(Pp,PL)
differences(Pp,Pb)

#treated spruce
differences(Su,Sl)
differences(Su,Sb)
differences(Sb,Sl)

# Example of all
Pu = c(34719.11155,	41131.58877)
PL= c(5490.904107,	8861.017028)
Pb= c(15034.06526,	14663.07375)
Pp= c(3425.179771,	3700.598578)
Su= c(2105.466642,	1130.474522)
Sl= c(5029.278007,	4478.871919)
Sb= c(1191.336502,	1547.295811)
As= c(2549.42711,	3027.629345)
oak= c(8952.544863,	9031.142067)

differences(Pu,Su)
differences(Pu,As)
differences(Pu,oak)
differences(oak,Su)
differences(As,Su)
differences(As,oak)
#treated pine
differences(Pu,PL)
differences(Pu,Pb)
differences(Pu,Pp)
differences(Pb,PL)
differences(Pp,PL)
differences(Pp,Pb)

#treated spruce
differences(Su,Sl)
differences(Su,Sb)
differences(Sb,Sl)
