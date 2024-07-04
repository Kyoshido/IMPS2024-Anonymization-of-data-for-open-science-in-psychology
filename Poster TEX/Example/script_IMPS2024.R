###############################################################################
###############################################################################
###############################################################################

# Synthetic Data Generation in R

##############################################################################

# Packages --------------------------------------------------------------------
library(synthpop)
library(ggplot2)
library(reshape2)

# Dataset ---------------------------------------------------------------------
df <- read.csv("alzheimers_disease_data.csv")

df$Gender <- as.factor(df$Gender)
df$Ethnicity <- as.factor(df$Ethnicity)
df$EducationLevel <- as.factor(df$EducationLevel)
df$Smoking <- as.factor(df$Smoking)

# View the first few rows of the dataset
head(df)

# Summary of the dataset to understand its structure and contents
summary(df)

# Synthesize the dataset ------------------------------------------------------

# Examine the features that will be relevant to synthesising.
codebook.syn(df)

# Synthesizing the dataset
synthetic <- syn(df
                 ,method = "cart"
                 ,seed = 123
                 )
synthetic

# View the synthetic dataset
summary(synthetic)

# Compare ---------------------------------------------------------------------

orig <- df
synt <- synthetic$syn

# Correlation Analysis ........................................................
cor_original <- cor(orig[, sapply(orig, 
                                  is.numeric)])
cor_synthetic <- cor(synt[, sapply(synt,
                                   is.numeric)])

# Plotting the correlation differences
corr_diff <- abs(cor_original - cor_synthetic)
ggplot(melt(corr_diff),
       aes(Var1, Var2, fill = value)
) + 
   geom_tile() + 
   scale_fill_gradient2(low = "#33D326", #"green", 
                        high = "red",
                        mid = "white",
                        midpoint = 0.1, 
                        limit = c(0, 
                                  0.2#1
                        ),
                        name="Correlation\nDifference") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45,
                                    hjust = 1)
   ) +
   labs(#title = "Difference in Correlations Between Original and Synthetic"
      ,x = "",
      ,y = "")

ggsave("Correlation_Difference.png"
       ,dpi = 600
       ,width = 20,
       ,height = 20
       ,units = "cm"
)




# Mosaic plot for the original and synthetic data .............................
library(vcd)

orig <- df
synt <- synthetic$syn

variable <- c("Gender"
              ,"Ethnicity" 
              ,"EducationLevel"
)

mosaic(table(orig[variable]), main = "Original")
mosaic(table(synt[variable]), main = "Synthetic")
# Export png 500x400

###############################################################################
###############################################################################
###############################################################################