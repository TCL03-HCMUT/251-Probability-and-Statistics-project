data2 <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",") # Read the preprocessed data
colnames(data2) <- c("Height", "Width", "Aspect_ratio", "Local", "is_Ad") # Rename columns for easier reference

#Helper function to extract summary statistics
summary_stats <- function(a) {
    var_name <- deparse(substitute(a))
    cat("Summary statistics for ", var_name, ":", sep = "")
    # summary(a)  
    cat("\n- Mean:", mean(a))
    cat("\n- Median:", median(a))
    cat("\n- Standard Deviation:", sd(a))
    cat("\n- Variance:", var(a))
    cat("\n- Min:", min(a))
    cat("\n- Max:", max(a))
    cat("\n- Quantiles:", quantile(a))
    
}

#Table 4.1:
print(table(data2$is_Ad))
adprob <- prop.table(table(data2$is_Ad))
print(adprob)

#Table 4.2:
print(table(data2$Local))
print(prop.table(table(data2$Local)))

#Figure 4.1:
X11()
sizegap <- adprob
names(sizegap) = c("Non-Ad", "Ad") # give names
pct <- round(sizegap*100)
lbls <- paste(names(sizegap), "\n", pct, "%", sep = "") # Add numbers to the pie chart
pie(sizegap, labels = lbls, col=c("#0066CC","#FF3700"), main="Proportion of Ads vs Non-Ads")   
# Add legend
legend("topright", legend = c("Non-Ad", "Ad"), fill = c("#0066CC", "#FF3700"))

#Table 4.3:
summary_stats(data2$Height)
summary_stats(data2$Width)
summary_stats(data2$Aspect_ratio)

#Figure 4.2:
X11()        # Opens a plotting window on Linux
hist(data2$Height, main = "Distribution of Height", xlab = "Height", col = "lightblue", border = "black")

#Figure 4.3:
X11()
hist(data2$Width, main = "Distribution of Width", xlab = "Width", col = "lightgreen", border = "black")

#Figure 4.4:
X11()
hist(data2$Aspect_ratio, main = "Distribution of Aspect Ratio", xlab = "Aspect Ratio", col = "lightpink", border = "black")

#Table 4.4:
print(table(data2$is_Ad, data2$Local))

#Figure 4.5:
X11()        # Opens a plotting window on Linux
# Put graphs in 1 row and 1 column
panel.cor <- function(x, y, digits = 4, prefix = "", cex.cor, ...)
{
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2, vfont = c("sans serif", "bold"))
}
par(mfrow = c(1, 1))
pairs(data2[, c("Height", "Width", "Aspect_ratio")], upper.panel = panel.cor, col = ifelse(data2$is_Ad == 1, "#FF3700", "#0066CC"), gap = 1/10, pch = 20, cex = 1.5, main = "Scatterplot Matrix of Height, Width, and Aspect Ratio" )

#Figure 4.6:
X11()        # Opens a plotting window on Linux
par(mfcol = c(1, 2)) # Put graphs in 1 row and 2 columns, filled column-wise, square plotting area, no spaces between plots
plot(data2$Height[data2$is_Ad == 1], data2$Width[data2$is_Ad == 1], main = "Distribution relatively to height and width of ad", xlab = "Height", ylab = "Width", col = "#FF3700", pch = 20, cex = 1.5)
plot(data2$Height[data2$is_Ad == 0], data2$Width[data2$is_Ad == 0], main = "Distribution relatively to height and width of non-ad", xlab = "Height", ylab = "Width", col = "#0066CC", pch = 20, cex = 1.5)

#Table 4.5:
summary_stats(data2$Height[data2$is_Ad == 0])
summary_stats(data2$Height[data2$is_Ad == 1])

#Figure 4.7:
X11()
par(mfcol = c(1, 1)) # Put graphs in 3 columns and 1 row
boxplot(data2$Height ~ data2$is_Ad, main = "Boxplot of Height by Ad Type", xlab = "Ad Type", ylab = "Height", names = c("Non-Ad", "Ad"), col = c("#0066CC", "#FF3700") )

#Table 4.6:
summary_stats(data2$Width[data2$is_Ad == 0])
summary_stats(data2$Width[data2$is_Ad == 1])

#Figure 4.8:
X11()
par(mfcol = c(1, 1)) # Put graphs in 3 columns and 1 row
boxplot(data2$Width ~ data2$is_Ad, main = "Boxplot of Width by Ad Type", xlab = "Ad Type", ylab = "Width", names = c("Non-Ad", "Ad"), col = c("#0066CC", "#FF3700"))

#Table 4.7:
summary_stats(data2$Aspect_ratio[data2$is_Ad == 0]) 
summary_stats(data2$Aspect_ratio[data2$is_Ad == 1])

#Figure 4.9:
X11()
par(mfcol = c(1, 1)) # Put graphs in 3 columns and 1 row
boxplot(data2$Aspect_ratio ~ data2$is_Ad, main = "Boxplot of Aspect Ratio by Ad Type", xlab = "Ad Type", ylab = "Aspect Ratio", names = c("Non-Ad", "Ad"), col = c("#0066CC", "#FF3700"))



