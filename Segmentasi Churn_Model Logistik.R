library(readr)
# import data churn
data1 <- read.delim("clipboard", header = TRUE, sep = "\t")
head(data1)
str(data1)

# cek data
sum(is.na(data1))
sum(duplicated(data1))    

# visualisasi
library(ggplot2)

summary(data1)
table(data1$Exited)
hist(data1$Balance)
class(data1)

ggplot(data1, aes(x = factor(Exited))) +
         geom_bar(fill = "steelblue") +
         labs(title = "Distribusi Exited (Churn)", x ="Churn (0 = Tidak, 1 = Ya)", y = "Jumlah")

# Menganalisis faktor Churn
# Mengubah data kategorik
data1$Geography <- factor(data1$Geography, levels = c("France", "Spain", "Germany"))
data1$Geography <- as.numeric(data1$Geography)

data1$Gender <- ifelse(data1$Gender == "Male", 1, 0)
data1$Gender 

# Perubahan data
head(data1)

# Membagi menjadi data latih dan data uji
set.seed(42)
library(caret)

trainIndex <- createDataPartition(data1$Exited, p = 0.7, list = FALSE)
train_data1 <- data1[trainIndex, ]
test_data1 <- data1[-trainIndex, ]

# Membangun model regresi logistik
model_log <- glm(Exited~ CreditScore + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember 
                 + EstimatedSalary + Geography + Gender, 
                 data = train_data1, family = binomial)
summary(model_log)

# Evaluasi Model 
# Memprediksi hasil menggunakan model
predictions <- predict(model_log, newdata = test_data1, type = "response")

# Menentukan threshold
threshold <- 0.05
predicted_class <- ifelse(predictions > threshold, 1, 0)

# Membuat confusion matrix
confusionMatrix(as.factor(predicted_class), as.factor(test_data1$Exited))

# Akurasi model
accuracy <- mean(predicted_class == test_data1$Exited)
accuracy

# Precision
precision <- sum(predicted_class == 1 & test_data1$Exited == 1) / sum(predicted_class == 1)
precision

# Recall
recall <- sum(predicted_class == 1 & test_data1$Exited == 1) / sum(test_data1$Exited == 1)
recall

# F1-Score
f1_score <- (2 *(precision * recall)) / (precision + recall)
f1_score

# AUC-ROC
library(pROC)
roc_curve <- roc(test_data1$Exited, predictions)
auc(roc_curve)

# Menambahkan kolom probabilitas ke dataset
test_data1$PredictedProb <- predictions

# Segmentasi berdasarkan probabilitas churn 
test_data1$RiskSegment <- ifelse(test_data1$PredictedProb < 0.33, "Low Risk",
                                 ifelse(test_data1$PredictedProb < 0.66, "Medium Risk", 
                                 "High Risk"))

# Melihat distribusi risiko
table(test_data1$RiskSegment)

# Visualisasi segmen risiko
ggplot(test_data1, aes(x = factor(RiskSegment), fill = factor(Exited))) +
  geom_bar(position = "fill") +
  labs(title = "Segmentasi Risiko Churn", x = "Segmentasi Risiko", y = "Proporsi Churn")

# Mengetahui rata-rata nilai kriteria untuk tiap segmen risiko
aggregate(cbind(Age, Balance, CreditScore)~ RiskSegment, data = test_data1, FUN = mean)          



