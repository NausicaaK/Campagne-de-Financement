# Charger les bibliothèques nécessaires
library(tidyverse)
library(caret)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ROCR)
# définition du répertoire 

setwd("~/Desktop/HEC_MsC/MATH60602 An multi appl/Devroi1")

# Charger les données
members <- read.csv("MembersList.csv")
donations <- read.csv("DonationHistory.csv")
newsletter <- read.csv("NewsletterRead.csv")
social <- read.csv("SocialNetworkUsage.csv")

# Observations des données 

str(donations)
str(members)
str(newsletter)
str(social)

summary(donations)
summary(members)
summary(newsletter)
summary(social)

# Transformation des bases de donnes avant fusion

g_members <- mutate(members, membership =  2023 - Joined) # ajout une variable qui controle le nombre d'années depuis que le personne a rejoint l'organisme  

g_donation <- donations |>
  group_by(ID) |>
  summarise(Amount = sum(Amount)) # un membre fait un don sur des années différentes, l'années n'est pas importante ici pour l'analyse. On se concentre donner ou pas

# Fusionner les jeux de données
data <- g_members %>%
  left_join(g_donation, by = "ID") %>%
  left_join(newsletter, by = "email") 

social_cleaned <- social %>%
  distinct(email, .keep_all = TRUE)

  data <- data %>%
  left_join(social_cleaned, by = "email")

# Créer une variable 'TotalNewslettersRead' en ajoutant les colonnes Jan à Dec 

data <- data %>% 
mutate(newsletter = rowSums(select(., Jan:Dec), na.rm = TRUE))  # Somme des colonnes Jan à Dec 


#supprimer les variables inutiles
data <- data |>
  select(-c(LastName, FirstName, email, Name, Joined))

data <- data |>
  select(-c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))

# Créer des tranches de salaire 
data <- data %>% 
  mutate(SalaryGroup = cut(Salary,  
                           breaks = c(0, 66000, 93000, Inf),  
                           labels = c("0-66k", "66k-93k", "93k+"), 
                           right = FALSE)) 
# Remplacer les valeurs manquantes par des zéros (si nécessaire)
data[is.na(data)] <- 0

# Définir la variable cible (si le membre a fait un don)
data$Donation_2023 <- ifelse(data$Amount > 0, 1, 0)


#ANALYSE EXPLORATOIRE DES VARIABLES CREES 
data_sample <- data %>% sample_frac(0.8)

#Graphique proportions de Dons en fonction des groupes de likes

#Proportion des dons effectués en fonction des tranches de salaire
# Créer des tranches de salaire 
summary(data$Salary)

data_sample <- data_sample %>% 
  mutate(LikesGroup = cut(Likes,  
   breaks = c(0, 10, 50, 100, Inf),  
   labels = c("0-10", "10-50", "50-100", "100+"), 
    right = FALSE)) 

#Graphique proportions de Dons en fonction des groupes de likes

ggplot(data_sample, aes(x = LikesGroup, fill = factor(Donation_2023))) + 
geom_bar(position = "fill") +  # Barres empilées avec proportions 
  labs(title = "Proportion de Dons en Fonction des Groupes de Likes", 
        x = "Groupe de Likes", 
        y = "Proportion", 
        fill = "Don effectué") + 
        theme_minimal() +  # Appliquer un thème propre et minimaliste 
        scale_fill_manual(values = c("red", "blue"), labels = c("Non", "Oui")) +  # Rouge pour 'Non', Bleu pour 'Oui' 
 theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes pour plus de lisibilité 


#Graphique proportion de dons en fonction du nombre de newlestter lues

ggplot(data_sample, aes(x = as.factor(newsletter), fill = factor(Donation_2023))) + 
geom_bar(position = "fill") +  # 'fill' pour montrer les proportions 
labs(title = "Proportion de Dons en Fonction du Nombre de Newsletters Lues",  
x = "Nombre de Newsletters Lues",  
y = "Proportion",  
fill = "Don effectué") + 
theme_minimal() + 
scale_fill_manual(values = c("red", "blue"), labels = c("Non", "Oui"))  # Rouge pour non, bleu pour oui 


#Proportion des dons en fonction de la ville
ggplot(data_sample, aes(x = City, fill = factor(Donation_2023))) + 
geom_bar(position = "fill") +  # Barres empilées avec proportions 
labs(title = "Proportion de Dons en Fonction de la Ville", 
       x = "Ville", 
       y = "Proportion", 
       fill = "Don effectué") + 
  theme_minimal() +  # Appliquer un thème propre et minimaliste 
  scale_fill_manual(values = c("red", "blue"), labels = c("Non", "Oui")) +  # Rouge pour 'Non', Bleu pour 'Oui' 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes de la ville pour plus de lisibilité 

#Proportion des dons en fonction du niveau d'éducation
ggplot(data_sample, aes(x = Education, fill = factor(Donation_2023))) + 
  geom_bar(position = "fill") +  # Barres empilées avec proportions 
  labs(title = "Proportion de Dons en Fonction du Niveau d'Éducation", 
        x = "Niveau d'Éducation", 
        y = "Proportion", 
        fill = "Don effectué") + 
        theme_minimal() +  # Appliquer un thème propre et minimaliste 
        scale_fill_manual(values = c("red", "blue"), labels = c("Non", "Oui")) +  # Rouge pour 'Non', Bleu pour 'Oui' 
       theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Incliner les étiquettes 

# Exclure les valeurs manquantes dans SalaryGroup
data_sample_filtered <- data_sample %>%
  filter(!is.na(SalaryGroup))

# Créer un graphique montrant la proportion de dons effectués en fonction des tranches de salaire 

print(ggplot(data_sample_filtered, aes(x = SalaryGroup, fill = factor(Donation_2023))) + 
        geom_bar(position = "fill") +  # Barres empilées avec proportions 
        labs(title = "Proportion de Dons en Fonction des Tranches de Salaire", 
             x = "Tranche de Salaire", 
             y = "Proportion", 
             fill = "Don effectué") + 
        theme_minimal() +  # Appliquer un thème propre et minimaliste 
        scale_fill_manual(values = c("red", "blue"), labels = c("Non", "Oui")) +  # Rouge pour 'Non', Bleu pour 'Oui'
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) ) # Incliner les étiquettes pour plus de lisibilité


#coréalation entre les varibles 
cor_matrix <- cor(data_sample %>% select_if(is.numeric))
corrplot(cor_matrix, method = "circle")



#MODELISATION

# Séparer les données en ensemble d'entraînement et de test
set.seed(123)
dataShuffled <- data[sample(nrow(data)), ]
trainIndex <- createDataPartition(dataShuffled$Donation_2023, p = .80, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data[trainIndex,]
test_data <- data[-trainIndex,]

amount <- test_data$Amount
print (amount)
test_data<- test_data |>
  select(-Salary)

train_data<- train_data |>
  select(-Salary)

# Construire un modèle de régression logistique
model <- glm(Donation_2023 ~ . -ID - Amount, data = train_data, family = binomial)

#Tableau résumé avec coefficients
summary(model)
# coeffeicents du modele
coefficients <- summary(model)$coef
round(coefficients, 4)
# cotes
cote <- exp(model$coefficients)

#Tester la significativité globale à l'aide du rapport de vraisemblance
anova(model, test = 'Chisq')

# Critères d'information
np <- length(coef(model))
n <- nrow(data)
AIC(model)
# -2*logLik(modele1) + 2*np
BIC(model)
# -2*logLik(modele1) + log(n)*np


# Faire des prédictions sur l'ensemble de test
predictions <- predict(model, newdata = test_data, type = "response")
predictions[1:10]

# si point de coupure 0.5
test_data$predicted <- ifelse(predictions > 0.5, 1, 0)
#prediction1 <- as.numeric(predictions > .5)
test_data$predicted[1:10]

# ROC / AUC
library(ROCR)
roc_curve <- prediction(predictions,test_data$Donation_2023)
curve_R <- performance(roc_curve, "tpr", "fpr")
plot(curve_R, main ="Courbe ROC - Validation Externe", col= "darkblue")
abline(a=0, b=1, col ="red")

# Calcul de l'AUC 
auc_vecor <- performance(roc_curve, measure = "auc")
auc_value <- auc_vecor@y.values[[1]] 
auc_value
#EVALUATIONPERFORMANCE

# Évaluer la performance du modèle
confusionMatrix(as.factor(test_data$predicted), as.factor(test_data$Donation_2023))

# Décider qui recevra une trousse de remerciement
# Supposez que vous envoyez la trousse à ceux ayant une probabilité > 0.5
test_data$receive_kit <- ifelse(predictions > 0.5, 1, 0)
#print (test_data$receive_kit)
 
# Calculer le coût des trousses et les dons totaux
cost_per_kit <- 5
sum(is.na(test_data$receive_kit))
test_data$receive_kit[is.na(test_data$receive_kit)] <- 0
total_kits <- sum(test_data$receive_kit)
print(total_kits)
total_cost <- total_kits * cost_per_kit
amount[is.na(amount)] <- 0

total_donations <- sum(amount)
print(total_donations)
print(total_cost)

# Résultat net
net_profit <- total_donations - total_cost
print(paste("Net profit: ", net_profit))

# Sauvegarder la liste des membres qui recevront la trousse
write.csv(test_data[test_data$receive_kit == 1, "ID"], "kits_to_send.csv", row.names = FALSE)





#***********
VALIDATION CROISEE

data_sample<- data_sample |>
  select(-c(Salary, Amount))

#on prend le modele ajusté avec glm, on calcule la prédiction à l'aide de la validation croisée à 10 groupes, répétée 10 fois. 

data_sample_clean <- data_sample %>%
  filter(!is.na(SalaryGroup))


# Convertir Donation_2023 en facteur à deux niveaux
data_sample_clean$Donation_2023 <- as.factor(data_sample_clean$Donation_2023)

# Vérifier que Donation_2023 est bien un facteur à deux niveaux
levels(data_sample_clean$Donation_2023)

# Ajuster le modèle avec la fonction train() pour la classification binaire
mod <- train(Donation_2023 ~ . - ID,  # Exclure la colonne ID
             data = data_sample_clean,  
             method = "glm", 
             family = "binomial",  # Pour une régression logistique
             trControl = trainControl(method = "cv", number = 10))  # Validation croisée

# Faire des prédictions avec validation croisée
pred <- predict(mod, newdata = data_sample_clean)

# Calculer la matrice de confusion pour évaluer les performances
confusionMatrix(pred, data_sample_clean$Donation_2023)


# Obtenir les probabilités de la classe 1 (probabilité que Donation_2023 soit 1)
probs <- predict(mod, newdata = data_sample_clean, type = "prob")[, 2]

# Initialiser un data frame pour stocker les résultats
cutoffs <- seq(0, 1, by = 0.01)  # Points de coupure de 0 à 1 par pas de 0.01
results <- data.frame(coupe = cutoffs, pcorrect = NA)

# Calculer le taux de bonne classification pour chaque point de coupure
for (i in 1:length(cutoffs)) {
  cutoff <- cutoffs[i]
  
  # Prédictions binaires en fonction du point de coupure
  pred_class <- ifelse(probs >= cutoff, 1, 0)
  
  # Calculer la matrice de confusion
  confusion <- table(Prediction = pred_class, Reference = data_sample_clean$Donation_2023)
  
  # Calculer l'accuracy (taux de bonne classification)
  accuracy <- sum(diag(confusion)) / sum(confusion)
  
  # Enregistrer l'accuracy dans le tableau des résultats
  results$pcorrect[i] <- accuracy * 100  # Multiplier par 100 pour avoir un pourcentage
}

# Tracer le graphique avec ggplot2
ggplot(data = results, aes(x = coupe, y = pcorrect)) + 
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = "Point de coupure", 
       y = "Taux de bonne classification (%)", 
       subtitle = "Taux de bonne classification en fonction du point de coupure")


# on identifie et affiche le point de coupure optimal, c'est à dire le seuil qui le taux de bonne clasification le plus élevé
opt <- which.max(results$pcorrect)
knitr::kable(results[opt,], digits = 2)

#graphique de l'efficaité de recepteur (ROC) et on rapporte l'aire sous la courbe estimée à l'aide de la validatoin croisée 
roc_obj <- roc(data_sample_clean$Donation_2023, probs)
auc_value <- auc(roc_obj)

coords(roc_obj, "all", ret = "threshold")

plot(roc_obj, main = paste("Courbe ROC (AUC =", round(auc_value, 3), ")"))

# Ajouter la ligne d'égalité (ligne aléatoire pour comparaison)
abline(a = 0, b = 1, lty = 2, col = "gray")


#***********************************************************************************************************************
#MODELE 1 
data1<- data |>
  select(-c(Supporter, Shares,Education, Member))

# Séparer les données en ensemble d'entraînement et de test
set.seed(34567)
trainIndex1 <- createDataPartition(data1$Donation_2023, p = .80, 
                                  list = FALSE, 
                                  times = 1)
train_data1 <- data1[trainIndex1,]
test_data1 <- data1[-trainIndex1,]



# Construire un modèle de régression logistique
model1 <- glm(Donation_2023 ~ . -ID , data = train_data1, family = binomial)

#Tableau résumé avec coefficients
summary(model1)
# coeffeicents du modele
coefficients1 <- summary(model1)$coef
round(coefficients1, 4)
# cotes
cote <- exp(model$coefficients)

#Tester la significativité globale à l'aide du rapport de vraisemblance
anova(model, test = 'Chisq')

# Critères d'information
np <- length(coef(model))
n <- nrow(data)
AIC(model)
# -2*logLik(modele1) + 2*np
BIC(model)
# -2*logLik(modele1) + log(n)*np


# Faire des prédictions sur l'ensemble de test
predictions1 <- predict(model1, newdata = test_data1, type = "response")
#predictions[1:10]

# si point de coupure 0.5
test_data1$predicted1 <- ifelse(predictions1 > 0.5, 1, 0)
#prediction1 <- as.numeric(predictions > .5)
#test_data$predicted[1:10]

# ROC / AUC
library(pROC)
roc_curve1 <- roc(test_data1$Donation_2023, predictions1)
plot(roc_curve1, main ="Courbe ROC - Validation Externe")
auc_value1 <- auc(roc_curve1)

cat("AUC:", auc_value1,"\n")

#EVALUATIONPERFORMANCE

# Évaluer la performance du modèle
confusionMatrix(as.factor(test_data1$predicted1), as.factor(test_data$Donation_2023))

# Décider qui recevra une trousse de remerciement
# Supposez que vous envoyez la trousse à ceux ayant une probabilité > 0.5
test_data$receive_kit <- ifelse(predictions > 0.5, 1, 0)


# Calculer le coût des trousses et les dons totaux
cost_per_kit <- 5
total_kits <- sum(test_data$receive_kit)
total_cost <- total_kits * cost_per_kit
total_donations <- sum(test_data$Amount)

# Résultat net
net_profit <- total_donations - total_cost
print(paste("Net profit: ", net_profit))

# Sauvegarder la liste des membres qui recevront la trousse
write.csv(test_data[test_data$receive_kit == 1, "MemberID"], "kits_to_send.csv", row.names = FALSE)
