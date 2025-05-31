library(readr)
library(scales)
library(ggplot2)
library(gridExtra)
library(GGally)
library(dplyr)
library(tidyr)
library(car)
library(patchwork)
library(rsample)
library(boot)
library(caret)
library(MASS)
library(AER)
library(pROC)

################  Papildomos funkcijos  ###################
isskirtys<-function(modelis){
  plt1<-ggplot(data = NULL, aes(x = seq_along(cooks.distance(modelis)), y = cooks.distance(modelis))) +
    geom_point(color = "#228B22", shape = 16) +
    labs(x = "Stebėjimo indeksas",y = "Kuko atstumas") +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "gray", linewidth = 0.5),  
          panel.grid = element_line(color = "gray", linewidth = 0.5),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12), 
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
  
  plt2<-ggplot(data = NULL, aes(x = seq_along(rstandard(modelis)), y = rstandard(modelis))) +
    geom_point(color = "#228B22", shape = 16) +
    geom_hline(yintercept = c(-3, 3), color = "red", linetype = "dashed", linewidth=1) + 
    scale_y_continuous(limits = c(min(rstandard(modelis))-1,max(rstandard(modelis))+1), 
                       breaks=seq(min(floor(rstandard(modelis)))-1,floor(max(rstandard(modelis)))+1,1))+
    labs(x = "Stebėjimo indeksas",y = "Standartizuota reikšmė") +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "gray", linewidth = 0.5),  
          panel.grid = element_line(color = "gray", linewidth = 0.5),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12), 
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
  
  plt3<-ggplot(data = NULL, aes(x = seq_along(rstudent(modelis)), y = rstudent(modelis))) +
    geom_point(color = "#228B22", shape = 16) +
    geom_hline(yintercept = c(-3, 3), color = "red", linetype = "dashed", linewidth=1) + 
    scale_y_continuous(limits = c(floor(min(rstudent(modelis)))-1,floor(max(rstudent(modelis)))+1), 
                       breaks=seq(floor(min(rstudent(modelis)))-1,floor(max(rstudent(modelis)))+1,1))+
    labs(x = "Stebėjimo indeksas",y = "Stjudentizuota reikšmė") +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "gray", linewidth = 0.5),  
          panel.grid = element_line(color = "gray", linewidth = 0.5),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12), 
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
  
  graf1 <- (plt1 / (plt2|plt3))
  print(graf1)
}

tikslumas<-function(modelis){
  tikimybes <- predict(modelis, newdata = test_aibe, type = "response")
  modelio_klases <- ifelse(tikimybes > 0.5, 1, 0)
  
  #Klasifikavimo lentelė
  conf_matrix <- table(Tikros = test_aibe$label,Prog = modelio_klases)
  print("Klasifikavimo lentele:")
  print(conf_matrix)
  
  TP<-conf_matrix[2,2] 
  TN<-conf_matrix[1,1]  
  FP<-conf_matrix[1,2] 
  FN<- conf_matrix[2,1] 
  
  #Tikslumas
  tikslumas <- (TN+TP)/(TN+TP+FP+FN)
  
  #print(paste("Tikslumas:",tikslumas))
  
  #Jautrumas
  jautrumas <- TP/(TP+FN)
  #print(paste("Jautrumas:",jautrumas))
  
  #Specifiskumas
  spec <- TN/(TN+FP)
  #print(paste("Specifiskumas:", spec))
  
  #Preciziskumas
  prec<-TP/(TP+FP)
  #print(paste("Preziciskumas:",prec))
  
  #F1 
  F1 <- (2*(prec*jautrumas))/(prec+jautrumas)
  #print(paste("F_1:", F1))
  
  #F0.5
  F0.5 <- ((1+0.5^2)*(prec*jautrumas))/(0.5^2*prec+jautrumas)
  #print(paste("F_0.5:", F0.5))
  
  #F2 
  F2 <- ((1+2^2)*(prec*jautrumas))/(4*prec+jautrumas)
  #print(paste("F_2:", F2))
  
  #Matthew koreliacijos koef.
  #MCC<-((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  #print(paste("Matthews koreliacijos koeficientas (MCC):", round(MCC, 3)))
  
  #Yoden indeksas
  Yoden_indeksas<-jautrumas+spec-1
  #print(paste("Yoden indeksas:", Yoden_indeksas))
  
  ## ROC
  roc_kreive <- roc(test_aibe$label, tikimybes)
  plotas_po_roc <- auc(roc_kreive)
  #print(paste("AUC:", round(plotas_po_roc, 3)))
  
  roc_df <- data.frame(
    Specificity = rev(1 - roc_kreive$specificities),  
    Sensitivity = rev(roc_kreive$sensitivities)       
  )
  print(
  ggplot(roc_df, aes(x = Specificity, y = Sensitivity)) +
    geom_step(color = "#228B22", linewidth = 1.8, direction = "hv") +  
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) + 
    annotate("label", x = 0.8, y = 0.3, label = paste("Plotas po ROC kreive:", round(plotas_po_roc, 3)), 
             color = "black", size = 5, fill = "white", label.size = 0.5, fontface = "bold") + 
    labs(title = "ROC kreivė", x = "1 - Specifiškumas", y = "Jautrumas") +
    theme_minimal() +  
    theme(
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )
  )
  cat("Tiklsumas: ",tikslumas, "\n Jautrumas: ", jautrumas, "\n Specifiskumas:", spec,"\n Preciziskumas: ",prec, "\n F1: ", F1,
      "\n F0.5: ",F0.5, "\n F2: ",F2, "\n Yoden:", Yoden_indeksas,"\n AUC: ",plotas_po_roc,"\n AIC: ",AIC(modelis))
  
}



######## Duomenys ########
data <- read.csv("email_phishing_data.csv")
data$label<-as.factor(data$label)

label_1_data <- data %>%
  filter(label == 1)

label_0_data <- data %>%
  filter(label == 0) %>%
  slice_head(n = 6949)

data1 <- bind_rows(label_1_data, label_0_data)
table(data1$label)

##################### Pradinė analize ##########################
pavadinimai <- c(  
  num_words = "Žodžių kiekis",
  num_unique_words = "Unikalūs žodžiai",
  num_stopwords = "Jungtukai",
  num_links = "Nuorodos",
  num_unique_domains = "Domenai",
  num_email_addresses = "El. pašto adresai",
  num_spelling_errors = "Rašybos klaidos",
  num_urgent_keywords = "Raktažodžiai"
)


data1 %>%
  pivot_longer(cols = c(num_words,num_unique_words,num_stopwords,num_links,num_unique_domains,num_email_addresses,
                        num_spelling_errors,num_urgent_keywords), 
               names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = factor(label), y = log(Value+0.001), fill = factor(label))) + 
  geom_boxplot(color = "black") +  
  facet_wrap(~ Variable, scales = "free", ncol = 3, labeller = labeller(Variable = pavadinimai)) +  
  labs( y = "Logaritmuota  reikšmė", x='',fill = "") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(vjust = 1, size = 12),
        axis.text.y = element_text(vjust = 1, size = 12),
        axis.title.x = element_text(vjust = 1, size = 12),
        axis.title.y = element_text(vjust = 1, size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +  
  scale_fill_manual(values = c("0" = "#228B22", "1" = '#B22222'),
                    name = "Laiško tipas:",labels = c("0 - Saugus", "1 - Kenksmingas"))

data1 %>% 
  dplyr::select(-label)%>%
  ggpairs(., columnLabels = pavadinimai,
          diag = list(continuous = wrap("densityDiag", fill = "lightblue")),
          upper = list(continuous = wrap("cor", size = 10, color = "black"))) +  
  theme_bw() +  
  theme(strip.text = element_text(size = 15, face = "bold", color = "black"), 
        axis.text = element_text(size = 10, color = "black"), 
        axis.title = element_text(size = 12, color = "black"),
        axis.text.x = element_text(vjust = 1, hjust = 1, size = 10),
        axis.title.x = element_text(vjust = 1, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold")) +
  labs(title = "Kintamųjų sklaidos diagramų matrica")

##################  Binarinio atsako modelis #########################
#Mokymo ir testavimo aibe:
set.seed(976) 
split_0 <- initial_split(data1 %>% filter(label == 0), prop = 0.8)
split_1 <- initial_split(data1 %>% filter(label == 1), prop = 0.8)

train_0 <- training(split_0)
test_0  <- testing(split_0)

train_1 <- training(split_1)
test_1  <- testing(split_1)

train_aibe <- bind_rows(train_0, train_1)
test_aibe  <- bind_rows(test_0, test_1)

train_aibe <- train_aibe %>% sample_frac(1)
test_aibe  <- test_aibe %>% sample_frac(1)


table(train_aibe$label)/nrow(train_aibe)*100
table(test_aibe$label)/nrow(test_aibe)*100

set.seed(123)
train_aibe <- train_aibe[sample(nrow(train_aibe)), ]


#logit modelis ------
logit_modelis <- glm(label ~ num_words+ num_unique_words + num_stopwords +
                       num_links + num_unique_domains +
                       num_spelling_errors + num_urgent_keywords + num_email_addresses,
                     data = train_aibe,
                     family = binomial(logit))

isskirtys(logit_modelis) #išskircių yra, jas šaliname

train_aibe2 <- train_aibe[rstandard(logit_modelis) < 3 & rstandard(logit_modelis) > -3, ]

logit_modelis1 <- glm(label ~ num_words+ num_unique_words + num_stopwords +
                       num_links + num_unique_domains +
                       num_spelling_errors + num_urgent_keywords + num_email_addresses,
                     data = train_aibe2,
                     family = binomial(logit))

isskirtys(logit_modelis1) #išskircių nebera
summary(logit_modelis1)
vif(logit_modelis1) # 5 kovariantes multikolinearios: num_words, num_unique_words, num_stopwords,
#num_links ir num_unique_domains
#modeliai kandidatai -----
#num_words
logit_modelis_num_words <- glm(label ~ num_words + num_spelling_errors + num_urgent_keywords + num_email_addresses,
                               data = train_aibe2,
                               family = binomial(logit))

summary(logit_modelis_num_words) #num_spelling_errors nereikšmingas
vif(logit_modelis_num_words)

logit_modelis_num_words <- glm(label ~ num_words + num_urgent_keywords + num_email_addresses,
                               data = train_aibe2,
                               family = binomial(logit))

summary(logit_modelis_num_words) #num_words nereikšmingas
vif(logit_modelis_num_words)

logit_modelis_num_words <- glm(label ~ num_urgent_keywords + num_email_addresses,
                               data = train_aibe2,
                               family = binomial(logit))

summary(logit_modelis_num_words)
vif(logit_modelis_num_words)


#num_unique_words
logit_modelis_num_unique_words <- glm(label ~ num_unique_words + num_spelling_errors + num_urgent_keywords + num_email_addresses,
                               data = train_aibe2,
                               family = binomial(logit))

summary(logit_modelis_num_unique_words)
vif(logit_modelis_num_unique_words)

#num_stopwords
logit_modelis_num_stopwords <- glm(label ~ num_stopwords + num_spelling_errors + num_urgent_keywords + num_email_addresses,
                                      data = train_aibe2,
                                      family = binomial(logit))

summary(logit_modelis_num_stopwords) #num_stopwords nereikšmingas
vif(logit_modelis_num_stopwords)

logit_modelis_num_stopwords <- glm(label ~ num_spelling_errors + num_urgent_keywords + num_email_addresses,
                                   data = train_aibe2,
                                   family = binomial(logit))

summary(logit_modelis_num_stopwords) #num_spelling_errors nereikšmingas
vif(logit_modelis_num_stopwords)

logit_modelis_num_stopwords <- glm(label ~ num_urgent_keywords + num_email_addresses,
                                   data = train_aibe2,
                                   family = binomial(logit))

summary(logit_modelis_num_stopwords)
vif(logit_modelis_num_stopwords)


#num_links
logit_modelis_num_links <- glm(label ~ num_links + num_spelling_errors + num_urgent_keywords + num_email_addresses,
                                   data = train_aibe2,
                                   family = binomial(logit))

summary(logit_modelis_num_links) #num_spelling_errors nereikšmingas
vif(logit_modelis_num_links)

logit_modelis_num_links <- glm(label ~ num_links + num_urgent_keywords + num_email_addresses,
                               data = train_aibe2,
                               family = binomial(logit))

summary(logit_modelis_num_links) 
vif(logit_modelis_num_links)

#num_unique_domains
logit_modelis_num_unique_domains <- glm(label ~ num_unique_domains + num_spelling_errors + num_urgent_keywords + num_email_addresses,
                               data = train_aibe2,
                               family = binomial(logit))

summary(logit_modelis_num_unique_domains) #num_spelling_errors nereikšmingas
vif(logit_modelis_num_unique_domains)


logit_modelis_num_unique_domains <- glm(label ~ num_unique_domains + num_urgent_keywords + num_email_addresses,
                                        data = train_aibe2,
                                        family = binomial(logit))

summary(logit_modelis_num_unique_domains) 
vif(logit_modelis_num_unique_domains)

aic_values <- c(
  num_links = AIC(logit_modelis_num_links),
  num_stopwords2 = AIC(logit_modelis_num_stopwords),
  num_unique_domains = AIC(logit_modelis_num_unique_domains),
  num_unique_words = AIC(logit_modelis_num_unique_words),
  num_words2 = AIC(logit_modelis_num_words)
)

which.min(aic_values)
aic_values[which.min(aic_values)]

logit_modelis_num_unique_words <- glm(label ~ num_unique_words + num_spelling_errors + num_urgent_keywords + num_email_addresses,
                                      data = train_aibe2,
                                      family = binomial(logit))

summary(logit_modelis_num_unique_words)
vif(logit_modelis_num_unique_words)

saveika_logit<-glm(label ~ num_unique_words + num_spelling_errors*num_urgent_keywords + num_email_addresses,
                                        data = train_aibe2,
                                        family = binomial(logit))
summary(saveika_logit) 
AIC(logit_modelis_num_unique_words)
AIC(saveika_logit)
anova(logit_modelis_num_unique_words, saveika_logit)

tikslumas(saveika_logit)

#probit modelis ----
probit_modelis <- glm(label ~ num_words+ num_unique_words + num_stopwords +
                        num_links + num_unique_domains +
                        num_spelling_errors + num_urgent_keywords + num_email_addresses,
                      data = train_aibe,
                      family = binomial(probit),
                      control = glm.control(maxit = 10000))


isskirtys(probit_modelis) #isškircių yra

train_aibe2.2 <- train_aibe[rstandard(probit_modelis) < 3 & rstandard(probit_modelis) > -3, ]

probit_modelis1 <- glm(label ~ num_words+ num_unique_words + num_stopwords +
                        num_links + num_unique_domains +
                        num_spelling_errors + num_urgent_keywords + num_email_addresses,
                      data = train_aibe2.2,
                      family = binomial(probit),
                      control = glm.control(maxit = 10000))
isskirtys(probit_modelis1)
vif(probit_modelis1) # 5 kovariantes multikolinearios: num_words, num_unique_words, num_stopwords
# num_links irnum_unique_domains

#modeliai kandidatai -----
#num_words
probit_modelis_num_words <- glm(label ~ num_words + num_urgent_keywords + num_spelling_errors + num_email_addresses,
                                data = train_aibe2,
                                family = binomial(probit))

summary(probit_modelis_num_words) #num_spelling_errors nereikšmingas
vif(probit_modelis_num_words)

probit_modelis_num_words <- glm(label ~ num_words + num_urgent_keywords + num_email_addresses,
                                data = train_aibe2,
                                family = binomial(probit))

summary(probit_modelis_num_words) #num_words nereikšmingas
vif(probit_modelis_num_words)

probit_modelis_num_words <- glm(label ~ num_urgent_keywords + num_email_addresses,
                                data = train_aibe2,
                                family = binomial(probit))

summary(probit_modelis_num_words)
vif(probit_modelis_num_words)

#num_unique_words
probit_modelis_num_unique_words <- glm(label ~ num_unique_words + num_urgent_keywords + num_spelling_errors + num_email_addresses,
                                       data = train_aibe2,
                                       family = binomial(probit))

summary(probit_modelis_num_unique_words)
vif(probit_modelis_num_unique_words)

#num_stopwords
probit_modelis_num_stopwords <- glm(label ~ num_stopwords + num_urgent_keywords + num_spelling_errors + num_email_addresses,
                                    data = train_aibe2,
                                    family = binomial(probit))

summary(probit_modelis_num_stopwords) #num_stopwords nereikšmingas
vif(probit_modelis_num_stopwords)

probit_modelis_num_stopwords <- glm(label ~ num_urgent_keywords + num_email_addresses,
                                    data = train_aibe2,
                                    family = binomial(probit))

summary(probit_modelis_num_stopwords) #num_spelling_errors nereikšmingas
vif(probit_modelis_num_stopwords)

#num_links
probit_modelis_num_links <- glm(label ~ num_links + num_urgent_keywords + num_spelling_errors + num_email_addresses,
                                data = train_aibe2,
                                family = binomial(probit))

summary(probit_modelis_num_links) #num_spelling_errors nereikšmingas
vif(probit_modelis_num_links)

probit_modelis_num_links <- glm(label ~ num_links + num_urgent_keywords + num_email_addresses,
                                data = train_aibe2,
                                family = binomial(probit))

summary(probit_modelis_num_links) 
vif(probit_modelis_num_links)

#num_unique_domains
probit_modelis_num_unique_domains <- glm(label ~ num_unique_domains + num_urgent_keywords + num_spelling_errors + num_email_addresses,
                                         data = train_aibe2,
                                         family = binomial(probit))

summary(probit_modelis_num_unique_domains)
vif(probit_modelis_num_unique_domains)

aic_values <- c(
  num_links = AIC(probit_modelis_num_links),
  num_stopwords2 = AIC(probit_modelis_num_stopwords),
  num_unique_domains = AIC(probit_modelis_num_unique_domains),
  num_unique_words = AIC(probit_modelis_num_unique_words),
  num_words2 = AIC(probit_modelis_num_words)
)

which.min(aic_values)
aic_values[which.min(aic_values)]

probit_modelis_num_unique_domains <- glm(label ~ num_unique_domains + num_urgent_keywords + num_spelling_errors + num_email_addresses,
                                         data = train_aibe2,
                                         family = binomial(probit))

summary(probit_modelis_num_unique_domains)
vif(probit_modelis_num_unique_domains)

saveika_probit <- glm(label ~ num_unique_domains + num_urgent_keywords*num_spelling_errors + num_email_addresses,
                                         data = train_aibe2,
                                         family = binomial(probit))

summary(saveika_probit)
AIC(probit_modelis_num_unique_domains)
AIC(saveika_probit)
anova(probit_modelis_num_unique_domains, saveika_probit)
tikslumas(saveika_probit)


#kuri imti logit ar probit - žiurime kuris geriau veikia ant testavimo aibes

tikslumas(saveika_logit)
tikslumas(saveika_probit) 
#pagal tiksluma testavimo aibeje logit yra geresnis
AIC(saveika_logit)
AIC(saveika_probit)
#pagal AIC tinka labiau logit
summary(saveika_logit)
#parametrų interpretacija
exp(coef(saveika_logit))
exp(confint.default(saveika_logit, level=0.95))



################################################################################################
###### Modelis įvykių skaičiui prognozuoti ######
################################################################################################
data1 %>%
  count(num_spelling_errors) %>%
  arrange(num_spelling_errors) %>%
  ggplot(aes(x = factor(num_spelling_errors), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Rašybos klaidų pasiskirstymas", y = "Dažnis", title = "Rašybos klaidų skaičius") +
  geom_vline(xintercept = which(sort(unique(data1$num_spelling_errors)) == 50), 
             color = "red", linetype = "dashed") +
  scale_x_discrete(
    breaks = as.character(seq(0, max(data1$num_spelling_errors), by = 50))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5))

data1<-data1%>%filter(num_spelling_errors<=50) #išrenkame tik stebėjimus, kur num_spelling_errors<=50

data1 %>%
  count(num_spelling_errors) %>%
  arrange(num_spelling_errors) %>%
  ggplot(aes(x = factor(num_spelling_errors), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Rašybos klaidų pasiskirstymas", y = "Dažnis", title = "Rašybos klaidų skaičius") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5))

#mokymo ir testavimo aibes 
set.seed(100)  
train_index <- createDataPartition(data1$num_spelling_errors, p = 0.8, list = FALSE)
datatrain <- data1[train_index, ]  
datatest  <- data1[-train_index, ] 

mean(datatrain$num_spelling_errors)
var(datatrain$num_spelling_errors) #nesutampa, greičiausiai puasono regresija netiks

#puasono modelis 
poisson_model <- glm(num_spelling_errors ~ num_words + num_unique_words + num_stopwords +
                       num_links + num_unique_domains + num_email_addresses +
                       num_urgent_keywords + label,
                     data = datatrain,
                     family = poisson)
summary(poisson_model)
vif(poisson_model)
cat("deviacijos ir laisves laispniu skaiciaus santykis: ",poisson_model$deviance/poisson_model$df.residual," netinkamas")

isskirtys(poisson_model)#taip pat labai daug išskircių, kas rodo kad puasono modelis netinka 

#H0: puasono regresija tinka (t.y. vid=disp) 
#HA: puasano regresija netinka (t.y. vid<disp)
dispersiontest(poisson_model)
#kadangi p-reiksme<𝛼(0.05), puasono modelis netinka 

dispersiontest(poisson_model, trafo = 2)
#kadangi 𝛼>0 turime overdispersion atveji 

#neigiamas binominis modelis
neg_binom_model <- glm.nb(num_spelling_errors ~ num_words + num_unique_words + num_stopwords +
                            num_links + num_unique_domains + num_email_addresses +
                            num_urgent_keywords + label,
                          data = datatrain)
summary(neg_binom_model)
cat("deviacijos ir laisves laispniu skaiciaus santykis: ",neg_binom_model$deviance/neg_binom_model$df.residual," tinkamas")

isskirtys(neg_binom_model) #yra išskircių, jas pašaliname

datatrain2 <- datatrain[rstandard(neg_binom_model) < 3 & rstandard(neg_binom_model) > -3, ]

neg_binom_model1 <- glm.nb(num_spelling_errors ~ num_words + num_unique_words + num_stopwords +
                             num_links + num_unique_domains + num_email_addresses +
                             num_urgent_keywords + label,
                           data = datatrain2,
                           control = glm.control(maxit = 350))
isskirtys(neg_binom_model1) #isskirciu nebera arba labai arti ribos 
summary(neg_binom_model1)
vif(neg_binom_model1)
#3 kovariantes mulikolinearios reikia jas deti į atskirus modelius 
#su nemultikolineariom ir žiureti, kuris geriausias pagal AIC() - 
#gaunasi 3 modeliai kandidatai, žiureti kurio AIC mažiausias


#Modelis su num_words
model_1 <- glm.nb(num_spelling_errors ~ num_words + num_links + num_email_addresses +
                    num_urgent_keywords + label+num_unique_domains,
                  data = datatrain2,
                  control = glm.control(maxit = 350))
summary(model_1)
vif(model_1)

#Modelis su num_unique_words
model_2 <- glm.nb(num_spelling_errors ~ num_unique_words + num_links + num_email_addresses +
                    num_urgent_keywords + label+num_unique_domains,
                  data = datatrain2,
                  control = glm.control(maxit = 350))
summary(model_2)
vif(model_2)

#Modelis su num_stopwords
model_3 <- glm.nb(num_spelling_errors ~ num_stopwords + num_links + num_email_addresses +
                    num_urgent_keywords + label+num_unique_domains,
                  data = datatrain2,
                  control = glm.control(maxit = 350))
summary(model_3) #num_urgent_keywords  nereikšminga

model_3 <- glm.nb(num_spelling_errors ~ num_stopwords + num_links + num_email_addresses +
                     label+num_unique_domains,
                  data = datatrain2,
                  control = glm.control(maxit = 350))
summary(model_3)
vif(model_3)

#Pasirenkame geriausį iš trijų
AIC(model_1)
AIC(model_2)
AIC(model_3)

#Mažiausią AIC turi modelis su kovariante num_unique_words, pasirenkame jį


#Modelio tobulinimas su sąveika
model_2_saveika <- glm.nb(num_spelling_errors ~ num_unique_words + num_links*num_email_addresses +
                            num_urgent_keywords + label+num_unique_domains,
                  data = datatrain2,
                  control = glm.control(maxit = 350))
summary(model_2_saveika) #saveika stat. reikšminga

AIC(model_2)
AIC(model_2_saveika)
anova(model_2, model_2_saveika) #Skirtumas statistiškai reikšmingas

rbind(obs = table(datatrain2$num_spelling_errors)[1:10], 
      exp = round(sapply(0:9, function(x) sum(dnbinom(x, mu=fitted(model_2_saveika),size=model_2_saveika$theta)))))

data.frame(
  reviews = factor(0:9),
  obs = as.numeric(table(datatrain2$num_spelling_errors)[1:10]),
  exp1 = round(sapply(0:9, function(x) sum(dnbinom(x, mu=fitted(model_2_saveika),size=model_2_saveika$theta)))))%>%
  pivot_longer(., cols = c("obs", "exp1"), names_to = "Type", values_to = "Count")%>%
  ggplot(., aes(x = reviews, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 15) +
  scale_fill_manual(
    values = c("obs" = "blue", "exp1" = "red"),
    name = "Reikšmė",  
    labels = c("obs" = "Stebėta", "exp1" = "Prognozuota"))+
  scale_x_discrete(limits = factor(seq(0,9,1)), breaks = seq(0,9,1))+
  labs(title = "Tikėtinų ir stebimų reikšmių dažniai mokymo aibėje",
       x = "Rašybos klaidų skaičius",
       y = "Dažnis",
       fill = "Reikšmė") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


#Parametrų interpretacija
exp(coef(model_2_saveika))

exp(confint.default(model_2_saveika, level=0.95))


#Modelio tikslumas ---------------------
data.frame(reviews = factor(0:9),
           obs = as.numeric(table(factor(datatest$num_spelling_errors, levels = 0:9))),  
           exp = round(sapply(0:9, function(x) sum(dnbinom(x, mu = predict(model_2_saveika, newdata = datatest, type = "response"), size = model_2_saveika$theta)))))%>%
  pivot_longer(., cols = c("obs", "exp"), names_to = "Type", values_to = "Count")%>%
  ggplot(., aes(x = reviews, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 15) +
  scale_fill_manual(
    values = c("obs" = "blue", "exp" = "red"),
    name = "Reikšmė",  
    labels = c("obs" = "Stebėta", "exp" = "Prognozuota"))+
  #scale_y_continuous(limits = c(0,300), breaks = seq(0,300,10))+
  scale_x_discrete(limits = factor(seq(0,9,1)), breaks = seq(0,9,1))+
  labs(title = "Tikėtinų ir stebimų reikšmių dažniai testavimo aibėje",
       x = "Rašybos klaidų skaičius",
       y = "Dažnis",
       fill = "Reikšmė") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

predictions <- predict(model_2_saveika, newdata = datatest, type = "response")
df<-data.frame(Actual = datatest$num_spelling_errors, Predicted = predictions)

ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth=1) +
  theme_minimal(base_size = 15) +
  labs(
    title = "Tikrosios ir prognozuotos reikšmės",
    x = "Tikrosios",
    y = "Prognozuotos"
  ) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

cor(df$Actual, df$Predicted, method = "spearman")
