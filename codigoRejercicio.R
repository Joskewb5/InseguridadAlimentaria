#Trabajo final
#Josias Milan Sirpa Pinto

##Realice el ejercicio en este R, para dejar el documento RMarkdown mas ordenado y mas limpio, ya que correr el codigo ahi era mas molestoso


rm(list = ls())
library(dplyr)
library(tidyr)
load("C:/Users/Josias/Desktop/7mo/Mineria II/eh23.RData")
library(labelled)
library(ggplot2)
library(caret)

str(eh23e$s08b_1)
#01 es de baja
attributes(eh23sa$s07a_01) #[1] "Durante los últimos 12 meses, alguna vez en su hogar por falta de dinero u otros recursos: ¿Usted o alguna persona en su hogar se ha preocupado por no tener suficientes alimentos para comer?"
#06 es de alta gravedad
attributes(eh23sa$s07a_06)#[1] "Durante los últimos 12 meses, alguna vez en su hogar por falta de dinero u otros recursos: Pensando aún en los últimos 12 meses, ¿alguna vez en su hogar se quedaron sin alimentos por falta de dinero u otros recursos?"

##Pregunta de estudio:
#Es posible predecir la presencia de inseguridad alimentaria en los hogares bolivianos a partir de variables demográficas, socioeconómicas y de condiciones de vivienda?

#basado en caracteristicas de un hogar y jefe de hogar, 
##Primer modelo, para un grado de seriedad leve, preocupacion

#primero filtramos jefes de hogares 
eh23p_jefes <- eh23p %>% filter(s01a_05 == 1)
#utilizaremos eh23p, eh23e, eh23v

##EH23E

#De la eh23e seleccionamos items importantes, 
#item 2 2. Cocina (a gas, eléctrica, etc), horno? 
#item 4 4. Refrigerador/freezer congeladora?
# Filtrar item 2 (cocina) y 4 (refrigerador) en eh23e
eh23e1 <- eh23e %>%
  filter(item %in% c(2, 4))
eh23ef <- eh23e1 %>%
  filter(item %in% c(2, 4)) %>%
  mutate(
    bien = case_when(
      item == 2 ~ "cocina",
      item == 4 ~ "refrigerador"
    ),
    tiene = ifelse(s08b_1 == 1, 1, 0)
  ) %>% select(folio, bien, tiene) %>% pivot_wider(names_from = bien, values_from = tiene, values_fill = 0)
eh23ef <- eh23ef %>% select(-cocina)

#eh23ef lista, cocina y reefrigerador, 1 tienen 0 no tienen

##EH23P
##PREPARAMOS eh23p_jefes
attributes(eh23p_jefes$s03c_16a)

eh23p_f <- eh23p_jefes %>%
  select(folio, sexo = s01a_02, edad= s01a_03, p_indig=s01a_09, gestora=s04f_35, aestudio, yhog)
eh23p_f <- eh23p_f %>%
  mutate(p_indig = ifelse(p_indig == 1, 1, 0))


eh23p_f <- eh23p_f %>%
  mutate(gestora = ifelse(gestora == 1, 1, 0))


##EH23V

eh23v_f <- eh23v %>%
  select(folio, depto, area, totper,
         s06a_02, s06a_06, s06a_07,
         s06a_09, s06a_14, s06a_19)
eh23v_f <- eh23v_f %>%
  mutate(s06a_14 = ifelse(s06a_14 == 1, 1, 0))
eh23v_f <- eh23v_f %>%
  mutate(s06a_19 = ifelse(s06a_19 == 1, 1, 0))

##aEH23sa

eh23sa_f <- eh23sa %>%
  select(folio, s07a_01, s07a_06)
attributes(eh23sa_f$s07a_06)
#volvemos las variables binarias, 1 si y 2 y 3 a 0
eh23sa_f <- eh23sa_f %>%
  mutate(s07a_01 = ifelse(s07a_01 == 1, 1, 0))
eh23sa_f <- eh23sa_f %>%
  mutate(s07a_06 = ifelse(s07a_06 == 1, 1, 0))
#Armamos primero para s07a_01 (leve, preocupacion)

attributes(eh23v$s06a_19)
str(eh23v)

#Base de datos final

library(dplyr)
bd_final_leve <- eh23sa_f %>%
  select(folio, s07a_01) %>%
  left_join(eh23p_f, by = "folio") %>%
  left_join(eh23v_f, by = "folio") %>%
  left_join(eh23ef, by = "folio")

#sexo 1 hombre, 2 mujer
#p_indig pertenece a pueblo indigena, 1 si, 0 no, donde 0 es no o que no es boliviano
#leer y esc: 1 si, 0 no
#gestora 1 si, 0 no
#depto: 1 sucre, 2 lp, 3 cbba, 4 oruro, 5 potosi, 6 tarija, 7 sc, 8 beni, 9 pando
#s06a_14: tiene cocina como habitacion 1 si, 0 no
#s06a_19: internet, 1 si 0 no
str(bd_final_leve)

#attributes(bd_final_leve$s06a_01)
#eliminamos leer y esc, s06a_01, s06a_08a, s06a_12, cocina
#eliminamos variables no significativas
m_leve <- glm(s07a_01 ~ sexo + edad + p_indig + gestora +
                aestudio + yhog + area + totper +
                s06a_02 + s06a_06 + s06a_07 +
                s06a_09+ s06a_14 +
                s06a_19 + refrigerador,
              data = bd_final_leve,
              family = binomial(link = "logit"))
summary(m_leve)
step(m_leve)
#eliminamos algunos
m_leve1 <- glm(s07a_01 ~ sexo + edad + p_indig + gestora +
                aestudio + yhog + area + totper +
                s06a_02 + s06a_06 + s06a_07 +
                s06a_09 + s06a_14 +
                s06a_19 + refrigerador,
              data = bd_final_leve,
              family = binomial(link = "logit"))
summary(m_leve1)
step(m_leve)
#eliminamos 2 mas
m_leve2 <- glm(s07a_01 ~ sexo + edad + p_indig + gestora +
                 aestudio + yhog + totper +
                 s06a_02 + s06a_06 +
                 s06a_09 + s06a_14 +
                 s06a_19 + refrigerador,
               data = bd_final_leve,
               family = binomial(link = "logit"))
summary(m_leve2)
step(m_leve2)

#bd final
bd <- bd_final_leve %>%
  select(folio, s07a_01, sexo, edad, p_indig, aestudio, yhog, totper, s06a_02, s06a_06, s06a_14, s06a_19, refrigerador)
str(bd)
bd<-na.omit(bd)
bdf<-bd %>% to_factor()

#Dividimos bd
library(caret)
library(dplyr)
set.seed(1234)

bdtrain <- bdf %>% sample_frac(0.7)
bdtest <- bdf %>% anti_join(bdtrain, by = "folio")
bdtrain<- bdtrain %>% select(-folio)
bdtest<- bdtest %>% select(-folio)

##LOGIT 
modelo_logit <- glm(s07a_01 ~ sexo + edad + p_indig + aestudio + yhog + totper +
                      s06a_02 + s06a_06 + s06a_14 + s06a_19 + refrigerador,
                    data = bdtrain,
                    family = binomial(link = "logit"))
summary(modelo_logit)
step(modelo_logit)
m1<-step(modelo_logit)

prob_logit <- predict(modelo_logit, bdtest, type = "response")
prob_logit >0.5
boxplot(prob_logit)

bdtest <- bdtest %>% mutate(pred_logit = ifelse(prob_logit > 0.5, 1, 0))
tl<-table(bdtest$s07a_01, bdtest$pred_logit)
tl
confusionMatrix(tl)

## PROBIT
modelo_probit <- glm(s07a_01 ~ sexo + edad + p_indig + aestudio + yhog + totper +
                       s06a_02 + s06a_06 + s06a_14 + s06a_19 + refrigerador,
                     data = bdtrain,
                     family = binomial(link = "probit"))

summary(modelo_probit)
m2 <- step(modelo_probit)
prob_probit <- predict(modelo_probit, bdtest, type = "response")
bdtest <- bdtest %>%
  mutate(pred_probit = ifelse(prob_probit > 0.5, 1, 0))

tp <- table(bdtest$s07a_01, bdtest$pred_probit)
tp
confusionMatrix(tp)

#reporte
reporte<-NULL

reporte<-c(reporte,
           confusionMatrix(tl)$overall[1])
names(reporte)[length(reporte)] <- "Logit"


reporte<-c(reporte,
           confusionMatrix(tp)$overall[1])
names(reporte)[length(reporte)] <- "Probit"
reporte

##NAIVE BAYES

library(naivebayes)
#dependiente a factor
str(bdtrain$s07a_01)
bdtrain <- bdtrain %>% mutate(s07a_01 = factor(s07a_01, levels = c(0,1)))
bdtest <- bdtest %>% mutate(s07a_01 = factor(s07a_01, levels = c(0,1)))

modelo_nb <- naive_bayes(s07a_01 ~ ., data = bdtrain, usekernel = TRUE, usepoisson = TRUE)
summary(modelo_nb)

#Predicción en bdtest
pred_nb <- predict(modelo_nb, bdtest, type = "class")
tnb <- table(bdtest$s07a_01, pred_nb)
tnb
confusionMatrix(tnb)

#reporte
reporte <- c(reporte, confusionMatrix(tnb)$overall[1])
names(reporte)[length(reporte)] <- "Naive_bayes"

reporte


##KNN

library(class)
library(caret)
library(dplyr)
library(haven)



bdtrain_knn <- bdtrain 
bdtest_knn <- bdtest %>% select(-pred_logit, -pred_probit)



bdtrain_knn <- na.omit(bdtrain_knn)
bdtest_knn <- na.omit(bdtest_knn)
bdtrain_knn <- bdtrain_knn %>% mutate(across(everything(), ~as.numeric(zap_labels(.))))
bdtest_knn <- bdtest_knn %>% mutate(across(everything(), ~as.numeric(zap_labels(.))))

clase_train <- as.factor(bdtrain_knn$s07a_01)
clase_test <- as.factor(bdtest_knn$s07a_01)

train <- scale(bdtrain_knn %>% select(-s07a_01))
test <- scale(bdtest_knn %>% select(-s07a_01))

k <- floor(sqrt(nrow(bdtrain_knn)))
pred_knn <- knn(train, test, clase_train, k = k)

tknn <- table(clase_test, pred_knn)
tknn
confusionMatrix(tknn)

reporte <- c(reporte, confusionMatrix(tknn)$overall[1])
names(reporte)[length(reporte)] <- "KNN"
reporte


##CART
library(rpart)
library(rpart.plot)

bdtrain$y <- factor(ifelse(bdtrain$s07a_01 == 1, "SI", "NO"))
bdtest$y <- factor(ifelse(bdtest$s07a_01 == 1, "SI", "NO"))


modelo_cart <- rpart(y ~ . -s07a_01, data = bdtrain, 
            control = rpart.control(cp = 0, minsplit = 1, minbucket = 1, maxdepth = 5))

rpart.plot(modelo_cart)
plotcp(modelo_cart)

clase_modelocart <- predict(modelo_cart, bdtest, type = "class")

tcart <- table(bdtest$y, clase_modelocart)
print(tcart)

confusionMatrix(tcart)

#reporte
reporte <- c(reporte, confusionMatrix(tcart)$overall[1])
names(reporte)[length(reporte)] <- "CART"
reporte


##C50
library(C50)
library(labelled)

bdtrain_c50 <- bdtrain_knn %>% mutate(across(everything(), ~as.numeric(zap_labels(.))))
bdtest_c50 <- bdtest_knn %>% mutate(across(everything(), ~as.numeric(zap_labels(.))))


bdtrain_c50 <- bdtrain_c50 %>%
  mutate(y = factor(ifelse(s07a_01 == 1, "SI", "NO"))) %>%
  select(-s07a_01) %>%
  remove_labels() %>%
  as.data.frame()

bdtest_c50 <- bdtest_c50 %>%
  mutate(y = factor(ifelse(s07a_01 == 1, "SI", "NO"))) %>%
  select(-s07a_01) %>%
  remove_labels()


m50 <- C5.0(y ~ ., data = bdtrain_c50)

summary(m50)
plot(m50)  # Árbol de C5.0

pred_c50 <- predict(m50, bdtest_c50)

tc50 <- table(bdtest_c50$y, pred_c50)
print(tc50)
confusionMatrix(tc50)

reporte <- c(reporte, confusionMatrix(tc50)$overall[1])
names(reporte)[length(reporte)] <- "C5.0"
reporte


#REDES NEURONALES
library(nnet)
library(caret)
str(bdtest_red$y)
bdtrain_red <- bdtrain %>% mutate(across(everything(), ~as.numeric(zap_labels(.))))
bdtest_red <- bdtest %>% mutate(across(everything(), ~as.numeric(zap_labels(.))))
bdtest_red <- bdtest_red %>% select(-pred_logit, -pred_probit)
bdtrain_red <- bdtrain_red %>% select(-pred_logit, -pred_probit)

bdtrain_red$y <- factor(bdtrain_red$y, levels = c(1, 2), labels = c("NO", "SI"))
bdtest_red$y <- factor(bdtest_red$y, levels = c(1, 2), labels = c("NO", "SI"))


bdtrain_red$y <- factor(bdtrain_red$y, levels = c("NO", "SI"))
bdtest_red$y <- factor(bdtest_red$y, levels = c("NO", "SI"))
bdtrain_red <- bdtrain_red %>% select(-s07a_01)
bdtest_red <- bdtest_red %>% select(-s07a_01)


# Entrenar el modelo de red neuronal
set.seed(2239)
m7 <- nnet(y ~ ., data = bdtrain_red, size = 20, maxit = 1000)

# Predecir sobre la base test, tipo = "raw" da probabilidades
pred_probs <- predict(m7, bdtest_red, type = "raw")

# Convertir a clases con threshold 0.5
pred_class <- ifelse(pred_probs > 0.5, "SI", "NO")
pred_class <- factor(pred_class, levels = c("NO", "SI"))

# Matriz de confusión
tcnn <- table(bdtest_red$y, pred_class)
confusionMatrix(tcnn)

reporte <- c(reporte, confusionMatrix(tcnn)$overall[1])
names(reporte)[length(reporte)] <- "Nnet"
reporte

#str(eh23sa$s07a_01)
#FIN DE MODELOS ESCOGEMOS LOGIT POR MAYOR ACCURACY E INTERPRETABILIDAD

#PREDICCION
predecir_inseguridad <- function(sexo, edad, p_indig, aestudio, yhog, totper,
                                 s06a_02, s06a_06, s06a_14, s06a_19, refrigerador) {
  # Crear una fila con los mismos nombres y tipos que bdtrain
  nuevo_hogar <- data.frame(
    sexo = factor(sexo, levels = levels(bdtrain$sexo)),
    edad = edad,
    p_indig = p_indig,
    aestudio = aestudio,
    yhog = yhog,
    totper = totper,
    s06a_02 = factor(s06a_02, levels = levels(bdtrain$s06a_02)),
    s06a_06 = factor(s06a_06, levels = levels(bdtrain$s06a_06)),
    s06a_14 = s06a_14,
    s06a_19 = s06a_19,
    refrigerador = refrigerador
  )
  
  # Validación: que no haya NA en ningún factor
  if (any(is.na(nuevo_hogar))) {
    print("Error: alguno de los valores ingresados no coincide con los niveles esperados.")
    return(nuevo_hogar)
  }
  
  # Predecir probabilidad
  prob <- predict(m1, newdata = nuevo_hogar, type = "response")
  
  # Clasificación
  resultado <- ifelse(prob > 0.5, "SÍ sufre inseguridad alimentaria leve", 
                      "NO sufre inseguridad alimentaria leve")
  
  list(probabilidad = round(prob, 3), resultado = resultado)
}

#levels(bdtrain$sexo)
#levels(bdtrain$s06a_02)
#levels(bdtrain$s06a_06)

predecir_inseguridad(
  sexo = "1. Hombre",
  edad = 40,
  p_indig = 0,
  aestudio = 8,
  yhog = 2000,
  totper = 4,
  s06a_02 = "3. ¿Alquilada?",
  s06a_06 = "5. CEMENTO",
  s06a_14 = 1,
  s06a_19 = 1,
  refrigerador = 1
)
