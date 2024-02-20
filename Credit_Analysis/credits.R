# import libraries 
library(tidyverse)
library(showtext)
library(naniar)
library(ggdist)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(pls)
library(mice)
library(glmnet)
library(ggcorrplot)
library(Metrics)

#import data set
credits <- read_delim("C:/Users/jorge/Desktop/R datasets/Analisis de creditos/creditos.txt")

#import fonts
font_add_google("Montserrat",'Thin 100')
showtext_auto()

#create the new dataset
variables = c('AAAAMM_SOL','ESTRATOS','PRODS_SOLIC','NUM_DE_PERSONAS_A_CARGO','PORC_ENDTOT_CON_NUEVO_CRED',
              'PORC_DEUDA_SEC_FINANCIERO','ENDEUD_NUEVO_CREDITO')
cred.filt <- credits %>% select(-variables) 

#validate dataset
summary(cred.filt)
vis_miss(cred.filt,warn_large_data = F)
# 1. relative frequencies for gender variable 

freq<- table(cred.filt$SEXO)
rel.freq <- prop.table(freq) # 61% are men and 38% are women 
# 2. relative frequencies for gender based on education level 
freq.ed.sx <- table(cred.filt$NIVEL_ESTUDIOS,cred.filt$SEXO)
p.table<- prop.table(freq.ed.sx,margin = 1)
props.plt2<- as.data.frame(p.table)
props.plt2$label<-round(props.plt2$Freq*100,1)

ggplot(props.plt2,aes(factor(Var1,levels=c('BAS','MED','TEC',
                                         'PRF','NOG','UNV',
                                         'POS','DOC')),Freq,
                      fill=Var2)) + geom_col(width=0.8) +
  geom_text(aes(x=Var1,y=ifelse(Var2=='M',
                                 Freq/2,
                                 1-Freq/2)),
            label=paste0(props.plt2$label,'%'))+
  theme_classic(base_size = 15) + 
  scale_y_continuous(expand = expansion(c(0, 0))) +
  labs(title = 'Education level per sex',
       x='Education level',
       y='Percentage (%)',
       color='Gender') +
  theme(text= element_text(family='Thin 100'),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20,
                                  face='bold'),
        legend.title = element_blank()
# 3. Due to multiple outliers in total_income, and a severly right skewed distribution, 
# a log transformation was performed to aproximate a normal distribution.

cred.filt %>% sex_trans() %>% ggplot(aes(SEXO,log(INGRESOS_DECLARADOS_TOTA,base=10))) + 
         ggdist::stat_halfeye(alpha=0.5,justification=-0.04) + 
  geom_boxplot(width=0.1,
               alpha=0.2,
               outlier.color = 'red') + 
  geom_hline(yintercept = c(6.1,6.4),linetype="dashed",alpha=0.3) + 
  theme_classic(base_size = 15) + 
  scale_y_continuous(expand = expansion(c(0, 0))) +
  labs(title = 'Distribution of income per sex',
       x='Gender',
       y='Log10(Income)') +
  theme(text= element_text(family='Thin 100'),
        axis.text.x = element_text(size=10),
        plot.title = element_text(size=20,
                                  face='bold'),
        legend.title = element_blank())

cred.filt$SEXO <- as.factor(cred.filt$SEXO)  
cred.filt$NIVEL_ESTUDIOS <- as.factor(cred.filt$NIVEL_ESTUDIOS)


#4. Test independency of social class and educaction level by sex 

cont_table<-table(cred.filt$ESTRATO,cred.filt$NIVEL_ESTUDIOS)
cont_matrix <- matrix(c(cont_table),
                      nrow = 7,byrow =F,
                      dimnames = list(c("0","1","2","3","4","5","6"),
                                      c(levels(as.factor(cred.filt$NIVEL_ESTUDIOS))
                                                                      )))
chisq<-  chisq.test(cont_matrix,simulate.p.value = T,B=10000)
chisq

ed.by.sex<- cred.filt %>% group_by(SEXO,NIVEL_ESTUDIOS) %>% summarise(total=n()) %>%
  mutate(tot_sex=sum(total),perc=total/tot_sex) 

ggplot(ed.by.sex,aes(factor(NIVEL_ESTUDIOS,
                    levels=c('BAS','MED','TEC',
                             'PRF','NOG','UNV',
                             'POS','DOC')),perc,fill=SEXO)) +
  geom_col(position = 'dodge',width=0.6)

#find significant differences between proportions
total_sex <- c(50177,31359)
ed_level <- table(cred.filt$SEXO,cred.filt$NIVEL_ESTUDIOS)
ed_lev_list <-list()
test <-list()
df_prop <- data.frame()
col.names<-levels(as.factor(cred.filt$NIVEL_ESTUDIOS))
for (i in 1:ncol(ed_level)) {
  ed_lev_list[[i]]<-ed_level[,i]
  test[[i]] <-prop.test(c(ed_lev_list[[i]][1],ed_lev_list[[i]][2]),total_sex)
  prop_test_df <- data.frame(level=col.names[i],p_value=test[[i]]$p.value)
  df_prop <- rbind(df_prop,prop_test_df)
}


#compare income between man an woman 

log.income <- cred.filt %>% 
  mutate(log_income= log(INGRESOS_DECLARADOS_TOTA,base=10))
men_income<- log.income %>% filter(SEXO=='H') %>% pull(log_income)
women_income <- log.income %>% filter(SEXO=='M') %>% pull(log_income)

cred.filt %>% sex_trans() %>% 
  ggplot(aes(y=INGRESOS_DECLARADOS_TOTA,col=SEXO)) +
  geom_boxplot() +  
  scale_color_manual(values = c("#2B3D5E","#D2DFF7")) +
  theme_classic(base_size = 15) + 
  scale_y_continuous(expand = expansion(c(0, 0))) +
  labs(title = 'Income distribution by sex',
       x='Sex',
       y='Income',
       color='Gender') +
  theme(text= element_text(family='Thin 100'),
        axis.title.x = element_text(size=22),
        axis.text = element_text(size=18),
        plot.title = element_text(size=32,
                                  face='bold'),
        legend.title = element_blank(),
        legend.text = element_text(size=18))

model_sd<- lm(log_income~SEXO,data = log.income) #adjust a model to verify assumptions 
par(mfrow=c(2,1))
plot(model_sd,2)
plot(model_sd,3)
t.test(men_income,women_income)

cred.filt %>% group_by(SEXO) %>% count(SEXO)
#Analisis de componentes principales
#remove NA and zeros for PCA purpose
pca_creds<- cred.filt %>% select(-c('SEXO','TIPO_VIVI','ESTRATO'))
pca_creds[pca_creds==0] <- NA
pca_creds<- pca_creds %>% mutate(MONTO_TOTAL_OTORGADO=log(MONTO_TOTAL_OTORGADO,base=10),
         INGRESOS_DECLARADOS_TOTA=log(INGRESOS_DECLARADOS_TOTA,base=10),
         EGRESOS_DECLARADOS_TOTAL=log(EGRESOS_DECLARADOS_TOTAL,base=10),
         EDAD=sqrt(EDAD),
         VALOR_CUOTAS_CARTBANC=log(VALOR_CUOTAS_CARTBANC,base=10),
         SALDO_ACTUAL_SEC_FINANCIERO=log(SALDO_ACTUAL_SEC_FINANCIERO,base=10),
         SALDO_TODOS_SECTORES=log(SALDO_TODOS_SECTORES,base=10),
         VALOR_CUOTA_TODOS_SECTORES =log(VALOR_CUOTA_TODOS_SECTORES,base=10),
         CUOTA_NUEVO_CREDITO=log(CUOTA_NUEVO_CREDITO,base=10)) 

pca_creds <- pca_creds %>% filter(MONTO_TOTAL_OTORGADO < 9.141073)

box <- boxplot(pca_creds$MONTO_TOTAL_OTORGADO)
length(box$out)

#We'll use stochastic imputation to preserve the relation between variables when handling missing data
imp <- mice(pca_creds, method = "norm.nob", m = 3) # Impute data
data_sto <- complete(imp) 
# Here we conclude data preparation 

## Continue to perfom PCA
data_sto_pca<- data_sto %>% select(-c(NIVEL_ESTUDIOS,monto_otorgado))
res.pca <- PCA(data_sto_pca,graph = F)  #mean imputation was used to replace NA values
eigen_val<- get_eigenvalue(res.pca)
round(eigen_val,3)
fviz_eig(res.pca,addlabels = T)
var <- get_pca_var(res.pca)
load_scores<-sweep(res.pca$var$coord,2,sqrt(res.pca$eig[1:ncol(var$coord),1]),FUN="/")
round(load_scores,3)
#get the loadings scores with factominer

fviz_pca_var(res.pca,col.var = 'black') #visualize the biplot 

data_norm <- scale(data_sto)
corr_matrix <- cor(data_sto)
ggcorrplot(corr_matrix) + theme_classic(base_size = 15) #see the corretaltion of variables after log trans and sthocastic regression imputation

#See how each variable is represented in PCs, it's related to the 

fviz_cos2(res.pca,choice = 'var',axes = 1:2)
fviz_pca_var(res.pca, col.var ="black",
             repel = TRUE)+
  theme_minimal(base_size = 18)# see that except for age and score_acierta, 
                           # all variables al realtively well represented in the 
                           # PCs 1-2 

#Now let's see which variables has the most impact for every component

fviz_contrib(res.pca, choice = "var", axes = 1:3, top = 10) # with the first three PC we managed to explain 
                                                            # ~80% of the total variation in the data set
                                                            # so, variables with a % above the expected average 
                                                            # contribution are considered as the most important
                                                            # to the components.

# Ajustamos un modelo de regresion lineal multiple

data_sto<- data_sto %>% rename(monto_otorgado=MONTO_TOTAL_OTORGADO)
data_mult<-data_sto
data_cor <- data_sto %>% select(-NIVEL_ESTUDIOS)

## verificamos supuesto de no multicolinealidad 

multico<- cor(data_cor,method = 'pearson')
ggcorrplot(multico, method='square',type='upper') + 
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust=1,
                                   hjust=1),
        axis.title = element_blank())

## dividimos el dataset

size <- ceiling(0.8*nrow(data_mult))

df.train<- data_mult[1:size,]
df.test<- data_mult[(size+1):nrow(data_mult),]

#Ajustamos el modelo 

model_mul <- lm(monto_otorgado~.,data=df.train)
summary(model_mul)

 #Verificamo supuestos
par(mfrow=c(2,2))
plot(model_mul) 

#Podemoso revisar si la varianza de alguno de los coeficientes se ve inflada 
#debido a la multicolinealidad y relacionanddo los coeficientes obtenidos, podemos 
#retirar algunas variables

car::vif(model_mul)

data_train_filt <- df.train %>% select(-c(VALOR_CUOTAS_CARTBANC,
                                         SALDO_TODOS_SECTORES,
                                         VALOR_CUOTA_TODOS_SECTORES,
                                         SALDO_ACTUAL_SEC_FINANCIERO))

data_test_filt <- df.test %>% select(-c(VALOR_CUOTAS_CARTBANC,
                                        SALDO_TODOS_SECTORES,
                                        VALOR_CUOTA_TODOS_SECTORES,
                                        SALDO_ACTUAL_SEC_FINANCIERO))
#Ajustamos nuevamente el modelo

model_mul_2 <- lm(monto_otorgado~.,data=data_train_filt)
summary(model_mul_2)

#Verificamos nuevamente el supuesto de multicolinealidad y demas
car::vif(model_mul_2)
plot(model_mul_2)

#Validamos el modelo calculando rmse

preds <- predict(model_mul_2, newdata = data_test_filt )
error_train <- rmse(actual= data_test_filt$ monto_otorgado, predicted= preds)
error_train

#Aunque el modelo parece generalizar bastante bien, podemos probar con regularizaciones.
#Dividimos nuevamente el dataset

X_train<- model.matrix(monto_otorgado ~.-1,data=data_train_filt)
X_test <- model.matrix(monto_otorgado ~.-1,data=data_test_filt)
Y_train <- data_train_filt$monto_otorgado
Y_test <- data_test_filt$monto_otorgado

#Regression con 0 > alpha > 1, para lasso = 1, ridge= 0 

list.of.fits <- list()

for (i in 0:10) { 
  fit.name <- paste0('alpha',i/10)
  list.of.fits[[fit.name]] <- 
    cv.glmnet(X_train,Y_train, type.measure = 'mse',
              alpha=i/10, family='gaussian')
}
result<- data.frame()
for (i in 0:10) { 
  fit.name<- paste0('alpha',i/10)
  predicted_train <- predict(list.of.fits[[fit.name]],
                             s=list.of.fits[[fit.name]]$lambda.min,
                             newx = X_train)
  predicted_test <- predict(list.of.fits[[fit.name]],
                       s=list.of.fits[[fit.name]]$lambda.min,
                       newx = X_test)
  rmse_train<- rmse(actual=Y_train,predicted = predicted_train)
  rmse_test<- rmse(actual=Y_test,predicted = predicted_test)
  temp<- data.frame(alpha=i/10, rmse_train=rmse_train, rmse_test=rmse_test, 
                    lambda=list.of.fits[[fit.name]]$lambda.min, fit.name=fit.name)
  result<- rbind(result,temp)
}


#weighted linear regression with elasctic net 
elast_net<- cv.glmnet(X_train,Y_train, type.measure = 'mse',
                       alpha=0.3, family='gaussian')

y_hat<- predict(elast_net, s= elast_net$lambda.min, newx = X_train)
rmse(actual=Y_train,predicted = y_hat)
wt1<- 1/y_hat

#weighted linear regression with elasctic net and wt = 1/y_hat
wt_elast_net1 <- cv.glmnet(X_train,Y_train, type.measure = 'mse', weights = wt1,
                           alpha=0.3, family='gaussian') 
y_hat.2 <- predict(wt_elast_net1, s= wt_elast_net1$lambda.1se, newx = X_train)
resid.y_hat.2 <- abs(Y_train-y_hat.2)
rmse(actual=Y_train,predicted = y_hat.2)
fit2 <- lm(resid.y_hat.2~X_train)
wt2<- 1/fit2$fitted.values^2


#weighted linear regression with elastic net and wt = 1/y_hat^2
wt_elast_net2 <- cv.glmnet(X_train,Y_train, type.measure = 'mse', weights = wt2,
                           alpha=0.3, family='gaussian') 
y_hat.3 <- predict(wt_elast_net2, s= wt_elast_net2$lambda.min, newx = X_train)
rmse(actual=Y_train,predicted = y_hat.3)
resid.y_hat.3 <- abs(Y_train-y_hat.3)^2
fit3 <- lm(resid.y_hat.3~X_train)
wt3<- abs(1/fit3$fitted.values)

#weighted linear regression with elastic net with abs(sigma)^2 and wt = 1/y_hat

wt_elast_net3 <- cv.glmnet(X_train,Y_train, type.measure = 'mse', weights = wt3,
                           alpha=1, family='gaussian') 
y_hat.4 <- predict(wt_elast_net3, s= wt_elast_net3$lambda.min, newx = X_train)
rmse(actual=Y_train,predicted = y_hat.4)
test_pred<- predict(wt_elast_net3, s= wt_elast_net3$lambda.min, newx = X_test)
rmse(actual=Y_test,predicted = test_pred)

#momento, el modelo de regresion  elastico con hiperparametro alpha=0.7 es el mejor

#Ahora vamos a realizar regresion por componentes principales , debemos retirar la variable categorica
# ya que no es posible usarla durante la reduccion dimensional 
#las 5 primeras PC explican el 92 % de la variabilidad, es decir que podemos utilizarlas para 
#ajustar un modelo de regresion, sin embargo el numero de PC optimas se determinara mediante validacion cruzada

data_pcr <- data_mult %>% select(-NIVEL_ESTUDIOS)

# dividimos nuevamente el dataset
df.train.pcr<- data_pcr[1:size,]
df.test.pcr<- data_pcr[(size+1):nrow(data_mult),]

model_pcr<- pcr(monto_otorgado ~ ., data=df.train.pcr, center=T, scale=T, ncomp=5)
summary(model_pcr)
coefficients<- coef(model_pcr,ncomp=5,intercept = F)/model_pcr$scale
  
#aplicamos validacion cruzada para escoger el numero adecuado de componenetes
#con base en el error cuadrado medio 

model_pcr_cv<- pcr(monto_otorgado ~ ., data=df.train.pcr, center=T, scale=T, validation='CV')
model_pcr_msp<- MSEP(model_pcr_cv, estimate = 'CV') # a partir de la septima componenete el MSE practicamente no varia

#para ver cuales son los coeficientes de cada variable en el modelo, es necesario desescalar los valores
coefficients<- coef(model_pcr_cv,ncomp=7,intercept = F)/model_pcr$scale

#evaluamos el modelo calculando el rmse 
pcr_pred<- predict(model_pcr_cv,df.test.pcr,ncomp = 7)
rsme_pcr_test<- rmse(actual = df.test.pcr$monto_otorgado, predicted = as.numeric(pcr_pred))
rsme_pcr_
##function for predictions

data.process <- function(x) {
  df.pred <- data.frame()
  temp.2 <- rbind(df.pred,x) %>% mutate(INGRESOS_DECLARADOS_TOTA=log(as.numeric(x[,1]),base=10),
                                        EGRESOS_DECLARADOS_TOTAL=log(as.numeric(x[,2]),base=10),
                                        EDAD=sqrt(x[,3]),
                                        NIVEL_ESTUDIOS=factor(x[,4],levels=c('BAS','MED','TEC',
                                                                             'PRF','NOG','UNV',
                                                                             'POS','DOC')),
                                        SCORE_ACIERTA= log(as.numeric(x[,5]),base=10),
                                        SALDO_ACTUAL_SEC_FINANCIERO=log(as.numeric(x[,6]),base=10),
                                        CUOTA_NUEVO_CREDITO= log(as.numeric(x[,7]),base=10))
  design.matrix<- model.matrix(~.-1,data=temp.2)
  temp.2$monto.otorgado= 10^predict(elast_net, s=elast_net$lambda.min, newx =design.matrix)
  temp.2
}

data<- data.frame(INGRESOS_DECLARADOS_TOTA=6541000,
                EGRESOS_DECLARADOS_TOTAL=850000,
                EDAD=26,
                NIVEL_ESTUDIOS='POS', 
                SCORE_ACIERTA=830,
                SALDO_ACTUAL_SEC_FINANCIERO=71500000, 
                CUOTA_NUEVO_CREDITO=1000000)
data.process(data)


#Ahora ajustaremos un modelo de clasificacion mediante K-means
#Vamos primero a retirar las variables categoricas del dataset (i.e Estrato y Nivel de estudios)

df.clasf <- data_mult %>% select(-NIVEL_ESTUDIOS)
df.clasf<- scale(df.clasf)

#calculamos matriz de distancia

fviz_nbclust(df.clasf, kmeans, method = 'wss')
m.dist <-dist(df.clasf)
url <- "https://raw.githubusercontent.com/jaranda27/credits_analysis/main/creditos.txt"
df2 <- read.delim(url(url))


