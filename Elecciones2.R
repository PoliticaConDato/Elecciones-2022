## Load packages
library(lubridate)
library(reshape2)
library(scales)
library(tidyr)
library(dplyr)
library(ggplot2)
library(xgboost)
library(kableExtra)

## Assumptions for "voto en blanco"

blanco.1 <- 0.6 # Senate elections adjustment
blanco.2 <- 0.4 # Polling adjustment
blanco.3 <- 0.017 # Historical average

##### REGRESSION MODEL #####
# Regression model based on the 2018 senate and presidential elections

# Read the data into a data frame.
senado.df <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/Sen2018.csv")
presi.df <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/Pre2018.csv")

senado.22 <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/senado2022.csv")

# Initial data cleanup
senado.df <- senado.df[-c(1123,1124),]
presi.df <- presi.df[-1123,]

data.df <- merge(senado.df,presi.df, by = "Code", all = TRUE)

# Remove unnecessary columns
data.df <- subset(data.df, select = c(Votos.Válidos..Nacional.,Alianza.Verde,Cambio.Radical, Centro.Democrático, Partido.Conservador, 
                             FARC, MIRA, Opción.Ciudadana, Partido.Liberal, Polo.Democrático.Alternativo, Partido.de.la.U,
                             Somos, Todos.Somos.Colombia, Coalición.Lista.de.la.Decencia..ASI.UP.MAIS., GSC.Colombia.Justa.Libres, 
                             GSC.Si.se.puede, GSC.Unión.con.Fortaleza, Voto.en.Blanco..Nacional., Votos.Válidos, Iván.Duque, Gustavo.Petro,
                             Sergio.Fajardo, Germán.Vargas.Lleras, Humberto.De.La.Calle, Jorge.Antonio.Trujillo, Viviane.Morales, 
                             Promotores.Voto.en.Blanco, Voto.en.Blanco))

# Group small parties
data.df$Pacto.Historico <- data.df$FARC + data.df$Coalición.Lista.de.la.Decencia..ASI.UP.MAIS. + data.df$Polo.Democrático.Alternativo
data.df$Coalicion.Centro <- data.df$Alianza.Verde
data.df$Otros.Sen <- data.df$GSC.Si.se.puede + data.df$GSC.Unión.con.Fortaleza + data.df$Opción.Ciudadana + data.df$Somos + data.df$Todos.Somos.Colombia
data.df$MIRA.Justa <- data.df$MIRA + data.df$GSC.Colombia.Justa.Libres
data.df$Blanco.Sen <- data.df$Voto.en.Blanco..Nacional.

data.df$Petro <- data.df$Gustavo.Petro
data.df$Fajardo <- data.df$Sergio.Fajardo
data.df$Fico <- data.df$Iván.Duque
data.df$Vargas <- data.df$Germán.Vargas.Lleras
data.df$Otros.Pres <- data.df$Humberto.De.La.Calle + data.df$Jorge.Antonio.Trujillo + data.df$Viviane.Morales
data.df$Blanco.Pres <- data.df$Promotores.Voto.en.Blanco + data.df$Voto.en.Blanco

data.df <- subset(data.df, select = c(Votos.Válidos..Nacional.,Coalicion.Centro,Cambio.Radical, Centro.Democrático, Partido.Conservador, 
                                      Pacto.Historico, MIRA.Justa, Partido.Liberal, Partido.de.la.U, Otros.Sen,
                                      Blanco.Sen, Votos.Válidos, Fico, Petro,
                                      Fajardo, Vargas, Otros.Pres, Blanco.Pres))

data.df <- data.df[data.df$Votos.Válidos..Nacional. > 100,]

# Prepare test set
test.df <- dcast(senado.22[,c(2,3,5)], Code_REGIS ~ Nom_Partido, value.var = 'Votos', fun=sum)
test.df$Coalicion.Centro <- test.df$`COALICIÓN ALIANZA VERDE Y CENTRO ESPERANZA` + test.df$`PARTIDO NUEVO LIBERALISMO`
test.df$Cambio.Radical <- test.df$`PARTIDO CAMBIO RADICAL`
test.df$Centro.Democrático <- test.df$`PARTIDO CENTRO DEMOCRÁTICO`
test.df$Partido.Conservador <- test.df$`PARTIDO CONSERVADOR COLOMBIANO`
test.df$Pacto.Historico <- test.df$`PARTIDO COMUNES` + test.df$`PACTO HISTÓRICO` + test.df$`FUERZA CIUDADANA LA FUERZA DEL CAMBIO`
test.df$MIRA.Justa <- test.df$`COALICIÓN MIRA -  COLOMBIA JUSTA LIBRES`
test.df$Partido.Liberal <- test.df$`PARTIDO LIBERAL COLOMBIANO`
test.df$Partido.de.la.U <- test.df$`PARTIDO DE LA UNIÓN POR LA GENTE "PARTIDO DE LA U"`
test.df$Blanco.Sen <- test.df$`VOTO EN BLANCO`
test.df$Otros.Sen <- test.df$`ESTAMOS LISTAS COLOMBIA` + test.df$`MOVIMIENTO DE SALVACIÓN NACIONAL` + test.df$`MOVIMIENTO GENTE NUEVA` + test.df$`MOVIMIENTO NACIONAL SECTOR ORGANIZADO DE LA SALUD SOS COLOMBIA` + test.df$`MOVIMIENTO UNITARIO METAPOLITICO`

test.df2 <- test.df
test.df <- test.df[,seq(21,30)]
     
test.df <- test.df[, c(colnames(data.df[, c(2:11)]))]
                                               
## Train regression model
data.df.log <- data.df
data.df.log[, c(13,14,15,16,17,18)] <- log(data.df.log[, c(13,14,15,16,17,18)] + 1)

# Prepare matrices for XGBoost
xtrain <- as.matrix(data.df.log[, c(2:11)])
ytrain.Fico <- as.vector(data.df.log$Fico)
ytrain.Petro <- as.vector(data.df.log$Petro)
ytrain.Fajardo <- as.vector(data.df.log$Fajardo)
ytrain.Vargas <- as.vector(data.df.log$Vargas)
ytrain.Otros.Pres <- as.vector(data.df.log$Otros.Pres)
ytrain.Blanco.Pres <- as.vector(data.df.log$Blanco.Pres)

xtest <- as.matrix(test.df)

# Fit Xgboost
xgboost_train.Fico = xgb.DMatrix(data=xtrain, label=ytrain.Fico)
xgboost_train.Petro = xgb.DMatrix(data=xtrain, label=ytrain.Petro)
xgboost_train.Fajardo = xgb.DMatrix(data=xtrain, label=ytrain.Fajardo)
xgboost_train.Vargas = xgb.DMatrix(data=xtrain, label=ytrain.Vargas)
xgboost_train.Otros.Pres = xgb.DMatrix(data=xtrain, label=ytrain.Otros.Pres)
xgboost_train.Blanco.Pres = xgb.DMatrix(data=xtrain, label=ytrain.Blanco.Pres)

xgboost_test <- xgb.DMatrix(data=xtest)

nround    <- 500 # number of XGBoost rounds
cv.nfold  <- 5

cv_model.Fico <- xgboost(xgboost_train.Fico, nrounds = nround, cv.nfold = 5, verbose = FALSE)
cv_model.Petro <- xgboost(xgboost_train.Petro, nrounds = nround, cv.nfold = 5, verbose = FALSE)
cv_model.Fajardo <- xgboost(xgboost_train.Fajardo, nrounds = nround, cv.nfold = 5, verbose = FALSE)
cv_model.Vargas <- xgboost(xgboost_train.Vargas, nrounds = nround, cv.nfold = 5, verbose = FALSE)
cv_model.Otros.Pres <- xgboost(xgboost_train.Otros.Pres, nrounds = nround, cv.nfold = 5, verbose = FALSE)
cv_model.Blanco.Pres <- xgboost(xgboost_train.Blanco.Pres, nrounds = nround, cv.nfold = 5, verbose = FALSE)

xgb.Fico.pred <- predict(cv_model.Fico, xgboost_test, reshape=T)
xgb.Petro.pred <- predict(cv_model.Petro, xgboost_test, reshape=T)
xgb.Fajardo.pred <- predict(cv_model.Fajardo, xgboost_test, reshape=T)
xgb.Vargas.pred <- predict(cv_model.Vargas, xgboost_test, reshape=T)
xgb.Otros.Pres.pred <- predict(cv_model.Otros.Pres, xgboost_test, reshape=T)
xgb.Blanco.Pres.pred <- predict(cv_model.Blanco.Pres, xgboost_test, reshape=T)

xgb.pred <- cbind(xgb.Fico.pred, xgb.Petro.pred, xgb.Fajardo.pred, xgb.Vargas.pred, xgb.Otros.Pres.pred, xgb.Blanco.Pres.pred)
xgb.pred <- round(exp(xgb.pred)-1)

xgb.pred <- data.frame(xgb.pred)

xgb.pred$total <- rowSums(xgb.pred)

## Integrate data from "Consultas"

consultas <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/consultas2022.csv")
consultas <- consultas[,-1]

cons.df <- dcast(consultas, CODE_REGIS ~ Presi, value.var = 'VOTOS', fun=sum)
cons.df$Total <- cons.df$Fajardo + cons.df$Fico + cons.df$Petro

test.df2 <- test.df2[,c(1,seq(21,30))]
test.df2$Total.Sen <- rowSums(test.df2[,2:11])

test.df2 <- merge(test.df2, cons.df, by.x = "Code_REGIS", by.y = "CODE_REGIS", all.x = TRUE)
test.df2$part <- test.df2$Total / test.df2$Total.Sen

test.df2 <- cbind(test.df2, xgb.pred)

test.df2$top3 <- (test.df2$xgb.Fico.pred + test.df2$xgb.Fajardo.pred + test.df2$xgb.Petro.pred)/test.df2$total

test.df2$con.fajardo.per <- test.df2$top3*test.df2$Fajardo/test.df2$Total
test.df2$con.fico.per <- test.df2$top3*test.df2$Fico/test.df2$Total
test.df2$con.petro.per <- test.df2$top3*test.df2$Petro/test.df2$Total
test.df2$fajardo.out <- (test.df2$xgb.Fajardo.pred*(1-test.df2$part)/test.df2$total + test.df2$con.fajardo.per*test.df2$part)*test.df2$total
test.df2$fico.out <- (test.df2$xgb.Fico.pred*(1-test.df2$part)/test.df2$total + test.df2$con.fico.per*test.df2$part)*test.df2$total
test.df2$petro.out <- (test.df2$xgb.Petro.pred*(1-test.df2$part)/test.df2$total + test.df2$con.petro.per*test.df2$part)*test.df2$total
test.df2$blanco.out <- test.df2$xgb.Blanco.Pres.pred
test.df2$otros.out <- test.df2$total - test.df2$fajardo.out - test.df2$fico.out - test.df2$petro.out - test.df2$blanco.out

test.df2 <- colSums(test.df2[,29:33])
test.df2 <- test.df2/sum(test.df2)

# Regression output model
reg.model <- test.df2

blanco.1 <- blanco.1 * sum(test.df$Blanco.Sen) / sum(test.df)

# Clean environment 
remove(cv.nfold, nround, xgb.Blanco.Pres.pred, xgb.Fajardo.pred, xgb.Fico.pred, xgb.Otros.Pres.pred, xgb.Petro.pred, xgboost_test, xgboost_train.Blanco.Pres, xgboost_train.Fajardo, xgboost_train.Fico, xgboost_train.Otros.Pres, xgboost_train.Petro, xgboost_train.Vargas, ytrain.Blanco.Pres, ytrain.Fajardo, ytrain.Fico, ytrain.Otros.Pres, ytrain.Petro, ytrain.Vargas, xgb.Vargas.pred, test.df2, cons.df, consultas, cv_model.Blanco.Pres, cv_model.Fajardo, cv_model.Fico, cv_model.Otros.Pres, cv_model.Petro, cv_model.Vargas, data.df, data.df.log, presi.df, senado.22, senado.df, test.df, xgb.pred, xtest, xtrain)

##### POLL MODEL #####

# Load dataset
data.e <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/Encuestas.csv")

# Clean data
data.e$Fecha <- mdy(data.e$Fecha)
data.e[is.na(data.e)] <- 0

# Create variables
data.e$Indecisos <- data.e$Blanco + data.e$NS.NR

# Create dataset for full plot
data.e.plot <- data.e[,-c(1,2,4,6,14,15,16)]
data.e.plot <- reshape2::melt(data.e.plot, id=c("Fecha","Rating","Muestra"))
data.e.plot$value <- as.numeric(data.e.plot$value)
data.e.plot <- data.e.plot[,-c(2,3)]

# Create dataset for plot without undecided
data.e.plot.und <- data.e[,-c(1,2,5,6,7,14,15,16)]
data.e.plot.und <- reshape2::melt(data.e.plot.und, id=c("Fecha","Encuestadora"))
data.e.plot.und$value <- as.numeric(data.e.plot.und$value)
data.e.plot.und <- data.e.plot.und[data.e.plot.und$variable != "Indecisos" & !is.na(data.e.plot.und$variable),]
data.e.plot.und <- group_by(data.e.plot.und, variable, Fecha, Encuestadora)

data.e.plot.und$value <- as.numeric(data.e.plot.und$value)
data.e.plot.und$value[is.na(data.e.plot.und$value)] <- 0
data.e.plot.und <- summarise(data.e.plot.und, 
                      value = sum(value)
)

data.e.plot.und.total <- group_by(data.e.plot.und, Fecha, Encuestadora)
data.e.plot.und.total <- summarise(data.e.plot.und.total, 
                        value = sum(value)
)
data.e.plot.und <- merge(data.e.plot.und, data.e.plot.und.total, by = c("Fecha","Encuestadora"), all.x = TRUE)
data.e.plot.und$value <- data.e.plot.und$value.x / data.e.plot.und$value.y

data.e.plot.und <- data.e.plot.und[,c(1,3,6)]

# Add variables for model weighting

data.e.model <- data.e

data.e.model$rating.weight <- data.e.model$Rating / 5
data.e.model$error.weight <- 1-data.e.model$Error*3

election.date <- mdy("05/28/2022")
start.date <- mdy("04/01/2021")

i <- start.date
data.e.model.out <- data.e.model[1,]
data.e.model.out <- data.e.model.out[,c(3,8,9,10,11,12,13,14,15)]

end.date <- mdy("05/19/2022")

while (i <=  end.date) {
  data.loop <- data.e.model
  data.loop$days <- i - data.loop$Fecha
  data.loop$date.weight <- ifelse(data.loop$days < 0, 0, ifelse(data.loop$days > 60, 0, 1.2*(1-data.loop$days/60)))
  data.loop$weight <- data.loop$rating.weight * data.loop$error.weight * data.loop$date.weight
  data.loop <- data.loop[rev(order(data.loop$Encuestadora, data.loop$Fecha)),]
  data.loop <- data.loop[data.loop$date.weight > 0, ]
  data.loop <- data.loop[!duplicated(data.loop$Encuestadora),]
  vote.cols <- data.loop[,seq(8,15)]
  vote.cols <- vote.cols*data.loop$weight
    vote.cols <- colSums(vote.cols)
  vote.cols <- data.frame(t(vote.cols))
  total.weight <- sum(data.loop$weight)
  vote.cols <- vote.cols / total.weight
  vote.cols$Fecha <- i
  data.e.model.out <- rbind(data.e.model.out, vote.cols)
  
  i <- i + 1
}

data.e.model.out <- data.e.model.out[-1,]
data.e.model.out$Indecisos <- data.e.model.out$Blanco + data.e.model.out$NS.NR

# Create dataset to plot model output
data.e.model.plot <- data.e.model.out[,-c(8,9)]
data.e.model.plot <- reshape2::melt(data.e.model.plot, id=c("Fecha"))
data.e.model.plot$value <- as.numeric(data.e.model.plot$value)

data.e.plot <- data.e.plot[data.e.plot$Fecha > start.date,]

# Plot full plot

group.colors <- c(Otros = "#ff0000", Fico = "#0000ff", Fajardo ="#14ce14", Indecisos = "#808285", Petro = "#800080", Rodolfo = "#c8c800", Ingrid = "#008000")

data.e.plot <- data.e.plot[data.e.plot$Fecha > mdy("01/01/2022"),]
data.e.model.plot.filter <- data.e.model.plot[data.e.model.plot$Fecha >= min(data.e.plot$Fecha),]

data.plot <- ggplot(data.e.plot, aes(x=Fecha, y=value, color=variable)) +
  geom_point() +
  stat_smooth(aes(fill=variable)) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Encuestas Presidencia Colombia 2022") +
  xlab("Fecha") + 
  ylab("Intención de voto") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) + 
  geom_line(data = data.e.model.plot.filter, aes(x=Fecha, y=value, color=variable), linetype = "dashed") 

data.plot

# Create dataset to plot model output without undecided
data.e.model.plot <- data.e.model.out[,-c(8,9)]
data.e.model.plot <- reshape2::melt(data.e.model.plot, id=c("Fecha"))
data.e.model.plot$value <- as.numeric(data.e.model.plot$value)
data.e.model.plot <- data.e.model.plot[data.e.model.plot$variable != "Indecisos" & !is.na(data.e.model.plot$variable),]
data.e.model.plot <- group_by(data.e.model.plot, variable, Fecha)
data.e.model.plot$value <- as.numeric(data.e.model.plot$value)
data.e.model.plot$value[is.na(data.e.model.plot$value)] <- 0
data.e.model.plot <- summarise(data.e.model.plot, 
                             value = sum(value)
)
data.e.model.plot.total <- group_by(data.e.model.plot, Fecha)
data.e.model.plot.total <- summarise(data.e.model.plot.total, 
                                   value = sum(value)
)
data.e.model.plot <- merge(data.e.model.plot, data.e.model.plot.total, by = c("Fecha"), all.x = TRUE)
data.e.model.plot$value <- data.e.model.plot$value.x / data.e.model.plot$value.y
data.e.model.plot <- data.e.model.plot[,c(1,2,5)]

data.e.plot.und <- data.e.plot.und[data.e.plot.und$Fecha > start.date,]

# Plot full plot

group.colors <- c(Otros = "#ff0000", Fico = "#0000ff", Fajardo ="#14ce14", Petro = "#800080", Rodolfo = "#c8c800", Ingrid = "#008000")

data.e.plot.filter <- data.e.plot.und[data.e.plot.und$Fecha > mdy("01/01/2022"),]
data.e.model.plot.filter <- data.e.model.plot[data.e.model.plot$Fecha >= min(data.e.plot$Fecha),]

data.plot <- ggplot(data.e.plot.filter, aes(x=Fecha, y=value, color=variable)) +
  geom_point() +
  stat_smooth(aes(fill=variable)) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Encuestas Presidencia Colombia 2022") +
  xlab("Fecha") + 
  ylab("Intención de voto") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) + 
  geom_line(data = data.e.model.plot.filter, aes(x=Fecha, y=value, color=variable), linetype = "dashed") 

data.plot

otros <- data.e.model.out$Otros[nrow(data.e.model.out)]/(1-data.e.model.out$NS.NR[nrow(data.e.model.out)])
blanco.2 <- data.e.model.out$Blanco[nrow(data.e.model.out)]/(1-data.e.model.out$NS.NR[nrow(data.e.model.out)])*blanco.2

# Polling output model
poll.model <- data.e.model.plot[data.e.model.plot$Fecha == max(data.e.model.plot$Fecha),]
poll.model <- poll.model[,-1]

# Clean environment
remove(election.date, end.date, group.colors, i, start.date, total.weight, data.e, date.e.model, data.e.model.out, data.e.model.plot, data.e.model.plot.total, data.e.plot, data.e.plot.und, data.e.plot.und.total, data.loop, data.plot, vote.cols)



##### TRENDS MODEL ####

# Import google trends data with intent (presidente)
test <- gtrendsR::gtrends(c("Petro Presidente", "Fico Presidente", "Fajardo Presidente", "Ingrid Presidente", "Rodolfo Presidente"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]

 write.csv(test, "data/trends_intent_presidente.csv")
# # UPDATE MAY 27!!!!
#  test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/trends_intent_presidente.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$`Petro Presidente`, k = 7)
test.d2 <- zoo::rollmean(test.d$`Fico Presidente`, k = 7)
test.d3 <- zoo::rollmean(test.d$`Fajardo Presidente`, k = 7)
test.d4 <- zoo::rollmean(test.d$`Ingrid Presidente`, k = 7)
test.d5 <- zoo::rollmean(test.d$`Rodolfo Presidente`, k = 7)

test.dalt$Petro <- test.d1
test.dalt$Fico <- test.d2
test.dalt$Fajardo <- test.d3
test.dalt$Ingrid <- test.d4
test.dalt$Rodolfo <- test.d5
test.dalt <- test.dalt[,-c(2,3,4,5,6)]
test.dalt$total <- test.dalt$Petro + test.dalt$Fico + test.dalt$Fajardo + test.dalt$Ingrid + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,6)]
test.dalt.per <- test.dalt.per / test.dalt$total

blanco <- (blanco.1 + blanco.2 + blanco.3 + as.numeric(reg.model[4]))/4


test.dalt.per <- test.dalt.per * (1-blanco-otros)
test.dalt.per$Blanco <- blanco
test.dalt.per$Otros <- otros
test.dalt.per$date <- test.dalt$date 

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Otros = "#ff0000", Fico = "#0000ff", Fajardo ="#14ce14", Petro = "#800080", Rodolfo = "#c8c800", Ingrid = "#008000")

data.plot <- ggplot(test.dalt.per, aes(x=date, y=value, color=variable)) +
  geom_point() +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  stat_smooth(aes(fill=variable)) +
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Google Trends con Intención de voto") +
  xlab("Fecha") + 
  ylab("% de busqueda") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

data.plot

trends.1 <- test.dalt.per

# Output most recent data point for model
trends.model.1 <- test.dalt.per[test.dalt.per$date == max(test.dalt.per$date),]
trends.model.1 <- trends.model.1[,-1]

# Import google trends data based on topics 
test <- gtrendsR::gtrends(c("/m/03c3tmt", "/g/11b7gphb96", "/m/02vyn0k", "/m/019d8_", "/g/11hcszl05g"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)

test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0

write.csv(test, "data/trends_category.csv")

# # UPDATE MAY 27!!!!
# test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/trends_category.csv")
# test <- test[,-1]
# test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
colnames(test.d) <- c("date","Fico","Rodolfo","Ingrid","Fajardo","Petro")

test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$Petro, k = 7)
test.d2 <- zoo::rollmean(test.d$Fico, k = 7)
test.d3 <- zoo::rollmean(test.d$Fajardo, k = 7)
test.d4 <- zoo::rollmean(test.d$Ingrid, k = 7)
test.d5 <- zoo::rollmean(test.d$Rodolfo, k = 7)

test.dalt$Petro <- test.d1*0.5
test.dalt$Fico <- test.d2*1.5
test.dalt$Fajardo <- test.d3*1.5
test.dalt$Ingrid <- test.d4
test.dalt$Rodolfo <- test.d5
test.dalt$total <- test.dalt$Petro + test.dalt$Fico + test.dalt$Fajardo + test.dalt$Ingrid + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,6)]
test.dalt.per <- test.dalt.per / test.dalt$total
test.dalt.per <- test.dalt.per * (1-blanco-otros)
test.dalt.per$Blanco <- blanco
test.dalt.per$Otros <- otros
test.dalt.per$date <- test.dalt$date 

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot

group.colors <- c(Otros = "#ff0000", Fico = "#0000ff", Fajardo ="#14ce14", Petro = "#800080", Rodolfo = "#c8c800", Ingrid = "#008000")

data.plot <- ggplot(test.dalt.per, aes(x=date, y=value, color=variable)) +
  geom_point() +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  
  stat_smooth(aes(fill=variable)) +
  
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Google Trends Interes por Candidato") +
  xlab("Fecha") + 
  ylab("% de busqueda") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

data.plot

trends.2 <- test.dalt.per


# Output of the trends model by category/topic
trends.model.2 <- test.dalt.per[test.dalt.per$date == max(test.dalt.per$date),]
trends.model.2 <- trends.model.2[,-1]

# Import google trends data with intent (2022)
test <- gtrendsR::gtrends(c("Petro 2022", "Fico 2022", "Fajardo 2022", "Ingrid 2022", "Rodolfo 2022"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0

write.csv(test, "data/trends_intent_2022.csv")
# # UPDATE MAY 27!!!!
#  test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/trends_intent_2022.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$`Petro 2022`, k = 7)
test.d2 <- zoo::rollmean(test.d$`Fico 2022`, k = 7)
test.d3 <- zoo::rollmean(test.d$`Fajardo 2022`, k = 7)
test.d4 <- zoo::rollmean(test.d$`Ingrid 2022`, k = 7)
test.d5 <- zoo::rollmean(test.d$`Rodolfo 2022`, k = 7)

test.dalt$Petro <- test.d1*0.5
test.dalt$Fico <- test.d2
test.dalt$Fajardo <- test.d3
test.dalt$Ingrid <- test.d4
test.dalt$Rodolfo <- test.d5
test.dalt <- test.dalt[,-c(2,3,4,5,6)]
test.dalt$total <- test.dalt$Petro + test.dalt$Fico + test.dalt$Fajardo + test.dalt$Ingrid + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,6)]
test.dalt.per <- test.dalt.per / test.dalt$total

blanco <- (blanco.1 + blanco.2 + blanco.3 + as.numeric(reg.model[4]))/4


test.dalt.per <- test.dalt.per * (1-blanco-otros)
test.dalt.per$Blanco <- blanco
test.dalt.per$Otros <- otros
test.dalt.per$date <- test.dalt$date 

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Otros = "#ff0000", Fico = "#0000ff", Fajardo ="#14ce14", Petro = "#800080", Rodolfo = "#c8c800", Ingrid = "#008000")

data.plot <- ggplot(test.dalt.per, aes(x=date, y=value, color=variable)) +
  geom_point() +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  stat_smooth(aes(fill=variable)) +
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Google Trends con Intención de voto") +
  xlab("Fecha") + 
  ylab("% de busqueda") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

data.plot

trends.3 <- test.dalt.per

# Output most recent data point for model
trends.model.3 <- test.dalt.per[test.dalt.per$date == max(test.dalt.per$date),]
trends.model.3 <- trends.model.3[,-1]

# Import google trends data with intent (2022)
test <- gtrendsR::gtrends(c("Petro Propuestas", "Fico Propuestas", "Fajardo Propuestas", "Ingrid Propuestas", "Rodolfo Propuestas"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0

write.csv(test, "data/trends_intent_propuestas.csv")
# # UPDATE MAY 27!!!!
#  test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/trends_intent_propuestas.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$`Petro Propuestas`, k = 7)
test.d2 <- zoo::rollmean(test.d$`Fico Propuestas`, k = 7)
test.d3 <- zoo::rollmean(test.d$`Fajardo Propuestas`, k = 7)
test.d4 <- zoo::rollmean(test.d$`Ingrid Propuestas`, k = 7)
test.d5 <- zoo::rollmean(test.d$`Rodolfo Propuestas`, k = 7)

test.dalt$Petro <- test.d1*0.5
test.dalt$Fico <- test.d2
test.dalt$Fajardo <- test.d3
test.dalt$Ingrid <- test.d4
test.dalt$Rodolfo <- test.d5
test.dalt <- test.dalt[,-c(2,3,4,5,6)]
test.dalt$total <- test.dalt$Petro + test.dalt$Fico + test.dalt$Fajardo + test.dalt$Ingrid + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,6)]
test.dalt.per <- test.dalt.per / test.dalt$total

blanco <- (blanco.1 + blanco.2 + blanco.3 + as.numeric(reg.model[4]))/4


test.dalt.per <- test.dalt.per * (1-blanco-otros)
test.dalt.per$Blanco <- blanco
test.dalt.per$Otros <- otros
test.dalt.per$date <- test.dalt$date 

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Otros = "#ff0000", Fico = "#0000ff", Fajardo ="#14ce14", Petro = "#800080", Rodolfo = "#c8c800", Ingrid = "#008000")

data.plot <- ggplot(test.dalt.per, aes(x=date, y=value, color=variable)) +
  geom_point() +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  stat_smooth(aes(fill=variable)) +
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Google Trends con Intención de voto") +
  xlab("Fecha") + 
  ylab("% de busqueda") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

data.plot

trends.4 <- test.dalt.per

# Output most recent data point for model
trends.model.4 <- test.dalt.per[test.dalt.per$date == max(test.dalt.per$date),]
trends.model.4 <- trends.model.4[,-1]

colnames(trends.model.1) <- c("variable","model1")
colnames(trends.model.2) <- c("variable","model2")
colnames(trends.model.3) <- c("variable","model3")
colnames(trends.model.4) <- c("variable","model4")

trends.model <- merge(trends.model.1, trends.model.2, by = "variable", all = TRUE)
trends.model <- merge(trends.model, trends.model.3, by = "variable", all = TRUE)
trends.model <- merge(trends.model, trends.model.4, by = "variable", all = TRUE)
trends.model$value <- (trends.model$model1+ trends.model$model2+trends.model$model3+trends.model$model4)/4

# Output for Trends Model
trends.model <- trends.model[,c(1,6)]

# Plot average model

trends.e <- list(trends.1, trends.2, trends.3, trends.4)
trends.e <- Reduce(function(x, y) merge(x, y, all=TRUE, by = c("date","variable")), trends.e)
trends.e$value <- rowMeans(trends.e[,3:6])
trends.e <- trends.e[,-c(3,4,5,6)]

data.plot <- ggplot(trends.e, aes(x=date, y=value, color=variable)) +
  geom_point() +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  stat_smooth(aes(fill=variable)) +
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Google Trends con Intención de voto") +
  xlab("Fecha") + 
  ylab("% de busqueda") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

data.plot

# Clear environment
remove(group.colors, otros, test.d1, test.d2, test.d3, test.d4, test.d5,data.e.model, data.plot, test, test.d, test.dalt, test.dalt.per, trends.model.1, trends.model.2, trends.model.3, trends.model.4, trends.1, trends.2, trends.3, trends.4)

##### ENSEMBLE MODEL ####

reg.model <- t(data.frame(reg.model))
colnames(reg.model) <- c("Fajardo","Fico","Petro","Blanco","Otros")
reg.model <- data.frame(t(reg.model))
reg.model$variable <- rownames(reg.model)

## Model Top Candidates
reg.model.top <- reg.model[c(1,2,3,5),]
reg.model.top$reg.model <- as.numeric(reg.model.top$reg.model)
reg.model.top$reg.model <- reg.model.top$reg.model/sum(reg.model.top$reg.model)

poll.model.top <- poll.model[is.element(poll.model$variable,c("Petro","Fico","Fajardo")),]
poll.model.top.sum <- sum(poll.model.top$value)
poll.model.top <- poll.model[is.element(poll.model$variable,c("Petro","Fico","Fajardo","Otros")),]
poll.model.top$value[4] <- 1-poll.model.top.sum

trends.model.top <- trends.model[is.element(trends.model$variable,c("Petro","Fico","Fajardo")),]
trends.model.top.sum <- sum(trends.model.top$value)
trends.model.top <- trends.model[is.element(trends.model$variable,c("Petro","Fico","Fajardo","Otros")),]
trends.model.top$value[trends.model.top$variable == "Otros"] <- 1-trends.model.top.sum-blanco
trends.model.top$value <- trends.model.top$value/sum(trends.model.top$value)

ensemble.model.top <- merge(reg.model.top, poll.model.top, by = "variable", all = TRUE )
ensemble.model.top <- merge(ensemble.model.top, trends.model.top, by = "variable", all = TRUE )
colnames(ensemble.model.top) <- c("Candidato","Regression","Polls","Trends")
ensemble.model.top$Ensemble <- ensemble.model.top$Regression*0.35 + ensemble.model.top$Polls*0.5 + ensemble.model.top$Trends*0.15


## Model Mid Candidates
poll.model.mid <- poll.model[is.element(poll.model$variable,c("Ingrid","Rodolfo","Otros")),]
poll.model.mid$value <- poll.model.mid$value/sum(poll.model.mid$value)

trends.model.mid <- trends.model[is.element(trends.model$variable,c("Ingrid","Rodolfo","Otros")),]
trends.model.mid$value <- trends.model.mid$value/sum(trends.model.mid$value)

ensemble.model.mid <- merge(poll.model.mid, trends.model.mid, by = "variable", all = TRUE )
colnames(ensemble.model.mid) <- c("Candidato","Polls","Trends")
ensemble.model.mid$Ensemble <- ensemble.model.mid$Polls*0.75 + ensemble.model.mid$Trends*0.25
ensemble.model.mid$Ensemble <- ensemble.model.mid$Ensemble * ensemble.model.top$Ensemble[ensemble.model.top$Candidato == "Otros"]

## Join and create final model
ensemble.model <- rbind(ensemble.model.top[ensemble.model.top$Candidato != "Otros",c(1,5)], ensemble.model.mid[,c(1,4)])
ensemble.model$Ensemble <- ensemble.model$Ensemble*(1-blanco)
ensemble.model <- rbind(ensemble.model, c("Blanco",blanco))
ensemble.model$Ensemble <- as.numeric(ensemble.model$Ensemble)

ensemble.model.otros <- rbind(c("Perez",ensemble.model$Ensemble[5]*2/10), c("Milton",ensemble.model$Ensemble[5]*4/10),c("Gomez",ensemble.model$Ensemble[5]*4/10))
colnames(ensemble.model.otros) <- c("Candidato","Ensemble")
ensemble.model <- rbind(ensemble.model[-5,], ensemble.model.otros)
colnames(ensemble.model) <- c("candidato","int_voto")
ensemble.model$int_voto <- as.numeric(ensemble.model$int_voto)*100


##### OUTPUT ####

ensemble.model %>% 

  # Crear algunas variables
  dplyr::mutate(nombres = case_when(candidato=="Fajardo" ~ "Sergio Fajardo",
                                    candidato=="Fico" ~ "Federico Gutierrez",
                                    candidato=="Petro" ~ "Gustavo Petro",
                                    candidato=="Ingrid" ~ "Ingrid Betancourt",
                                    candidato=="Rodolfo" ~ "Rodolfo Hernandez",
                                    candidato=="Perez" ~ "Luis Perez",
                                    candidato=="Milton" ~ "John M. Rodriguez",
                                    candidato=="Gomez" ~ "Enrique Gomez",
                                    candidato=="Blanco" ~ "Blanco") %>%
                  factor()) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarise(pronostico=mean(int_voto,na.rm=TRUE)) %>%
  dplyr::mutate(intervalo = "ND") %>%
  dplyr::arrange(desc(pronostico)) %>%
  kable("html", 
        digits=1,
        caption = "Pronostico: % votos por candidato (Basado en modelo PoliData)") %>% 
  kable_styling(full_width = F) %>% 
  footnote(number = c("Cocinero: PoliData","Twitter: @PoliticaConDato","Fecha pronóstico: 2022-05-27"))


