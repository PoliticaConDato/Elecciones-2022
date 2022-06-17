## Load packages
library(lubridate)
library(reshape2)
library(scales)
library(tidyr)
library(dplyr)
library(ggplot2)
library(xgboost)
library(kableExtra)
library(png)
library(cowplot)
library(magick)
library(tidyverse)

## Assumptions for "voto en blanco"

blanco.1 <- 0.04 # 1st Round elections adjustment
blanco.2 <- 0.4 # Polling adjustment
blanco.3 <- 0.04 # Historical average

##### POLL MODEL #####

# Load dataset
data.e <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Encuestas.csv")

# Clean data
data.e$Fecha <- mdy(data.e$Fecha)
data.e[is.na(data.e)] <- 0

# Create variables
data.e$Indecisos <- data.e$Blanco + data.e$NS.NR

# Create dataset for full plot
data.e.plot <- data.e[,-c(1,2,4,6,10,11,12)]
data.e.plot <- reshape2::melt(data.e.plot, id=c("Fecha","Rating","Muestra"))
data.e.plot$value <- as.numeric(data.e.plot$value)
data.e.plot <- data.e.plot[,-c(2,3)]

# Add variables for model weighting

data.e.model <- data.e

data.e.model$rating.weight <- data.e.model$Rating / 10
data.e.model$error.weight <- 1-data.e.model$Error*3

election.date <- mdy("06/19/2022")
start.date <- mdy("04/01/2022")

i <- start.date
data.e.model.out <- data.e.model[1,]
data.e.model.out <- data.e.model.out[,c(3,8,9,10,11,12)]

end.date <- mdy("06/14/2022")

while (i <=  end.date) {
  data.loop <- data.e.model
  data.loop$days <- i - data.loop$Fecha
  data.loop$date.weight <- ifelse(data.loop$days < 0, 0, ifelse(data.loop$days > 60, 0, 1.2*(1-data.loop$days/60)))
  data.loop$weight <- data.loop$rating.weight * data.loop$error.weight * data.loop$date.weight
  data.loop <- data.loop[rev(order(data.loop$Encuestadora, data.loop$Fecha)),]
  data.loop <- data.loop[data.loop$date.weight > 0, ]
  data.loop <- data.loop[!duplicated(data.loop$Encuestadora),]
  vote.cols <- data.loop[,c(8,9,10,11,12)]
  blanco.cols <- data.loop[data.loop$Blanco > 0,]
  blanco.cols <- blanco.cols[,c(10,11)]
  blanco.cols$Blanco.Norm <-blanco.cols$Blanco / (1 - blanco.cols$NS.NR)
  
  vote.cols <- vote.cols*data.loop$weight
  vote.cols <- colSums(vote.cols)
  vote.cols <- data.frame(t(vote.cols))
  total.weight <- sum(data.loop$weight)
  vote.cols <- vote.cols / total.weight
  vote.cols$Fecha <- i
  data.e.model.out <- rbind(data.e.model.out, vote.cols)
  
  blanco.cols <- blanco.cols*data.loop$weight[data.loop$Blanco > 0]
  blanco.cols <- colSums(blanco.cols)
  blanco.cols <- data.frame(t(blanco.cols))
  total.weight.blanco <- sum(data.loop$weight[data.loop$Blanco > 0])
  blanco.cols <- blanco.cols / total.weight.blanco
  
  i <- i + 1
}


data.e.model.out <- data.e.model.out[-1,]
data.e.model.out$Indecisos <- data.e.model.out$Blanco + data.e.model.out$NS.NR

blanco.2 <- blanco.cols$Blanco.Norm
polls <- data.loop

# Create dataset to plot model output
data.e.model.plot <- data.e.model.out[,-c(4,5,6)]
data.e.model.plot <- reshape2::melt(data.e.model.plot, id=c("Fecha"))
data.e.model.plot$value <- as.numeric(data.e.model.plot$value)

data.e.plot <- data.e.plot[data.e.plot$Fecha > start.date,]

# Plot full plot

group.colors <- c(Indecisos = "#808285", Petro = "#800080", Rodolfo = "#c8c800")

data.e.plot <- data.e.plot[data.e.plot$Fecha > mdy("04/01/2022"),]
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

my_plot_1 <- ggdraw() +
  draw_plot(data.plot) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_1

png("Encuestas_Completas.png", width = 1200, height = 900, res = 120)
my_plot_1
dev.off()

# Create dataset to plot model output without undecided

blanco <- (blanco.1 + blanco.2 + blanco.3)/3

data.e.model.plot <- data.e.model.out[,-c(4,5,6)]
data.e.model.plot$Petro <- data.e.model.plot$Petro / (1 - data.e.model.plot$Indecisos)
data.e.model.plot$Rodolfo <- data.e.model.plot$Rodolfo / (1 - data.e.model.plot$Indecisos)
data.e.model.plot$Petro <- data.e.model.plot$Petro * (1 - blanco)
data.e.model.plot$Rodolfo <- data.e.model.plot$Rodolfo * (1 - blanco)
data.e.model.plot$Margen <- data.e.model.plot$Petro - data.e.model.plot$Rodolfo
data.e.model.plot <- data.e.model.plot[,c(1,5)]

data.e.model.plot <- reshape2::melt(data.e.model.plot, id=c("Fecha"))
data.e.model.plot$value <- as.numeric(data.e.model.plot$value)

# Create dataset for plot without undecided
data.e.plot.und <- data.e[,-c(1,2,5,6,7,10,11,12)]
data.e.plot.und$Petro <- data.e.plot.und$Petro / (1-data.e.plot.und$Indecisos)
data.e.plot.und$Rodolfo <- data.e.plot.und$Rodolfo / (1-data.e.plot.und$Indecisos)
data.e.plot.und$Petro <- data.e.plot.und$Petro * (1 - blanco)
data.e.plot.und$Rodolfo <- data.e.plot.und$Rodolfo * (1 - blanco)
data.e.plot.und$Margen <- data.e.plot.und$Petro - data.e.plot.und$Rodolfo
data.e.plot.und <- data.e.plot.und[,c(1,2,6)]

data.e.plot.und <- reshape2::melt(data.e.plot.und, id=c("Fecha","Encuestadora"))
data.e.plot.und$value <- as.numeric(data.e.plot.und$value)
data.e.plot.und <- data.e.plot.und[data.e.plot.und$Fecha > start.date,]

# Plot full plot
data.e.plot.filter <- data.e.plot.und[data.e.plot.und$Fecha > mdy("04/01/2022"),]
data.e.model.plot.filter <- data.e.model.plot[data.e.model.plot$Fecha >= min(data.e.plot$Fecha),]

data.plot <- ggplot(data.e.plot.filter, aes(x=Fecha, y=value)) +
  geom_point() +
  stat_smooth(aes()) +
  scale_fill_manual(values=group.colors) +
  scale_color_manual(values=group.colors) + scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Encuestas Presidencia Colombia 2022") +
  xlab("Fecha") + 
  ylab("Margen") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) + 
  geom_line(data = data.e.model.plot.filter, aes(x=Fecha, y=value), linetype = "dashed") +
  geom_ribbon(aes(ymin=-Inf, ymax=0), alpha=0.3, fill = "#c8c800") +
  #scale_fill_manual(values=c("#c8c800"), name="Rodolfo") + 
  geom_ribbon(aes(ymin=0, ymax=Inf), alpha=0.3, fill = "#800080") 
  #scale_fill_manual(values=c("#800080"), name="Petro")

my_plot_2 <- ggdraw() +
  draw_plot(data.plot) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Petro.png", scale = 0.1, x = -0.315, y = 0.385 ) + 
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Rodolfo.png", scale = 0.1, x = -0.315, y = -0.36 ) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_2

png("Encuestas_Margen.png", width = 1200, height = 900, res = 120)
my_plot_2
dev.off()

# Polling output model
poll.model <- data.e.model.out[data.e.model.out$Fecha == max(data.e.model.out$Fecha),]
poll.model$Petro <- poll.model$Petro / (1-poll.model$Indecisos)
poll.model$Rodolfo <- poll.model$Rodolfo / (1-poll.model$Indecisos)
poll.model$Blanco <- blanco
poll.model <- poll.model[,c(2,3,4)]
poll.model$Petro <- poll.model$Petro * (1 - poll.model$Blanco)
poll.model$Rodolfo <- poll.model$Rodolfo * (1 - poll.model$Blanco)
poll.model <- data.frame(t(poll.model))
poll.model$variable <- rownames(poll.model)
colnames(poll.model) <- c("Polls","variable")

# Clean environment
remove(election.date, end.date, group.colors, i, start.date, total.weight, data.e, date.e.model, data.e.model.out, data.e.model.plot, data.e.model.plot.total, data.e.plot, data.e.plot.und, data.e.plot.und.total, data.loop, data.plot, vote.cols, data.e.model.plot.filter, data.e.plot.filter, vote.cols.base, vote.cols.base.dif, vote.cols.base.mean, vote.cols.max, vote.cols.min, vote.cols.base.error.max, vote.cols.base.error.min, vote.cols.base.sd, vote.cols.base.sd.max, vote.cols.base.sd.min, blanco.cols, my_plot_1, my_plot_2, total.weight.blanco)


##### TRENDS MODEL ####

# Import google trends data with intent (presidente)
test <- gtrendsR::gtrends(c("Petro Presidente", "Rodolfo Presidente"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0
 write.csv(test, "data_2nda/trends_intent_presidente.csv")

#  test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/trends_intent_presidente.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$`Petro Presidente`, k = 7)
test.d2 <- zoo::rollmean(test.d$`Rodolfo Presidente`, k = 7)
test.d1.30 <- zoo::rollmean(test.d$`Petro Presidente`, k = 30)
test.d2.30 <- zoo::rollmean(test.d$`Rodolfo Presidente`, k = 30)

test.dalt$Petro <- test.d1*0.8
test.dalt$Rodolfo <- test.d2*1.0
test.dalt <- test.dalt[,-c(2,3)]
test.dalt$total <- test.dalt$Petro + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,3)]
test.dalt.per <- test.dalt.per / test.dalt$total

test.dalt.30 <- test.d[-seq(1,29),]
test.dalt.30$Petro <- test.d1.30*0.85
test.dalt.30$Rodolfo <- test.d2.30*1.0
test.dalt.30 <- test.dalt.30[,-c(2,3)]
test.dalt.30$total <- test.dalt.30$Petro + test.dalt.30$Rodolfo
test.dalt.per.30 <- test.dalt.30[,seq(2,3)]
test.dalt.per.30 <- test.dalt.per.30 / test.dalt.30$total

test.dalt.per <- test.dalt.per * (1-blanco)
test.dalt.per$Blanco <- blanco
test.dalt.per$date <- test.dalt$date 
test.dalt.per$Margen.7 <- test.dalt.per$Petro - test.dalt.per$Rodolfo
test.dalt.per <-  test.dalt.per[,c(4,5)]

test.dalt.per.30 <- test.dalt.per.30 * (1-blanco)
test.dalt.per.30$Blanco <- blanco
test.dalt.per.30$date <- test.dalt.30$date 
test.dalt.per.30$Margen.30 <- test.dalt.per.30$Petro - test.dalt.per.30$Rodolfo
test.dalt.per.30 <-  test.dalt.per.30[,c(4,5)]
test.dalt.per <- merge(test.dalt.per, test.dalt.per.30, by = "date", all.x = TRUE)

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Margen.7 = "#ff0000", Margen.30 = "#0000ff")

test.dalt.per.filter <- test.dalt.per[test.dalt.per$date > mdy("05/15/2022"),]

data.plot <- ggplot(test.dalt.per.filter, aes(x=date, y=value, color=variable)) +
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
test <- gtrendsR::gtrends(c("/g/11hcszl05g", "/m/03c3tmt"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0
write.csv(test, "data_2nda/trends_category.csv")

#test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/trends_category.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
colnames(test.d) <- c("date","Rodolfo","Petro")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$Petro, k = 7)
test.d2 <- zoo::rollmean(test.d$Rodolfo, k = 7)
test.d1.30 <- zoo::rollmean(test.d$Petro, k = 30)
test.d2.30 <- zoo::rollmean(test.d$Rodolfo, k = 30)

test.dalt$Petro <- test.d1*0.8
test.dalt$Rodolfo <- test.d2*1.0
test.dalt$total <- test.dalt$Petro + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,3)]
test.dalt.per <- test.dalt.per / test.dalt$total

test.dalt.30 <- test.d[-seq(1,29),]
test.dalt.30$Petro <- test.d1.30*0.85
test.dalt.30$Rodolfo <- test.d2.30*1.0
test.dalt.30$total <- test.dalt.30$Petro + test.dalt.30$Rodolfo
test.dalt.per.30 <- test.dalt.30[,seq(2,3)]
test.dalt.per.30 <- test.dalt.per.30 / test.dalt.30$total

test.dalt.per <- test.dalt.per * (1-blanco)
test.dalt.per$Blanco <- blanco
test.dalt.per$date <- test.dalt$date 
test.dalt.per$Margen.7 <- test.dalt.per$Petro - test.dalt.per$Rodolfo
test.dalt.per <-  test.dalt.per[,c(4,5)]

test.dalt.per.30 <- test.dalt.per.30 * (1-blanco)
test.dalt.per.30$Blanco <- blanco
test.dalt.per.30$date <- test.dalt.30$date 
test.dalt.per.30$Margen.30 <- test.dalt.per.30$Petro - test.dalt.per.30$Rodolfo
test.dalt.per.30 <-  test.dalt.per.30[,c(4,5)]
test.dalt.per <- merge(test.dalt.per, test.dalt.per.30, by = "date", all.x = TRUE)

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Margen.7 = "#ff0000", Margen.30 = "#0000ff")

test.dalt.per.filter <- test.dalt.per[test.dalt.per$date > mdy("05/15/2022"),]

data.plot <- ggplot(test.dalt.per.filter, aes(x=date, y=value, color=variable)) +
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

trends.2 <- test.dalt.per

# Output most recent data point for model
trends.model.2 <- test.dalt.per[test.dalt.per$date == max(test.dalt.per$date),]
trends.model.2 <- trends.model.2[,-1]


# Import google trends data with intent (2022)
test <- gtrendsR::gtrends(c("Petro 2022", "Rodolfo 2022"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0
write.csv(test, "data_2nda/trends_intent_2022.csv")

#  test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/trends_intent_2022.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$`Petro 2022`, k = 7)
test.d2 <- zoo::rollmean(test.d$`Rodolfo 2022`, k = 7)
test.d1.30 <- zoo::rollmean(test.d$`Petro 2022`, k = 30)
test.d2.30 <- zoo::rollmean(test.d$`Rodolfo 2022`, k = 30)

test.dalt$Petro <- test.d1*0.85
test.dalt$Rodolfo <- test.d2*1.0
test.dalt <- test.dalt[,-c(2,3)]
test.dalt$total <- test.dalt$Petro + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,3)]
test.dalt.per <- test.dalt.per / test.dalt$total

test.dalt.30 <- test.d[-seq(1,29),]
test.dalt.30$Petro <- test.d1.30*0.85
test.dalt.30$Rodolfo <- test.d2.30*1.0
test.dalt.30 <- test.dalt.30[,-c(2,3)]
test.dalt.30$total <- test.dalt.30$Petro + test.dalt.30$Rodolfo
test.dalt.per.30 <- test.dalt.30[,seq(2,3)]
test.dalt.per.30 <- test.dalt.per.30 / test.dalt.30$total

test.dalt.per <- test.dalt.per * (1-blanco)
test.dalt.per$Blanco <- blanco
test.dalt.per$date <- test.dalt$date 
test.dalt.per$Margen.7 <- test.dalt.per$Petro - test.dalt.per$Rodolfo
test.dalt.per <-  test.dalt.per[,c(4,5)]

test.dalt.per.30 <- test.dalt.per.30 * (1-blanco)
test.dalt.per.30$Blanco <- blanco
test.dalt.per.30$date <- test.dalt.30$date 
test.dalt.per.30$Margen.30 <- test.dalt.per.30$Petro - test.dalt.per.30$Rodolfo
test.dalt.per.30 <-  test.dalt.per.30[,c(4,5)]
test.dalt.per <- merge(test.dalt.per, test.dalt.per.30, by = "date", all.x = TRUE)

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Margen.7 = "#ff0000", Margen.30 = "#0000ff")

test.dalt.per.filter <- test.dalt.per[test.dalt.per$date > mdy("05/15/2022"),]

data.plot <- ggplot(test.dalt.per.filter, aes(x=date, y=value, color=variable)) +
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



# Import google trends data with intent (propuestas)
test <- gtrendsR::gtrends(c("Petro Propuestas", "Rodolfo Propuestas"), geo = "CO", time = "today 3-m", onlyInterest = TRUE)
test <- test$interest_over_time
test <- test[,c(1,2,3)]
test$hits <- as.numeric(test$hits)
test$hits[is.na(test$hits)] <- 0
test[is.na(test)] <- 0
write.csv(test, "data_2nda/trends_intent_propuestas.csv")

#  test <- read.csv("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data/trends_intent_propuestas.csv")
#  test <- test[,-1]
#  test$date <- ymd(test$date)

# Create model based on moving averages
test.d <- dcast(data = test, formula = date ~ keyword, fun.aggregate = sum, value.var = "hits")
test.dalt <- test.d[-seq(1,6),]
test.d1 <- zoo::rollmean(test.d$`Petro Propuestas`, k = 7)
test.d2 <- zoo::rollmean(test.d$`Rodolfo Propuestas`, k = 7)
test.d1.30 <- zoo::rollmean(test.d$`Petro Propuestas`, k = 30)
test.d2.30 <- zoo::rollmean(test.d$`Rodolfo Propuestas`, k = 30)

test.dalt$Petro <- test.d1*0.8
test.dalt$Rodolfo <- test.d2*1.0
test.dalt <- test.dalt[,-c(2,3)]
test.dalt$total <- test.dalt$Petro + test.dalt$Rodolfo
test.dalt.per <- test.dalt[,seq(2,3)]
test.dalt.per <- test.dalt.per / test.dalt$total

test.dalt.30 <- test.d[-seq(1,29),]
test.dalt.30$Petro <- test.d1.30*0.85
test.dalt.30$Rodolfo <- test.d2.30*1.0
test.dalt.30 <- test.dalt.30[,-c(2,3)]
test.dalt.30$total <- test.dalt.30$Petro + test.dalt.30$Rodolfo
test.dalt.per.30 <- test.dalt.30[,seq(2,3)]
test.dalt.per.30 <- test.dalt.per.30 / test.dalt.30$total

test.dalt.per <- test.dalt.per * (1-blanco)
test.dalt.per$Blanco <- blanco
test.dalt.per$date <- test.dalt$date 
test.dalt.per$Margen.7 <- test.dalt.per$Petro - test.dalt.per$Rodolfo
test.dalt.per <-  test.dalt.per[,c(4,5)]

test.dalt.per.30 <- test.dalt.per.30 * (1-blanco)
test.dalt.per.30$Blanco <- blanco
test.dalt.per.30$date <- test.dalt.30$date 
test.dalt.per.30$Margen.30 <- test.dalt.per.30$Petro - test.dalt.per.30$Rodolfo
test.dalt.per.30 <-  test.dalt.per.30[,c(4,5)]
test.dalt.per <- merge(test.dalt.per, test.dalt.per.30, by = "date", all.x = TRUE)

test.dalt.per <- reshape2::melt(test.dalt.per, id=c("date"))

# Plot full plot
group.colors <- c(Margen.7 = "#ff0000", Margen.30 = "#0000ff")

test.dalt.per.filter <- test.dalt.per[test.dalt.per$date > mdy("05/15/2022"),]

data.plot <- ggplot(test.dalt.per.filter, aes(x=date, y=value, color=variable)) +
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
trends.model$value <- (trends.model$model1+trends.model$model3+trends.model$model4)/3

# Output for Trends Model
trends.model <- trends.model[,c(1,6)]

# Plot average model

trends.e <- list(trends.1, trends.2, trends.3, trends.4)
trends.e <- Reduce(function(x, y) merge(x, y, all=TRUE, by = c("date","variable")), trends.e)
trends.e$value <- rowMeans(trends.e[,c(3,5,6)])
trends.e <- trends.e[,-c(3,4,5,6)]
trends.e <- dcast(data = trends.e, formula = date ~ variable, fun.aggregate = mean, value.var = "value")
trends.e$Margen <- (trends.e$Margen.7 + trends.e$Margen.30)/2
trends.e <- trends.e[,c(1,4)]

trends.e.filter <- trends.e[trends.e$date > mdy("05/15/2022"),]
trends.e.filter

data.plot <- ggplot(trends.e.filter, aes(x=date, y=Margen)) +
  geom_point() +
  stat_smooth(aes()) +
  
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Google Trends con Intención de voto") +
  xlab("Fecha") + 
  ylab("Margen") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
geom_ribbon(aes(ymin=-Inf, ymax=0), alpha=0.3, fill = "#c8c800") +
  #scale_fill_manual(values=c("#c8c800"), name="Rodolfo") + 
  geom_ribbon(aes(ymin=0, ymax=Inf), alpha=0.3, fill = "#800080") 
  #scale_fill_manual(values=c("#800080"), name="Petro")
data.plot

my_plot_3 <- ggdraw() +
  draw_plot(data.plot) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Petro.png", scale = 0.1, x = -0.315, y = 0.385 ) + 
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Rodolfo.png", scale = 0.1, x = -0.315, y = -0.36 ) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_3

png("GTrends_Margen.png", width = 1200, height = 900, res = 120)
my_plot_3
dev.off()

# Clear environment
remove(group.colors, otros, test.d1, test.d2, test.d3, test.d4, test.d5,data.e.model, data.plot, test, test.d, test.dalt, test.dalt.per, trends.model.1, trends.model.2, trends.model.3, trends.model.4, trends.1, trends.2, trends.3, trends.4, master.trend, master.trend.sum, trends.e, test.dalt.30, test.dalt.per.30, test.dalt.per.filter, trends.e.filter, test.d1.30, test.d2.30)

##### ENSEMBLE MODEL ####

trends.margin <- mean(trends.model$value)

ensemble.model <- poll.model
ensemble.model$trends[ensemble.model$variable == "Blanco"] <- blanco
ensemble.model$trends[ensemble.model$variable == "Petro"] <- (1+trends.margin-blanco)/2
ensemble.model$trends[ensemble.model$variable == "Rodolfo"] <- (1-trends.margin-blanco)/2

ensemble.model$ensemble <- ensemble.model$Polls*0.8 + ensemble.model$trends*0.2
ensemble.model <- ensemble.model[,c(2,4)]

colnames(ensemble.model) <- c("candidato","int_voto")
ensemble.model$int_voto <- as.numeric(ensemble.model$int_voto)*100

## Create confidence interval 
polls.prob <- polls[,c(3,7,8,9,18)]
polls.prob$total <- polls.prob$Petro + polls.prob$Rodolfo
polls.prob$Petro <- polls.prob$Petro / polls.prob$total
polls.prob$Rodolfo <- polls.prob$Rodolfo / polls.prob$total

polls.prob$margin <- polls.prob$Petro - polls.prob$Rodolfo

polls.prob2 <- polls.prob[,c(2,3,5)]
weight <- sum(polls.prob$weight)
polls.prob$weight <- polls.prob$weight/weight

polls.binom <- apply(polls.prob2, 1, function(x) rbinom(n = x[3]*1000000, size = x[1], prob = x[2]))

output.vec <- as.numeric(unlist(polls.binom[1])) / polls.prob2$Muestra[1]

for(i in 2:nrow(polls.prob)) {
  output.vec <- c(output.vec,as.numeric(unlist(polls.binom[i])) / polls.prob2$Muestra[i])
  
}

quant.petro <- as.numeric(quantile(output.vec, c(0.05, 0.95)))*(1-blanco)
quant.rodolfo <- 1 - quant.petro - blanco
quant.blanco <- quant.rodolfo
quant.blanco[1] <- blanco.1
quant.blanco[2] <- blanco.2

quant <- as.data.frame(rbind(sort(quant.petro), sort(quant.rodolfo), quant.blanco))
quant <- quant*100
quant$inter <- paste0(round(quant$V1,1),"-",round(quant$V2,1))

ensemble.model <- cbind(ensemble.model, quant$inter)
colnames(ensemble.model)[3] <- "inter"


ensemble.model <- arrange(ensemble.model, desc(int_voto))

##### OUTPUT ####

ensemble.model %>% 
  
  # Crear algunas variables
  dplyr::mutate(nombres = case_when(candidato=="Petro" ~ "Gustavo Petro",
                                    candidato=="Rodolfo" ~ "Rodolfo Hernandez",
                                    candidato=="Blanco" ~ "Blanco") %>%
                  factor()) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarise(pronostico=mean(int_voto,na.rm=TRUE)) %>%
  dplyr::arrange(desc(pronostico))  %>%
  dplyr::mutate(intervalo = ensemble.model$inter) %>%
  kable("html", 
        digits=1,
        caption = "Pronostico: % votos por candidato (Basado en modelo PoliData)") %>% 
  kable_styling(full_width = F) %>% 
  footnote(number = c("Cocinero: PoliData","Twitter: @PoliticaConDato","Fecha pronóstico: 2022-06-17"))

ensemble.model$Polls <- poll.model$Polls*100

output.model <- mutate(ensemble.model, nombres = case_when(candidato=="Petro" ~ "Gustavo Petro",
                                                           candidato=="Rodolfo" ~ "Rodolfo Hernandez",
                                                           candidato=="Blanco" ~ "Blanco") %>%
                         factor())


output.model <- output.model %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarise(modelo_ensamble=mean(int_voto,na.rm=TRUE)) %>%
  dplyr::arrange(desc(modelo_ensamble)) 

output.model2 <- mutate(ensemble.model, nombres = case_when(candidato=="Petro" ~ "Gustavo Petro",
                                                            candidato=="Rodolfo" ~ "Rodolfo Hernandez",
                                                            candidato=="Blanco" ~ "Blanco") %>%
                          factor()) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarise(modelo_encuestas=mean(Polls,na.rm=TRUE))  %>%
  dplyr::arrange(desc(modelo_encuestas)) %>%
  dplyr::mutate(intervalo = ensemble.model$inter)

output.model <- cbind(output.model, output.model2[,c(2,3)])

kable(output.model, "html", 
      digits=1,
      caption = "Pronostico: % votos por candidato (Basado en modelo PoliData)") %>% 
  kable_styling(full_width = F) %>% 
  footnote(number = c("Cocinero: PoliData","Twitter: @PoliticaConDato","Fecha pronóstico: 2022-06-17"))
  
##### PROBABILISTIC MODEL #####

output.mar <- 2*(output.vec) -1 

# Histogram
hist(output.mar, prob = TRUE,
     main = "Histogram with density curve")


lines(density(output.mar), col = 4, lwd = 2)

output.mar <- data.frame(output.mar)
output.dense <- density(output.mar$output.mar)
output.dense <- cbind(output.dense$x, output.dense$y)
output.dense <- as.data.frame(output.dense)
output.dense$group <- ifelse(output.dense$V1 < 0, "Rodolfo", "Petro")

ensemble.margin <- ensemble.model$int_voto[1]/(1-blanco) - ensemble.model$int_voto[2]/(1-blanco) 

group.colors <- c(Petro = "#800080", Rodolfo = "#c8c800")


data.hist <- ggplot(output.dense, aes(x=V1, y=V2, fill=group)) +
  geom_area() +
  geom_line() +
  scale_fill_manual(values=group.colors) +
  
  theme(legend.position="top", legend.title = element_blank(), legend.box = "horizontal", plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  ggtitle("Distribución de margen 2nda vuelta") +
  xlab("Margen (Petro % - Rodolfo%)") +
  scale_x_continuous(labels = scales::percent)   +
  geom_vline(aes(xintercept = mean(output.mar$output.mar)),col='red', linetype="dashed",size=1) +
  geom_text(aes(mean(output.mar$output.mar), 2, label=paste0(round(mean(output.mar$output.mar)*100,1),"%"), color = 'Modelo Encuestas', hjust = 1.4, vjust = -20)) + 
geom_vline(aes(xintercept = ensemble.margin/100),col='blue', linetype="dashed",size=1) +
  geom_text(aes(ensemble.margin/100, 2, label=paste0(round(ensemble.margin,1),"%"), color = 'Modelo Ensamble', hjust = -1, vjust = -20))

data.hist


my_plot_4 <- ggdraw() +
  draw_plot(data.hist) +
  #draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Petro.png", scale = 0.1, x = -0.315, y = 0.385 ) + 
  #draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/Rodolfo.png", scale = 0.1, x = -0.315, y = -0.36 ) +
  draw_image("https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png", scale = 0.07, x = 0.475, y = -0.47 ) +
  draw_text("@PoliticaConDato", size = 12, x = 0.85, y = 0.03)

my_plot_4

png("Probabilidad.png", width = 1200, height = 900, res = 120)
my_plot_4
dev.off()





c50 <- length(which(output.vec<0.5))/length(output.vec)



#### Probabilistic model trials #####

polls.new <- polls.prob %>% 
  mutate(margin = Petro - Rodolfo )


results <- polls.new %>% 
  summarize(avg = mean(margin), 
            sd = sd(margin),
            se = sd(margin) / sqrt(length(margin))) %>% 
  mutate(start = avg - 1.96 * se, 
         end = avg + 1.96 * se) 
round(results * 100, 1)




B <- 10000
mu <- 0
tau <- 0.02 #0.035

petro_win <- replicate(B, {
  results %>% mutate(sigma = sqrt(se^2 + .025^2), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     result = rnorm(length(posterior_mean), 
                                    posterior_mean, posterior_se),
                     petro = ifelse( result > 0, 1, 0)) %>%
    summarise(petro =  sum(petro)) %>%
    pull(petro)
})

petro_per <- replicate(B, {
  results %>% mutate(sigma = sqrt(se^2 + .025^2), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     result = rnorm(length(posterior_mean), 
                                    posterior_mean, posterior_se)) %>%
    pull(result)
})



mean(petro_win)
hist(petro_per)
c50.2 <- length(which(petro_per>0))/length(petro_per)
