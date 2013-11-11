## When does a voter consider himself as nationalist
## @griverorz
## Sat Oct 26 14:44:08 PDT 2013

setwd("~/Documents/datablog/thresholds")

set.seed(20130947)

library(ggplot2)
library(foreign)
library(rjags)
library(mnormt)

# mycolors
theme_set(theme_bw())
myblue <- rgb(100, 170, 200, max = 255)
myred <- rgb(240, 140, 100, max = 255)
mygreen <- rgb(70, 120, 35, max = 255)


#################### read data ####################
parties <- lapply(list.files(path = "./dta", pattern = ".sav", full.names = TRUE), 
                  function(x) read.spss(x, to.data.frame = TRUE))
names(parties) <- c("Catalonia", "Euskadi", "Galicia")

locs <- list(NA, 3)
locs$Catalonia <- data.frame("nac" = parties$Catalonia$P29,
                             "onac" = parties$Catalonia$P24,
                             "voto" = parties$Catalonia$P20A)
locs$Euskadi <- data.frame("nac" = parties$Euskadi$P20,
                           "onac" = parties$Euskadi$P22,
                           "voto" = parties$Euskadi$P40A)
locs$Galicia <- data.frame("nac" = parties$Galicia$P20,
                           "onac" = parties$Galicia$P22,
                           "voto" = parties$Galicia$P40A)

#################### CLEAN VARIABLES ####################
# cheat catalonia: ciu, psoe, pp, erc, icv
levels(locs$Catalonia$voto) <- c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA, NA, NA)
locs$Catalonia$nacparty <- ifelse(locs$Catalonia$voto %in% c(1,4,5), 1, 2)

# cheat euskadi: pp, pse, pnv, amaiur
levels(locs$Euskadi$voto) <- c(1, 2, NA, NA, 3, 4, NA, NA, NA, NA, NA)
locs$Euskadi$nacparty <- ifelse(locs$Euskadi$voto %in% c(3,4), 1, 2)

# cheat galicia: pp, psoe, bng
levels(locs$Galicia$voto) <- c(1, 2, NA, NA, 3, NA, NA, NA, NA, NA)
locs$Galicia$nacparty <- ifelse(locs$Galicia$voto == 3, 1, 2)

#################### remove na's ####################
levels(locs$Catalonia$onac) <- 1:8
locs$Catalonia$onac <- as.numeric(locs$Catalonia$onac)
locs$Catalonia[, "onac"] <- ifelse(locs$Catalonia[, "onac"] >= 6, 
                                   NA, 
                                   locs$Catalonia[, "onac"])

levels(locs$Euskadi$onac) <- 1:8
locs$Euskadi$onac <- as.numeric(locs$Euskadi$onac)
locs$Euskadi[, "onac"] <- ifelse(locs$Euskadi[, "onac"] >= 6, 
                                 NA, 
                                 locs$Euskadi[, "onac"])

levels(locs$Galicia$onac) <- 1:8
locs$Galicia$onac <- as.numeric(locs$Galicia$onac)
locs$Galicia[, "onac"] <- ifelse(locs$Galicia[, "onac"] >= 6, 
                                 NA, 
                                 locs$Galicia[, "onac"])

#################### join data ####################
locs <- rbind(locs$Catalonia, locs$Euskadi, locs$Galicia)

locs$election <- c(rep("Catalonia", nrow(parties$Catalonia)),
                   rep("Euskadi", nrow(parties$Euskadi)),
                   rep("Galicia", nrow(parties$Galicia)))

locs$voto <- as.numeric(as.character(locs$voto))

locs[, "nac"] <- ifelse(locs[, "nac"] > 90, NA, locs[, "nac"])

locs <- na.omit(locs)

#################### descriptives ####################
## Frequency by group
toplot <- lapply(split(locs, locs$election), function(x) 
                 as.data.frame(table(x[, c("nac", "onac")])))

total <- lapply(split(locs, locs$election), function(x) 
                rep(as.data.frame(table(x[, "onac"]))$Freq, each = 10))

toplot <- do.call("rbind", toplot); total <- unlist(total)
toplot$Freq <- toplot$Freq/total
toplot$election <- do.call("rbind", 
                           strsplit(rownames(toplot), split = ".", fixed = TRUE))[,1]


p <- ggplot(toplot, aes(x = nac, y = Freq, group = election))
pq <- p + geom_step(size = 1, aes(colour = election, linetype = election)) + 
    scale_y_continuous("Frequency") +
    scale_x_discrete("Nationalism") +
    facet_grid( ~ onac) + 
    scale_linetype_manual(name = "Election", 
                          breaks = c("Catalonia", 
                              "Euskadi",
                              "Galicia"),
                          labels = c("Catalonia", 
                              "Euskadi",
                              "Galicia"),
                          values = c("Catalonia" = 1, 
                              "Euskadi" = 2,
                              "Galicia" = 4)) +
    scale_colour_manual(name = "Election", 
                        breaks = c("Catalonia", 
                            "Euskadi",
                            "Galicia"),
                        labels = c("Catalonia", 
                            "Euskadi",
                            "Galicia"),
                        values = c("Catalonia" = myred, 
                            "Euskadi" = mygreen,
                            "Galicia" = myblue))
ggsave(filename = "img/descriptives.png", pq, width = 2*par("din")[1])

#################### modeling the moreno question ####################

region <- c("Catalonia", "Euskadi", "Galicia")
jfit <- list(NA, 3)

for (i in 1:3) {
    jdata <- list("N" = nrow(locs[locs$election == region[i],]), 
                  "T" = 4, # nthresholds
                  "onac" = locs$onac[locs$election == region[i]],
                  "nac" = locs$nac[locs$election == region[i]], 
                  "pred" = rep(NA, nrow(locs[locs$election == region[i],]))) 
    
    jinits <- function() {
        return(list(
            "thr" = sort(runif(jdata$T, 1, 10)))) # common threshold
    }
    
    jmodel <- jags.model("src/jags/thresholds.jags", 
                         data = jdata,
                         inits = jinits,
                         n.chains = 2,
                         n.adapt = 1E3)
    
    jfit[[i]] <- coda.samples(model = jmodel,
                              variable.names = "thr", 
                              n.iter = 1E3, 
                              thin = 1)
}

#################### plot results ####################

dfit <- llply(jfit, as.matrix)
get_quantiles <- function(x) quantile(x, c(.025, .5, .975))
dfit <- lapply(dfit, function(x) t(apply(x, 2, get_quantiles)))
dfit <- do.call(rbind, dfit)
dfit <- data.frame(dfit, row.names = 1:nrow(dfit))
names(dfit) <- c("ymin", "ymean", "ymax")
dfit$elections <- rep(region, each = 4)
dfit$threshold <- rep(1:4, 3)

p <- ggplot(dfit, aes(x = threshold, y = ymean, ymin = ymin, ymax = ymax))
pq <- p + geom_pointrange(size = 1) +
  ggtitle("Posterior distribution of the thresholds") +
  scale_y_continuous("Nationalism", limits = c(1, 10)) +
  scale_x_discrete("Threshold") +
  facet_grid(. ~ elections, scales = "free_x") +
  geom_hline(aes(yintercept = 5.5, colour = myred), linetype = 2) + 
  coord_flip() 
ggsave("img/thresholds.png", pq, width = 2.5*par("din")[1])

