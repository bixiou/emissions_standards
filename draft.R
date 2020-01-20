# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
  install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package("readr")
package("data.table")
package("dplyr")
package("tidyverse")

setwd("Data/")

##### Data extraction ####
pats <- read_csv("PATS_IPC-en.csv.zip")
# Retain patents' IPC listed in Appendix B (Patent Search Strategies) of OECD (2011): Invention and Transfer of Environmental Technologies
pats_env <- pats[pats$IPC %in% c("B01D", "B03C", "C10L", "C21B", "F01N", "F01N", "F23V", "F23C", "F23J", "G08B", "F23G", "B63J", "C02F", "C05F",
                             "C09K", "E02B", "E03B", "E03C", "E03F", "C05F", "A23K", "A43B", "A61L", "B03B", "B09B", "B09C", "B22F", "B27B",
                             "B29B", "B30B", "B26D", "B65F", "B65H", "C04B", "C05F", "C09K", "C09K", "C10G", "C10L", "C10M", "C22B", "D01B",
                             "D01G", "D21B", "D21C", "D21H", "E01H", "F23G", "B01D", "F02B", "F02M", "F01N", "F02D", "G01M", "F02M", "F02P",
                             "F01M", "B01D", "B01J", "B62D", "B60C", "B60T", "B60G", "B60K", "B60W", "B60L", "B60R", "B60S", "H01M", "F02B",
                             "B62D", "B30B", "D21B", "B29B", "C08J", "A23K", "B03B", "B30B", "B65D", "C03B", "C03C", "C05F", "C09K", "B09B", "F23G"),]
write_tsv(pats_env, "pats_env.tsv")
rm(pats)

abbrev <- function(old) {
  new <- old
  for (i in 1:length(old)) {
    if (grepl("GM", old[i], ignore.case = T) | grepl("General Motors", old[i], ignore.case = T)) new[i] <- "General Motors" # /!\ Mostly GM Korea
    if (grepl("Daimler", old[i], ignore.case = T)) new[i] <- "Daimler"
    if (grepl("Chevrolet", old[i], ignore.case = T)) new[i] <- "Chevrolet"
    if (grepl("Citroen", old[i], ignore.case = T)) new[i] <- "Citroen"
    if (grepl("Daihatsu", old[i], ignore.case = T)) new[i] <- "Daihatsu"
    if (grepl("Peugeot", old[i], ignore.case = T)) new[i] <- "Peugeot"
    if (grepl("Volkswagen", old[i], ignore.case = T)) new[i] <- "Volkswagen"
    if (grepl("Opel", old[i], ignore.case = T)) new[i] <- "Opel"
    if (grepl("Audi", old[i], ignore.case = T)) new[i] <- "Audi"
    if (grepl("Renault", old[i], ignore.case = T)) new[i] <- "Renault"
    if (grepl("Fiat", old[i], ignore.case = T)) new[i] <- "Fiat"
    if (grepl("BMW", old[i], ignore.case = T) | grepl("Bayerische", old[i], ignore.case = T)) new[i] <- "BMW"
    if (grepl("Toyota", old[i], ignore.case = T)) new[i] <- "Toyota"
    if (grepl("Ford", old[i], ignore.case = T)) new[i] <- "Ford"
    if (grepl("Skoda", old[i], ignore.case = T)) new[i] <- "Skoda"
    if (grepl("Nissan", old[i], ignore.case = T)) new[i] <- "Nissan"
    if (grepl("Volvo", old[i], ignore.case = T)) new[i] <- "Volvo"
    if (grepl("KIA", old[i], ignore.case = T)) new[i] <- "KIA"
    if (grepl("Hyundai", old[i], ignore.case = T)) new[i] <- "Hyundai"
    if (grepl("Seat", old[i], ignore.case = T)) new[i] <- "Seat"
    if (grepl("Chrysler", old[i], ignore.case = T)) new[i] <- "Chrysler"
    if (grepl("Mazda", old[i], ignore.case = T)) new[i] <- "Mazda"
    if (grepl("Daewoo", old[i], ignore.case = T)) new[i] <- "Daewoo"
    if (grepl("Mitsubishi", old[i], ignore.case = T)) new[i] <- "Mitsubishi"
    if (grepl("Honda", old[i], ignore.case = T)) new[i] <- "Honda"
    if (grepl("Dacia", old[i], ignore.case = T)) new[i] <- "Dacia"
    if (grepl("Land Rover", old[i], ignore.case = T)) new[i] <- "Land Rover"
    if (grepl("Suzuki", old[i], ignore.case = T)) new[i] <- "Suzuki"
    if (grepl("Porsche", old[i], ignore.case = T)) new[i] <- "Porsche"
    if (grepl("Jaguar", old[i], ignore.case = T)) new[i] <- "Jaguar"
    if (grepl("Mercedes", old[i], ignore.case = T)) new[i] <- "Mercedes"
    if (grepl("Alfa Romeo", old[i], ignore.case = T)) new[i] <- "Alfa Romeo"
    if (grepl("Tesla", old[i], ignore.case = T)) new[i] <- "Tesla"
    if (grepl("Aston Martin", old[i], ignore.case = T)) new[i] <- "Aston Martin"
    if (grepl("Bentley", old[i], ignore.case = T)) new[i] <- "Bentley"
    if (grepl("Rolls Royce", old[i], ignore.case = T)) new[i] <- "Rolls Royce"
    if (grepl("Bugatti", old[i], ignore.case = T)) new[i] <- "Bugatti"
    if (grepl("Tata", old[i], ignore.case = T)) new[i] <- "Tata"
    if (grepl("Saab", old[i], ignore.case = T)) new[i] <- "Saab"
    if (grepl("Ferrari", old[i], ignore.case = T)) new[i] <- "Ferrari"
    if (grepl("Fuji", old[i], ignore.case = T)) new[i] <- "Fuji"
  }
  return(new)
} # TODO: improve speed by vectorizing
constructors <- c("General Motors", "Daimler", "Citroen", "Ferrari", "Fuji", 
                  "Peugeot", "Volkswagen", "Opel", "Audi", "Renault", "Fiat", "BMW", "Toyota", "Ford", 
                  "Skoda", "Nissan", "Volvo", "KIA", "Hyundai", "Seat", "Chrysler", "Mazda", 
                  "Mitsubishi", "Honda", "Dacia", "Suzuki", "Porsche", "Jaguar", "Mercedes", "Alfa Romeo", 
                  "Tesla", "Aston Martin", "Bentley", "Rolls Royce", "Bugatti", "Tata")
additional_constructors <- c("Daewoo", "Chevrolet", "Saab", "Daihatsu", "Land Rover")

# co2_17 <- read_tsv("CO2_passenger_cars_v17_csv.zip")
# download and unzip automatically TODO
# for (i in 1:7) { # TODO: replace 8 by 9
#   co2[[i]] <-  read_tsv(paste("CO2_passenger_cars_v", min(2*i, 17), ".csv", sep=""))
#   names(co2[[i]])
# }
co2 <- list()
max_i <- 6 # TODO: replace 6 by 9
for (i in 1:max_i) {
  temp <- read_tsv(paste("CO2_passenger_cars_v", min(2*i, 17), ".csv", sep=""))
  # constr <- "Mh"
  # model <- "Cn"
  # co2_nedc <- "e (g/km)" # WLTP replaces NEDC as homologation cycle in 2017
  # # co2_wltp <- "Enwltp (g/km)" # TODO
  # weight <- "m (kg)"
  # type <- "Ft"
  # if (i >= 8) co2 <- "Enedc (g/km)"
  # if (i == 1) constr <- "mh"; weight <- "M (kg)"; co2_nedc <- "E (g/km)";
  # temp <- temp[,c(constr, model, co2_nedc, weight, type)]
  if (i >= 8) temp <- temp %>% rename(co2_nedc = `Enedc (g/km)`)
  if (i == 1) temp <- temp %>% rename(Mh = mh, `m (kg)` = `M (kg)`, `e (g/km)` = `E (g/km)`)
  temp <- temp %>% rename(weight = `m (kg)`, co2_nedc = `e (g/km)`)
  temp <- temp[,c("Mh", "Cn", "co2_nedc", "weight", "Ft")]
  write_tsv(temp, paste("co2_", 9+i, ".tsv", sep=""))
  temp$Mh <- abbrev(temp$Mh)
  temp <- temp[temp$Mh %in% constructors,] # Remove small constructors TODO: how many?
  co2[[9+i]] <- temp %>% group_by(Mh) %>% summarise(n = n(), co2 = mean(co2_nedc, na.rm = T), weight = mean(weight, na.rm = T))
  # following three lines save time (wrt previous three) but wrong because duplicate constructors
  # temp <- temp %>% group_by(Mh) %>% summarise(n = n(), co2 = mean(co2_nedc, na.rm = T), weight = mean(weight, na.rm = T))
  # temp$Mh <- abbrev(temp$Mh)
  # co2[[9+i]] <- temp[temp$Mh %in% constructors,] # Remove small constructors TODO: how many?
  rm(temp)
}
# co2_17 <- read_tsv("CO2_passenger_cars_v17.csv")
# co2_17 <- co2_17[,c("Mh", "Cn", "m (kg)", "Enedc (g/km)", "Ewltp (g/km)", "Ft")]
# write_tsv(co2_17, "co2_17.tsv")
# # co2__17 <- merge(aggregate(`Enedc (g/km)` ~ Mh, co2_17, mean), aggregate(`m (kg)` ~ Mh, co2_17, mean))
# co2_17_main <- co2_17 %>% group_by(Mh) %>% summarise(n = n(), co2 = mean(`Enedc (g/km)`, na.rm = T), weight = mean(`m (kg)`, na.rm = T))
# write_tsv(co2_17_main, "co2_17_main.tsv")

for (v in c("n", "co2", "weight")) {
  co2[[v]] <- matrix(ncol = max_i, nrow = length(co2[[10]]$Mh), dimnames = list(Mh = co2[[15]]$Mh, yr = c(1:max_i)+9))
  for (y in c(1:max_i)+9) co2[[v]][,y] <- co2[[y]][[v]] # order by constructor alphabetically TODO
}

rm(co2_17)


##### Data loading #####
pats_env <- read_tsv("pats_env.csv")
co2_17 <- read_tsv("co2_17_main.tsv")
