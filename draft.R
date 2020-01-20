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

# pats <- read_csv("PATS_IPC-en.7z")
pats <- read_csv("PATS_IPC-en.zip")
pats <- pats[pats$IPC %in% c("B01D", "B03C", "C10L", "C21B", "F01N", "F01N", "F23V", "F23C", "F23J", "G08B", "F23G", "B63J", "C02F", "C05F", 
                             "C09K", "E02B", "E03B", "E03C", "E03F", "C05F", "A23K", "A43B", "A61L", "B03B", "B09B", "B09C", "B22F", "B27B", 
                             "B29B", "B30B", "B26D", "B65F", "B65H", "C04B", "C05F", "C09K", "C09K", "C10G", "C10L", "C10M", "C22B", "D01B", 
                             "D01G", "D21B", "D21C", "D21H", "E01H", "F23G", "B01D", "F02B", "F02M", "F01N", "F02D", "G01M", "F02M", "F02P", 
                             "F01M", "B01D", "B01J", "B62D", "B60C", "B60T", "B60G", "B60K", "B60W", "B60L", "B60R", "B60S", "H01M", "F02B", 
                             "B62D", "B30B", "D21B", "B29B", "C08J", "A23K", "B03B", "B30B", "B65D", "C03B", "C03C", "C05F", "C09K", "B09B", "F23G"),]
write_tsv(pats, "pats_main.tsv")

# co2_17 <- read_tsv("CO2_passenger_cars_v17_csv.zip")
# unzip automatically
co2_17 <- read_tsv("CO2_passenger_cars_v17.csv")
co2_17 <- co2_17[,c("Mh", "Cn", "m (kg)", "Enedc (g/km)", "Ewltp (g/km)", "Ft")]
write_tsv(co2_17, "co2_17.tsv")
# co2__17 <- merge(aggregate(`Enedc (g/km)` ~ Mh, co2_17, mean), aggregate(`m (kg)` ~ Mh, co2_17, mean))
co2_17_main <- co2_17 %>% group_by(Mh) %>% summarise(n = n(), co2 = mean(`Enedc (g/km)`, na.rm = T), weight = mean(`m (kg)`, na.rm = T))
write_tsv(co2_17_main, "co2_17_main.tsv")

rm(co2_17)
