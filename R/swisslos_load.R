library(tidyverse)
library(RJDBC)

drv <- JDBC(
  driverClass = "com.toshiba.mwcloud.gs.sql.Driver",
  # Point this to your gridstore jar
  classPath = "/jdbc/bin/gridstore-jdbc.jar"
)

# IP and port depend on your setup
griddb <- dbConnect(drv, "jdbc:gs://172.20.0.42:20001/dockerGridDB/public", "admin", "admin")

dbInsertTable <- function(conn, name, df, append = TRUE) {
  for (i in seq_len(nrow(df))) {
    RJDBC::dbWriteTable(conn, name, df[i, ], append = append)
  }
}

RJDBC::dbSendUpdate(
  griddb, 
  "CREATE TABLE IF NOT EXISTS swisslos_jackpots (date STRING, jackpot INTEGER);"
)
dbInsertTable(griddb, "swisslos_jackpots", read_csv("data/swisslos_jackpots.csv"))


RJDBC::dbSendUpdate(
  griddb, 
  "CREATE TABLE IF NOT EXISTS swisslos_payouts (combination STRING, winners INTEGER, prize FLOAT, date STRING);"
)
dbInsertTable(griddb, "swisslos_payouts", read_csv("data/swisslos_payouts.csv"))


RJDBC::dbSendUpdate(
  griddb, 
  "CREATE TABLE IF NOT EXISTS swisslos_numbers (type STRING, number INTEGER, date STRING);"
)
dbInsertTable(griddb, "swisslos_numbers", read_csv("data/swisslos_numbers.csv"))

dbRemoveTable(griddb, "swisslos_numbers")

# dbGetQuery(griddb, "SELECT * FROM swisslos_numbers;")

dbDisconnect(griddb)
