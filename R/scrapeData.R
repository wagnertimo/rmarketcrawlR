

library(httr)
library(xml2)

date_from <- '07.03.2017'
date_to <- '14.03.2017'
uenb_type <- '2' # 50Hz (4), TenneT (2), Amprion (3), TransnetBW (1), Netzregelverbund (6), IGCC (11)
rl_type <- 'SRL' # SRL, MRL, RZ_SALDO, REBAP, ZUSATZMASSNAHMEN, NOTHILFE

scrape_rl_calls <- function(date_from, date_to, uenb_type, rl_type) {

  url = 'https://www.regelleistung.net/ext/data/';

  payload = list(
     'from' = date_from,
     'to' = date_to,
     'download' = 'true',
     '_download' = 'on',
     'tsoId' = uenb_type,
     'dataType' = rl_type
  );

  r <- POST(url, body = payload, encode = "form", verbose())

}

response <- scrape_rl_calls(date_from, date_to, uenb_type, rl_type)

content(response, "text")



