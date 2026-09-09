# =============================================================================
# TOPPTIPSET - 100% AUTOMATISKT SKRIPT V3.1 (ANPASSAT EFTER SVENSKA SPEL API)
# Läser både odds, streck, omsättning och lagnamn direkt ur Svenska Spels JSON.
# V3.1: Utökad modell med 16 variabler (entropi, varians, bias, value ratio m.m.)
# =============================================================================

# --- Paket ---
for (pkg in c("dplyr", "tidyr", "readr", "stringr", "httr", "jsonlite")) {
  if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, dependencies=TRUE)
}
library(dplyr); library(tidyr); library(readr); library(stringr); library(httr); library(jsonlite)

# =============================================================================
# INSTÄLLNINGAR
# =============================================================================

SÖKVÄG_MATCHDATA   <- "/Users/fabianlindh/Desktop/Projekt toptips/data/TipsXtra_Topptipset_Statistik_Detaljer.csv"
SÖKVÄG_OMGÅNGSDATA <- "/Users/fabianlindh/Desktop/Projekt toptips/data/TipsXtra_Topptipset_Statistik_Summering.csv"
SÖKVÄG_EXPORT_FIL  <- "/Users/fabianlindh/Desktop/Projekt toptips/topptipset_kupong.txt"  

TRÖSKEL         <- 0.15     # Minsta förväntad avkastning per rad (0.10 = 10%)
MIN_SANNOLIKHET <- 0.0003   # Minsta chans att raden går in (0.0003 = 0.03%)
TOPP_N          <- 400      # Max antal rader att visa / exportera
RAD_INSATS      <- 1        # Radinsats i kr (1, 2, 5 eller 10 kr enligt Svenska Spel)

# =============================================================================
# STEG 1: LÄSER IN OCH FÖRBEREDER TRÄNINGSDATA (automatiskt)
# =============================================================================

cat("Läser träningsdata från lokal fil...\n")
raw <- read_delim(SÖKVÄG_MATCHDATA, delim=";", show_col_types=FALSE,
                  locale=locale(encoding="UTF-8")) %>%
  # --- ROBUST METOD FÖR ATT FILTRERA ---
  group_by(omgang) %>% 
  filter(n() == 8) %>% 
  ungroup() %>%
  # ------------------------------------------------
mutate(
  utfall_num = case_when(utfall=="1"~1L, utfall=="X"~2L, utfall=="2"~3L),
  oddset1 = as.numeric(oddset1),
  oddsetx = as.numeric(oddsetx),
  oddset2 = as.numeric(oddset2),
  sf1 = as.numeric(svenska_folket1)/100,
  sfx = as.numeric(svenska_folketx)/100,
  sf2 = as.numeric(svenska_folket2)/100,
  po1 = (1/oddset1)/(1/oddset1+1/oddsetx+1/oddset2),
  pox = (1/oddsetx)/(1/oddset1+1/oddsetx+1/oddset2),
  po2 = (1/oddset2)/(1/oddset1+1/oddsetx+1/oddset2)
) %>%
  filter(!is.na(utfall_num), !is.na(oddset1))

omg_data <- read_delim(SÖKVÄG_OMGÅNGSDATA, delim=";", show_col_types=FALSE) %>%
  filter(produktnamn == "Topptipset")

brett <- raw %>%
  arrange(omgang, matchnummer) %>%
  group_by(omgang) %>%
  mutate(match_id = row_number()) %>%
  ungroup()

po_mat <- dplyr::select(brett, omgang, match_id, po1, pox, po2) %>%
  pivot_wider(names_from=match_id, values_from=c(po1,pox,po2), names_sep="_m")

sf_mat <- dplyr::select(brett, omgang, match_id, sf1, sfx, sf2) %>%
  pivot_wider(names_from=match_id, values_from=c(sf1,sfx,sf2), names_sep="_m")

facit <- brett %>%
  group_by(omgang) %>%
  summarise(correct_row=paste(case_when(
    utfall_num==1~"1", utfall_num==2~"X", utfall_num==3~"2"), collapse=""), .groups="drop")

brett <- po_mat %>%
  left_join(sf_mat, by="omgang") %>%
  left_join(facit,  by="omgang") %>%
  filter(nchar(correct_row)==8) %>%
  left_join(raw %>% dplyr::select(omgang, svspelinfo_id) %>% distinct(), by="omgang") %>%
  left_join(dplyr::select(omg_data, id, turnover, utd13, ant13), by=c("svspelinfo_id"="id")) %>%
  filter(!is.na(turnover), turnover>0, !is.na(utd13))

cat("Antal omgångar för träning:", nrow(brett), "\n")

# =============================================================================
# STEG 2: HJÄLPFUNKTIONER
# =============================================================================

hämta_po <- function(omg_rad, k, u) {
  as.numeric(omg_rad[[switch(as.character(u),
                             "1"=paste0("po1_m",k), "2"=paste0("pox_m",k), "3"=paste0("po2_m",k))]])
}

hämta_sf <- function(omg_rad, k, u) {
  as.numeric(omg_rad[[switch(as.character(u),
                             "1"=paste0("sf1_m",k), "2"=paste0("sfx_m",k), "3"=paste0("sf2_m",k))]])
}

beräkna_x <- function(omg_rad, rv) {
  po <- sapply(1:8, function(k) hämta_po(omg_rad, k, rv[k]))
  sf <- sapply(1:8, function(k) hämta_sf(omg_rad, k, rv[k]))
  
  # --- Originalvariabler (x1-x7) ---
  x1 <- prod(po)
  x2 <- prod(sf)
  x3 <- x1^2
  x4 <- x1 * x2
  x5 <- (x1 * x2)^2
  x6 <- x1 / x2
  x7 <- (x1 / x2)^2
  
  # --- Nya variabler (x8-x16) ---
  diffs <- abs(sf - po)
  x8  <- mean(diffs)
  x9  <- max(diffs)
  x10 <- -sum(po * log(po + 1e-9))
  x11 <- sum(po > 0.5) / 8
  x12 <- var(po)
  x13 <- -sum(sf * log(sf + 1e-9))
  x14 <- sum(po < 0.20)
  x15 <- max(po / (sf + 1e-9))
  x16 <- mean(sf - po)
  
  c(x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6, x7=x7,
    x8=x8, x9=x9, x10=x10, x11=x11, x12=x12, x13=x13, x14=x14, x15=x15, x16=x16)
}

# =============================================================================
# STEG 3: SKATTA MODELLEN
# =============================================================================

cat("Skattar modellen...\n")

n <- nrow(brett)

# Konvertera alla correct_row till en n×8 heltalsmatris
rv_mat <- do.call(rbind, lapply(strsplit(brett$correct_row, ""), function(chars) {
  ifelse(chars == "1", 1L, ifelse(chars == "X", 2L, 3L))
}))

# Plocka ut po- och sf-kolumnerna
po1_m <- as.matrix(brett[, paste0("po1_m", 1:8)])
pox_m <- as.matrix(brett[, paste0("pox_m", 1:8)])
po2_m <- as.matrix(brett[, paste0("po2_m", 1:8)])
sf1_m <- as.matrix(brett[, paste0("sf1_m", 1:8)])
sfx_m <- as.matrix(brett[, paste0("sfx_m", 1:8)])
sf2_m <- as.matrix(brett[, paste0("sf2_m", 1:8)])

# Indexering
row_idx <- matrix(rep(seq_len(n), 8), ncol = 8)
col_idx <- rv_mat

po_arr <- array(c(po1_m, pox_m, po2_m), dim = c(n, 8, 3))
sf_arr <- array(c(sf1_m, sfx_m, sf2_m), dim = c(n, 8, 3))

po_sel <- matrix(po_arr[cbind(rep(seq_len(n), 8), rep(1:8, each = n), as.vector(col_idx))], nrow = n, ncol = 8)
sf_sel <- matrix(sf_arr[cbind(rep(seq_len(n), 8), rep(1:8, each = n), as.vector(col_idx))], nrow = n, ncol = 8)

# --- SKAPAR DE 16 UTVALDA VARIABLERNA ---
diffs <- abs(sf_sel - po_sel)

x1  <- apply(po_sel, 1, prod)
x2  <- apply(sf_sel, 1, prod)
x3  <- x1^2
x4  <- x1 * x2
x5  <- (x1 * x2)^2
x6  <- x1 / x2
x7  <- (x1 / x2)^2
x8  <- rowMeans(diffs)
x9  <- apply(diffs, 1, max)
x10 <- -rowSums(po_sel * log(po_sel + 1e-9))
x11 <- rowSums(po_sel > 0.5) / 8
x12 <- apply(po_sel, 1, var)
x13 <- -rowSums(sf_sel * log(sf_sel + 1e-9))
x14 <- rowSums(po_sel < 0.20)
x15 <- apply(po_sel / (sf_sel + 1e-9), 1, max)
x16 <- rowMeans(sf_sel - po_sel)

x_mat <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
colnames(x_mat) <- paste0("x", 1:16)

modelldata         <- as.data.frame(x_mat)
modelldata$y       <- brett$ant13
modelldata$n_total <- brett$turnover

modell <- glm(cbind(y, n_total - y) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
                x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16, 
              data = modelldata, 
              family = quasibinomial(link = "logit"))

cat("Modell klar!\n\n")

# =============================================================================
# LIVE-HÄMTNING: SVENSKA SPEL API (DIREKT-PARSNING)
# =============================================================================

cat("Kopplar upp mot Svenska Spel API (Topptipset)...\n")
url_tt <- "https://api.spela.svenskaspel.se/draw/1/topptipsetfamily/draws"
res_tt <- GET(url_tt)

if (status_code(res_tt) != 200) {
  stop("Kunde inte hämta data från Svenska Spel API. Kontrollera internetanslutningen.")
}

api_data <- fromJSON(content(res_tt, "text", encoding = "UTF-8"), simplifyVector = FALSE)

# Plocka ut den första tillgängliga Topptipset-omgången
omgang_aktuell <- NULL
for (d in api_data$draws) {
  if (d$productName == "Topptipset") {
    omgang_aktuell <- d
    break
  }
}
if (is.null(omgang_aktuell)) omgang_aktuell <- api_data$draws[[1]]

OMK_NUMMER <- omgang_aktuell$drawNumber
OMSÄTTNING <- as.numeric(gsub(",", ".", gsub(" ", "", omgang_aktuell$currentNetSale)))
JACKPOTT   <- ifelse(is.null(omgang_aktuell$fund$extraMoney), 0, as.numeric(gsub(",", ".", omgang_aktuell$fund$extraMoney)))
STÄNGNING  <- omgang_aktuell$regCloseTime

coup_type <- NULL
if (!is.null(omgang_aktuell$productName) && omgang_aktuell$productName == "Topptipset Stryk") coup_type <- "Stryk"
if (!is.null(omgang_aktuell$productName) && omgang_aktuell$productName == "Topptipset Europa") coup_type <- "Europa"

cat(sprintf("Hämtade %s omgång %s (Stänger: %s)\n", omgang_aktuell$productName, OMK_NUMMER, STÄNGNING))

# =============================================================================
# AUTOMATISK TIMING: VÄNTA TILLS PRECIS INNAN STÄNGNING
# =============================================================================
SEKUNDER_FÖRE_STÄNGNING <- 999999


stängning_tid <- tryCatch({
  as.POSIXct(STÄNGNING, format = "%Y-%m-%dT%H:%M:%S", tz = "Europe/Stockholm")
}, error = function(e) {
  as.POSIXct(gsub("T", " ", substr(STÄNGNING, 1, 19)),
             format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Stockholm")
})

if (is.na(stängning_tid)) {
  cat("VARNING: Kunde inte tolka stängningstiden. Kör beräkningarna omedelbart.\n")
} else {
  kör_vid <- stängning_tid - SEKUNDER_FÖRE_STÄNGNING
  nu      <- Sys.time()
  vänta_s <- as.numeric(difftime(kör_vid, nu, units = "secs"))
  
  if (vänta_s > 0) {
    cat(sprintf(
      "Stängning: %s\nKör beräkningar kl: %s (om %.0f sekunder / %.1f minuter)\n",
      format(stängning_tid, "%Y-%m-%d %H:%M:%S"),
      format(kör_vid,       "%Y-%m-%d %H:%M:%S"),
      vänta_s, vänta_s / 60
    ))
    cat("Väntar...\n")
    
    while ({nu <- Sys.time(); kvar <- as.numeric(difftime(kör_vid, nu, units="secs")); kvar > 60}) {
      cat(sprintf("  %.0f minuter kvar...\n", kvar / 60))
      Sys.sleep(60)
    }
    kvar_s <- as.numeric(difftime(kör_vid, Sys.time(), units="secs"))
    if (kvar_s > 0) {
      cat(sprintf("  %.0f sekunder kvar - väntar till körning...\n", kvar_s))
      Sys.sleep(kvar_s)
    }
    cat("Tid att köra! Hämtar senaste streckdata och beräknar rader...\n\n")
    
    res_tt2 <- GET(url_tt)
    if (status_code(res_tt2) == 200) {
      api_data2      <- fromJSON(content(res_tt2, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      omgang_aktuell <- NULL
      for (d in api_data2$draws) {
        if (d$drawNumber == OMK_NUMMER) { omgang_aktuell <- d; break }
      }
      if (is.null(omgang_aktuell)) omgang_aktuell <- api_data2$draws[[1]]
      OMSÄTTNING <- as.numeric(gsub(",", ".", gsub(" ", "", omgang_aktuell$currentNetSale)))
      JACKPOTT   <- ifelse(is.null(omgang_aktuell$fund$extraMoney), 0,
                           as.numeric(gsub(",", ".", omgang_aktuell$fund$extraMoney)))
      events     <- omgang_aktuell$drawEvents
      events     <- events[order(sapply(events, function(e) e$eventNumber))]
      cat(sprintf("Uppdaterad omsättning: %s kr\n", format(round(OMSÄTTNING, 0), big.mark=" ")))
    } else {
      cat("VARNING: Kunde inte uppdatera API-data. Använder data som hämtades vid start.\n")
    }
  } else {
    cat(sprintf(
      "VARNING: Stängningstiden (%s) har redan passerat eller är inom %d sek. Kör omedelbart.\n",
      format(stängning_tid, "%Y-%m-%d %H:%M:%S"), SEKUNDER_FÖRE_STÄNGNING
    ))
  }
}

events <- omgang_aktuell$drawEvents
events <- events[order(sapply(events, function(e) e$eventNumber))]

hämta_odds <- function(events, fält, fallback = "3,00") {
  as.numeric(gsub(",", ".", sapply(events, function(e) {
    val <- e$odds[[fält]]
    if (is.null(val)) val <- e$startOdds[[fält]]
    if (is.null(val)) val <- fallback
    val
  })))
}

match_data <- data.frame(
  odds1 = hämta_odds(events, "one"),
  oddsx = hämta_odds(events, "x"),
  odds2 = hämta_odds(events, "two"),
  sf1   = as.numeric(sapply(events, function(e) e$svenskaFolket$one)),
  sfx   = as.numeric(sapply(events, function(e) e$svenskaFolket$x)),
  sf2   = as.numeric(sapply(events, function(e) e$svenskaFolket$two)),
  hemma = sapply(events, function(e) {
    p <- Filter(function(p) p$type == "home", e$match$participants)
    if (length(p)) p[[1]]$name else "Hemma"
  }),
  borta = sapply(events, function(e) {
    p <- Filter(function(p) p$type == "away", e$match$participants)
    if (length(p)) p[[1]]$name else "Borta"
  }),
  stringsAsFactors = FALSE
)

# =============================================================================
# STEG 4: BERÄKNA SPELVÄRDA RADER FÖR DAGENS OMGÅNG
# =============================================================================

inv_sum <- rowSums(1/match_data[,c("odds1","oddsx","odds2")])
omg_rad <- as.data.frame(as.list(c(
  setNames((1/match_data$odds1)/inv_sum, paste0("po1_m",1:8)),
  setNames((1/match_data$oddsx)/inv_sum, paste0("pox_m",1:8)),
  setNames((1/match_data$odds2)/inv_sum, paste0("po2_m",1:8)),
  setNames(match_data$sf1/100,           paste0("sf1_m",1:8)),
  setNames(match_data$sfx/100,           paste0("sfx_m",1:8)),
  setNames(match_data$sf2/100,           paste0("sf2_m",1:8))
)))

ALLA_RADER <- expand.grid(m1=1:3,m2=1:3,m3=1:3,m4=1:3,
                          m5=1:3,m6=1:3,m7=1:3,m8=1:3) %>% as.matrix()

cat("Beräknar spelvärde för alla 6561 rader...\n")

po1_v <- unlist(omg_rad[, paste0("po1_m", 1:8)])
pox_v <- unlist(omg_rad[, paste0("pox_m", 1:8)])
po2_v <- unlist(omg_rad[, paste0("po2_m", 1:8)])
sf1_v <- unlist(omg_rad[, paste0("sf1_m", 1:8)])
sfx_v <- unlist(omg_rad[, paste0("sfx_m", 1:8)])
sf2_v <- unlist(omg_rad[, paste0("sf2_m", 1:8)])

N_rows <- nrow(ALLA_RADER)
M <- 8

rv_all <- ALLA_RADER

po_tbl <- rbind(po1_v, pox_v, po2_v)
sf_tbl <- rbind(sf1_v, sfx_v, sf2_v)

po_mat_all <- matrix(po_tbl[cbind(as.vector(rv_all), rep(1:M, each = N_rows))], nrow = N_rows, ncol = M)
sf_mat_all <- matrix(sf_tbl[cbind(as.vector(rv_all), rep(1:M, each = N_rows))], nrow = N_rows, ncol = M)

# --- APPLICERAR DE 16 UTVALDA VARIABLERNA PÅ LIVE-DATAN ---
diffs_all <- abs(sf_mat_all - po_mat_all)

x1  <- apply(po_mat_all, 1, prod)
x2  <- apply(sf_mat_all, 1, prod)
x3  <- x1^2
x4  <- x1 * x2
x5  <- (x1 * x2)^2
x6  <- x1 / x2
x7  <- (x1 / x2)^2
x8  <- rowMeans(diffs_all)
x9  <- apply(diffs_all, 1, max)
x10 <- -rowSums(po_mat_all * log(po_mat_all + 1e-9))
x11 <- rowSums(po_mat_all > 0.5) / 8
x12 <- apply(po_mat_all, 1, var)
x13 <- -rowSums(sf_mat_all * log(sf_mat_all + 1e-9))
x14 <- rowSums(po_mat_all < 0.20)
x15 <- apply(po_mat_all / (sf_mat_all + 1e-9), 1, max)
x16 <- rowMeans(sf_mat_all - po_mat_all)

x_df_all <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)

# Prediktera alla 6561 rader
pi_hat_all <- predict(modell, newdata = x_df_all, type = "response")
y_hat_all  <- (OMSÄTTNING + 1) * pi_hat_all
a_hat_all  <- (0.7 * (OMSÄTTNING + 1) + JACKPOTT) / (y_hat_all + 1)
ev_all     <- -1 * (1 - x1) + a_hat_all * x1

resultat <- data.frame(
  rad         = apply(ALLA_RADER, 1, function(rv) paste(c("1","X","2")[rv], collapse="")),
  ev_per_kr   = ev_all,
  utdelning   = a_hat_all,
  sannolikhet = x1,
  stringsAsFactors = FALSE
)

rekar <- resultat %>%
  filter(ev_per_kr > TRÖSKEL) %>%
  filter(sannolikhet > MIN_SANNOLIKHET) %>% 
  arrange(desc(ev_per_kr)) %>%
  head(TOPP_N)

# =============================================================================
# STEG 5: BERÄKNA TOTALT VÄNTEVÄRDE FÖR SAMTLIGA REKOMMENDERADE RADER
# =============================================================================

n_rader       <- nrow(rekar)
kostnad       <- n_rader * RAD_INSATS
ev_vinst_tot  <- sum(rekar$utdelning * rekar$sannolikhet)  
ev_netto_tot  <- ev_vinst_tot - kostnad                    
ev_tot_per_kr <- ev_netto_tot / kostnad                    
p_vinst_tot   <- sum(rekar$sannolikhet)  
ev_utdelning_vid_vinst <- ev_vinst_tot / p_vinst_tot

# =============================================================================
# STEG 6: SKRIV UT RESULTAT & EXPORTERA TEXTFIL
# =============================================================================

{
  # Kompakt rubrik
  # Kompakt rubrik med omgångsnummer
  cat(sprintf("\n=== TOPPTIPSET OMGÅNG: %s === Tröskel: %.0f%% | Oms: %s kr | JP: %s kr ===\n\n",
              OMK_NUMMER, TRÖSKEL * 100, format(round(OMSÄTTNING, 1), big.mark=" "), format(JACKPOTT, big.mark=" ")))
  
  # Linjerad rubrik för matchöversikten (Exakt samma bredd som dataraderna)
  cat(sprintf("%-32s | %-8s | %-8s | %s\n", " Matcher", "SF 1-X-2", "Bolag %", "Odds 1-X-2"))
  
  for (k in 1:8) {
    # Använder format() för att garantera exakt 12 tecken även vid specialtecken
    hemma_str <- format(substring(match_data$hemma[k], 1, 12), width = 12, justify = "left")
    borta_str <- format(substring(match_data$borta[k], 1, 12), width = 12, justify = "left")
    
    # Beräknar spelbolagens implicita sannolikheter i procent (normaliserade så att de summerar till 100%)
    raw_1   <- 1 / match_data$odds1[k]
    raw_x   <- 1 / match_data$oddsx[k]
    raw_2   <- 1 / match_data$odds2[k]
    raw_sum <- raw_1 + raw_x + raw_2
    bolag_1 <- 100 * raw_1 / raw_sum
    bolag_x <- 100 * raw_x / raw_sum
    bolag_2 <- 100 * raw_2 / raw_sum
    
    cat(sprintf(" M%d: %s - %s | %2.0f-%2.0f-%2.0f | %2.0f-%2.0f-%2.0f | %.2f-%.2f-%.2f\n",
                k, hemma_str, borta_str,
                match_data$sf1[k], match_data$sfx[k], match_data$sf2[k],
                bolag_1, bolag_x, bolag_2,
                match_data$odds1[k], match_data$oddsx[k], match_data$odds2[k]))
  }
  
  if (nrow(rekar) == 0) {
    cat("\nInga rader uppfyller tröskeln denna omgång.\n")
  } else {
    # Begränsar rad-utskriften till max 10
    max_print <- min(nrow(rekar), 10)
    
    # Huvudrubrik för rader
    cat(sprintf("\n--- Visar topp %d (av %d över tröskel) ---\n",
                max_print, n_rader))
    
    # Linjerade kolumnrubriker med "Rad"
    cat(sprintf("%5s%-8s : %7s | %10s | %s\n", "", "Rad", "E[V]%", "Utdelning", "P(vinst)%"))
    
    # Skriver ut raderna
    for (i in 1:max_print) {
      cat(sprintf(" %2d. %-8s : %6.1f%% | %7.0f kr | %7.4f%%\n",
                  i, rekar$rad[i], rekar$ev_per_kr[i] * 100,
                  rekar$utdelning[i], rekar$sannolikhet[i] * 100))
    }
    
    if(nrow(rekar) > max_print) {
      cat(" ... (Resterande rader finns i den exporterade filen)\n")
    }
    
    # Sammanfattning med separata rader
    cat("\n--- Sammanfattning (Samtliga rekommenderade rader) ---\n")
    cat(sprintf("Antal rader (kostnad):     %d kr (à %d kr)\n",      n_rader, RAD_INSATS))
    cat(sprintf("P(En rad vinner):          %.2f%%\n",      p_vinst_tot * 100))
    cat(sprintf("Förväntad vinst:           %.0f kr\n",     ev_vinst_tot))
    cat(sprintf("Förväntat netto:           %.0f kr\n",     ev_netto_tot))
    cat(sprintf("E[Utbetalning|Vinst]:      %.0f kr\n",      ev_utdelning_vid_vinst))
    cat(sprintf("E[V] per satsad krona:     %.2f kr\n",     ev_vinst_tot / kostnad))
    cat(sprintf("Procentuell avkastning:    %.1f%%\n",      ev_tot_per_kr * 100))
    
    # Filhantering
    if (!is.null(coup_type)) {
      header_line <- sprintf("Topptipset,%s,Omg=%s,Insats=%d", coup_type, OMK_NUMMER, RAD_INSATS)
    } else {
      header_line <- sprintf("Topptipset,Omg=%s,Insats=%d", OMK_NUMMER, RAD_INSATS)
    }
    
    enkelrader <- sapply(rekar$rad, function(r) {
      chars <- strsplit(r, "")[[1]]
      paste0("E,", paste(chars, collapse=","))
    })
    
    fil_innehåll <- c(header_line, enkelrader)
    writeLines(fil_innehåll, con = SÖKVÄG_EXPORT_FIL, useBytes = TRUE)
    
    cat(sprintf("\n[KLART] %d rader sparade till: %s\n", nrow(rekar), SÖKVÄG_EXPORT_FIL))
  }
}