# topptipset-ev-prediction

Ett självinitierat fritidsprojekt i R som skattar sannolikheter och förväntat värde (EV) för Svenska Spels Topptipset, baserat på oddsimplicita sannolikheter och "Svenska Folket"-data.

## Vad gör skriptet?

1. **Tränar en modell** på historisk Topptipset-data med en quasibinomial GLM (`glm(cbind(y, n-y) ~ ..., family = quasibinomial)`)
2. **Konstruerar 16 features (x1–x16)** utifrån oddsimplicita sannolikheter och folkets tips, t.ex.:
   - Produkten av sannolikheter för en given rad
   - Entropi och varians över matchernas sannolikheter
   - Avvikelse mellan bookmakerns odds och folkets tips ("bias")
3. **Hämtar liveodds** direkt från Svenska Spels öppna API för aktuell omgång
4. **Beräknar förväntat värde** för samtliga 6 561 möjliga rader och rangordnar dem
5. **Exporterar** de mest spelvärda raderna enligt valfri tröskel

## Data som krävs

Träningsdatan kommer från TipsXtra:s statistikexport och är **inte inkluderad** i detta repo (ej publikt tillgänglig data). För att köra skriptet behöver du:
- `TipsXtra_Topptipset_Statistik_Detaljer.csv`
- `TipsXtra_Topptipset_Statistik_Summering.csv`

Uppdatera sökvägarna i toppen av skriptet till dina egna filer.

## Paket som krävs

```r
install.packages(c("dplyr", "tidyr", "readr", "stringr", "httr", "jsonlite"))
```

## Metod

Modellen bygger på idén att avvikelser mellan bookmakerns implicita sannolikheter och folkets tips kan indikera "spelvärde" — rader där den förväntade utdelningen (given omsättning och jackpott) överstiger insatsen. Detta är i grunden samma logik som används inom quantitative finance för att identifiera felprissatta tillgångar.

## Disclaimer

Detta är ett personligt projekt i sannolikhetsmodellering och statistisk inferens, inte en rekommendation att spela. Använd på egen risk.
