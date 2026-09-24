# Vowel space explorer

Interactive Shiny app comparing the F1/F2 vowel spaces of several languages.
Live version: https://lcontrerasroa.shinyapps.io/vowels/

## Run locally

```r
install.packages(c("shiny", "bslib", "plotly"))
shiny::runApp()
```

## Deploy

```r
rsconnect::deployApp(appName = "vowels")
```

## Data

`data/vowels.csv` has one row per vowel:

| column | content |
|---|---|
| `language` | language or variety |
| `vowel` | IPA symbol |
| `F1`, `F2` | mean formant values in Hz |
| `speaker_sex` | F / M / mixed |
| `source` | full reference for the values |
| `audio` | path relative to `www/` (e.g. `audio/fr_i.mp3`), optional |

Every value is currently marked `TO VERIFY`: they come from the first version of the app and are
approximations. They must be replaced by values from published formant studies.

## To do

- [ ] Replace values with sourced data (speaker sex specified)
- [ ] Complete inventories (French /o/ and nasals, German lax vowels, Italian/Portuguese /o/, Japanese /ɯ/…)
- [ ] Add recordings in `www/audio/`
- [ ] Add Chilean Spanish
