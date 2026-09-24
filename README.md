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

`data/vowels.csv` has one row per vowel and speaker sex:

| column | content |
|---|---|
| `language` | language or variety |
| `vowel` | IPA symbol |
| `sex` | M / F |
| `F1`, `F2` | mean formant values in Hz, copied from the source table |
| `source_id` | key into `data/sources.csv` |
| `audio` | path relative to `www/` (e.g. `audio/fr_i.mp3`), optional |

`data/sources.csv` gives, for each source, the full reference, a link, the number of speakers
per sex, the speech style and caveats.

| Language | Source | Speakers (M/F) | Style |
|---|---|---|---|
| English (SSBE) | Deterding 1997, JIPA, table 2 | 5 / 5 | BBC read speech |
| French | Gendrot & Adda-Decker 2005, Interspeech, table 5 | not reported | Broadcast news, oral vowels only |
| German | Sendlmeier & Seebode 2006, TU Berlin, table 1 | 69 / 58 | Isolated words |
| Spanish (Madrid) | Chládková, Escudero & Boersma 2011, JASA, table I | 10 / 10 | Isolated words and sentences |
| Portuguese (EP, BP) | Escudero et al. 2009, JASA, table I | 10 / 10 each | Isolated words and carrier sentence |
| Japanese (Tokyo) | Yazawa & Kondo 2019, computed from the Zenodo dataset | 8 / 8 | Isolated words and carrier sentence |

The sources differ in speech style and method: small cross-language differences are not
meaningful. Values are never normalised in the CSV; the app offers Watt & Fabricius (2002)
normalisation, computed per language and sex (or per speaker for uploaded data), with the
/i/ and /a/ corners taken as the vowels with the highest F2 − F1 and the highest F1.

## To do

- [ ] Italian and Polish: find multi-speaker sources with M/F tables
- [ ] French nasal vowels (no M/F source consistent with the oral data yet)
- [ ] Add recordings in `www/audio/`
- [ ] Add Chilean Spanish
