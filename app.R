library(shiny)
library(bslib)
library(plotly)

# ---- Data -------------------------------------------------------------------
vowels  <- read.csv("data/vowels.csv",  encoding = "UTF-8", stringsAsFactors = FALSE)
sources <- read.csv("data/sources.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
languages <- unique(vowels$language)

# Audio: only keep paths to files that actually exist in www/
vowels$audio[is.na(vowels$audio)] <- ""
vowels$audio <- ifelse(
  nzchar(vowels$audio) & file.exists(file.path("www", vowels$audio)),
  vowels$audio, ""
)

# Earth tones for reference languages, bright blue reserved for user data
palette_earth <- c("#6B7F3A", "#8B5A2B", "#C8A03A", "#5A4632",
                   "#B5653A", "#9AA67A", "#A89060", "#4F6B3A")
lang_colors <- setNames(rep(palette_earth, length.out = length(languages)), languages)
user_color <- "#1E6FD9"

hz_to_bark <- function(f) 26.81 * f / (1960 + f) - 0.53

# Watt & Fabricius (2002): each formant is divided by the centroid of the
# triangle /i/, /a/, u', with u' = (F1 of /i/, F1 of /i/). The corners are
# found from the data (/i/ = highest F2 - F1, /a/ = highest F1), so the method
# works whatever the size of the inventory.
watt_fabricius <- function(d) {
  i <- which.max(d$F2 - d$F1)
  a <- which.max(d$F1)
  s1 <- (d$F1[i] + d$F1[a] + d$F1[i]) / 3
  s2 <- (d$F2[i] + d$F2[a] + d$F1[i]) / 3
  data.frame(x = d$F2 / s2, y = d$F1 / s1)
}

# Adds plotting coordinates x (F2) and y (F1) for the chosen scale.
# `groups` splits the data before normalising (one group per speaker).
transform_scale <- function(d, scale, groups = rep(1, nrow(d))) {
  if (scale == "hz")   { d$x <- d$F2; d$y <- d$F1 }
  if (scale == "bark") { d$x <- hz_to_bark(d$F2); d$y <- hz_to_bark(d$F1) }
  if (scale == "wf") {
    d$x <- NA_real_; d$y <- NA_real_
    for (g in unique(groups)) {
      k <- groups == g
      if (sum(k) >= 3) d[k, c("x", "y")] <- watt_fabricius(d[k, ])
    }
  }
  d
}

hull_of <- function(d) {
  if (nrow(d) < 3) return(NULL)
  i <- chull(d$x, d$y)
  d[c(i, i[1]), ]
}

hex_alpha <- function(hex, a) {
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], a)
}

scale_units <- c(hz = "Hz", bark = "Bark", wf = "normalised, Watt & Fabricius")

# Fixed axis ranges per scale, computed on every language and both sexes, so
# that charts stay comparable when the selection changes
ref_ranges <- lapply(setNames(names(scale_units), names(scale_units)), function(s) {
  d <- do.call(rbind, lapply(split(vowels, paste(vowels$language, vowels$sex)),
                             transform_scale, scale = s))
  list(x = range(d$x), y = range(d$y))
})

# ---- UI ---------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#FBF8F1", fg = "#3B3326",
  primary = user_color, secondary = "#8B5A2B",
  base_font = font_google("Ubuntu"), heading_font = font_google("Ubuntu")
)

ui <- page_sidebar(
  title = "Vowel space explorer",
  theme = theme,
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('playSound', function(msg) {
      new Audio(msg.src).play().catch(function(e) { console.warn(e); });
    });
  "))),
  sidebar = sidebar(
    width = 300,
    selectizeInput(
      "langs", "Languages", choices = languages,
      selected = head(languages, 2), multiple = TRUE,
      options = list(maxItems = 4, plugins = list("remove_button"))
    ),
    radioButtons("sex", "Speakers", c("Male" = "M", "Female" = "F"), inline = TRUE),
    radioButtons("scale", "Scale",
                 c("Hz" = "hz", "Bark" = "bark", "Normalised (Watt & Fabricius)" = "wf")),
    checkboxInput("hulls", "Show vowel space outlines", TRUE),
    checkboxInput("labels", "Show vowel labels", TRUE),
    hr(),
    fileInput("user_file", "Add your own data (CSV)", accept = ".csv"),
    helpText("Columns: vowel, F1, F2 (Hz). Optional: speaker.",
             "The normalised scale needs at least three vowels per speaker, including /i/ and /a/."),
    downloadLink("template", "Download a template")
  ),
  card(
    full_screen = TRUE,
    card_body(plotlyOutput("vowelPlot", height = "560px")),
    card_footer(textOutput("audio_note", inline = TRUE))
  ),
  accordion(
    open = FALSE,
    accordion_panel(
      "How to use",
      tags$ul(
        tags$li("Pick up to four languages; their vowels are superimposed on the same chart."),
        tags$li("Compare male or female speakers: never mix the two in Hz or Bark."),
        tags$li("The normalised scale divides each formant by the centre of the speaker's",
                "/i/-/a/-/u/ triangle. Use it to compare your own voice with a reference of the other sex."),
        tags$li("Axes are oriented like the vowel trapezium: front vowels on the left, close vowels at the top."),
        tags$li("Hover a point to see its values; click it to hear the vowel when a recording is available.")
      )
    ),
    accordion_panel(
      "About the data",
      p("Values are mean F1/F2 from published studies, one per language. The studies differ in",
        "speech style (isolated words, carrier sentences, broadcast speech), number of speakers and",
        "measurement method, so small differences between languages (below about 50 Hz for F1)",
        "should not be interpreted."),
      uiOutput("sources")
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  user_data <- reactive({
    req(input$user_file)
    d <- tryCatch(read.csv(input$user_file$datapath, encoding = "UTF-8",
                           stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (is.null(d) || !all(c("vowel", "F1", "F2") %in% names(d))) {
      showNotification("The file needs columns vowel, F1 and F2.", type = "error")
      return(NULL)
    }
    d <- d[is.finite(d$F1) & is.finite(d$F2), ]
    if (!"speaker" %in% names(d)) d$speaker <- "you"
    d$language <- "Your data"
    d$audio <- ""
    d
  })

  output$vowelPlot <- renderPlotly({
    unit <- scale_units[[input$scale]]
    hz <- input$scale == "hz"

    sets <- lapply(input$langs, function(l) {
      d <- vowels[vowels$language == l & vowels$sex == input$sex, ]
      d <- transform_scale(d, input$scale)
      d$col <- lang_colors[[l]]
      d
    })
    u <- if (isTruthy(input$user_file)) user_data() else NULL
    if (!is.null(u)) {
      u <- transform_scale(u, input$scale, groups = u$speaker)
      if (anyNA(u$x)) showNotification(
        "Some speakers have fewer than 3 vowels and cannot be normalised.", type = "warning")
      u <- u[!is.na(u$x), ]
      u$col <- user_color
      sets <- c(sets, list(u))
    }

    rx <- range(c(ref_ranges[[input$scale]]$x, u$x))
    ry <- range(c(ref_ranges[[input$scale]]$y, u$y))
    pad <- function(r) { m <- diff(r) * 0.08; c(r[1] - m, r[2] + m) }

    p <- plot_ly(source = "vowels")
    for (d in sets) {
      if (nrow(d) == 0) next
      col <- d$col[1]; lang <- d$language[1]
      if (input$hulls) {
        h <- hull_of(d)
        if (!is.null(h)) p <- add_trace(
          p, data = h, x = ~x, y = ~y, type = "scatter", mode = "lines",
          fill = "toself", fillcolor = hex_alpha(col, 0.10),
          line = list(color = hex_alpha(col, 0.6), width = 1.5),
          hoverinfo = "skip", legendgroup = lang, showlegend = FALSE
        )
      }
      norm_line <- if (hz) "" else
        paste0("<br>", sprintf(if (input$scale == "bark") "%.2f / %.2f" else "%.3f / %.3f", d$y, d$x),
               " (F1 / F2, ", unit, ")")
      p <- add_trace(
        p, data = d, x = ~x, y = ~y, type = "scatter",
        mode = if (input$labels) "markers+text" else "markers",
        text = ~vowel, textposition = "top center",
        textfont = if (input$labels) list(size = 15, color = col),
        marker = list(size = 9, color = col, line = list(color = "white", width = 1)),
        customdata = ~audio, name = lang, legendgroup = lang,
        hovertemplate = paste0(
          "<b>/%{text}/</b> ", lang,
          "<br>F1: ", round(d$F1), " Hz<br>F2: ", round(d$F2), " Hz",
          norm_line, "<extra></extra>"
        )
      )
    }

    p |>
      layout(
        xaxis = list(title = paste0("F2 (", unit, ")"), range = rev(pad(rx)),
                     side = "top", zeroline = FALSE, gridcolor = "#E6DFCF"),
        yaxis = list(title = paste0("F1 (", unit, ")"), range = rev(pad(ry)),
                     side = "right", zeroline = FALSE, gridcolor = "#E6DFCF"),
        legend = list(orientation = "h", y = -0.08),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Ubuntu, Roboto, sans-serif")
      ) |>
      config(displaylogo = FALSE) |>
      event_register("plotly_click")
  })

  observeEvent(event_data("plotly_click", source = "vowels"), {
    src <- event_data("plotly_click", source = "vowels")$customdata
    if (length(src) && !is.na(src[1]) && nzchar(src[1])) {
      session$sendCustomMessage("playSound", list(src = src[1]))
    } else {
      showNotification("No recording available for this vowel yet.", duration = 2)
    }
  })

  output$audio_note <- renderText({
    sel <- vowels$language %in% input$langs & vowels$sex == input$sex
    n <- sum(nzchar(vowels$audio[sel]))
    if (n == 0) "No recordings available for this selection."
    else paste(n, "vowels can be played: click a point.")
  })

  output$sources <- renderUI({
    s <- sources[sources$language %in% input$langs, ]
    tags$ul(lapply(seq_len(nrow(s)), function(i) {
      n <- if (is.na(s$n_M[i])) "number of speakers not reported"
           else sprintf("%d men, %d women", s$n_M[i], s$n_F[i])
      tags$li(
        tags$b(s$language[i]), ": ", s$reference[i], " ",
        tags$a(href = s$url[i], target = "_blank", "Link"),
        tags$br(),
        tags$small(s$style[i], "; ", n, ". ", s$notes[i])
      )
    }))
  })

  output$template <- downloadHandler(
    filename = "my_vowels.csv",
    content = function(file) write.csv(
      data.frame(speaker = "S01",
                 vowel = c("i", "e", "a", "o", "u"),
                 F1 = c(300, 450, 750, 480, 320),
                 F2 = c(2200, 1900, 1300, 950, 850)),
      file, row.names = FALSE, fileEncoding = "UTF-8"
    )
  )
}

shinyApp(ui, server)
