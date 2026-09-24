library(shiny)
library(bslib)
library(plotly)

# ---- Data -------------------------------------------------------------------
vowels <- read.csv("data/vowels.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
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

hull_of <- function(d) {
  if (nrow(d) < 3) return(NULL)
  i <- chull(d$x, d$y)
  d[c(i, i[1]), ]
}

hex_alpha <- function(hex, a) {
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], a)
}

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
    radioButtons("scale", "Scale", c("Hz" = "hz", "Bark" = "bark"), inline = TRUE),
    checkboxInput("hulls", "Show vowel space outlines", TRUE),
    checkboxInput("labels", "Show vowel labels", TRUE),
    hr(),
    fileInput("user_file", "Add your own data (CSV)", accept = ".csv"),
    helpText("Columns: vowel, F1, F2 (Hz). Optional: speaker."),
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
        tags$li("Axes are oriented like the vowel trapezium: front vowels on the left, close vowels at the top."),
        tags$li("Hover a point to see its values; click it to hear the vowel when a recording is available."),
        tags$li("Upload your own measurements (e.g. from Praat) to compare them with the reference values.")
      )
    ),
    accordion_panel(
      "About the data",
      p("Values are mean F1/F2 in Hz. Formant values depend strongly on speaker sex, age,",
        "variety and elicitation method; see the ", code("source"), " column in ",
        code("data/vowels.csv"), " for the reference of each value."),
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
    d$language <- "Your data"
    d$audio <- ""
    d
  })

  # Fixed axis ranges: comparisons stay stable when the selection changes
  ranges <- reactive({
    f <- if (input$scale == "bark") hz_to_bark else identity
    u <- if (isTruthy(input$user_file)) user_data() else NULL
    f1 <- f(c(vowels$F1, u$F1)); f2 <- f(c(vowels$F2, u$F2))
    pad <- function(r) { m <- diff(r) * 0.08; c(r[1] - m, r[2] + m) }
    list(x = rev(pad(range(f2))), y = rev(pad(range(f1))))
  })

  output$vowelPlot <- renderPlotly({
    f <- if (input$scale == "bark") hz_to_bark else identity
    unit <- if (input$scale == "bark") "Bark" else "Hz"

    sets <- lapply(input$langs, function(l) {
      d <- vowels[vowels$language == l, ]
      d$col <- lang_colors[[l]]
      d
    })
    if (isTruthy(input$user_file) && !is.null(user_data())) {
      u <- user_data(); u$col <- user_color
      sets <- c(sets, list(u[, c("language", "vowel", "F1", "F2", "audio", "col")]))
    }

    p <- plot_ly(source = "vowels")
    for (d in sets) {
      if (nrow(d) == 0) next
      d$x <- f(d$F2); d$y <- f(d$F1)
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
      p <- add_trace(
        p, data = d, x = ~x, y = ~y, type = "scatter",
        mode = if (input$labels) "markers+text" else "markers",
        text = ~vowel, textposition = "top center",
        textfont = if (input$labels) list(size = 15, color = col),
        marker = list(size = 9, color = col, line = list(color = "white", width = 1)),
        customdata = ~audio, name = lang, legendgroup = lang,
        hovertemplate = paste0(
          "<b>/%{text}/</b> ", lang,
          "<br>F1: ", round(d$F1), " Hz<br>F2: ", round(d$F2), " Hz<extra></extra>"
        )
      )
    }

    p |>
      layout(
        xaxis = list(title = paste0("F2 (", unit, ")"), range = ranges()$x,
                     side = "top", zeroline = FALSE, gridcolor = "#E6DFCF"),
        yaxis = list(title = paste0("F1 (", unit, ")"), range = ranges()$y,
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
    n <- sum(nzchar(vowels$audio[vowels$language %in% input$langs]))
    if (n == 0) "No recordings available for this selection."
    else paste(n, "vowels can be played: click a point.")
  })

  output$sources <- renderUI({
    s <- unique(vowels[vowels$language %in% input$langs, c("language", "source")])
    tags$ul(lapply(seq_len(nrow(s)), function(i) tags$li(s$language[i], ": ", s$source[i])))
  })

  output$template <- downloadHandler(
    filename = "my_vowels.csv",
    content = function(file) write.csv(
      data.frame(vowel = c("i", "a", "u"), F1 = c(300, 750, 320),
                 F2 = c(2200, 1300, 850), speaker = "S01"),
      file, row.names = FALSE, fileEncoding = "UTF-8"
    )
  )
}

shinyApp(ui, server)
