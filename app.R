library(shiny)
library(DBI)
library(ggplot2)
library(glue)

con <- dbConnect(RSQLite::SQLite(), ":memory:")
db <- ggplot2::diamonds
db_name <- "diamonds"
dbWriteTable(con, db_name, db)
lookup <- setdiff(colnames(db), "price")

# Generate template for parameter filtering
numerics <- lookup[!sapply(db[, lookup], is.factor)]
generate_where <- function(cols, numerics) {
  # If no columns selected, return NULL
  if (!length(cols)) {
    return()
  }
  filters <- paste(cols, "filter", sep = "_")
  criteria <- ifelse(!cols %in% numerics,
    paste0("IN ({", filters, "*})"),
    paste0("BETWEEN {", paste(filters, "min", sep = "_"), "} AND {", paste(filters, "max", sep = "_"), "}")
  )
  paste("WHERE", paste(paste0("`", cols, "`"), criteria, collapse = " AND "))
}

static_SQL <- paste("SELECT COUNT(*) AS n, MIN(price) AS min, MAX(PRICE) as max, AVG(PRICE) AS average FROM ", db_name, "")
flatten_names <- function(to_flatten) {
  if (!length(to_flatten)) {
    return(character())
  }
  paste(rep(to_flatten, each = 2), paste("filter", rep(c("min", "max"), length.out = 2 * length(to_flatten)), sep = "_"), sep = "_")
}

pad <- column(width = 3)
# Checkbox tracker
old_state <- setNames(rep(TRUE, length(lookup)), lookup)
# List of symbols for checkbox inbox to splice into reactive call
checkbox_inputs <- lapply(lookup, \(x) bquote(`$`(input, .(x))))
inputs <- lapply(seq_along(db[, lookup]), \(x){
  var_name <- lookup[[x]]
  filter_input <- if (is.factor(db[[var_name]])) {
    checkboxGroupInput(inputId = paste(var_name, "filter", sep = "_"), label = var_name, choices = levels(db[[var_name]]), selected = db[[var_name]], inline = TRUE)
  } else {{ sliderInput(inputId = paste(var_name, "filter", sep = "_"), label = var_name, min = min(db[[var_name]]), max = max(db[[var_name]]), value = c(.25, .75) * diff(range(db[[var_name]])) + min(db[[var_name]])) }}
  fluidRow(
    pad,
    column(filter_input, width = 4),
    column(checkboxInput(var_name, label = paste0("Include ", var_name, "?"), value = TRUE), width = 2),
    pad
  )
})

ui <- eval(bquote(
  navbarPage(
    "Dynamic Diamonds",
    tabPanel(
      "Main",
      shinyjs::useShinyjs(),
      tags$style(HTML("code {
   color: black;
   font-size: 20px;
}
strong {
font-size: 25px;
}")),
      fluidRow(tags$strong("Dynamic Diamonds"), tags$br(), align = "center"),
      fluidRow(
        pad,
        column("Select filtering conditions to apply: ",
          width = 4,
          style = "font-size:16px;"
        ),
        column("Select variables to filter on: ",
          width = 2,
          style = "font-size:16px;"
        ),
        pad
      ),
      ..(inputs),
      fluidRow(actionButton("submit", "Send Query"),
        align = "center"
      ),
      fluidRow(tableOutput("table"),
        align = "center"
      ),
      fluidRow(tags$strong("Your Query Template", style = "font-size: 20px;"),
        align = "center"
      ),
      fluidRow(
        pad,
        column(textOutput("query",
          container = tags$code,
        ), width = 6),
        pad,
        style = "text-align:left;"
      )
    ),
    tabPanel(
      "About",
      tags$style(HTML("p {
                     text-align: left;
                     font-size: 15px;
}")),
      fluidRow(
        pad,
        column(
          width = 6,
          tags$strong("About"),
          tags$p("This app demonstrates dynamic SQL. Select any combination of search parameters
                    and click the \"Send Query\" button, and the app will
                    generate a query template with the appropriate placeholders and substitute the values you selected.
                    You can see the query string at the bottom of the page."),
          tags$p("What parameters result in the most expensive set of diamonds?"),
          tags$p("Data sourced from the ggplot2 dataset \"diamonds.\""),
          align = "center"
        )
      ), pad
    )
  ),
  splice = TRUE
))

server <- function(input, output, session) {
  # Close connection on exit
  session$onSessionEnded(function() {
    dbDisconnect(con)
    stopApp()
  })
  query_call <- reactive({
    selections <- lookup[sapply(lookup, \(x) input[[x]])]
    dyn_SQL <- ""
    params <- list()
    if (length(selections)) {
      dyn_SQL <- generate_where(selections, numerics)
      params <- lapply(paste(selections, "filter", sep = "_"), \(x) input[[x]])

      if (length(splits <- selections %in% numerics)) {
        params <- split(params, splits)
        params[["TRUE"]] <- as.list(unlist(params[["TRUE"]]))
        non_numerics <- setdiff(selections, numerics)

        params <- setNames(unlist(params, recursive = FALSE), c(paste(non_numerics, rep("filter", length(non_numerics)), sep = "_"), flatten_names(intersect(selections, numerics))))
      }
    }
    list(SQL = paste(static_SQL, dyn_SQL), params = params)
  }) |>
    bindEvent(input$submit)

  results <- reactive({
    query_call <- query_call()
    query <- glue_data_sql(query_call$params, query_call$SQL, .con = con)
    dbGetQuery(con, query)
  }) |>
    bindEvent(query_call())

  output$table <- renderTable({
    res <- results()
    validate(need(rowSums(is.na(res)) == 0, "Query did not return any rows"))
    res[, -1] <- lapply(res[, -1], \(x) sprintf("$%.2f", x))
    res
  })

  # Keep track of which input checkboxes are checked
  filters_state <- eval(bquote(bindEvent(
    reactive({
      unlist(reactiveValuesToList(input)[lookup])
    }), ..(checkbox_inputs),
    ignoreInit = TRUE
  ), splice = TRUE))

  # Toggle a filter based on corresponding checkbox
  toggle_filters <- observe({
    new_state <- filters_state()
    changed <- old_state[old_state != new_state]
    to_change <- paste(names(changed), "filter", sep = "_")
    if (changed) { # Disabled, now enabled
      shinyjs::disable(to_change)
    } else { # vice versa
      shinyjs::enable(to_change)
    }
    old_state <<- new_state
  })

  output$query <- renderText({
    query_call()$SQL
  })
}

shinyApp(ui, server)
