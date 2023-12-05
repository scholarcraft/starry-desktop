library(shiny)
library(shinyAce)
library(shinyjs)
library(bslib)



# UI Function
codeInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("wrapper"),
        shinyAce::aceEditor(ns("code_area"), "", height = "100px"),
        actionButton(ns("run_code"), "Run Code"),
        actionButton(ns("remove_code"), "Remove"),
        hr(),
        verbatimTextOutput(ns("code_output"))
    )
  )
}

# Server Function
codeInputServer <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize state for the new code block
    if (is.null(state$code[[id]])) {
      state$code[[id]] <<- reactiveValues(text = "", output = "")
    }
    
    # Observe changes in code area and update state
    observe({
      state$code[[id]]$text <- input$code_area
    })
    
    observeEvent(input$run_code, {
      tryCatch({
        result <- eval(parse(text = input$code_area))
        output$code_output <- renderPrint({ result })
        state$code[[id]]$output <- capture.output(print(result))
      }, error = function(e) {
        output$code_output <- renderPrint({e$message})
        state$code[[id]]$output <- e$message
      })
    })
    
    observeEvent(input$remove_code, {
      removeUI(selector = paste0("#", ns("wrapper")))
      state$code[[id]] <<- NULL
    })
  })
}

# UI
ui <- page_sidebar(
  theme = bs_theme(`enable-rounded` = FALSE, spacer = "0.25rem", 
                   base_font = font_google("JetBrains Mono"), font_scale = 0.85),
  title = "Starry",
  useShinyjs(),
  tags$head(
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var addButton = document.getElementById('add_code');
        if(addButton) {
          addButton.addEventListener('click', function() {
            setTimeout(function() {
              var newEditor = document.querySelector('#code_blocks .ace_editor:not(.my-ace-editor)');
              if(newEditor) {
                newEditor.classList.add('my-ace-editor');
                var textInput = newEditor.querySelector('.ace_text-input');
                if (textInput) {
                  textInput.focus();
                  textInput.addEventListener('keydown', function(event) {
                    if (event.ctrlKey && event.keyCode === 13) {
                      var runButton = newEditor.closest('.wrapper').querySelector('.run_code');
                      if (runButton) {
                        runButton.click();
                      }
                    }
                  });
                }
              }
            }, 500);
          });
        }
      });
    "))
  ),
  
  
  tags$div(id = "code_blocks"),
  actionButton("add_code", "Add Code")
)

# Server
server <- function(input, output, session) {
  state <- reactiveValues(code = list())
  
  observeEvent(input$add_code, {
    id <- paste("code_block", input$add_code, sep = "_")
    state$code[[id]] <- list(text = "", output = "")
    insertUI(
      selector = "#code_blocks",
      where = "beforeEnd",
      ui = codeInputUI(id)
    )
    codeInputServer(id, state)
  })
  

}

# Run the app
shinyApp(ui, server)

