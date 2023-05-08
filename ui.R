library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            width = 3,
            h2("My Static website"),
            h3("Internal link example"),
            a(h4("Birds"),
              href = "#birds"),
            hr(),
            
            h3("External link example"),
            a(h4("Wikipedia"),
              href = "https://en.wikipedia.org/wiki/Main_Page",
              target = "blank")
        ),
        
        mainPanel(
            width = 9,
            
            h2("Link to a saved sample.html"),
            p("The url is https://abarrientosmendoza.github.io/samples/sample.html"),
            a(h3("Lovely Birds"),
              href = "https://abarrientosmendoza.github.io/samples/sample.html",
              target = "blank"),
            hr(),
            
            h2("Text and image example", id = "birds"),
            fluidRow(
                column(
                    7,
                    includeMarkdown("markdown/birds.Rmd")
                ),
                column(
                    5,
                    img(src = "birds.png", width = "100%")
                )
            ),
        )
    )
)