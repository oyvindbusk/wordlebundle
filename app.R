library(shiny)
library(DT)
library(tidyverse)
library(stringr)
library(ggplot2)
library(cowplot)
# appDir = "S:/analysis/dev/2022/wordl/wordlesolver"
# rsconnect::deployApp(appDir)

# Last inn ordbok med fembokstavers ord
github_csv <- "https://raw.githubusercontent.com/oyvindbusk/wordlebundle/main/fiveletterwords.csv"
#fivel_words <- read_delim(github_csv, delim=",",col_select = 'Word')
fivel_words <- read_delim(github_csv, delim=",",col_select = 'Word')
fivel_words <- fivel_words %>% mutate(xx = sapply(strsplit(Word, ""), paste, collapse=","))
fivel_words <- fivel_words %>% separate(xx, into = paste("V", 1:5, sep = "_"), sep = ",", remove=FALSE) %>% filter(!V_3 == ".") %>% filter(!V_5 == ".")
fivel_words <- fivel_words %>% select(-xx)

# Funksjoner:
# Function filter words containing letter
filter_included <- function(included_letters, data) {
    # finn alle ord som inneholder den ene eller den andre
    out_data <- data %>% filter(str_detect(string = Word, pattern = substring(included_letters,1,1), negate = FALSE))
    if (nchar(included_letters) > 1) {
        for (i in strsplit(included_letters, "")[[1]]) {
            out_data <- out_data %>% filter(str_detect(string = Word, pattern = i, negate = FALSE))
        }
    }
    return(out_data)
}

# Function filter words NOT containing letter
filter_excluded <- function(excluded_letters, data) {
    # finn alle ord som inneholder den ene eller den andre
    out_data <- data %>% filter(str_detect(string = Word, pattern = substring(excluded_letters,1,1), negate = TRUE))
    if (nchar(excluded_letters) > 1) {
        for (i in strsplit(excluded_letters, "")[[1]]) {
            out_data <- out_data %>% filter(str_detect(string = Word, pattern = i, negate = TRUE))
        }
    }
    return(out_data)
}

filter_include_position <- function(position, ltr, data) {
    return(data %>% filter(substr(Word, position,position) == ltr))
}

filter_exclude_position <- function(position, ltr, data) {
    for (i in strsplit(ltr, "")[[1]]) {
        data <- data %>% filter(substr(Word, position,position) != i)
    }
    return(data)
}

combine_filters <- function(included_letters, excluded_letters, incl_1, incl_2, incl_3, incl_4, incl_5, excl_1,excl_2,excl_3,excl_4,excl_5, data) {
    if (nchar(included_letters) != 0) {
        data <- filter_included(included_letters, data)
    }
    if (nchar(excluded_letters) != 0) {
        data <- filter_excluded(excluded_letters, data)
    }
    if (nchar(incl_1) != 0) {
        data <- filter_include_position(1, incl_1, data)
    }
    if (nchar(incl_2) != 0) {
        data <- filter_include_position(2, incl_2, data)
    }
    if (nchar(incl_3) != 0) {
        data <- filter_include_position(3, incl_3, data)
    }
    if (nchar(incl_4) != 0) {
        data <- filter_include_position(4, incl_4, data)
    }
    if (nchar(incl_5) != 0) {
        data <- filter_include_position(5, incl_5, data)
    }
    if (nchar(excl_1) != 0) {
        data <- filter_exclude_position(1, excl_1, data)
    }
    if (nchar(excl_2) != 0) {
        data <- filter_exclude_position(2, excl_2, data)
    }
    if (nchar(excl_3) != 0) {
        data <- filter_exclude_position(3, excl_3, data)
    }
    if (nchar(excl_4) != 0) {
        data <- filter_exclude_position(4, excl_4, data)
    }
    if (nchar(excl_5) != 0) {
        data <- filter_exclude_position(5, excl_5, data)
    }
    return(data)
}

# Funksjon for aa rate ord
bokstav_funk <- function(ord, t1, t) {
    sum = 0
    count = 1
    l <- length(t[[1]])
    for (bokstav in strsplit(ord, "")[[1]]) {
        sum = sum + (t[[which(names(t1) == bokstav),count]]/l)
        count = count + 1
    }
    return(sum)  
}

# Funksjon for aa:
# Legg til der det mangler bokstaver i tabell
utvid_tabell <- function(t){
  for (i in letters) {
    if (is.na(t[i])) {
      t[i] = 0
    }
  }
  return(t)
}



# Definer UI:
ui <- fluidPage(

    # Application title
    titlePanel("Øyvinds wordle-løser"),
 
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(3,textInput(inputId = "included_letters",
                      label = "Inkluderte bokstaver:",
                      value = "",
                      width = "200px")),
                column(3,textInput(inputId = "excluded_letters",
                      label = "Ekskluderte bokstaver:",
                      value = "",
                      width = "200px")),
            ),
            h4("Bokstaver tilstede i posisjon x: (kun en)"),
            fluidRow(
                column(2,textInput(inputId = "included_1",
                                   label = "Tilstede posisjon 1:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "included_2",
                                   label = "Tilstede posisjon 2:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "included_3",
                                   label = "Tilstede posisjon 3:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "included_4",
                                   label = "Tilstede posisjon 4:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "included_5",
                                   label = "Tilstede posisjon 5:",
                                   value = "",
                                   width = "100px"))
            ),
            h4("Bokstaver IKKE tilstede i posisjon x: (mulig med mer enn en bokstav (uten mellomrom eller komma))"),
            fluidRow(
                column(2,textInput(inputId = "excluded_1",
                                   label = "Ikke tilstede posisjon 1:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "excluded_2",
                                   label = "Ikke tilstede posisjon 2:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "excluded_3",
                                   label = "Ikke tilstede posisjon 3:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "excluded_4",
                                   label = "Ikke tilstede posisjon 4:",
                                   value = "",
                                   width = "100px")),
                column(2,textInput(inputId = "excluded_5",
                                   label = "Ikke tilstede posisjon 5:",
                                   value = "",
                                   width = "100px")),
            ),
            br(),
            DT::dataTableOutput("mytable"),
            br(),
            fluidRow(actionButton("forslag", "Trykk for forslag til ord"), textOutput("infotext")), 
            br(),
            br(),
            br(),
            fluidRow(actionButton("show", "Trykk for Ann Gøril-sitat!!")),
            br(),
            plotOutput("plots"),
            br(),
            tagList("Koden ligger her: (dette er slengt sammen ganske kjapt - så den er litt subpar..)", "https://github.com/oyvindbusk/wordlebundle"),
            br()
    )
)

# Definer server logic:
server <- function(input, output) {
    output$mytable = DT::renderDataTable({
        combine_filters(tolower(input$included_letters), tolower(input$excluded_letters), tolower(input$included_1), tolower(input$included_2), tolower(input$included_3), tolower(input$included_4), tolower(input$included_5), tolower(input$excluded_1), tolower(input$excluded_2), tolower(input$excluded_3), tolower(input$excluded_4), tolower(input$excluded_5), reactive_data() %>% select(Word))
    },options = list(searching = FALSE))
    
    reactive_data <- reactive({
        combine_filters(tolower(input$included_letters), tolower(input$excluded_letters), tolower(input$included_1), tolower(input$included_2), tolower(input$included_3), tolower(input$included_4), tolower(input$included_5), tolower(input$excluded_1), tolower(input$excluded_2), tolower(input$excluded_3), tolower(input$excluded_4), tolower(input$excluded_5), fivel_words)
    })
    output$infotext <- renderText({ 
      "OBS! Ikke trykk på denne før du har filtrert bort noen ord - ellers kan den bruke litt tid. Den foreslår det ordet som utelukker flest andre ord." 
    })
   
    output$plots <- renderPlot({
        p1 = ggplot(as.data.frame(table(reactive_data()$V_1)), aes(x=Var1,y = Freq)) + geom_bar(stat = "identity")+theme(axis.title.x = element_blank()) + ggtitle("Posisjon 1")
        p2 = ggplot(as.data.frame(table(reactive_data()$V_2)), aes(x=Var1,y = Freq)) + geom_bar(stat = "identity")+theme(axis.title.x = element_blank()) + ggtitle("Posisjon 2")
        p3 = ggplot(as.data.frame(table(reactive_data()$V_3)), aes(x=Var1,y = Freq)) + geom_bar(stat = "identity")+theme(axis.title.x = element_blank()) + ggtitle("Posisjon 3")
        p4 = ggplot(as.data.frame(table(reactive_data()$V_4)), aes(x=Var1,y = Freq)) + geom_bar(stat = "identity")+theme(axis.title.x = element_blank()) + ggtitle("Posisjon 4")
        p5 = ggplot(as.data.frame(table(reactive_data()$V_5)), aes(x=Var1,y = Freq)) + geom_bar(stat = "identity")+theme(axis.title.x = element_blank()) + ggtitle("Posisjon 5")
        plot_grid(plotlist = list(p1,p2,p3,p4,p5), ncol = 2)
        
    })
    
    observeEvent(input$show, {
        showModal(modalDialog(
            title = "Neida...",
            "Det skarru få sleppe...",
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    observeEvent(input$forslag, {
        #Lag tabeller:
        t1 <- table(reactive_data()$V_1)
        t2 <- table(reactive_data()$V_2)
        t3 <- table(reactive_data()$V_3)
        t4 <- table(reactive_data()$V_4)
        t5 <- table(reactive_data()$V_5)
        # Legg til 0 i de bokstavene som ikke er tilstede:
        t1 <- utvid_tabell(t1)
        t2 <- utvid_tabell(t2)
        t3 <- utvid_tabell(t3)
        t4 <- utvid_tabell(t4)
        t5 <- utvid_tabell(t5)
        
        # Sample i tibble
        ordtibb <- tibble(t1,t2,t3,t4,t5)
        
        # Kjør på hele tabell og hent ut max:
        scored <- reactive_data() %>% rowwise() %>% mutate(ordscore = bokstav_funk(Word,t1,ordtibb))
        ord <- toString(scored[which.max(scored$ordscore),] %>% select(Word))
        showModal(modalDialog(
          title = "Ordet er...",
          ord,
          easyClose = TRUE,
          footer = NULL
        ))
  
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)