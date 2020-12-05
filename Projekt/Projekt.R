library(shiny)
library(tidyRSS)
library(solrium)

library(tidyRSS);
library(tm);
library(httr);
library(XML);
library(wordcloud2);
library(stringi);
library(ggplot2);

#POBRANIE DANYCH ZE STRONY

aktualnosci <- tidyfeed(feed="http://www.rss.gofin.pl/prawopracy.xml")

#WCZYTANIE WYBRANYCH DANYCH DO OBIEKTOW (SENSOWNYCH)
tytul_data <- aktualnosci$entry_title
zawartosc_data <- aktualnosci$entry_content
URL_data <- aktualnosci$entry_url

#View(aktualnosci) PODGLAD DANYCH (POMOCNICZO)

#DZIALANIE W SOLR
polaczenie <-SolrClient$new(host="127.0.0.1", port = 8983, path = "/solr/ProjektWZWI/select");

dane = data.frame(matrix(ncol=4, nrow = 10));
colnames(dane)[1] <- "id";
colnames(dane)[2] <- "Tytul";
colnames(dane)[3] <- "Zawartosc";
colnames(dane)[4] <- "URL"

dane$id<-c(1,2,3,4,5,6,7,8,9,10);
dane$Tytul<-c(tytul_data);
dane$Zawartosc<-c(zawartosc_data);
dane$URL<-c(URL_data);

solrium::add(x=dane, conn=polaczenie, name="ProjektWZWI", commit=TRUE);


#FUNKCJE DO ANALIZY DANYCH

analiza1<-function(slowo, tabela2){
    #FUNKCJA ZWRACA ILE % WIERSZY TABELI TYTULY ZAWIERA WCZYTANE slowo
    dane1<-solr_search(conn = polaczenie, params = list(q=paste(tabela2,":",slowo),fl=paste(tabela2), rows=-1));
    ilosc_kolumn <- nrow(dane1)
    #View(dane1)
    
    return(ilosc_kolumn);
}

lematyzacja<-function(tekst)
{
    #FUNKCJA ODPOWIEDZIALNA ZA LEMATYZACJE TEKSTU (KOPIA Z LAB4)
    parametry<-list(lpmn="any2txt|wcrft2", text=tekst, user="FilarKamil04@gmail.com");
    odpowiedz<-POST("http://ws.clarin-pl.eu/nlprest2/base/process", body=parametry, encode="json", verbose());
    zawartosc<-content(odpowiedz, "text", encoding="UTF-8");
    xml<-xmlParse(zawartosc, encoding="UTF-8");
    slowa<-xpathSApply(xml, '//chunkList/chunk/sentence/tok/lex/base', xmlValue, encoding="UTF-8");
    
    return(paste(slowa, collapse=" "));
}

chmura<-function(tabela){
    #FUNKCJA ODPOWIEDZIALNA ZA TWORZENIE CHMURY SLOW
    stop<-as.vector(unlist(read.csv(file="slowa.txt", header=FALSE, sep=",", fileEncoding="UTF-8")));
    
    dane2<-solr_search(conn = polaczenie, params = list(q=paste(tabela,":*"),fl=paste(tabela), rows=-1));
    
    dokumenty<-Corpus(VectorSource(stri_enc_toutf8(dane2)));
    
    dokumenty<-tm_map(dokumenty, removePunctuation);
    dokumenty<-tm_map(dokumenty, removeNumbers);
    dokumenty<-tm_map(dokumenty, removeWords, stop);
    
    usun.znaki<-function(x) gsub("[–„”]", "", x);
    dokumenty<-tm_map(dokumenty, usun.znaki);

    for(d in 1:length(dokumenty))
    {
        dokumenty[[d]]$content<-lematyzacja(dokumenty[[d]]$content);
        dokumenty[[d]]$content<-stri_enc_toutf8(dokumenty[[d]]$content);
    }
    
    tdm1<-TermDocumentMatrix(dokumenty);
    View(tdm1)
    m1<-as.matrix(tdm1);
    View(m1)
    v<-sort(rowSums(m1),decreasing=TRUE);
    View(v)
    d<-data.frame(words=names(v), freq=v);
    View(d)
    
    return(d);
}

ilosc_slow<-function(tabela2, numberROW){
    #FUNKCJA ODPOWIEDZIALNA ZA TWORZENIE CHMURY SLOW
    stop<-as.vector(unlist(read.csv(file="slowa.txt", header=FALSE, sep=",", fileEncoding="UTF-8")));
    
    dane2<-solr_search(conn = polaczenie, params = list(q=paste(tabela2,":*"),fl=paste(tabela2), rows=paste(numberROW)));
    
    dokumenty<-Corpus(VectorSource(stri_enc_toutf8(dane2)));
    
    dokumenty<-tm_map(dokumenty, removePunctuation);
    dokumenty<-tm_map(dokumenty, removeNumbers);
    dokumenty<-tm_map(dokumenty, removeWords, stop);
    
    usun.znaki<-function(x) gsub("[–„”]", "", x);
    dokumenty<-tm_map(dokumenty, usun.znaki);
    
    for(d in 1:length(dokumenty))
    {
        dokumenty[[d]]$content<-lematyzacja(dokumenty[[d]]$content);
        dokumenty[[d]]$content<-stri_enc_toutf8(dokumenty[[d]]$content);
    }
    
    tdm1<-TermDocumentMatrix(dokumenty);
    m1<-as.matrix(tdm1);
    v<-sort(rowSums(m1),decreasing=TRUE);
    i<-data.frame(words=names(v), freq=v);
    View(i)
    
    
    return(i);
}

#UI aplikacji
ui <- fluidPage(
    
    includeCSS("style.css"),
    
    # Application title
    tags$div(class="Title",
        tags$p("Aplikacja webowa do akwizycji i analizy danych"),
        tags$p("z kanalu RSS portalu"),
        tags$p("Gofin.pl - prawo pracy")
    ),
    tags$div(class="Line1"),
    tags$div(class="Line2"),
    
    sidebarLayout(
        sidebarPanel(
           tags$p("Procentowa zawartosc konkretnego slowa", class="SubTitle"),
           
           selectInput(inputId="SelectTab2",label=h4("Wybierz dane"),choices = c("Tytul"="Tytul","Zawartosc"="Zawartosc"),
                       selected = "Tytul",multiple = F),
           textInput("slowo", h4("Slowo do wyszukania: ")),
           selectInput(inputId="color1",label=h4("Wybierz kolor"),
                       choices = c("Czerwony"="Czerwony","Niebieski"="Niebieski","Zielony"="Zielony"),
                        selected = "Zielony",multiple = F),
           
           
           actionButton("buttonA1","Start"),
           
        ),

        mainPanel(
            plotOutput("WykresA1")
        ),
        
    ),
    tags$div(class="LineX"),
    
    sidebarLayout(
        sidebarPanel(
            tags$p("Chmura slow", class="SubTitle"),
            selectInput(inputId="SelectTab1",label=h4("Wybierz dane"),choices = c("Tytul"="Tytul","Zawartosc"="Zawartosc"),
                        selected = "Tytul",multiple = F),
            selectInput(inputId="SelectShape",label=h4("Wybierz ksztalt"),
                        choices = c("Gwiazda"="Gwiazda","Trojkat"="Trojkat","Diament"="Diament"),
                        selected = "Gwiazda",multiple = F),
            
            numericInput("size", h4("Rozmiar"), value = 0.5, min = 0.4, max = 1, step = 0.1),
            numericInput("grid", h4("Odstep miedzy literami"), value = 5, min = 0, max = 25, step = 5),
            
            actionButton("buttonA2","Start"),
        ),
        
        mainPanel(
            wordcloud2Output('WykresA2')
        ),
    ),
    
    tags$div(class="LineX"),
    
    sidebarLayout(
        sidebarPanel(
            tags$p("Analiza czestosci wystepowania slowa", class="SubTitle"),
            selectInput(inputId="SelectTab3",label=h4("Wybierz dane"),choices = c("Tytul"="Tytul","Zawartosc"="Zawartosc"),
                        selected = "Tytul",multiple = F),
            numericInput("numberROW", h4("Numer rzedu:"), value = 5, min = 1, max = 10, step = 1),
            textInput("slowo2", h4("Slowo do wyszukania: ")),
            
            actionButton("buttonA3","Start"),
        ),
        
        mainPanel(
            fluidRow(class="SpecialBOX",
                tags$p(class="SubSubTitle", "Wczytana tabela:"),
                h3(textOutput("tabelaoutput", container = span)),
                tags$p(class="Separation","-------------------------------"),
                
                tags$p(class="SubSubTitle", "Wczytany nr. artykulu:"),
                h3(textOutput("nroutput", container = span)),
                tags$p(class="Separation","-------------------------------"),
                
                
                tags$p(class="SubSubTitle", "Wybrane slowo:"),
                h3(textOutput("slowooutput", container = span)),
                tags$p(class="Separation","-------------------------------"),
                
                
                tags$p(class="SubSubTitle", "Ilosc slow (po lematyzacji):"),
                plotOutput("WykresA3")
                
            )
        ),
    ),
    
    tags$div(class="LineX"),
    
    tags$div(class="Line3"),
    tags$div(class="Line4"),
    tags$div(class="FooterDescription",
             tags$p("Kamil Filar, Informatyka, rok III, lab1")
    )
    
)

# Logika serwera
server <- function(input, output) {
    
    
    observeEvent(input$buttonA1,
                 {
                    
                    output$WykresA1 <- renderPlot({
                        
                        if(input$color1=="Czerwony"){
                            sColor = "#ff3300"
                        }else if(input$color1=="Niebieski"){
                            sColor = "#0099ff"
                        }else if(input$color1=="Zielony"){
                            sColor = "#006600"
                        }
                        
                        x <- c((analiza1(input$slowo, input$SelectTab2))*10, (10-analiza1(input$slowo, input$SelectTab2))*10)
                        labels <- c(paste("Wystepuje w ",(analiza1(input$slowo,input$SelectTab2))*10,"% tytulow"),
                                  paste("Nie wystepuje w",(10-analiza1(input$slowo,input$SelectTab2))*10," % tytulow"))
                        
                        pie(x,labels, main=paste("Procent wystepowania slowa:",input$slowo," w ",input$SelectTab2), col = c(sColor,"grey"),
                            border="#006600")
                        
                    })
                 });
    observeEvent(input$buttonA2,
        {
            
            output$WykresA2 <- renderWordcloud2({
                
                dane <- chmura(input$SelectTab1);
                
                if(input$SelectShape=="Gwiazda"){
                    varshape = "star"
                }else if(input$SelectShape=="Diament"){
                    varshape = "diamond"
                }else if(input$SelectShape=="Trojkat"){
                    varshape = "triangle-forward"
                }

                wordcloud2(dane, size=input$size, gridSize = input$grid, 
                           color = "random-dark", backgroundColor = "white",
                           shape = varshape)
            })
            
        });
    
    observeEvent(input$buttonA3,
                 {
                     #WCZYTANE SLOWO
                     wypisane_slowo <- eventReactive(input$buttonA3, {
                         input$slowo2
                     })
                     output$slowooutput <- renderText({
                         wypisane_slowo()
                     })
                     #WCZYTANA TABELA
                     wypisana_tabela <- eventReactive(input$buttonA3, {
                         input$SelectTab3
                     })
                     output$tabelaoutput <- renderText({
                         wypisana_tabela()
                     })
                     #WCZYTANY NUMER
                     wypisany_nr <- eventReactive(input$buttonA3, {
                         input$numberROW
                     })
                     output$nroutput <- renderText({
                         wypisany_nr()
                     })
                     #WCZYTANA ILOSC SLOW
                     plot(ilosc_slow(input$SelectTab3, input$numberROW)) 
                 })
    
}

#Start aplikacji
shinyApp(ui = ui, server = server)
