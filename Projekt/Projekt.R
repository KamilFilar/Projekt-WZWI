#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(dplyr);
library(DT);
library(cluster);
library(dbscan);

#POBRANIE DANYCH ZE STRONY

aktualnosci <- tidyfeed(feed="http://www.rss.gofin.pl/prawopracy.xml")
#View(aktualnosci)
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

#FUNKCJA ZWRACA ILE % WIERSZY TABELI tabela2 ZAWIERA WCZYTANE slowo
analiza1<-function(slowo, tabela2){
    dane1<-solr_search(conn = polaczenie, params = list(q=paste(tabela2,":",slowo),fl=paste(tabela2), rows=-1));
    sprawdz_duplikaty <- duplicated(dane1)
    View(sprawdz_duplikaty)
    dane_koncowe <- unique(dane1)
    ilosc_kolumn <- nrow(dane_koncowe)
    
    return(ilosc_kolumn);
}

lematyzacja<-function(tekst){
    #FUNKCJA ODPOWIEDZIALNA ZA LEMATYZACJE TEKSTU (KOPIA Z LAB4) sprawdzanie duplikatow!
    parametry<-list(lpmn="any2txt|wcrft2", text=tekst, user="FilarKamil04@gmail.com");
    odpowiedz<-POST("http://ws.clarin-pl.eu/nlprest2/base/process", body=parametry, encode="json", verbose());
    zawartosc<-content(odpowiedz, "text", encoding="UTF-8");
    xml<-xmlParse(zawartosc, encoding="UTF-8");
    slowa<-xpathSApply(xml, '//chunkList/chunk/sentence/tok/lex/base', xmlValue, encoding="UTF-8");
    
    return(paste(slowa, collapse=" "));
}

#FUNKCJA ODPOWIEDZIALNA ZA PRZYGOTOWANIE DANYCH DO ANALIZY
przygotowanie_danych<-function(dane){
    stop<-as.vector(unlist(read.csv(file="stop_words_pl.txt", header=FALSE, sep=",", fileEncoding="UTF-8")));
    
    dokumenty<-Corpus(VectorSource(stri_enc_toutf8(dane)));
    
    dokumenty<-tm_map(dokumenty, removePunctuation, preserve_intra_word_dashes=TRUE);
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
    d<-data.frame(words=names(v), freq=v);
    
    return(d);
}

#FUNKCJA ODPOWIEDZIALNA ZA TWORZENIE CHMURY SLOW
chmura<-function(tabela){
    dane<-solr_search(conn = polaczenie, params = list(q=paste(tabela,":*"),fl=paste(tabela), rows=-1));
    sprawdz_duplikaty_chmura <- duplicated(dane)
    View(sprawdz_duplikaty)
    dane_z_solra <- przygotowanie_danych(dane)
    dane_koncowe <- unique(dane_z_solra)
    return(dane_koncowe);
}

#FUNKCJA SLUZACA DO KLASTERYZACJI PRZETWORZONYCH DANYCH
klasteryzacja <- function(tabela3, metryka, numberOFk){
   
    dane<-solr_search(conn = polaczenie, params = list(q=paste(tabela3,":*"),fl=paste(tabela3), rows=-1));
    sprawdz_duplikaty_klasteryzacja <- duplicated(dane)
    View(sprawdz_duplikaty)
    
    przetworzone_dane <- przygotowanie_danych(dane)
    dane_z_solra <- przygotowanie_danych(dane)
    dane_koncowe <- unique(dane_z_solra)
    
    klasteryzacja.h<-agnes(dane_koncowe, metric=paste(metryka), method="average");
    klaster<-cutree(klasteryzacja.h, k=paste(numberOFk));
    tabela.wynikowa.h<-cbind(dane_koncowe, klaster);
    
    plot(klasteryzacja.h, which.plots=2);
    
    return(tabela.wynikowa.h);
}

#FUNKCJA PRZYGOTOWUJE DANE DO WYPISANIA ILOSCI DANEGO SLOWA I STWORZENIA WYKRESU 10 NAJPOPULARNIEJSZYCH
ilosc_slow<-function(tabela2){
   
    stop<-as.vector(unlist(read.csv(file="stop_words_pl.txt", header=FALSE, sep=",", fileEncoding="UTF-8")));
    
    dane<-solr_search(conn = polaczenie, params = list(q=paste(tabela2,":*"),fl=paste(tabela2), rows=-1));
    sprawdz_duplikaty_ilosc_slow <- duplicated(dane)
    View(sprawdz_duplikaty_ilosc_slow)

    dane_koncowe <- unique(dane)
    
    dokumenty<-Corpus(VectorSource(stri_enc_toutf8(dane_koncowe)));
    dokumenty<-tm_map(dokumenty, removePunctuation, preserve_intra_word_dashes=TRUE);
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
    return(v);
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
            textInput("slowo2", h4("Slowo do wyszukania: ")),
            
            actionButton("buttonA3","Start"),
        ),
        
        mainPanel(
            fluidRow(class="SpecialBOX",
                tags$div(class="BorderSep",
                    tags$p(class="SubSubTitle", "Wczytane slowo:"),
                    h3(textOutput("slowooutput", container = span)),
                    tags$p(class="SubSubTitle", "Ile razy wystepuje:"),
                    tags$div(class="CenterBox", 
                             tableOutput('table'),
                    )
                ),
            ),
            plotOutput("WykresA3")
        ),
    ),
    tags$div(class="LineX"),
    
    sidebarLayout(
        sidebarPanel( 
            tags$p("Klasteryzacja", class="SubTitle"),
            selectInput(inputId="SelectTab4",label=h4("Wybierz dane"),choices = c("Tytul"="Tytul","Zawartosc"="Zawartosc"),
                        selected = "Tytul",multiple = F),
            selectInput(inputId="SelectTab5",label=h4("Metryka"),choices = c("euclidean"="euclidean","manhattan"="manhattan"),
                        selected = "euclidean",multiple = F),
            numericInput("numberOFk", h4("Ilosc klastrow"), value = 2, min = 2, max = 10, step = 1),
            actionButton("buttonA4","Start"),
            ),
        mainPanel(
            fluidRow(class="SpecialBOX",
                     tags$div(class="BorderSep",
                              tags$div(class="CenterBox2", 
                                       tableOutput('table2'),
                              )
                     ),
                ),
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
    
    observeEvent(input$buttonA2,{
            
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
    
    observeEvent(input$buttonA3,{
                     #WCZYTANY NUMER
                     wypisane_slowo2 <- eventReactive(input$buttonA3, {
                         input$slowo2
                     })
                     output$slowooutput <- renderText({
                         wypisane_slowo2()
                     })
                     #WCZYTANA ILOSC SLOW
                     dane <- data.frame(words=names(ilosc_slow(input$SelectTab3)), freq=ilosc_slow(input$SelectTab3))
                     output$WykresA3 <- renderPlot({
                         barplot(dane[1:10,]$freq, las = 2, names.arg = dane[1:10,]$word,
                                 col ="#336600", main ="10 najczesciej wystepujacych slow",
                                 ylab = "Czestotliwosc wystepowania", ylim = c(0,30),
                                 xlab = "Slowo")
                     })
                     
                    inputWORD <- input$slowo2
                     outputWORD <- filter(dane,
                                          words == toString(inputWORD)
                                          )
                     output$table <- renderTable({outputWORD})
                 });
    
    observeEvent(input$buttonA4,{
       output$table2<- renderTable(klasteryzacja(input$SelectTab4, input$SelectTab5, input$numberOFk))
    })
}
#Start aplikacji
shinyApp(ui = ui, server = server)