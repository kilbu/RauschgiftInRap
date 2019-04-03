Drogenkonsum in deutschen Raptexten
================

------------------------------------------------------------------------

'\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

### **Das wichtigste zuerst**

   

-   **Die Daten:**
    -   25.577 Songtexte von deutschen Rap-Musikern (genius.com)
    -   Kriminalstatistik des Bundeskriminalamts  
-   **Die Ergebnisse:**
    -   Es gibt eine eindeutige Zunahme der Raptexte, in denen über Kokain gesprochen wird
    -   Diese Zunahme kann nicht durch eine größere Zunahme von Kokaindelikten erklärt werden, bildet also keine gesellschaftliche Tendenz zu erhöhtem Konsum ab
    -   Vielmehr lässt sich die erhöhte Präsenz von Kokain in Raptexten durch einen -- sich selbst verstärkenden -- Trend erklären

'\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

------------------------------------------------------------------------

### Vorgehen

 

Um herauszufinden, ob und wie sich die Erwähnungen von Drogenkonsum in deutschen Raptexten verändert haben, müssen die geeigneten Daten gesammelt werden. Die dafür benutzten R-Skripte, sowie die Datensätze, befinden sich in diesem Repository im Ordner "data".  

Das Vorgehen ist wie folgt:  

-   Scraping der Namen deutscher Rapper von Wikipedia
-   Suchen dieser Namen über die Genius.com API
-   Download aller Songtexte derjenigen Künstler, die dort verfügbar sind, sowie der zugehörigen Metadaten (Release-Date, Album usw.)
-   Wörterbuch mit Rauschgiftsynonymen von Wikipedia und der Rauschgift-Beratungsseite www.drug-infopool.de
-   Kriminalstatistik der Rauschgiftdelikte vom Bundeskriminalamt [hier](https://de.statista.com/statistik/daten/studie/2441/umfrage/entwicklung-der-rauschgiftdelikte-in-verbindung-mit-kokain/) und [hier](https://de.statista.com/statistik/daten/studie/2443/umfrage/entwicklung-der-rauschgiftdelikte-in-verbindung-mit-cannabis/)
-   Alles wurde zwischen dem 10. und 17.03.2019 abgerufen     Im folgenden werden die so gewonnen Daten analysiert.   \*\*\*

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.3

``` r
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 3.5.3

``` r
lyrics_list <- read.csv2("lyrics_list_drugcheck1.csv", stringsAsFactors = F)[,-c(1:2)]
```

------------------------------------------------------------------------

  Verschaffen wir uns einen Überblick über die wichtigsten Daten:   Die Anzahl der Musiker\*innen im Datensatz:

``` r
length(unique(lyrics_list$artist_name))
```

    ## [1] 448

Die Anzahl der verfügbaren Lieder für jeden Jahrgang:

``` r
table(lyrics_list$release_year)
```

    ## 
    ##    1 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 
    ##    4    1    1   19   28   39   43   41  102  105  111  254  125  275  336 
    ## 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 
    ##  691  837  848  803 1103 1079 1106 1491 1588 2065 2339 2257 2287 2343  475

Wir sehen, dass nur sehr wenige Lieder vor dem Jahr 2000 verfügbar sind. Diese werden in der Folge ausgeschlossen, da sie vermutlich nicht repräsentativ für den ganzen Jahrgang sein können.

``` r
lyrics_list <- lyrics_list[which(lyrics_list$release_year >= 2000 & lyrics_list$release_year <= 2017),]
```

Im betreffenden R-Skript (siehe "data"-Ordner) wurden auch schon die Erwähnungen von Kokain oder Cannabis in Rapliedern herausgesucht.

``` r
number_songs <- table(lyrics_list$release_year)

number_coke_mentions <- table(lyrics_list$release_year[which(lyrics_list$Mentions_Cocain == 1)], lyrics_list$Mentions_Cocain[which(lyrics_list$Mentions_Cocain == 1)])

number_cannabis_mentions <- table(lyrics_list$release_year[which(lyrics_list$Mentions_Cannabis == 1)], lyrics_list$Mentions_Cannabis[which(lyrics_list$Mentions_Cannabis == 1)])

total_mentions <- data.frame("Total No. of Cocain Songs" = number_coke_mentions[,1], "Total No. of Songs" = number_songs, "Total No. of Cannabis Songs" = number_cannabis_mentions[,1])

totalMelted <- melt(total_mentions, id.var='Total.No..of.Songs.Var1')

ggplot(totalMelted, aes(x=Total.No..of.Songs.Var1, y=value, col=variable, group= variable)) + geom_line(size=1.2)  + geom_point(size=2.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](DrogenkonsumRap_files/figure-markdown_github/unnamed-chunk-5-1.png)

Die Suche nach Drogen-Namen wurde vorerst nur für Kokain und Cannabis durchgeführt. Es ist in diesem Graphen erkennbar, dass die Anzahl der Erwähnungen von Kokain (rote Linie) eindeutig eine steigende Tendenz hat. Ebenso verhält es sich mit Cannabis (blau). Allerdings kann man an der grünen Linie auch sehen, dass die Anzahl der Raplieder sowieso steigt. Daher kann der Anstieg der erwähnten Rauschmittel auch durch die allgemeine Zunahme der Lieder zustande kommen.   Die relativen Zahlen für jedes Jahr (also die Anzahl der Lieder mit Drogeninhalt im Verhältnis zur Gesamtzahl der Lieder in diesem Jahr) zeigt:

``` r
relative_coke_mentions <- table(lyrics_list$release_year, lyrics_list$Mentions_Cocain)[,2]/(table(lyrics_list$release_year, lyrics_list$Mentions_Cocain)[,1]+table(lyrics_list$release_year, lyrics_list$Mentions_Cocain)[,2])

relative_cannabis_mentions <- table(lyrics_list$release_year, lyrics_list$Mentions_Cannabis)[,2]/(table(lyrics_list$release_year, lyrics_list$Mentions_Cannabis)[,1]+table(lyrics_list$release_year, lyrics_list$Mentions_Cannabis)[,2])

relative_mentions <- data.frame("Year" = names(relative_cannabis_mentions),"Relative No. of Cocain Songs" = relative_coke_mentions, "Relative No. of Cannabis Songs" = relative_cannabis_mentions)

relativeMelted <- melt(relative_mentions, id.var='Year')

ggplot(relativeMelted, aes(x=Year, y=value, col=variable, group= variable))  + geom_line(size=1.2)  + geom_point(size=2.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](DrogenkonsumRap_files/figure-markdown_github/unnamed-chunk-6-1.png)

Für Kokain sehen wir hier, dass die Anzahl der Lieder, in denen es erwähnt wird auch relativ gesehen eine steigende Tendenz hat. Zwar sank der Wert in manchen Jahren (z.B. 2002, 2003 und 2011), trotzdem zeigt der Graph im ganzen aufwärts.  

Cannabis wurde in Lieder aus dem Jahr 2000 extrem häufig erwähnt. Dabei ist allerdings die sehr geringe Liederzahl aus dem Jahr zu bedenken, was zu Verzerrungen führen kann. Auch ist für dieses Rauschmittel für die meisten Jahre keine Tendenz auszumachen, der Gebrauch bleibt eher konstant. Nur in den letzten betrachteten Jahren (2016 und 2017) scheinen die Erwähnungen zu steigen.  

Die Polizeistatistiken des BKA geben die Anzahl der registrierten Drogendelikte für die jeweilige Droge an:

``` r
polizeistatistik_kokain <- read.csv2("polizeistatistik_kokain.csv", stringsAsFactors = F)
polizeistatistik_cannabis <- read.csv2("polizeistatistik_cannabis.csv", stringsAsFactors = F)

polizeistatistiken <- data.frame("Year" = polizeistatistik_cannabis$Jahr, "Polizeistatistik Kokain" = polizeistatistik_kokain$Polizeilich.erfasste.Fälle, "Polizeistatistik Cannabis" = polizeistatistik_cannabis$Polizeilich.erfasste.Fälle)

polizeiMelted <- melt(polizeistatistiken, id.var='Year')

ggplot(polizeiMelted, aes(x=Year, y=value, col=variable, group= variable)) + geom_line(size=1.2) + geom_point(size=2.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](DrogenkonsumRap_files/figure-markdown_github/unnamed-chunk-7-1.png)

Man sieht, dass die Anzahl der erfassten Drogendelikte für Kokain (und Crack) im Beobachtungszeitraum relativ gering und konstant ist. Die Zahl der erfassten Cannabisdelikte variiert deutlich mehr und ist vor allem in den späteren Jahren steigend.  

 

Kann also aus den vorhandenen Daten ein Einfluss identifiziert werden, der die tendenziell häufigere Erwähnung von Kokain im Deutschrap erklärt?   Der erste Versuch ist eine OLS-Regression. Wenn Rapper\*innen nur über das sprechen, was sie in ihrer Realität erleben, dann hat vielleicht die Anzahl der Kokaindelikte einen Einfluss auf deren Erwähnung.

``` r
options("scipen"=100, "digits"=4)

df <- data.frame(relative_coke_mentions,  "Kokain_Faelle"=polizeistatistik_kokain$Polizeilich.erfasste.Fälle)

summary(lm(df$relative_coke_mentions ~ df$Kokain_Faelle))
```

    ## 
    ## Call:
    ## lm(formula = df$relative_coke_mentions ~ df$Kokain_Faelle)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.07233 -0.00891  0.00769  0.01573  0.06183 
    ## 
    ## Coefficients:
    ##                     Estimate  Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.27198871  0.06218091    4.37  0.00047 ***
    ## df$Kokain_Faelle -0.00000879  0.00000494   -1.78  0.09437 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0325 on 16 degrees of freedom
    ## Multiple R-squared:  0.165,  Adjusted R-squared:  0.113 
    ## F-statistic: 3.16 on 1 and 16 DF,  p-value: 0.0944

``` r
confint(lm(df$relative_coke_mentions ~ df$Kokain_Faelle))
```

    ##                        2.5 %      97.5 %
    ## (Intercept)       0.14017107 0.403806353
    ## df$Kokain_Faelle -0.00001927 0.000001689

Das Ergebnis dieser Regression lässt zuerst vermuten, dass zumindest der polizeilich erfasste Kokainkonsum im betreffenden Jahr keinen Einfluss auf die Erwähnung von Kokain in Raptexten hat. Die Fallzahlen des BKA zeigen zwar einen statistisch minimal signifikanten Einfluss. Jedoch ist dessen Höhe verschwindend gering und eine nähere Betrachtung der Konfidenzintervalle zeigt, dass nicht mit hinreichender Sicherheit gesagt werden kann, ob dieser Effekt positiv oder negativ ist. Die durch das Modell erklärte Varianz ist ebenfalls recht gering.  

Deshalb sollten zwei weitere Variablen in Betracht gezogen werden.  

Erstens: Eine zeitliche Verzögerung des Einflusses von realem Kokainkonsum, bis sich dessen Erwähnung in Raptexten niederschlägt (wenn also bspw. wenn im Jahr 2004 viel Kokain konsumiert wird, kommen die Raptexte, die das besprechen eventuell erst 2005 auf den Markt).  

Zweitens: Es sollte in Betracht gezogen werden, dass die Erwähnung von bestimmten Drogen gar keinen Bezug zur Realität haben. In diesem Fall folgen Rapper\*innen nur einem Trend und sprechen über Kokainkonsum, weil alle anderen dasselbe tun.

 

Es wird also eine Variable für Kokaindelikte im Vorjahr und für Kokain-Erwähnungen im Vorjahr eingefügt und fließen in die Regression ein.

``` r
coke_mentions_t_minus_1 <- rep(NA, length(df$relative_coke_mentions))
for (i in 2:length(df$relative_coke_mentions)) {
  coke_mentions_t_minus_1[i] <- df$relative_coke_mentions[i-1]
}

Kokain_Faelle_t_minus_1 <- rep(NA, length(df$relative_coke_mentions))
for (i in 2:length(df$Kokain_Faelle)) {
  Kokain_Faelle_t_minus_1[i] <- df$Kokain_Faelle[i-1]
}

df <- data.frame(relative_coke_mentions,  "Kokain_Faelle"=polizeistatistik_kokain$Polizeilich.erfasste.Fälle, Kokain_Faelle_t_minus_1, coke_mentions_t_minus_1)


summary(lm(df$relative_coke_mentions ~ df$Kokain_Faelle + df$Kokain_Faelle_t_minus_1 + df$coke_mentions_t_minus_1))
```

    ## 
    ## Call:
    ## lm(formula = df$relative_coke_mentions ~ df$Kokain_Faelle + df$Kokain_Faelle_t_minus_1 + 
    ##     df$coke_mentions_t_minus_1)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.04679 -0.00901  0.00277  0.00871  0.02257 
    ## 
    ## Coefficients:
    ##                                Estimate   Std. Error t value Pr(>|t|)    
    ## (Intercept)                 0.029928384  0.068001987    0.44  0.66709    
    ## df$Kokain_Faelle            0.000002366  0.000004743    0.50  0.62623    
    ## df$Kokain_Faelle_t_minus_1 -0.000000791  0.000004932   -0.16  0.87497    
    ## df$coke_mentions_t_minus_1  0.735770363  0.171391335    4.29  0.00087 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0185 on 13 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.665,  Adjusted R-squared:  0.588 
    ## F-statistic: 8.61 on 3 and 13 DF,  p-value: 0.00209

Dieses Modell scheint schon deutlich besser geeignet zu sein, um die verstärkte Erwähnung von Kokain im deutschen Rap zu erklären.  

Es zeigt sich, dass die reale Anzahl der Kokaindelikte weder im selben, noch im vorherigen Jahr einen Einfluss auf dessen Erwähnung in Rapliedern hat. Einen stark positiven und statistisch hochsignifikanten Einfluss hat hingegen, ob im Vorjahr über Kokain gesprochen wurde. Ebenfalls erklärt dieses Modell nun etwa 60% der variation in der Anzahl der Lieder, die Kokain erwähnen.

 

Es scheint, als wäre der vorhandene Trend, sich in Raptexten auf Kokain zu beziehen keine Reaktion auf einen erhöhten Konsum in der Realität, sondern vielmehr ein Trend, dem die Rapper\*innen folgen. Mit welchem Einfluss solche Texte allerdings auf die Zielgruppe der Künstler zurückwirken und zu realem Rauschmittelkonsum ermutigen wäre zu diskutieren.  

Bei den Ergebnissen sind noch einige Anmerkungen bezüglich der Aussagekraft zu betrachten:  

-   Die Anzahl der betrachteten Jahre ('00 - '17) ist relativ gering
-   Ebenso ist die Anzahl der verfügbaren Lyrics vor allem in den frühen Jahren deutlich geringer als in den späteren
-   Es gibt eine mögliche Selektion der Artists: Es könnte in Betracht gezogen werden, dass vornehmlich aktuelle und erfolgreiche Künstler auf Wikipedia verzeichnet sind. Diese Selektion scheint jedoch für den Autor erträglich, da sie möglicherweise die Relevanz und Einfluss der Künstler\*innen auf das Publikum wiederspiegelt
-   Dasselbe gilt für die Verfügbarkeit von Songtexten auf Genius.com
-   Die Polizeistatistik ist insofern zu hinterfragen, als dass dort nicht ersichtlich ist, wie sich die Strategie zur Strafverfolgung verändert hat. Eine lückenlosere Statistik zu Kokainkonsum in Deutschland ließ sich jedoch nicht finden, sodass diese als Proxy dienen muss.
