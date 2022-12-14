# IntroDataScience
---
title: "The influence of surface on tennis matches"

author: "Martin Hebert"
date: "`r Sys.Date()`"
output:
  html_document:
   toc: true
bibliography: References tennis.bib
biblio-style: apa
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries, message=FALSE, warning=FALSE, echo=FALSE}
library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(jpeg)
library(readr)
References_tennis <- read_csv("References tennis.bib")
```

```{r dataset, message=FALSE, include=FALSE}
matches <- read_csv("KaggleMatches2015-19SuperClean.csv")
spec(matches)
```

## Introduction

Tennis [@tenniswiki] is a very renowned racket sport played in just about the whole world by millions of recreational players, but it also boasts a very important competitive scene, where professional players face each other in multiple tournaments. For men, those are organized by the Association of Tennis Professionals (ATP). For women, it is the Women's Tennis Association (WTA) that regulates competitions.

The most known tournaments are the four Grand Slams [@grandslamswiki], the Masters [@masterswiki] and the Olympic games [@olympicswiki]. Tennis is played on a court of a standard size, which is the same all over the world, not only in all tournaments, but also in all smaller clubs. However, although the size is the same everywhere, the surface of the tennis court can vary. There are several usual surfaces on which tennis is played: clay, hard and grass. These surfaces have technical differences, like the speed of the ball and the height of the rebound, that are explained on Neilson's website [@surfaceexplained].

The results that professional players obtain during tournaments give them ranking points - the better the result, the more points earned. The player with the most ranking points at any moment is the world number 1. In each tournament, players are given seeds according to their rank, which prevents the best players to play against each other in the early rounds of the tournament. Of course, every sport has its amount of randomness, which implies that this system doesn't always work and sometimes the player with the lower ranking beats the one with the higher ranking. This is what we call an **upset**.

In this report, I will investigate those upsets and link them to the surface on which matches are played. I will attempt to answer the following question: is the number of upsets in men's tennis matches dependent on the surface of the court? In other words, do upsets happen more frequently on clay, hard or grass courts? I don't expect any surface to be more prone to an upset, as the technical differences between courts are known to every player and can therefore be trained for by everybody.

![](Tennis%20surfaces%20image.jpeg)

## Data and methods

### Data

The dataset I will use for this report comes from [Kaggle](https://www.kaggle.com) and was collected by Jeff Sackmann [@jeffsackmann]. It contained, before some pruning, **A LOT** of information from **A LOT** of tennis matches across **A LOT** of years. I kept only the variables that were relevant for my analysis: *surface* of the court and both *ranks* of the winner and the loser of the match. The time period I will focus on ranges from 2015 to 2019. I chose those five years for several reasons (apart from it being easier to analyze):

-   My focus is on modern tennis, which I know well and have followed a lot during this time period.
-   The coronavirus pandemic deeply impacted the tennis world, some tournaments not being held, others not giving points, so I will not analyze matches from 2020 onward.
-   It was also important that the time frame I would analyze was structurally similar, so as not to find some effects due to the period of the sport, like very different playing styles or modified rules in the ATP tour.

### Hypotheses

Here are the hypotheses I will test:

-   **H0**: The number of upsets in tennis matches does not depend on the surface on which the match is played.
-   **H1**: The number of upsets in tennis matches depends on the surface on which the match is played.

### Method

To answer these hypotheses, I will investigate the number of upsets that happened on each surface between 2015 and 2019. Then, I will have to divide this absolute quantity by the number of matches played on each of the surfaces, in order to create a *percentage of upsets* for each type of court. Finally, I will perform a chi squared test to know if the percentage of upsets on each surface is statistically different.

### Data exploration

```{r frequencies}
ls(matches)
length(matches$surface)
surffreq <- table(matches$surface)
surffreq
barplot(surffreq)
```

Here are the number of matches played on each surface between 2015 and 2019. We can see that a very small amount were played on carpet and others don't have any surface indicated: they will be excluded from the analyses.

### Data wrangling

```{r separation}
G <- subset(matches, surface == "Grass")
H <- subset(matches, surface == "Hard")
C <- subset(matches, surface == "Clay")
G
H
C
```

Now all the matches played on the three surfaces of interest are in separate tables. It might also be useful to merge them for future analyses, so let's do it now to avoid future hassle.

```{r merging}
GHC <- subset(matches, surface %in% c("Grass", "Hard", "Clay"))
GHC
```

There are now "only" 14349 matches of interest.

The next step is to determine the number of upsets that happened on each of the surfaces. In order to do this, I have to code each instance in which the winner's rank is bigger than the loser's (which actually means that it is worse, although the number is bigger).

```{r number of upsets}
GHCupsets <- (GHC$winner_rank > GHC$loser_rank)
table(GHCupsets)
Gupsets <- (G$winner_rank > G$loser_rank)
table(Gupsets)
Hupsets <- (H$winner_rank > H$loser_rank)
table(Hupsets)
Cupsets <- (C$winner_rank > C$loser_rank)
table(Cupsets)
totUpsets <- data.frame(
  x = c("Total", "Grass", "Hard", "Clay"),
  y = c(4898, 582, 2798, 1518)
)

ggplot(totUpsets, aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = 0), color = "gray") +
  geom_point(color = "red") +
  ggtitle("Total upsets") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Surface of the court") +
  ylab("Number of upsets")
```

Here, "FALSE" means that the winner of the game had a lower rank than the loser, *ergo*, a better rank. And "TRUE" means the contrary!

## Results

### Descriptive results

I believe I can call those **RESULTS**, although there is still some work to make them clean and understandable. First, I was a little surprised because the number of matches on each surface isn't the number I get when I add the "TRUE" and "FALSE" values I got before. So I went back to my data and saw that a small number of matches were lacking the rank of one of the players, and therefore couldn't be a part of the analysis: perfect, I didn't want them anyway!

Next step: converting the values I obtained into percentages to make them comparable, as the number of matches on each surface is not the same.

#### Grass

1046 + 582 = 1628

1046/1628 = 0.6425061

1 - 0.6425061 = 0.3574939

#### Hard

5365 + 2798 = 8163

5365/8163 = 0.6572339

1 - 0.6572339 = 0.3427661

#### Clay

2806 + 1518 = 4324

2806/4324 = 0.6489362

1 - 0.6489362 = 0.3510638

We now know the percentage of upsets on each surface, and it would seem that they are very similar across all surfaces. Let's check it statistically!

### Statistical results

To test the significance of the results, the chi square test can be used [@chitestwiki]. First, I will create a contingency table with the percentages.

```{r contingency, echo=FALSE}
perctable <- matrix(c(0.6425061, 0.3574939, 0.6572339, 0.3427661, 0.6489362, 0.3510638), nrow = 2)
colnames(perctable) <- c("Grass", "Hard", "Clay")
rownames(perctable) <- c("Normal", "Upsets")
perctable
```

Perfect, all the percentages of "Normal" games and "Upsets" are neatly inside a table. Last but not least (it's actually the goal of the whole report), let's see if the differences are statistically significant.

```{r significance, warning=FALSE}
goal <- chisq.test(perctable)
print(goal$p.value)
```

That is a *very* high p-value, *very* far from the classic 0.05 cut-off of significance. Therefore, I cannot reject my null hypothesis, that stated that "the number of upsets in tennis matches does not depend on the surface on which the match is played". Maybe a graph is in order to see what this all looks like.

```{r echo=FALSE}
percUpsets <- c(0.3574939, 0.3427661, 0.3510638)
barplot(percUpsets, main = "Percentage of upsets across surfaces", ylim = c(0, 0.40), names.arg	= c("Grass", "Hard", "Clay"), ylab = "% of upsets", col = c("Green", "Blue", "Orange"))
```

We can clearly see that the percentage of upsets is very similar across all surfaces.

## Discussion

The goal of the report was to investigate the link between court surface and upsets in men's tennis matches. After analyzing the data I had, I found no influence of the surface of the court on the outcome of matches.

Dayekh (2022) had found similar results on the women's professional tour [@dayekh]. Both his and my results indicate that the surface of the court is not a determining factor for the frequency of upsets. In other words, no matter the surface, the player with the lower rank has more or less the same probability to win the match, beating the player with the better rank. This probability is around 35% which seems actually quite high when you think about it: 7 times out of 20, the unexpected result happens - no wonder the betting companies work so well.

Now, how exactly should I interpret this? Well, we know that every player has his strengths and weaknesses, which are influenced by the surface of the court. Just look at Rafael Nadal, who won 14 times on the clay courts of Roland-Garros [@nadalwiki]. Extrapolating his exceptional case to other players, we can assume that they all have a higher probability of winning matches on a specific surface, and a lower probability of winning on another surface. Basically, that everybody has a preferred court type. Combining this fact with my results, I can safely say that all those preferences even out across every player and every surface. So, there is no surface that is more prone to witness an upset: **CRAZY**!

Naturally, this analysis has a few limitations, often linked with the notion of upset. I defined an upset as a match won by the player with the lower rank of both contestants. This doesn't take into account the rank *difference*. Two players could have only a one rank difference and this would still be considered an upset. To counteract this, it could be interesting to analyze whether the upsets happen as frequently between players with a bigger rank difference. But then, a difference between the 1st and 10th player in the world is not the same as the one between the 21st and the 30th.

Moreover, the ranks are updated every week after tournaments, so a player coming back from an injury could have a lower rank than he should based purely on his actual form, making an upset more probable.

In addition to this, I only considered the *outcome* of the matches, without considering the *scores*, which could tell more about how the encounter actually went. It could be interesting for future research to investigate the upsets based on the number of points scored by each player.

## Conclusion

This report showed that there are no differences across court surfaces on the number of upsets in men's tennis matches. This finding is what I expected beforehand, but I now have the confirmation through data analysis, which makes my speculations sound much more like ***evidence***.

Lots of different research questions could still be analyzed with the data that was used. It is a promising and interesting field and it was enjoyable to dive into it.

## References
