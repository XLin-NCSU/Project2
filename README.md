Project 2: Clash Royale data analysis
================
Xi Lin
2023-09-26

## Introduction

The aim of this project is to show how to retrieve data from an API and
analyze the data. Since I am playing Clash Royale(a popular PVP mobile
game), I would use the API provided by its developer, Supercell company,
to demonstrate the work. The website of the API is
[here](https://developer.clashroyale.com/).

<img src="clash-royale.jpg" width="300px" style="display: block; margin: auto;" />

In this project, you could get the clan’s data and player’s data from
the functions that I created. Since Supercell limits the access of API
by IP address, please provide me your IP address and I will email your
token to you.

You can find your IP address from
[google](https://www.google.com/search?q=what%27s+my+ip+address).

## Requirements

The packages that I used in this project:

- `httr`: API interaction
- `jsonlite`: parse the data
- `tidyverse`: data manipulation and visualization

## API Interaction Functions

- `lastseendate`

This function is to correcting last seen date in clan data.

``` r
# subtract the date info(YYYYMMDD format) and convert it to date type.
lastseendate = function(data){
  lastSeen <- substr(data$lastSeen, 1, 8)
  lastSeen <- as.Date(data$lastSeen, "%Y%m%d")
}
```

- `clan`

This function enables you to get clan members’ basic performance info by
simply input the clan tag, the API token, the clan member’s role(leader,
elder, member) and the clan member’s arena level(high, medium, low). The
default argument will return all roles and all arena levels. Be careful
that don’t forget the single quotes when you input the clan tag. Correct
format: `'#8YVVGR2Y'`

``` r
clan = function(clantag, token, role = 'all', arena.level = 'all'){
# modify the url by replacing "#" symbol to "%23"
url = paste0("https://api.clashroyale.com/v1/clans/",'%23',substr(clantag, 2, nchar(clantag)))
# store the full token 
headers <- c(`Authorization` = sprintf('Bearer %s', token))
# get data
rawclandata <- httr::GET(url = url, httr::add_headers(.headers = headers))
# parse data
clandata <- jsonlite::fromJSON(rawToChar(rawclandata$content))
# extract the info we want
memberList <- clandata$memberList
# correct date variable
memberList$lastSeen <- lastseendate(memberList)

# argument filter
if (role == 'all'){
  if (arena.level == 'all'){
    output <- memberList
  } else if (arena.level == 'high'){
    output <- memberList %>% filter(arena$id >= 54000017)
  } else if (arena.level == 'medium'){
    output <- memberList %>% filter(arena$id < 54000017 & arena$id >= 54000012)
  } else if (arena.level == 'low'){
    output <- memberList %>% filter(arena$id < 54000012) 
  }
} else if (role == 'leader'){
    if (arena.level == 'all'){
    output <- memberList %>% filter(role %in% c('leader','coLeader'))
  } else if (arena.level == 'high'){
    output <- memberList %>% filter(role %in% c('leader','coLeader')) %>% filter(arena$id >= 54000017)
  } else if (arena.level == 'medium'){
    output <- memberList %>% filter(role %in% c('leader','coLeader')) %>% filter(arena$id < 54000017 & arena$id >= 54000012)
  } else if (arena.level == 'low'){
    output <- memberList %>% filter(role %in% c('leader','coLeader')) %>% filter(arena$id < 54000012) 
  }
} else if (role == 'elder'){
    if (arena.level == 'all'){
    output <- memberList %>% filter(role %in% c('elder'))
  } else if (arena.level == 'high'){
    output <- memberList %>% filter(role %in% c('elder')) %>% filter(arena$id >= 54000017)
  } else if (arena.level == 'medium'){
    output <- memberList %>% filter(role %in% c('elder')) %>% filter(arena$id < 54000017 & arena$id >= 54000012)
  } else if (arena.level == 'low'){
    output <- memberList %>% filter(role %in% c('elder')) %>% filter(arena$id < 54000012) 
  }
} else if (role == 'member'){
    if (arena.level == 'all'){
    output <- memberList %>% filter(role %in% c('member'))
  } else if (arena.level == 'high'){
    output <- memberList %>% filter(role %in% c('member')) %>% filter(arena$id >= 54000017)
  } else if (arena.level == 'medium'){
    output <- memberList %>% filter(role %in% c('member')) %>% filter(arena$id < 54000017 & arena$id >= 54000012)
  } else if (arena.level == 'low'){
    output <- memberList %>% filter(role %in% c('member')) %>% filter(arena$id < 54000012) 
  }
}
  
return(output)
}
```

- `player`

This function enables you to get information in detail or the upcoming
chests of a player based on player tag.

``` r
player = function(playertag, token, infotype = 'personal'){ 
  if (infotype == 'personal'){ # player info
    url = paste0("https://api.clashroyale.com/v1/players/",'%23',substr(playertag, 2, nchar(playertag)))
    headers <- c(`Authorization` = sprintf('Bearer %s', token))
    rawplayerdata <- httr::GET(url = url, httr::add_headers(.headers = headers))
    playerdata <- jsonlite::fromJSON(rawToChar(rawplayerdata$content))
    # return first 19 elements 
    return(playerdata[1:19])
    } else if (infotype == 'chests'){ #chests info
            url = paste0("https://api.clashroyale.com/v1/players/",'%23',substr(playertag, 2, nchar(playertag)),"/upcomingchests")
            headers <- c(`Authorization` = sprintf('Bearer %s', token))
            rawplayerchestsdata <- httr::GET(url = url, httr::add_headers(.headers = headers))
            playerchestsdata <- jsonlite::fromJSON(rawToChar(rawplayerchestsdata$content))
            # return first element 
            return(playerchestsdata[1])
    }
}
```

## Data Exploration

Now we can pull the data from the API and analyse it.

### Clan data

First, let’s try to pull the clan data. Here I will use my clan’s tag
and my API token as an example.

``` r
myclan <- clan(clantag = '#8YVVGR2Y', token = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6ImQ5OTM1ZGQwLTdjNzktNDQ0Yy05MGU0LWE1ZTk2YjQyZWNiOSIsImlhdCI6MTY5NTc0ODU1Nywic3ViIjoiZGV2ZWxvcGVyLzExNDhlMzZkLTU0YjAtOGZlYy1lMjM1LTY0Mzk3NjFmMzRjMiIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyI3NS4xMTAuNzAuNTYiXSwidHlwZSI6ImNsaWVudCJ9XX0._o3WTwKGwrkiNGuhEiAf963JJv2UueE4jgZ4JEClZqqToJ26ZEbi7vUOH2IFbV6yBt2y3ExUhEDQ5h525RAtKA')
```

There are four roles in the clan: leader, co-leader, elder and member.
Since every clan has only one leader, leader and co-leader are merged
together as role of leader.

``` r
# only change 'coLeader' to 'leader', keep others the same.
myclan <- myclan %>% 
  mutate(role = if_else(role == 'coLeader', 'leader', role))
# change the data type to factor and order it.
myclan$role <- factor(myclan$role, levels = c('leader','elder','member'))
```

I would like to see the difference between the roles through multiple
dimensions: the trophies, donations, and online frequency.

Trophies reflect the cards’ level and the skills of a player. More the
trophies, better the player is.

Clash Royale allows players to donate cards to clanmates in the clan.
Players can request every 7 hours for cards of their choosing. They are
allowed to request for any <span style="color: gray">**Common**</span>
or <span style="color: red">**Rare**</span> card they have unlocked. On
Epic Sundays, players are allowed to request
<span style="color: blueviolet">**Epic**</span> cards once. More
donations usually means more contributions the player made to the clan.

To measure the online frequency, I would create a new variable to
measure how many days the player were not seen online. For this
variable, less is better.

- Trophies over roles

Let’s make some statistics summary and plot the box plot.

``` r
# summarize the basic stats info grouping by role
myclan %>% group_by(role) %>% summarize(mean = mean(trophies), sd = sd(trophies), min = min(trophies), Q1 = quantile(trophies, 0.25), median = median(trophies), Q3 = quantile(trophies, 0.75), max = max(trophies))
```

    ## # A tibble: 3 × 8
    ##   role    mean    sd   min    Q1 median    Q3   max
    ##   <fct>  <dbl> <dbl> <int> <dbl>  <dbl> <dbl> <int>
    ## 1 leader 6423. 1204.  4237 5543   6359  7500   8207
    ## 2 elder  5741. 1479.  3359 5090.  6168. 6795.  7667
    ## 3 member 5014. 1631.  2420 3518.  5156. 6246.  7202

``` r
# make box plot
ggplot(myclan, aes(x = role, y = trophies)) +
  geom_boxplot(aes(color=role)) +
  scale_x_discrete("Role") +
  scale_y_continuous("Trophies") +
  labs(title = "Trophies over roles") # lables
```

![](README_files/figure-gfm/trophies%20plot-1.png)<!-- -->

- Donations over roles

``` r
# summarize the basic stats info grouping by role
myclan %>% group_by(role) %>% summarize(mean = mean(donations), sd = sd(donations), min = min(donations), Q1 = quantile(donations, 0.25), median = median(donations), Q3 = quantile(donations, 0.75), max = max(donations))
```

    ## # A tibble: 3 × 8
    ##   role    mean    sd   min    Q1 median    Q3   max
    ##   <fct>  <dbl> <dbl> <int> <dbl>  <dbl> <dbl> <int>
    ## 1 leader 102.  137.      0   0       64 131     456
    ## 2 elder   83.2  99.0     0  13.5     52  88     300
    ## 3 member  41.6  60.6     0   2       19  39.5   211

``` r
# make a jitter plot
ggplot(myclan, aes(x = role, y = donations)) +
  geom_jitter(aes(color=role)) +
  scale_x_discrete("Role") +
  scale_y_continuous("Donations") +
  labs(title = "Donations over roles") # lables
```

![](README_files/figure-gfm/donation%20plot-1.png)<!-- -->

- Online frequency over roles

``` r
# calculate the days between current date and last seen date.
myclan <- myclan %>% mutate(days = Sys.Date()-lastSeen)

# make jitter plot
ggplot(myclan, aes(x = role, y = days)) +
  geom_jitter(aes(color=role)) +
  scale_x_discrete("Role") +
  scale_y_continuous("Days not seen") +
  labs(title = "Online frequency over roles")
```

![](README_files/figure-gfm/days%20plot-1.png)<!-- -->

From these three plots, we can see that “leader” group has higher
trophies indicating that these players have player for a long time and
have better techniques. The “leader” group has a little bit more
donations on average than “elder” group and “member” group, indicating
that this group made more contributions to the clanmates. However, there
are three “leaders” not shown more than 10 days, one of them even over
60 days. It’s time to warn or demote them! Overall, the players in this
clan are very active.

Next, let’s look at the relationship between players’ exp and trophies.
Each time the player makes donation or upgrades the card, he/she will
gain some exp.

``` r
# make point plot
ggplot(myclan, aes(x = expLevel, y = trophies)) +
  geom_point(aes(color = role)) +
  labs(title = 'Exp over Trophies by role') +
  annotate("text", x=30, y=7000, label = paste("Correlation:", round(cor(myclan$expLevel, myclan$trophies), 2))) # add a correlation value
```

![](README_files/figure-gfm/exp%20over%20trophies-1.png)<!-- -->

From the plot we can see that there is positive linear relationship
between trophies and exp level. “leader” group has the highest exp level
and trophies on average following by elder and member.

We can use the heat map to show the overall picture. The purple color is
for larger numbers while the aqua color is for smaller numbers.

``` r
# rename the rows
rownames(myclan) <- myclan$name
# select variables and convert the data frame to matrix
heat <- myclan %>% select(expLevel, trophies, donations, donationsReceived) %>% as.matrix()
# make a heat map
heatmap(heat, 
        scale="column", 
        col = cm.colors(256), # color
        xlab="variable", 
        ylab="player", 
        main="Heat map of players' performance in the clan",
        cexRow = 0.6, # row font size
        cexCol = 1, # column font size
        Rowv = NA, 
        Colv = NA
        )
```

![](README_files/figure-gfm/heat%20map-1.png)<!-- -->

When we consider the arena level and the roles, we can make a
contingency table.

``` r
# create arena level variable
myclan <- myclan %>% mutate(arena_level = case_when(arena$id >= 54000017 ~ 'High',
                                          arena$id < 54000017 & arena$id >= 54000012 ~ 'Medium',
                                          arena$id < 54000012 ~ 'Low'))
# convert to factor type
myclan$arena_level <- factor(myclan$arena_level, c('High','Medium','Low'))

# make two way table
table(myclan$role, myclan$arena_level)
```

    ##         
    ##          High Medium Low
    ##   leader    7     10   0
    ##   elder     3      8   1
    ##   member    3     11   4

From the table we see that the “leader” group has none player in Low
arenas and has the most players in High arenas.

### Player data

Then, we can examine a specific player’s information and check the
upcoming chest that the player will get. From this endpoint, we are able
to collect lots of player’s data that are now shown in clan’s data such
as historical wins, losses, best trophies, challenges and total
donations, etc. I will use my player tag as an example.

First, we read in the data:

``` r
# pull player data
playerinfo <- player(playertag = '#8208VRQL', token = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6ImQ5OTM1ZGQwLTdjNzktNDQ0Yy05MGU0LWE1ZTk2YjQyZWNiOSIsImlhdCI6MTY5NTc0ODU1Nywic3ViIjoiZGV2ZWxvcGVyLzExNDhlMzZkLTU0YjAtOGZlYy1lMjM1LTY0Mzk3NjFmMzRjMiIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyI3NS4xMTAuNzAuNTYiXSwidHlwZSI6ImNsaWVudCJ9XX0._o3WTwKGwrkiNGuhEiAf963JJv2UueE4jgZ4JEClZqqToJ26ZEbi7vUOH2IFbV6yBt2y3ExUhEDQ5h525RAtKA')
```

To make the result more readable, we can transpose the data frame:

``` r
# transpose the tibble
t(as.tibble(playerinfo))
```

    ##                       [,1]        
    ## tag                   "#8208VRQL" 
    ## name                  "Grumpy Lin"
    ## expLevel              "57"        
    ## trophies              "8089"      
    ## bestTrophies          "8124"      
    ## wins                  "9073"      
    ## losses                "4870"      
    ## battleCount           "29468"     
    ## threeCrownWins        "6280"      
    ## challengeCardsWon     "2977"      
    ## challengeMaxWins      "12"        
    ## tournamentCardsWon    "157"       
    ## tournamentBattleCount "455"       
    ## role                  "coLeader"  
    ## donations             "402"       
    ## donationsReceived     "0"         
    ## totalDonations        "246696"    
    ## warDayWins            "174"       
    ## clanCardsCollected    "452291"

similarly, we can pull the upcoming chest info by adding the argument:

``` r
# pull chests data
chests <- player(playertag = '#8208VRQL', token = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6ImQ5OTM1ZGQwLTdjNzktNDQ0Yy05MGU0LWE1ZTk2YjQyZWNiOSIsImlhdCI6MTY5NTc0ODU1Nywic3ViIjoiZGV2ZWxvcGVyLzExNDhlMzZkLTU0YjAtOGZlYy1lMjM1LTY0Mzk3NjFmMzRjMiIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyI3NS4xMTAuNzAuNTYiXSwidHlwZSI6ImNsaWVudCJ9XX0._o3WTwKGwrkiNGuhEiAf963JJv2UueE4jgZ4JEClZqqToJ26ZEbi7vUOH2IFbV6yBt2y3ExUhEDQ5h525RAtKA', infotype = 'chests')

chests
```

    ## $items
    ##    index                   name
    ## 1      0             Gold Crate
    ## 2      1             Gold Crate
    ## 3      2           Golden Chest
    ## 4      3   Plentiful Gold Crate
    ## 5      4           Golden Chest
    ## 6      5           Golden Chest
    ## 7      6           Golden Chest
    ## 8      7          Magical Chest
    ## 9      8           Golden Chest
    ## 10    13            Giant Chest
    ## 11    34 Overflowing Gold Crate
    ## 12    60       Royal Wild Chest
    ## 13   138             Epic Chest
    ## 14   384        Legendary Chest
    ## 15   472   Mega Lightning Chest

### Clan data + Player data: Who is the best in clan?

If you win more and lose less, obviously you would get a higher winning
rate. But, if you are at the lower arena with lower trophies, most
likely you will meet rookie opponents, which makes you much more easily
to win; meanwhile if you are at the higher arena with higher trophies,
you will often meet experienced veteran opponents, even professional
players – and eat a Loss.

We want to put the winning rate and best trophies together to see who is
the best player in a clan: more trophies with higher winning rate.

``` r
# get all tags
clanplayers <- unlist(myclan$tag)
# setup list
playerlist <- NULL

for (i in 1:length(clanplayers)){
# store all players' info into list
  playerlist[[i]] <- player(playertag = clanplayers[i], token = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6ImQ5OTM1ZGQwLTdjNzktNDQ0Yy05MGU0LWE1ZTk2YjQyZWNiOSIsImlhdCI6MTY5NTc0ODU1Nywic3ViIjoiZGV2ZWxvcGVyLzExNDhlMzZkLTU0YjAtOGZlYy1lMjM1LTY0Mzk3NjFmMzRjMiIsInNjb3BlcyI6WyJyb3lhbGUiXSwibGltaXRzIjpbeyJ0aWVyIjoiZGV2ZWxvcGVyL3NpbHZlciIsInR5cGUiOiJ0aHJvdHRsaW5nIn0seyJjaWRycyI6WyI3NS4xMTAuNzAuNTYiXSwidHlwZSI6ImNsaWVudCJ9XX0._o3WTwKGwrkiNGuhEiAf963JJv2UueE4jgZ4JEClZqqToJ26ZEbi7vUOH2IFbV6yBt2y3ExUhEDQ5h525RAtKA')

}
# extract columns that we need
name <- unlist(sapply(playerlist, FUN = `[`, "name"))
wins <- unlist(sapply(playerlist, FUN = `[`, "wins"))
losses <- unlist(sapply(playerlist, FUN = `[`, "losses"))
best_trophies <- unlist(sapply(playerlist, FUN = `[`, "bestTrophies"))
role <- unlist(sapply(playerlist, FUN = `[`, "role"))
# combine together as a data frame
clanmates <- as.data.frame(cbind(name,role,wins,losses,best_trophies,total_donation))
# rename rows
rownames(clanmates) <- clanmates$name
# change data type to numeric
clanmates$wins <- as.numeric(clanmates$wins)
clanmates$losses <- as.numeric(clanmates$losses)
clanmates$best_trophies <- as.numeric(clanmates$best_trophies)
# calculate win rate
clanmates <- clanmates %>% mutate(win_rate = wins/(wins+losses))

# make point plot
ggplot(clanmates, aes(x = best_trophies, y = win_rate)) +
  geom_point(aes(color = role)) +
  labs(title = 'best_trophies over win_rate') + 
  geom_text(label = name, check_overlap=T) # add text label
```

![](README_files/figure-gfm/win%20rate-1.png)<!-- -->

From this plot, we can see that some “member” players have winning rate
higher than 0.65 but all below 6000 trophies.

Only one player has win rate over 0.65 and above 8000 trophies: Yes,
that’s me. :)

We can count the number of players based on winning rate and best
trophies.

``` r
clanmates <- clanmates %>% mutate(
  Winning_rate_group = case_when(win_rate < 0.45 ~ 'bad skill',
                           win_rate >= 0.45 & win_rate < 0.55 ~ 'normal skill',
                           win_rate >= 0.55 & win_rate < 0.65 ~ 'good skill',
                           win_rate >= 0.65 ~ 'excellent skill'), # creating winning rate group based on the range.
  best_trophies_group = case_when(best_trophies < 5000 ~ 'low arena level',
                                  best_trophies >= 5000 & best_trophies < 7000 ~ 'medium arena level',
                                  best_trophies >= 7000 ~ 'high arena level') # creating arena level based on best trophies
)
# change data type and order it.
clanmates$Winning_rate_group <- factor(clanmates$Winning_rate_group, levels = c('excellent skill','good skill','normal skill','bad skill'))
clanmates$best_trophies_group <- factor(clanmates$best_trophies_group, levels = c('high arena level','medium arena level','low arena level'))
# make two way table.
table(clanmates$Winning_rate_group, clanmates$best_trophies_group)
```

    ##                  
    ##                   high arena level medium arena level low arena level
    ##   excellent skill                1                  2               0
    ##   good skill                     3                  7               5
    ##   normal skill                   6                 14               4
    ##   bad skill                      0                  2               3

From the two-way table, we can see that at each arena level, the number
of players decreases when skill level increases. Most of the clanmates
are in median arena level, indicating that my clan is not one of the top
ranked clans. We have a long way to go.

Hope you enjoy my project.

:smiley:
