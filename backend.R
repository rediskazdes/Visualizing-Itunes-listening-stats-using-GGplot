library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)



df = read.csv("mam.csv")
df$Scrobles[is.na(df$Scrobles)] = 1
df$Added_Date = dmy_hm(df$Added_Date)
df$Added_Date = floor_date(df$Added_Date,unit="year")
df$Added_Date_y = year(df$Added_Date)

song_summ18 = df %>% filter(Year==2018)
song_summ18$Duration = ms(song_summ18$Duration) %>% as.duration()
song_summ18$total_dur = song_summ18$Duration * song_summ18$Scrobles
song_summ18 = song_summ18 %>%  select(Title,Artist,total_dur,Year)
songs18top7 = song_summ18 %>% arrange(-total_dur) %>% top_n(7,total_dur)

save(songs18top7,file="data/songs.rda")


#to change
# Artists
# Watsky & Invisible Inc.
# Loc-Dog
# Original Broadway Cast of Hamilton
df$Artist = str_replace(df$Artist,regex("Watsky & Invisible Inc."),"Watsky")
df$Artist = ifelse(df$Album== "Hamilton (Original Broadway Cast Recording)","Original Broadway Cast of Hamilton",as.character(df$Artist))
df$Album = ifelse(df$Album== "Jumpsuit / Nico And The Niners - Single","Trench",as.character(df$Album))

df$Artist_name = df$Artist
df$Artist = tolower(df$Artist)

# жанры
# View(table(df$Genre))

df$Genre = str_replace(df$Genre,regex("Хип-хоп/рэп|Hip-Hop/Rap|Хип-хоп|Рэп|Hip Hop/Rap|UK Hip Hop|Хардкор-рэп"),"Hip-Hop")
df$Genre = str_replace(df$Genre,regex("Альтернативная музыка|Альтернативный рэп|Alternative Rap"),"Alternative")
df$Genre = str_replace(df$Genre,regex("Электронная музыка|Электроника"),"Electronic")
df$Genre = str_replace(df$Genre,regex("Поп-музыка"),"Pop")
df$Genre = str_replace(df$Genre,regex("Рок|Folk-Rock|Indie Rock|Фолк-рок"),"Rock")
df$Genre = str_replace(df$Genre,regex("Саундтреки"),"Soundtrack")
df$Genre = str_replace(df$Genre,regex("Джаз|Blues"),"Jazz")
df$Genre = str_replace(df$Genre,regex("R&B и соул"),"R&B/Soul")
df$Genre = str_replace(df$Genre,regex("Танцевальная музыка"),"Dance")
df$Genre = str_replace(df$Genre,regex("Христианская музыка и госпел"),"Christian & Gospel")
df$Genre = str_replace(df$Genre,regex("Классическая музыка"),"Classical")
df$Genre = str_replace(df$Genre,regex("Тин-поп"),"Teen Pop")
df$Genre = str_replace(df$Genre,regex("Авторы-исполнители"),"Singer/Songwriter")
df$Genre = str_replace(df$Genre,regex("Регги"),"Reggae")

df$Duration = ms(df$Duration) %>% as.duration()
df$total_dur = df$Duration * df$Scrobles
art_names = select(df,Artist,Artist_name)
art_names = art_names[!duplicated(art_names$Artist),]

### Артисты
## total
art_summ = df %>% group_by(Artist) %>% dplyr::summarise(total_dur=sum(total_dur))
arttop7 = art_summ %>% ungroup() %>% arrange(-total_dur) %>% top_n(7,total_dur)
arttop7 = inner_join(arttop7,art_names)

## 18 year
art_summ18 = df %>% group_by(Artist,Added_Date_y) %>% dplyr::summarise(total_dur=sum(total_dur)) %>%
  filter(Added_Date_y == 2018)
art18top7 = art_summ18 %>% ungroup() %>% arrange(-total_dur) %>% top_n(7,total_dur)
art18top7 = inner_join(art18top7,art_names)
art18top7 = select(art18top7,-Added_Date_y)

## newbies
nbs = df %>% group_by(Artist) %>% dplyr::summarise(min_add_y = min(Added_Date))
nbs$min_add_y = year(nbs$min_add_y)
nbs = filter(nbs,min_add_y == 2018)
#df1 = dplyr::filter(df, Artist %in% nbs$Artist)
n_art_summ = df %>% group_by(Artist) %>% dplyr::summarise(total_dur=sum(total_dur)) %>%
  filter(Artist %in% nbs$Artist)
art18top7n = n_art_summ %>% ungroup() %>% arrange(-total_dur) %>% top_n(7,total_dur)
art18top7n = inner_join(art18top7n,art_names)
#art18top7n = select(art18top7n,-Added_Date_y)

save(arttop7,art18top7,art18top7n,file="data/artists.rda")


### Жанры

# change
gen_chg = df %>% group_by(Genre,Added_Date_y) %>% dplyr::summarise(total=sum(Scrobles),songs_n = n())
gen_chg18 = filter(gen_chg,Added_Date_y == 2018)
gen_chg1617 = filter(gen_chg,Added_Date_y < 2018)
gen_chg1617 = gen_chg1617 %>% group_by(Genre) %>% dplyr::summarise(total=sum(total),songs_n=sum(songs_n))

gen_chg18 = select(gen_chg18,-2)
names(gen_chg18) = c("Genre","total18","songs_n18")

gen_chg = full_join(gen_chg1617,gen_chg18)
gen_chg[!!is.na(gen_chg)] = 0
gen_chg = filter(gen_chg,songs_n18 > 9)

gen_chg$chg_scr = gen_chg$total18 / (gen_chg$total + gen_chg$total18)
df3 = gen_chg %>% arrange(-chg_scr) %>% top_n(7,chg_scr) %>% mutate(chg_scr=round(chg_scr*100,0))
df3$cat = "Change in '18 year"

genre_change = df3
save(genre_change,file="data/genre_change.rda")



## Total
alb_summ = df %>% group_by(Artist,Album) %>% dplyr::summarise(total_dur = sum(total_dur))
albtop7 = alb_summ %>% ungroup() %>% arrange(-total_dur) %>% top_n(7,total_dur)
albtop7 = inner_join(albtop7,art_names)


## 18 year
alb_summ18 = df %>% group_by(Artist, Album, Added_Date_y) %>% dplyr::summarise(total_dur = sum(total_dur)) %>%
  filter(Added_Date_y == 2018)
alb18top7 = alb_summ18 %>% ungroup() %>% arrange(-total_dur) %>% top_n(7,total_dur)
alb18top7 = inner_join(alb18top7,art_names)
alb18top7 = select(alb18top7,-Added_Date_y)


save(albtop7,alb18top7,file="data/albums.rda")



### General stats

df$time_spent = df$Duration * df$Scrobles
songs_added = df %>% group_by(Added_Date_y) %>% dplyr::summarise(songs=n())
total_duration = as.duration(sum(df$time_spent))

save(songs_added,total_duration,file="data/gen_stats.rda")















### notes

df %>% group_by(Added_Date_y) %>% summarise(dur = as.duration(mean(as.numeric(Duration))))

bep = filter(select(df,-2), Artist == "The Black Eyed Peas")

hist(bep$Scrobles)



df$Duration = ms(df$Duration)
as.duration(mean(as.numeric(df$Duration)))
