library(Rmisc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)



####### Albums and genres
{

gen_col = "#8C0737"

##### Genres

my_theme_gen <- function() {
  theme(
    axis.text.y = element_blank(),   # убираем текст осей
    axis.ticks = element_blank(),  # убираем отметки на осях
    panel.grid  = element_blank(), # убираем сетку на заднем фоне
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(color=gen_col,size=18,family = 'AvantGarde'),
    plot.title = element_text(size = 45,color=gen_col,hjust=0.5,family = "AvantGarde",face="bold"),
    plot.subtitle = element_text(size=24,color="grey",hjust=0.5,family="AvantGarde",face="italic")   #параметры подзаголовка
  )
}


load("~/my_am/data/genre_change.rda")
#genre_change$Genre = str_replace(genre_change$Genre,fixed("/"),"\n")
genre_change$ord = 7:1
genre_change = genre_change %>% ungroup() %>% mutate(Genre = reorder(Genre, -ord)) 


p3 = ggplot() + geom_bar(data=genre_change,aes(y=chg_scr,x=Genre),stat="identity",color=gen_col,fill="white") +
  my_theme_gen() + ggtitle("Genres Growth in 2018") + labs(subtitle="How the number of particular genres changed\nin scrobble stats in comparison to previous years") +
  annotate("text",x=genre_change$Genre,y=15,label=str_c(genre_change$chg_scr,"%"),color=gen_col,family="AvantGarde",size=12)
p3






##### Albums
my_theme <- function() {
  theme(
    axis.text.y = element_blank(),   # убираем текст осей
    axis.ticks = element_blank(),  # убираем отметки на осях
    panel.grid  = element_blank(), # убираем сетку на заднем фоне
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 45,color=gen_col,hjust=0.5,family = "AvantGarde",face="bold"),
    plot.subtitle = element_text(size=24,color="grey",hjust=0.5,family="AvantGarde",face="italic")   #параметры подзаголовка
  )
}

yl = c(0.5,7.5)
xl1 = c(0.9,2.2)
xl2 = c(0.9,3.25)
text_size = 10

load("~/my_am/data/albums.rda")

### all years


albtop7$total_dur = str_extract(as.duration(albtop7$total_dur),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
albtop7$unit = str_extract(albtop7$total_dur," hours| minutes| days")
albtop7$total_dur = ifelse(albtop7$unit == " days",
                           floor(extract_numeric(albtop7$total_dur)) %>% 
                             str_c(.," days ", round(24 * (extract_numeric(albtop7$total_dur) - floor(extract_numeric(albtop7$total_dur))), 0)," hours"),
                           ifelse(albtop7$unit == " hours",
                                  floor(extract_numeric(albtop7$total_dur)) %>% 
                                    str_c(.," hours ",
                                          round(60 * (extract_numeric(albtop7$total_dur) - floor(extract_numeric(albtop7$total_dur))), 0)," min"),
                                  floor(extract_numeric(albtop7$total_dur)) %>% 
                                    str_c(.," minutes ",
                                          round(60 * (extract_numeric(albtop7$total_dur) - floor(extract_numeric(albtop7$total_dur))), 0)," sec")))
albtop7$y_coor = 7:1


p1 = ggplot() + geom_text(data=albtop7,aes(y=y_coor-0.3,x=0.97,label=Artist_name, hjust = 0),family = "AvantGarde", colour = "#6E3145",size=text_size) +
  geom_text(data=albtop7,aes(y=y_coor,x=1,label=Album, hjust = 0),family = "AvantGarde", colour = gen_col,size=text_size) +
  geom_text(data=albtop7,aes(y=y_coor,x=1.9,label=total_dur,hjust = 0),family = "AvantGarde", colour = gen_col,size=text_size) +
  xlim(xl1) + ylim(yl) + my_theme() +ggtitle("Albums total") + labs(subtitle="Album title and how much time did I spend listening to it")




### 18 year

alb18top7$total_dur = str_extract(as.duration(alb18top7$total_dur),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
alb18top7$unit = str_extract(alb18top7$total_dur," hours| minutes| days")
alb18top7$total_dur = ifelse(alb18top7$unit == " days",
                           floor(extract_numeric(alb18top7$total_dur)) %>% 
                             str_c(.," days ", round(24 * (extract_numeric(alb18top7$total_dur) - floor(extract_numeric(alb18top7$total_dur))), 0)," hours"),
                           ifelse(alb18top7$unit == " hours",
                                  floor(extract_numeric(alb18top7$total_dur)) %>% 
                                    str_c(.," hours ",
                                          round(60 * (extract_numeric(alb18top7$total_dur) - floor(extract_numeric(alb18top7$total_dur))), 0)," min"),
                                  floor(extract_numeric(alb18top7$total_dur)) %>% 
                                    str_c(.," minutes ",
                                          round(60 * (extract_numeric(alb18top7$total_dur) - floor(extract_numeric(alb18top7$total_dur))), 0)," sec")))
alb18top7$y_coor = 7:1


p2 = ggplot() + geom_text(data=alb18top7,aes(y=y_coor-0.3,x=0.97,label=Artist_name, hjust = 0),family = "AvantGarde", colour = "#6E3145",size=text_size) +
  geom_text(data=alb18top7,aes(y=y_coor,x=1,label=Album, hjust = 0),family = "AvantGarde", colour = gen_col,size=text_size) +
  geom_text(data=alb18top7,aes(y=y_coor,x=2.7,label=total_dur,hjust = 0),family = "AvantGarde", colour = gen_col,size=text_size) +
  xlim(xl2) + ylim(yl) + my_theme() + ggtitle("Albums added in 2018") + labs(subtitle="Album title and how much time did I spend listening to it")

p2

multiplot(p1,p2,p3,cols=3)


}


####### Artists
{

load("~/my_am/data/artists.rda")

arttop7$total_dur = str_extract(as.duration(arttop7$total_dur),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
arttop7$unit = str_extract(arttop7$total_dur," hours| minutes| days")
arttop7$total_dur = ifelse(arttop7$unit == " days",
                       floor(extract_numeric(arttop7$total_dur)) %>% 
                         str_c(.," days ", round(24 * (extract_numeric(arttop7$total_dur) - floor(extract_numeric(arttop7$total_dur))), 0)," hours"),
                       ifelse(arttop7$unit == " hours",
                       floor(extract_numeric(arttop7$total_dur)) %>% 
                         str_c(.," hours ",
                               round(60 * (extract_numeric(arttop7$total_dur) - floor(extract_numeric(arttop7$total_dur))), 0)," min"),
                       floor(extract_numeric(arttop7$total_dur)) %>% 
                         str_c(.," minutes ",
                               round(60 * (extract_numeric(arttop7$total_dur) - floor(extract_numeric(arttop7$total_dur))), 0)," sec")))
arttop7$y_coor = 7:1




art18top7$total_dur = str_extract(as.duration(art18top7$total_dur),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
art18top7$unit = str_extract(art18top7$total_dur," hours| minutes| days")
art18top7$total_dur = ifelse(art18top7$unit == " days",
                           floor(extract_numeric(art18top7$total_dur)) %>% 
                             str_c(.," days ", round(24 * (extract_numeric(art18top7$total_dur) - floor(extract_numeric(art18top7$total_dur))), 0)," hours"),
                           ifelse(art18top7$unit == " hours",
                                  floor(extract_numeric(art18top7$total_dur)) %>% 
                                    str_c(.," hours ",
                                          round(60 * (extract_numeric(art18top7$total_dur) - floor(extract_numeric(art18top7$total_dur))), 0)," min"),
                                  floor(extract_numeric(art18top7$total_dur)) %>% 
                                    str_c(.," minutes ",
                                          round(60 * (extract_numeric(art18top7$total_dur) - floor(extract_numeric(art18top7$total_dur))), 0)," sec")))
art18top7$y_coor = 7:1



art18top7n$total_dur = str_extract(as.duration(art18top7n$total_dur),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
art18top7n$unit = str_extract(art18top7n$total_dur," hours| minutes| days")
art18top7n$total_dur = ifelse(art18top7n$unit == " days",
                             floor(extract_numeric(art18top7n$total_dur)) %>% 
                               str_c(.," days ", round(24 * (extract_numeric(art18top7n$total_dur) - floor(extract_numeric(art18top7n$total_dur))), 0)," hours"),
                             ifelse(art18top7n$unit == " hours",
                                    floor(extract_numeric(art18top7n$total_dur)) %>% 
                                      str_c(.," hours ",
                                            round(60 * (extract_numeric(art18top7n$total_dur) - floor(extract_numeric(art18top7n$total_dur))), 0)," min"),
                                    floor(extract_numeric(art18top7n$total_dur)) %>% 
                                      str_c(.," minutes ",
                                            round(60 * (extract_numeric(art18top7n$total_dur) - floor(extract_numeric(art18top7n$total_dur))), 0)," sec")))
art18top7n$y_coor = 7:1



art_col = "#075233"

##### Total

my_theme_a <- function() {
  theme(
    axis.text.y = element_blank(),   # убираем текст осей
    axis.ticks = element_blank(),  # убираем отметки на осях
    panel.grid  = element_blank(), # убираем сетку на заднем фоне
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 45,color=art_col,hjust=0.5,family = "AvantGarde",face="bold"),
    plot.subtitle = element_text(size=24,color="grey",hjust=0.5,family="AvantGarde",face="italic")   #параметры подзаголовка
    
  )
}

yl = c(0.5,7.5)
xl1 = c(0.9,2.2)
xl2 = c(0.9,3.25)
text_size = 10


p1 = ggplot() + geom_text(data=arttop7,aes(y=y_coor,x=1,label=Artist_name, hjust = 0),family = "AvantGarde", colour = art_col,size=text_size) +
  geom_text(data=arttop7,aes(y=y_coor,x=2.7,label=total_dur,hjust = 0),family = "AvantGarde", colour = art_col,size=text_size) +
  xlim(xl2) + ylim(yl) + my_theme_a() + ggtitle("Artists total") + labs(subtitle="Artist name and how much time did I spend listening to him/her")


##### 18 Year

p2 = ggplot() + geom_text(data=art18top7,aes(y=y_coor,x=1,label=Artist_name, hjust = 0),family = "AvantGarde", colour = art_col,size=text_size) +
  geom_text(data=art18top7,aes(y=y_coor,x=2.7,label=total_dur,hjust = 0),family = "AvantGarde", colour = art_col,size=text_size) +
  xlim(xl2) + ylim(yl) + my_theme_a() + ggtitle("Artists based on songs added in '18") + labs(subtitle="Artist name and how much time did I spend listening to him/her")



##### Freshman

p3 = ggplot() + geom_text(data=art18top7n,aes(y=y_coor,x=1,label=Artist_name, hjust = 0),family = "AvantGarde", colour = art_col,size=text_size) +
  geom_text(data=art18top7n,aes(y=y_coor,x=2.7,label=total_dur,hjust = 0),family = "AvantGarde", colour = art_col,size=text_size) +
  xlim(xl2) + ylim(yl) + my_theme_a() + ggtitle("First appearance in '18") + labs(subtitle="Artist name and how much time did I spend listening to him/her")


multiplot(p1,p2,p3,cols=3)

}





####### Songs


{

load("~/my_am/data/songs.rda")


songs18top7$total_dur = str_extract(as.duration(songs18top7$total_dur),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
songs18top7$unit = str_extract(songs18top7$total_dur," hours| minutes| days")
songs18top7$total_dur = ifelse(songs18top7$unit == " days",
                           floor(extract_numeric(songs18top7$total_dur)) %>% 
                             str_c(.," days ", round(24 * (extract_numeric(songs18top7$total_dur) - floor(extract_numeric(songs18top7$total_dur))), 0)," hours"),
                           ifelse(songs18top7$unit == " hours",
                                  floor(extract_numeric(songs18top7$total_dur)) %>% 
                                    str_c(.," hours ",
                                          round(60 * (extract_numeric(songs18top7$total_dur) - floor(extract_numeric(songs18top7$total_dur))), 0)," min"),
                                  floor(extract_numeric(songs18top7$total_dur)) %>% 
                                    str_c(.," minutes ",
                                          round(60 * (extract_numeric(songs18top7$total_dur) - floor(extract_numeric(songs18top7$total_dur))), 0)," sec")))
songs18top7$y_coor = 7:1


son_col = "#173669"  

my_theme_s <- function() {
  theme(
    axis.text.y = element_blank(),   # убираем текст осей
    axis.ticks = element_blank(),  # убираем отметки на осях
    panel.grid  = element_blank(), # убираем сетку на заднем фоне
    axis.title = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 45,color=son_col,hjust=0.5,family = "AvantGarde",face="bold"),
    plot.subtitle = element_text(size=24,color="grey",hjust=0.5,family="AvantGarde",face="italic")   #параметры подзаголовка
    )
}

yl = c(0.5,7.5)
xl1 = c(0.9,2.2)
xl2 = c(0.9,3.25)
text_size = 10



p2 = ggplot() + geom_text(data=songs18top7,aes(y=y_coor-0.3,x=0.97,label=Artist, hjust = 0),family = "AvantGarde", colour = "#343669",size=text_size) +
  geom_text(data=songs18top7,aes(y=y_coor,x=1,label=Title, hjust = 0),family = "AvantGarde", colour = son_col,size=text_size) +
  geom_text(data=songs18top7,aes(y=y_coor,x=2.7,label=total_dur,hjust = 0),family = "AvantGarde", colour = son_col,size=text_size) +
  xlim(xl2) + ylim(yl) + my_theme_s() + ggtitle("Top songs added in '18") + ggtitle("Songs added per year") + labs(subtitle="Song name and how much time did I spend listening to it")

p2
}



####### General Stats

load("~/my_am/data/gen_stats.rda")


total_duration = str_extract(as.duration(total_duration),regex("~.*")) %>%
  str_replace_all(regex("~|\\)"),"")
total_duration = floor(extract_numeric(total_duration)) %>% 
                                  str_c(.," weeks ", round(7 * (extract_numeric(total_duration) - floor(extract_numeric(total_duration))), 0)," days")

# total_duration = ifelse(str_extract(total_duration," weeks| hours| minutes| days") == " weeks",
#                                floor(extract_numeric(total_duration)) %>% 
#                                  str_c(.," days ", round(24 * (extract_numeric(total_duration) - floor(extract_numeric(total_duration))), 0)," hours"),
#                                ifelse(total_duration == " hours",
#                                       floor(extract_numeric(total_duration)) %>% 
#                                         str_c(.," hours ",
#                                               round(60 * (extract_numeric(total_duration) - floor(extract_numeric(total_duration))), 0)," min"),
#                                       floor(extract_numeric(total_duration)) %>% 
#                                         str_c(.," minutes ",
#                                               round(60 * (extract_numeric(total_duration) - floor(extract_numeric(total_duration))), 0)," sec")))

p1 = ggplot() + geom_rect(aes(xmin=-1,ymin=-1,xmax=1,ymax=1), fill=son_col) + coord_polar() +
        annotate("text",x=-1,y=-0.5,label="Time spent",family = "AvantGarde", colour = "white",size=text_size*2.1,fontface='bold') +
        annotate("text",x=0,y=-1,label="in Apple Music",family = "AvantGarde", colour = "white",size=text_size*2.1,fontface='bold') +
        annotate("text",x=0,y=-0.5,label=total_duration,family = "AvantGarde", colour = "white",size=text_size*1.7) +      
        my_theme_s()
p1

ncols = nrow(songs_added)
x_points = (1 / (ncols-1))
x_points = x_points * 0:(ncols-1)
x_shift = 0.3
x_bord = c(min(x_points)-x_shift,max(x_points)+x_shift)

p3 = ggplot() + geom_text(data=songs_added,aes(y=0.66,x=x_points,label=Added_Date_y,hjust=0.5),family = "AvantGarde", colour = son_col,size=text_size*2,fontface='bold') +
           geom_text(data=songs_added,aes(y=0.33,x=x_points,label=songs,hjust=0.5),family = "AvantGarde", colour = "#343669",size=text_size*2) +
           xlim(x_bord) + ylim(-0.2,1.2) + my_theme_s() + ggtitle("Songs added per year") + labs(subtitle="The number of the songs added during each year")

p3

multiplot(p1,p2,p3,cols=3)
