# SocialEvolution_RMsc_CommunicationScience

```{r libraries}
library("tidyverse")
library("lubridate")
library("plotly")
library("ggrepel")
```
Read CSV's
```{r csv}
read_csv("MusicGenrePreference.csv")
flusymptoms <- read_csv("FluSymptoms.csv")
```
##Music Genre Preference
```{r tidying_musicgenrepreference}

#Scope of data: The musicgenrepreference tibble contains user.id, date, and several music genre columns. The user.id refers to the respondent´s id. The date refers to the time of measurement. The several music genre columns indicate the degree of interest the respondent has in the respective music genre, measured on a scale from 1 (slight Interest) to 3 (high interest).

#Original primary key: Originally the primary key was user.id and date.

#-------------------------------------------------------------------------------

musicgenrepreference %>% 
  count(user.id, date) %>% 
  filter(n > 1)

#Originally user.id and date is the primary key.

sum(is.na(musicgenrepreference))

#There are quite a lot of missing values, it is therefore more logical to make transform columns into rows, so that missing values can be removed.

musicgenrepreference_tidy <- musicgenrepreference %>%
  #The separate genre column names are used as labels for the new music.genre column, the values are transferred to the music.interest column, the missing values are removed.
  pivot_longer(cols ="indie / alternative rock":"other", names_to = "music.genre", values_to = "music.interest",  values_drop_na = TRUE) %>%
  #music.interest is split into a column for the code, and one for the label.
  separate(music.interest, into = c("music.interest.code", "music.interest.label")
           , sep = " ", extra = "merge")

head(musicgenrepreference_tidy)

musicgenrepreference_tidy %>% 
  count(user.id, date, music.genre) %>% 
  filter(n > 1)

#-------------------------------------------------------------------------------

#Primary key after tidying: The new primary key is user.id, date and music.genre. Each case is thus a user's interest in a genre at a particular moment in time.

  #musicgenrepreference & subjects:

#user.id acts as a foreign key in the musicgenrepreference table, given that it is the primary key of the (tidied) subjects table. 

#This linkage would be a one-to-many. Namely every user occurs only once in the subjects table, while a user occurs multiple times in the musicgenrepreference table.

#5 cases (students) in the subjects table are not present in the musicgenrepreference table. These seem to miss at random, and therefore do not seem to obscure the quality of the data.

```
##Flu Symptoms
```{r tidying_flusymptoms}

#Scope of the data: The flusymptoms table contains user_id, time and several columns containing the presence of a specific flu symptom. user_id refers to the respondent´s id. time refers to the time of measurement. The multiple flu symptom columns indicate if the respective fly symptom occurred yes(1) or no (0).

#Original primary key: Originally the primary key was user_id and time.

#-------------------------------------------------------------------------------

flusymptoms %>% 
  distinct() %>%
  count(user_id, time) %>% 
  filter(n > 1)

#Originally the primary key was user_id and time.

flusymptoms_tidy <- flusymptoms %>%
  #The data table contains duplicate rows that need to be removed.
  distinct() %>%
  #There are two instances of cases in which the values contradict each other: at the same time for the same user, different values occur. These are removed. In addition there also seems to be a wrong date.
  filter(!(user_id == 4 & time == ymd_hms("2009-01-10 21:39:00")) & 
          !(user_id == 70 & time == ymd_hms("2009-01-13 14:09:00")) &
           !(time == ymd_hms("2003-01-01 11:00:00"))) %>%
  #The labels of the different columns of symptoms are transformed to rows of a symptoms column. The values are put in a values column, missing values are removed. Although not necessary, this is done since we will (mostly) use symptoms as a grouping variable.
  pivot_longer(cols ="sore.throat.cough":"open.stressed", names_to = "symptoms", values_to = "values",  values_drop_na = TRUE) %>%
  mutate(symptoms=recode(symptoms,
                         "sad.depressed" = "sad / depressed",
                         "nausea.vomiting.diarrhea" = "nausea / vomiting / diarrhea",
                         "runnynose.congestion.sneezing" = "runnynose / congestion / sneezing",
                         "open.stressed" = "stressed",
                         "sore.throat.cough" = "sore throat / cough"))

head(flusymptoms_tidy)

flusymptoms_tidy %>% 
  count(user_id, time, symptoms) %>% 
  filter(n > 1)

#Primary key is user_id, time and symptoms.

#-------------------------------------------------------------------------------

#Primary key after tidying: The primary key after tidying is user_id, time and symptoms. Each case thus represents if a respondent has a symptom at a particular moment in time yes or no.

  #flusymptoms & subjects:

#user.id acts as a foreign key in the flusymptoms table, given that it is the primary key of the (tidied) subjects table. 

#This linkage would be a one-to-many. Namely every user occurs only once in the subjects table, while a user occurs multiple times in the flusymptoms table.

#19 cases (students) in the subjects table are not present in the flusymptoms table. Although no clear pattern can be found as to why they are missing, it does almost constitute 1/4th of all cases in the subjects table.

```
## Visualisation
```{r visualisation_M0EZz, include = TRUE}

# Final_Plot 
final_plot <- favorite_genre_sad_or_stress %>%
  ggplot() +
  #By reordering 'music.genre' with 'symptom.score', the number of preference ratings, where the less represented finding 'jazz' is portrayed left and the most represented finding 'techno / lounge / electronic.' 
  geom_bar(mapping = aes(x = reorder(music.genre, symptom.score, FUN=sum), y = symptom.score, fill = symptoms),
           position = "dodge", 
           stat = "identity") +
  #The colors are used to visually distinguish the bars of stressed and depressed
          scale_fill_manual(values = c("darkorange1", "forestgreen")) +
  #The 'theme_classic' is chosen to generate a plot that is easy to be understood by the general reader
          theme_classic(base_size = 12, base_family = "Arial") + 
  #The labels are used to state the x and y axis and to display the original research question as a subtitle         
          labs(subtitle = "To what extent does depressed or stressed symptoms correlate to the particular music genre among the participants?",
               x = "music genre",
               y = "symptom score") +
  #The legend is placed at the right and states the different symptoms that are measured: 'stressed' and 'depressed'
          theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold")) +
          ggtitle("Music Genre Preference based on Stress and Depressed Symptoms") 

#Final_Plot
#For an interactive version that allows you to retrieve the information of each specific column, use the ggplotly() function below:
ggplotly(final_plot) %>%
  layout(showlegend = TRUE,
         xaxis = list(title = "music genre"),
         yaxis = list(title = "symptom score"))

```
