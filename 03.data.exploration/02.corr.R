# No correlation between any of the variables I tried other than two derived
# variables used for text mining. Probably a weak starting point for any
# type of modeling. Better to explore topic modeling

library(tidyverse)

nuforc <-
  read_csv("./01.build.dataset/nuforc_reports_usa.csv") %>%
  glimpse()

nuforc %>% 
  mutate(
    shape_num = case_when(
        is.na(shape_bin) ~ 0,
        shape_bin == "lights" ~ 1,
        shape_bin == "disks" ~ 2,
        shape_bin == "sphere" ~ 3,
        shape_bin == "unknowns" ~ 4,
        shape_bin == "triangles" ~ 5,
        shape_bin == "cigars" ~ 6,
        shape_bin == "teardrops" ~ 7,
        shape_bin == "formation" ~ 8,
        shape_bin == "changing" ~ 9,
        shape_bin == "cross" ~ 10,
        TRUE ~ NA
      )
    ) %>%
  select(
    low_information_score,
    shape_num,
    duration_in_minutes,
    day_of_year,
    time_occurred_hour,
    latitude,
    longitude,
    description_length,
    #shape,
    afinn_sentiment_score,
    perc_positive
  )  %>% 
  cor(use = "pairwise.complete.obs")

# low_information_score     shape_num duration_in_minutes  day_of_year time_occurred_hour
# low_information_score           1.000000000  0.0865643076       -0.0063562470  0.009419129        0.084695634
# shape_num                       0.086564308  1.0000000000        0.0002611959  0.004127573       -0.010260430
# duration_in_minutes            -0.006356247  0.0002611959        1.0000000000 -0.003157685       -0.029768865
# day_of_year                     0.009419129  0.0041275726       -0.0031576847  1.000000000        0.022041281
# time_occurred_hour              0.084695634 -0.0102604301       -0.0297688650  0.022041281        1.000000000
# latitude                       -0.019391965 -0.0218914360       -0.0009995531  0.016617713        0.009317679
# longitude                      -0.018417738  0.0018824028        0.0034686568  0.025250440        0.019850126
# description_length              0.166541868  0.0887553968        0.0119724756 -0.006497335        0.008155538
# afinn_sentiment_score           0.036044754 -0.0057059233       -0.0037846167  0.003941359        0.027297854
# perc_positive                  -0.006841927 -0.0002462613       -0.0025158047  0.008745680        0.050609002
# latitude    longitude description_length afinn_sentiment_score perc_positive
# low_information_score -0.0193919647 -0.018417738        0.166541868           0.036044754 -0.0068419267
# shape_num             -0.0218914360  0.001882403        0.088755397          -0.005705923 -0.0002462613
# duration_in_minutes   -0.0009995531  0.003468657        0.011972476          -0.003784617 -0.0025158047
# day_of_year            0.0166177131  0.025250440       -0.006497335           0.003941359  0.0087456801
# time_occurred_hour     0.0093176794  0.019850126        0.008155538           0.027297854  0.0506090018
# latitude               1.0000000000 -0.096972801       -0.005403561           0.002315729  0.0086154207
# longitude             -0.0969728013  1.000000000       -0.030877837          -0.026809551  0.0026050308
# description_length    -0.0054035612 -0.030877837        1.000000000           0.012577764 -0.0645349168
# afinn_sentiment_score  0.0023157289 -0.026809551        0.012577764           1.000000000  0.2183601950
# perc_positive          0.0086154207  0.002605031       -0.064534917           0.218360195  1.0000000000
