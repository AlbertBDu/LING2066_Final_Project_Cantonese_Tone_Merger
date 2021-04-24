#LING2066 2021 Spring Final Project
#Albert Baichen Du
#Statistics for Tone Merger

library(tidyverse)

#load overall result from csv file
read_csv("results_v1.csv", col_names = FALSE) -> results

#load selected tokens from meta csv
read_csv("meta_v2.csv", col_names = TRUE) -> meta

#change the header of each file
colnames(results) <- c("speaker", "tone", "environment", "F0_00", "F0_05", "F0_10", "F0_15", "F0_20", "F0_25", "F0_30", "F0_35", "F0_40", "F0_45", "F0_50", "F0_55", "F0_60", "F0_65", "F0_70", "F0_75", "F0_80", "F0_85", "F0_90", "F0_95", "F0_100", "Tone.st", "distance" )
colnames(meta) <- c("speaker", "number", "number_by_speaker", "token", "token_tone", "gender", "following_token", "following_tone", "environment", "age", "age.range")

#join two sheets together
left_join(results, meta, by = c("environment", "speaker")) -> merged_results

#delete first and last two points to avoid perturbations
cols.dont.want <- c ("F0_00", "F0_05", "F0_95", "F0_100")
merged_results <- merged_results[, ! names(merged_results) %in% cols.dont.want, drop = F]

#set all pitch columns as numeric (better way to do this?)
merged_results$F0_10 <- as.numeric(merged_results$F0_10)
merged_results$F0_15 <- as.numeric(merged_results$F0_15)
merged_results$F0_20 <- as.numeric(merged_results$F0_20)
merged_results$F0_25 <- as.numeric(merged_results$F0_25)
merged_results$F0_30 <- as.numeric(merged_results$F0_30)
merged_results$F0_35 <- as.numeric(merged_results$F0_35)
merged_results$F0_40 <- as.numeric(merged_results$F0_40)
merged_results$F0_45 <- as.numeric(merged_results$F0_45)
merged_results$F0_50 <- as.numeric(merged_results$F0_50)
merged_results$F0_55 <- as.numeric(merged_results$F0_55)
merged_results$F0_60 <- as.numeric(merged_results$F0_60)
merged_results$F0_65 <- as.numeric(merged_results$F0_65)
merged_results$F0_70 <- as.numeric(merged_results$F0_70)
merged_results$F0_75 <- as.numeric(merged_results$F0_75)
merged_results$F0_80 <- as.numeric(merged_results$F0_80)
merged_results$F0_85 <- as.numeric(merged_results$F0_85)
merged_results$F0_90 <- as.numeric(merged_results$F0_90)


#read normalization data
normalization <- read_csv("normalization.csv")

#mmerge calculated Tone3 figures into the sheet
left_join(merged_results, normalization, by = "speaker") -> merged_results

#make columns rows
merged_results %>%
  pivot_longer(cols = starts_with("F0", ignore.case = FALSE), names_to = c("F0", "timepoint"), names_sep="_", names_repair="unique") %>%
  mutate(timepoint = as.numeric(timepoint)) -> merged_results_longer

#normalization by speaker
merged_results_longer$Tone.st = ((12 * log(merged_results_longer$value / merged_results_longer$mean_50)) / log(2))

#plot tone contour (change settings here to see different demonsions)
merged_results_longer %>%
  group_by(speaker, timepoint, tone, gender, age.range) %>%
  ggplot(aes(x = timepoint, y = Tone.st, color = tone, group = tone)) + 
  geom_smooth(method="loess") +
  facet_wrap(age.range ~ .)

#calculate the average tone by following tone, tone, and speaker
merged_results_longer %>%
  filter(timepoint == "85") %>%
  group_by(speaker, tone, following_tone) %>%
  summarise(tone.st_by_following = mean(Tone.st, na.rm = TRUE)) -> tone_by_following

filter(merged_results_longer, timepoint == "85") ->tone.st_85

left_join(tone.st_85, tone_by_following, by = c("speaker", "following_tone", "tone")) -> tone.st_85

#calculate average tone without grouped by following tone
tone.st_85 %>%
  group_by(speaker, tone)%>%
  summarise(average.tone = mean(Tone.st, na.rm = TRUE)) %>%
  left_join(tone.st_85, by = c("speaker", "tone")) ->tone.st_85

#output dataframe to csv file
write.csv(tone.st_85,"/Users/albertdu/Desktop/tone.st_85.csv", row.names = FALSE)

#select 
merged_results_longer %>%
  filter(gender == "male") %>%
  ggplot(aes(x = gender, y = Tone.st, fill = tone)) + geom_boxplot() + facet_grid( ~ timepoint)

  

