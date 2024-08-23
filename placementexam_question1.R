library(tidyverse)
library(rvest)
library(ggplot2)

# scrape data
wikipedia = read_html("https://en.wikipedia.org/wiki/List_of_natural_disasters_by_death_toll")

df20 = html_table(wikipedia)[[3]] %>% 
  janitor::clean_names()
df21 = html_table(wikipedia)[[4]] %>% 
  janitor::clean_names()

# converting columns

df20 = df20 %>% 
  mutate(
    death_toll = str_remove_all(death_toll, ",")
  ) %>% 
  mutate(
    death_toll = str_remove(death_toll, "[:symbol:]")
  ) %>% 
  mutate(
    death_toll = str_remove(death_toll, "([:punct:])([:digit:])([:digit:])([:punct:])")
  )

for (i in 1:101) {
  if (str_detect(df20$death_toll[i], "[:punct:]")) {
    separate = str_split(df20$death_toll[i], "[:punct:]")[[1]]
    low = as.integer(separate[1])
    high = as.integer(separate[2])
    df20$death_toll[i] = mean(c(low, high)) 
  }
}

df21 = df21 %>% 
  mutate(
    death_toll = str_remove_all(death_toll, ",")
  ) %>% 
  mutate(
    death_toll = str_remove(death_toll, "[:symbol:]")
  ) %>% 
  mutate(
    death_toll = str_remove(death_toll, "([:punct:])([:digit:])([:digit:])([:punct:])")
  ) 

for (i in 1:24) {
  if (str_detect(df21$death_toll[i], "[:punct:]")) {
    separate = str_split(df21$death_toll[i], "[:punct:]")[[1]]
    low = as.integer(separate[1])
    high = as.integer(separate[2])
    df21$death_toll[i] = mean(c(low, high)) 
  }
}

# merge data sets

df = full_join(df20, df21) %>% 
  mutate(
    death_toll = as.integer(death_toll)
  )

# graph

df_plot = ggplot(df) + geom_point(mapping = aes(x = year, y = death_toll, color = type)) + 
  ylab("log10 of death toll") + scale_y_log10() + ggtitle("Death toll of worst natural disasters (1900-present)")

pdf("death_toll_plot.pdf")
print(df_plot)
dev.off()
