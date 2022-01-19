str(ww)
s <-
  ww                          %>% # Pipe df into group_by
  group_by(Year,Source)              %>% # grouping by 'type' column
  summarise(name_count = n())     # calculate the name count for each group
## 'df_summary' now contains the summary data for each 'type'
s
s$Date<-as.Date(with(s, paste(Year, Month,"1", sep="-")), "%Y-%m-%d")

ggplot(s, aes(x=Year, y=name_count,group=Source)) +
  geom_line(aes(color=Source))+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(size=11)
  ) +
  ggtitle("WW") +
  xlab("")

ggplot(data=s, aes(x=Year, y=name_count, group=Source, fill = Source, lablel=Source)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("A barplot of Orca Master") +
  xlab("")



s <-om                          %>% # Pipe df into group_by
  group_by(Year,Month,Source)              %>% # grouping by 'type' column
  summarise(name_count = n())     # calculate the name count for each group
## 'df_summary' now contains the summary data for each 'type'
str(s)

summary(s)

s$Date<-as.Date(with(s, paste(Year, Month,"1", sep="-")), "%Y-%m-%d")

ggplot(s, aes(x=Date, y=name_count,group=Source)) +
  geom_line(aes(color=Source))+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(size=11)
  ) +
  ggtitle("OM") +
  xlab("")

ggplot(data=s, aes(x=Year, y=name_count, group=Source, fill = Source, lablel=Source)) +
  geom_bar(position="stack", stat="identity")+
scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("A barplot of Orca Master") +
  xlab("")



str(bccsn0)
s <-
  bccsn0                          %>% # Pipe df into group_by
  group_by(Year,Source)              %>% # grouping by 'type' column
  summarise(name_count = n())     # calculate the name count for each group
## 'df_summary' now contains the summary data for each 'type'
s


s$Date<-as.Date(with(s, paste(Year, Month,"1", sep="-")), "%Y-%m-%d")

ggplot(s, aes(x=Date, y=name_count,group=Source)) +
  geom_line(aes(color=Source))+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(size=11)
  ) +
  ggtitle("BCCSN0") +
  xlab("")

ggplot(data=s, aes(x=Year, y=name_count, group=Source, fill = Source, lablel=Source)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("A barplot of BCCSN + OM 2002 - 2016") +
  xlab("")



str(bccsn1)
s <-
  bccsn1                          %>% # Pipe df into group_by
  group_by(Year,ObserverType)              %>% # grouping by 'type' column
  summarise(name_count = n())     # calculate the name count for each group
## 'df_summary' now contains the summary data for each 'type'
s


s$Date<-as.Date(with(s, paste(Year, Month,"1", sep="-")), "%Y-%m-%d")

ggplot(s, aes(x=Date, y=name_count,group=ObserverType)) +
  geom_line(aes(color=ObserverType))+
    geom_dl(aes(label = ObserverType), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(size=11)
  ) +
  ggtitle("BCCSN1") +
  xlab("")

ggplot(data=s, aes(x=Year, y=name_count, group=ObserverType, fill = ObserverType, lablel=ObserverType)) +
  geom_bar(position="stack", stat="identity")+
  
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
        plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")






