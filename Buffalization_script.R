## LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(forcats)

## LOAD DATA
data = read.csv("data.csv")

## FILTER OUT MISSING DATA
data_filtered = data %>% 
  filter(Common.Name != "" & Height != 0) %>% 
  na.omit()

## GROUP BY COMMON.NAME AND AGGREGATE
results = data_filtered %>% 
  group_by(Common.Name) %>%
  summarise(avg=mean(Height), max=max(Height), count=n(), 
            pct=n()/length(data_filtered$Common.Name)*100) %>%
  arrange(desc(avg)) %>%
  add_row(Common.Name="Average", avg=mean(data_filtered$Height),
          max=102, count=length(data_filtered$Common.Name), pct=100)

## OUTPUT RESULTS
print(results, n=110)
top_ten = results %>% head(10)

## PROPORTION OF TOTAL TREES
print(paste("Top ten species take a total of: ", sum(top_ten$pct)))

## CALCULATE BOXPLOT MARGINS TO MATCH PROPORTIONS
top_ten$right = cumsum(top_ten$count)
top_ten$left = c(0, top_ten$right[1:9])

margin = 0.5

for (i in 1:10)
{
  top_ten$left[i] = top_ten$left[i] + i * margin
  top_ten$right[i] = top_ten$right[i] + i * margin
}

print(top_ten)


##MAKE A PLOT
colors = c("#344e41", "#405949", "#4c6451", "#596f59", "#657a61",
           "#718469", "#7d8f71", "#899a79", "#96a581", "#a3b18a")

ggplot(top_ten, 
       aes(y=avg,
           fill=fct_inorder(as.factor(paste(Common.Name, " - ", 
                                            round(avg, 2), "ft"))),
           width=pct)) + 
  xlim(0, max(top_ten$right)) +
  scale_fill_manual(values=colors) +
  geom_rect(aes(xmin=left, xmax=right, ymin=0, ymax=avg)) +
  geom_abline(slope=0, intercept=mean(data_filtered$Height),
              color="white", linewidth=1.5) +
  geom_rect(aes(xmin = 0, xmax = max(right), 
                ymin = 0, ymax = mean(data_filtered$Height)), 
            fill="white", alpha=0.1) +
  labs(fill="Tree Species - Mean Height", 
       title="Which Are 10 Tallest Tree Species in Boulder?",
       subtitle="In this plot, 10 tallest tree species from Boulder 
are presented, based on mean height of respective specimen. 
Width of elements on circular chart match the frequency of 
respective species among the top 10. For comparison, 
white circle shows the mean height among all of trees in Boulder.") +
  scale_x_continuous(labels = "", breaks=NULL) +
  scale_y_continuous(limits=c(0, 90), breaks=seq(50, 80, by = 10)) +
  ylab("Average Tree Height (feet)") +
  coord_polar(start=0) +
  annotate(geom="text", x=0, y=0, label=paste("Mean Height: ", 
                round(mean(data_filtered$Height), 1), "ft"))
