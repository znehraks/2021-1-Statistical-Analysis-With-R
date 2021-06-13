library(ggplot2)
library(dplyr)
str(mpg)
df = mpg
df = df %>% filter(class=="suv") %>% 
  group_by(manufacturer) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty)) %>% 
  head(5)
df

ggplot(data=df, aes(x=reorder(manufacturer, -mean_cty), 
                    y=mean_cty)) + geom_col()
