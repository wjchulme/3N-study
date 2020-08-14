

library('viridis')


# function to format percentages to 1dp
pct <- scales::label_percent(accuracy=0.1, scale=100)

stack_theme <- 
  theme_bw(base_size = 12) + 
  theme(
    panel.border = element_blank(),# axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(colour="grey"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    
    legend.position = "bottom", 
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face= "italic")
  )

hist_theme <- 
  theme_bw(base_size = 12) + 
  theme(
    panel.border = element_blank(), axis.line.x = element_line(colour = "black"),
    panel.grid = element_blank(),
    #panel.grid.major.x = element_line(colour="grey"),
    panel.grid.major.y = element_line(colour="grey"),
    strip.background = element_blank(),
    
    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0, face= "italic")
  )



## correlation plot #####

data_cors <- survey %>% 
  select(starts_with("Q")) %>% 
  cor(use="pairwise.complete.obs") %>%
  as.data.frame.table(responseName="cor")



plot_cors <- 
  data_cors %>%
  filter(substr(Var1,2,3)<=substr(Var2,2,3)) %>%
  ggplot()+
  geom_tile(aes(x=Var1, y=fct_rev(Var2), fill=cor))+
  geom_text(aes(x=Var1, y=fct_rev(Var2), label=scales::label_number(accuracy=0.01)(cor)),size=3)+
  scale_fill_viridis(option="cividis", limits=c(-1,1)) +
  labs(x=NULL, y=NULL)+
  theme_bw()+
theme(
  legend.justification = c(1, 0),
  legend.position = c(0.9, 0.7),
  legend.direction = "horizontal",
  panel.grid = element_blank()
)



## stack plot #####






## time-series ##

plot_each <-
survey %>%
  mutate(
    ptid=scales::label_number(accuracy=1, prefix="p", trim=FALSE)(ptid)
  ) %>%
ggplot()+
  geom_line(aes(dt, nfr_score, group=ptid), alpha=0.5)+
  geom_point(aes(dt, nfr_score, group=ptid, colour=phase), alpha=0.5)+
  facet_wrap(facets=vars(as.factor(ptid)))+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )


## before-after ##

plot_scatter <-
  survey_avg_wide %>%
  ggplot() +
  naniar::geom_miss_point(aes(p2_during, p3_post), size=2, alpha=0.3)+
  geom_abline(aes(intercept=0, slope=1), linetype='dashed')+
  labs(x="During", y="Post")+
  theme_bw()+
  theme(legend.justification=c(0,1), legend.position = c(0.01, 0.99), legend.title = element_blank())



## histogram ##

plot_hist <-
  survey_avg_long %>%
  filter(phase!="p1_pre") %>%
  ggplot() +
  geom_bar(aes(x=nfr_score, group=phase), colour="white", stat="count", width=0.98)+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks=seq(0,50,2))+
  scale_x_continuous(breaks=seq(0,10,1), limits=c(-0.5, 10.5))+
  facet_wrap(facets=vars(phase), ncol=1)+
  labs(
    title="NfR score",
    x="score", y="Respondents")+
  hist_theme+
  NULL



ggsave(filename=here::here("figures", "each.png"), plot_each)

ggsave(filename=here::here("figures", "scatter.png"), plot_scatter)

plot_line <- 
  survey_avg_long %>%
  ggplot() +
  geom_line(aes(phase, nfr_score, group=ptid, colour=profession), alpha=0.5)+
  geom_jitter(aes(phase, nfr_score, group=ptid, colour=profession), alpha=0.5, width=0, height=0)+
  theme_bw()+
  theme(
    legend.position='bottom'
  )


## sumary stats



survey_avg_long %>% 
  filter(phase !="p1_pre") %>%
  group_by(phase) %>%
  summarise(
    n=n(),
    mean = mean(nfr_score, na.rm=TRUE),
    mean_bsci = list(Hmisc::smean.cl.boot(nfr_score, conf.int=0.95, B=100000, reps=FALSE)),
    mean.ll = map_dbl(mean_bsci, ~.[2]),
    mean.ul = map_dbl(mean_bsci, ~.[3]),
    median = median(nfr_score, na.rm=TRUE),
    median_bsci = list(DescTools::MedianCI(nfr_score, conf.level=0.95, na.rm = TRUE, R=100000, method="boot")),
    median.ll = map_dbl(median_bsci, ~.[2]),
    median.ul = map_dbl(median_bsci, ~.[3]),
    
    mean_text = willsutils::print_est2bracket(mean, mean.ll, mean.ul, 1),
    median_text = willsutils::print_est2bracket(median, median.ll, median.ul, 1),
  ) %>%
  select(phase, mean_text, median_text)






survey_avg_wide %>% 
  ungroup() %>%
  filter(is.na(p3_post), !is.na(p2_during)) %>%
  summarise(
    n=n(),
    mean = mean(p2_during, na.rm=TRUE),
    mean_bsci = list(Hmisc::smean.cl.boot(p2_during, conf.int=0.95, B=100000, reps=FALSE)),
    mean.ll = map_dbl(mean_bsci, ~.[2]),
    mean.ul = map_dbl(mean_bsci, ~.[3]),
    median = median(p2_during, na.rm=TRUE),
    median_bsci = list(DescTools::MedianCI(p2_during, conf.level=0.95, na.rm = TRUE, R=100000, method="boot")),
    median.ll = map_dbl(median_bsci, ~.[2]),
    median.ul = map_dbl(median_bsci, ~.[3]),
    
    mean_text = willsutils::print_est2bracket(mean, mean.ll, mean.ul, 1),
    median_text = willsutils::print_est2bracket(median, median.ll, median.ul, 1),
  ) %>%
  select(mean_text, median_text)





## model #


library('nlme')
reCAR1ML1 <- lme(nfr_score ~ phase, data=survey %>% filter(phase!="pre NNH"),random=~1|ptid,
                 correlation=corCAR1(0.3,form=~dt|ptid))
summary(reCAR1ML1)

reCAR1ML2 <- lme(nfr_score ~ phase, data=survey %>% filter(phase!="pre NNH"),random=~1|ptid,
                 correlation=Initialize(corCAR1(0.95,form=~dt|ptid), data=survey))
summary(reCAR1ML2)
