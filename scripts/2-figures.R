

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


## model #


library('nlme')
reCAR1ML1 <- lme(nfr_score ~ phase, data=survey %>% filter(phase!="pre NNH"),random=~1|ptid,
                 correlation=corCAR1(0.3,form=~dt|ptid))
summary(reCAR1ML1)

reCAR1ML2 <- lme(nfr_score ~ phase, data=survey %>% filter(phase!="pre NNH"),random=~1|ptid,
                 correlation=Initialize(corCAR1(0.95,form=~dt|ptid), data=survey))
summary(reCAR1ML2)
