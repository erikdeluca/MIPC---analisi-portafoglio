p <- max_sr %>%
  gather(tickerList[1]:tickerList[length(tickerList)], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights),
             y = Weights,
             fill = Asset,
             label = str_c(round(Weights*100,2), "%"))) +
  geom_bar(stat = 'identity') +
  geom_label(nudge_y = .01) +
  theme_minimal() +
  labs(x = 'Tickers',
       y = 'Weights',
       title = "Weights of the portfolio tangent to the efficient frontier") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") +
  coord_flip()
p


p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annual risk',
       y = 'Annual return',
       title = "Portfolio optimization and efficient frontier") +
  geom_point(aes(x = Risk,
                 y = Return),
             data = max_sr,
             color = 'darkred') +
  geom_label(aes(x = Risk,
                 y = Return,
                 label = round(SharpeRatio,2)),
             data = max_sr,
             color = 'darkred',
             nudge_x = .0002,
             nudge_y = .0005,
  )
p




returnTickerIndici = returnTicker %>% 
  as.tibble() %>%
  summarise_all(sum) %>% 
  pivot_longer(1:length(tickerList), names_to = "Tickers", values_to = "Rendimento")  %>% 
  add_column(returnTicker %>%
               as.tibble() %>%
               summarise_all(sd) %>%
               pivot_longer(1:length(tickerList), names_to = "Titoli", values_to = "Varianza") %>% 
               dplyr::select(Varianza)
  ) %>% 
  mutate(Variazione = ifelse(round(Rendimento, 2) != 0,
                             Varianza/abs(Rendimento),
                             1)) 

returnTickerIndici %>% 
  ggplot(aes(Tickers, Variazione, fill = Varianza)) +
  geom_col() + 
  coord_flip() +
  scale_fill_gradient2(high = "mediumvioletred",
                       mid = "sienna1",
                       low = "mediumspringgreen",
                       midpoint = mean(returnTickerIndici$Varianza))

returnTickerIndici %>% 
  ggplot(aes(Rendimento, Varianza, fill = Variazione, label = Tickers)) +
  geom_point() +
  scale_x_percent() +
  scale_y_percent() +
  ggrepel::geom_label_repel(max.overlaps = 100, min.segment.length = .1) +
  scale_fill_distiller(name = "Variation index", palette = "PiYG") +
  # guides(guide_legend(title = ć))
  theme_classic() +
  ylab("Standard deviation") + 
  xlab("Return") 





correlazione %>% 
  reshape2::melt() %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_distiller(name = "Correlation",
                       palette = "RdYlGn") +
  geom_label(aes(label = round(value,2)), size =2,label.size = 0) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 30,vjust = .95, hjust = .95))



ggplot() +
  stat_density()






# nuova versione



max_sr %>%
  gather(detailPortfolio$tickerList, key = Asset,
         value = Weights) %>%
  cbind(Category = factor(detailPortfolio$category)) %>% 
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights),
             y = Weights,
             fill = Category,
             label = str_c(round(Weights*100,2), "%"))) +
  geom_bar(stat = 'identity') +
  geom_label(nudge_y = .01, size = 3) +
  theme_minimal() +
  labs(x = 'Tickers',
       y = 'Weights',
       title = "Weights of the portfolio tangent to the efficient frontier") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) + 
  theme(legend.position = "bottom") +
  coord_flip()
