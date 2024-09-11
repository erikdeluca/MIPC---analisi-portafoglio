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
  ggplot(aes(Titoli, Variazione, fill = Varianza)) +
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
                       palette = "RdYlGn",
                       direction = 1) +
  geom_label(aes(label = round(value,2)), size =2,label.size = 0) +
  theme_bw() +
  scale_x_discrete(limits = rev(rownames(correlazione))) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 30,vjust = .95, hjust = .95))





myPortfolio %>% 
  fortify() %>%
  ggplot(aes(x = Index, y = Portfolio.Open)) + 
  geom_smooth(method = "gam",
              formula = formula(y ~ s(x)),
              fill = pal[5],
              aes(color = pal[5]),
              alpha = .3) +
  geom_line(color = pal[3]) +
  geom_pointrange(aes(ymin = Portfolio.Low, 
                      ymax = Portfolio.High), 
                  alpha = 0.22,
                  fill = 'turquoise1',
                  size = .1) +
  # geom_col(aes(y = Portfolio.Volume),inherit.aes = F) +
  xlab("") +
  ylab("") +
  # scale_x_datetime(date_breaks = "month")+
  scale_y_continuous(labels = function(x) scales::percent(x-1)) +
  scale_color_manual(values = pal[5], labels = "Prediction of portfolio trends using splines") +
  theme(legend.position = "none",
        legend.title = element_blank())


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


returnTickerIndici %>%
  mutate(quantili = punif(Rendimento,
                          min = hValMin * min(Rendimento), # se non metto hvalmin il min di Rendimento vale 0 e di conseguenza il log tende a meno infinito
                          max = hValMax * max(Rendimento),
                          log.p = T)) %>% 
  ggplot(aes(y = quantili,
             x = Varianza,
             color = Variazione,
             label = Titoli,
             z = Rendimento # serve solo per l'etichetta nel grafico interattivo
  )) +
  geom_point(size = 2) + 
  ggrepel::geom_label_repel(max.overlaps = 100, min.segment.length = .1) +
  scale_color_distiller(palette = "RdYlGn", direction = -1) +
  scale_x_log10(labels = scales::percent_format(accuracy = .2),
                breaks = scales::breaks_log(n = 10, base = 10)) +
  scale_y_continuous(labels = function(x) scales::percent(qunif(x,
                                                                min = hValMin * min(returnTickerIndici$Rendimento),
                                                                max = hValMax * max(returnTickerIndici$Rendimento),
                                                                log.p = T), # associo i valori originali invertendo la funzione di ripartizione
                                                          scale = 1),
                     breaks = scales::pretty_breaks(10)) +
  labs(x = "Variation", y = "Return", color = "Coefficient \nof variation") +
  theme(legend.position = "right", 
        legend.title.align = 0) 
