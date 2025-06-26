library(metafor); library(tidyverse); library(patchwork) # Pacotes para MA e gráfico

dat <- dat.furukawa2003 # Exemplo usa dados de Furukawa et al 2003

dat <- escalc(measure="SMD", data=dat, n1i=Ne, n2i=Nc,
              m1i=Me, m2i=Mc, sd1i=Se, sd2i=Sc, slab=author) # Calculando effect sizes

res <- rma(yi, vi, data=dat) # Meta análise

summary(res)
forest(res) # Forest padrão

## data

dat_meta <- broom::tidy(res, include_studies=TRUE, conf.int=T) |> # Extrair efeitos e 95%CI
  mutate(term = if_else(term == "overall", "Meta-análise", term)) |> # Mudando nome do efeito da MA
  mutate(effect = paste0(format(round(estimate, digits=2), nsmall=2),
                         " [",
                         format(round(conf.low, digits=2), nsmall=2), "; ",
                         format(round(conf.high, digits=2), nsmall=2), # Formatando o texto do efeito + 95%CI
                         "]"))

dat_meta$n_treat <- c(dat$Ne, NA_real_) # Incluindo outras informações no banco
dat_meta$n_con <- c(dat$Nc, NA_real_) 
dat_meta$score_treat <- c(dat$Me, NA_real_)
dat_meta$score_con <- c(dat$Mc, NA_real_)
dat_meta$weights <- c(weights(res), max(weights(res))) # Pesos

dat_meta$term <- factor(dat_meta$term, levels = rev(dat_meta$term)) # Ordenando estudos

dat_meta <- dat_meta |> mutate(back = rep(c("a", "b"), 9)) # Para fazer o fundo preto e branco depois

## plot

# plot 1, basico

(
p1 <- ggplot(dat_meta, aes(x=estimate, xmin=conf.low, xmax=conf.high, y=term)) +
  geom_point() + # Gráfico de pontos 
  geom_linerange() + # Barra de erro
  scale_y_discrete(expand = c(.05, 0, .15, 0)) +
  theme_bw() + 
  theme(axis.ticks.y = element_blank())
)

(
  p1_no_author <- ggplot(dat_meta, aes(x=estimate, xmin=conf.low, xmax=conf.high, y=term)) +
    geom_point() +
    geom_linerange() +
    theme_bw() +
    labs(x=NULL, y=NULL) +
    scale_y_discrete(expand = c(.05, 0, .15, 0)) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
)

## plot 2, tamanhos de efeito

(
p_effects <- ggplot(dat_meta, aes(y=term)) +
  geom_hline(yintercept=18.65, linetype="solid") +
  geom_text(aes(label=effect, x=0), size=3.5, hjust=1) + # Plotando o texto dos efeitos
  annotate(geom='text', x=0, y=19, label="SMD (95% CI)\n", fontface="bold", hjust=1, size=3.5, vjust=0.5) +
  scale_y_discrete(expand = c(.05, 0, .15, 0)) +
  xlim(c(-.45, 0)) +
  geom_hline(aes(yintercept=term, color=back), size=6, alpha=.1) +
  scale_color_manual(values=c("black", "white")) +
  guides(color="none") +
  theme_void() 
)

(p1 + p_effects) + # Juntando os plots com Patchwork
  plot_layout(widths = c(1, 0.35))

## plot 3, detalhes adicionais (n, escores de cada estudo)

(
p_info <- ggplot(dat_meta, aes(y=term, x=5)) +
    geom_hline(aes(yintercept=term, color=back), size=6, alpha=.1) +
    geom_hline(yintercept=18.65, linetype="solid") +
    geom_text(aes(label=term, x=-.5), size=3.5, hjust=0) +
    geom_text(aes(label=n_treat, x=0.5), size=3.5) +
    geom_text(aes(label=n_con, x=1), size=3.5) +
    annotate(geom='text', size=3.5,x=-.5, y=19, label="Estudo\n", fontface="bold", vjust=.5, hjust=0) +
    annotate(geom='text', size=3.5,x=0.5, y=19, label="n \nTratamento", fontface="bold", vjust=0) +
    annotate(geom='text', size=3.5,x=1, y=19, label="n \nPlacebo", fontface="bold", vjust=0) +
    scale_y_discrete(expand = c(.05, 0, .15, 0)) +
    scale_color_manual(values=c("black", "white")) +
    guides(color="none") +
    xlim(c(-.55, 1.1)) +
    theme_void()
)

(p_info | p1_no_author | p_effects) +
  plot_layout(widths = c(.85, 1, 0.35)) # Esse argumento controla os tamanhos proporcionalmente de cada gráfico

## plot 4, final details

(
  p1_no_author <- ggplot(dat_meta, aes(x=estimate, xmin=conf.low, xmax=conf.high, y=term)) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_linerange() +
    geom_point(aes(shape=type, fill=type, size=weights)) + # Considerando pesos e formatos diferentes pros pontos
    theme_bw() +
    labs(x=NULL, y=NULL) +
    scale_shape_manual(values=c(22, 23)) +
    guides(shape="none", fill="none", size="none") +
    scale_size_continuous(range=c(2, 4)) +
    scale_y_discrete(expand = c(.05, 0, .15, 0)) +
    scale_fill_manual(values=c("indianred", '#6b58a6')) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
)


(p_info | p1_no_author | p_effects) +
  plot_layout(widths = c(.85, 1, 0.35))
 
