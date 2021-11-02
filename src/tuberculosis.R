library('devtools')
library('ggpubr')
library('ggplot2')
library('patchwork')
library('scales')
library('svglite')
library('tidyverse')

## Instalar paquete: feather color palette
devtools::install_github(repo = "shandiya/feathers", ref = "main")

library('feathers')

## Preparar el directorio de trabajo
## ESCRIBIR en setwd() la ruta local del repositorio
setwd('')

## Cargar tema por defecto
source('../src/bw_no_axes.R')


tuber <- read.csv(file = 'tuberculosis_ap.tsv', sep='\t')

# Convertir los años en fechas 
tuber$Year      <- as.Date(ISOdate(tuber$Year, 12, 31))

## Distribución tuberculosis

tuber_dist <- ggplot(data = tuber) +
    geom_line(aes(x=Year, y=Mortality), color="#cf2236", size=2, alpha=0.4) +
    geom_area(aes(x=Year, y=Mortality), fill="#d683ad", alpha = 0.5, position = 'identity') +
    geom_text(x=as.Date('1918-12-31'), y=35000, label='Gripe de 1918') +
    geom_text(x=as.Date('1944-12-31'), y=20000, label='Posguerra Civil') +
    geom_text(x=as.Date('1963-12-31'), y=25000, label='Descubrimiento de la isoniacida') +
    geom_segment(aes(x = as.Date('1956-12-31'), y = 24000, 
                     xend = as.Date('1952-12-31'), yend = 20000),
                     arrow = arrow(length = unit(0.5, "cm"))) +
    scale_x_date(date_labels = "%Y", date_breaks="5 years", expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,40000, 5000),
                       labels = unit_format(scale = 1e-3, prefix=''),
                       expand = c(0,0)) +
    expand_limits(y=40000) +
    labs(x= '', y='Miles de casos') +
    ggtitle(label='Mortalidad total por tuberculosis respiratoria') +
    bw_no_axes()


tuber_by_factor <- tuber %>%
    mutate(timepoint = case_when(
        Year <= as.Date('1944-12-31') ~ 'before_antibiotics',
        Year >= as.Date('1944-12-31') & Year <= as.Date('1952-12-31') ~ 'before_isoniacide',
        Year >= as.Date('1952-12-31') ~ 'after_isoniacide'
    )) %>%
    mutate(timepoint = as.factor(timepoint)) %>%
    group_by(timepoint) %>%
    mutate(median_mortality=median(Mortality, na.rm=TRUE))
    

tuber_by_factor$timepoint <- fct_reorder(tuber_by_factor$timepoint, -tuber_by_factor$median_mortality)


## Plot hist expression / group
set.seed(1994)

comparisons_to_perform <- list( c("before_antibiotics", "after_isoniacide"), c("before_isoniacide", "after_isoniacide"), c("before_antibiotics", "before_isoniacide") )

event_mortality <- ggplot(tuber_by_factor, aes(x=timepoint, y=Mortality, fill=timepoint)) +
    geom_boxplot() +
    stat_compare_means(comparisons = comparisons_to_perform,
                       method='wilcox.test',
                       label="p.signif",
                       ref.group = 'before_antibiotics') +
    stat_compare_means(method = 'kruskal.test', label.x = 2.5, label.y=45000) +
    scale_y_continuous(breaks = seq(0,50000, 5000),
                       labels = unit_format(scale = 1e-3, prefix='')) +
    scale_x_discrete(labels=NULL) +
    scale_fill_manual(labels=c('Anterior a la penicilina', 'Posterior a penicilina', 'Posterior a isoniacida'),
                      values=get_pal("yellow_robin")) +
    bw_no_axes() +
    theme(axis.ticks.x=element_blank()) +
    labs(x='', y='Miles de muertos', fill='Periodo') +
    ggtitle('Impacto de la isoniacida en la mortalidad total') 
    

final_tuber <- tuber_dist | event_mortality

final_tuber <- final_tuber + plot_annotation(tag_level='A') + plot_layout(guides = 'collect')

