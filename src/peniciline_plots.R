library('devtools')
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

# Cargar los archivos de mortalidad anual
bronco    <- read.csv('bronconeumonia.tsv', sep='\t')
mortality <- read.csv('total_mortality.csv', sep='\t') 
infect_mort <- read.csv('mortalidad_infecciones.tsv', sep='\t')

# Convertir los años en fechas 
bronco$Year      <- as.Date(ISOdate(bronco$Year, 12, 31))
mortality$Year   <- as.Date(ISOdate(mortality$Year, 12, 31))
infect_mort$Year <- as.Date(ISOdate(infect_mort$Year, 12, 31))

bronco_dist <- ggplot(data = bronco) +
    geom_line(aes(x=Year, y=Mortality), color="#6c905e", size=2, alpha=0.8) +
    geom_area(aes(x=Year, y=Mortality), fill="#bee183", alpha = 0.5, position = 'identity') +
    geom_text(x=as.Date('1918-12-31'), y=61000, label="Gripe de 1918") +
    geom_vline(xintercept=as.Date('1944-12-31'), linetype="dashed") +
    geom_text(x=as.Date('1963-12-31'), y=45000, label="Llegada de la penicilina a España") +
    geom_smooth(aes(Year, Mortality, color='Tendencia global'), method='loess', formula= y~x, se=FALSE) +
    geom_smooth(data = bronco[bronco$Year >= '1944-12-31',], aes(Year, Mortality, color='Tendencia tras penicilina'), method='glm', se=FALSE) +
    scale_colour_manual(name="", values=c("#b8c9dc", "#2f7ab9")) +
    scale_x_date(date_labels = "%Y",
                 date_breaks = "10 years",
                 expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0,60000,10000),
                       labels = unit_format(scale = 1e-3, prefix=''),
                       expand = c(0,0)) +
    expand_limits(y=70000) +
    labs(x = '', y='Miles de casos') +
    ggtitle(label='Mortalidad total por neumonía y bronconeumonía') +
    bw_no_axes() 



#bronco_dist
## test 
#ggsave(filename = 'test_dist.png', plot = bronco_dist)



## Plot contra mortalidad
bronco_relativo <- merge(x=mortality,
                         y=bronco,
                         by.x = "Year",
                         by.y = "Year",
                         suffixes = c('_global', '_bronco'))

# Eliminamos missings
bronco_relativo <- na.omit(bronco_relativo)

bronco_relativo$Decade  <- as.factor(paste0(substr(bronco_relativo$Year, start = 1, stop = 3),0, 's'))
bronco_relativo$relative_mort <- round(bronco_relativo$Mortality_bronco / bronco_relativo$Mortality_global, digits=3)

relative_mort <- ggplot(data=bronco_relativo, aes(x=Decade, y=relative_mort, fill=Decade)) +
    geom_boxplot(outlier.shape = 8) +
    scale_fill_manual(values = get_pal("oriole")) +
    stat_summary(fun=median, geom='point') +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(labels=scales::percent_format()) +
    labs(x='Década', y='Proporción relativa') +
    ggtitle(label='Proporción de muertes atribuibles a neumonía y bronconeumonía') +
    bw_no_axes() +
    theme(legend.position = "none") 


## Combinar las tres tablas en una por año

## Subset 1931 > 
bronco_tr1 <- bronco[bronco$Year >= as.Date('1930-12-31'),]
mortality  <- mortality[mortality$Year >= as.Date('1930-12-31'),]

combined_bronco_mort <- merge(x=bronco_tr1,
                              y=mortality,
                              by.x='Year',
                              by.y = 'Year',
                              suffixes = c('_bronco', '_total'))


combined_bronco_mort_inf <- merge(x=combined_bronco_mort,
                              y=infect_mort,
                              by.x='Year',
                              by.y = 'Year')

colnames(combined_bronco_mort_inf) <- c('Year', 'bronco_death', 'total_death', 'infect_death')


combined_bronco_mort_inf$bronco_proportion <- combined_bronco_mort_inf$bronco_death / combined_bronco_mort_inf$total_death
combined_bronco_mort_inf$infect_proportion <- combined_bronco_mort_inf$infect_death / combined_bronco_mort_inf$total_death

combined_bronco_mort_inf$bronco_proportion <- round(combined_bronco_mort_inf$bronco_proportion, digits = 3)
combined_bronco_mort_inf$infect_proportion <- round(combined_bronco_mort_inf$infect_proportion, digits = 3)

combined_bronco_mort_inf <- combined_bronco_mort_inf[,c('Year', 'bronco_proportion', 'infect_proportion')]

combined_melted <- reshape2::melt(data=combined_bronco_mort_inf,
                                  id.vars='Year', variable.name='type_of_death',
                                  value.name='deaths')


## Let's plot
combined_melted$type_of_death <- as.factor(combined_melted$type_of_death)


combined_dist <- ggplot(data=combined_melted) +
    geom_line(aes(colour=type_of_death, x=Year, y=deaths)) +
   # geom_vline(xintercept=as.Date('1944-12-31'), linetype="dashed") +
    scale_x_date(date_labels = "%Y",
                 date_breaks = "5 years",
                 expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x='', y='Proporción de muertes') +
    ggtitle(label='Evolución de la mortalidad atribuida') +
    scale_color_manual(name='', labels = c('Neumonía y bronconeumonía', 'Infecciones'), values = get_pal('cassowary')) +
    bw_no_axes()
    



figure_1 <- (bronco_dist | relative_mort ) / combined_dist + plot_annotation(tag_levels = 'A')

ggsave(plot = figure_1, filename = '../plots/figure_1.png', dpi = 300, width = 20, height = 15)
ggsave(plot = figure_1, filename = '../plots/figure_1.svg')

