# Donut Chart 

## Datos 

Lo primero será crear un `data.table` con datos simulados. En este caso son 2 categorías.

-   Sexo que irá en la torta

-   Grupo que irá en facets

-   N que son recuentos

``` r
library(data.table)
library(scales)
library(ggplot2)
library(forcats)

# Partimos con data numerica de cantidades
gdata <- data.table(
    SEXO = rep(c("Hombre", "Mujer"), 4),
    GRUPO = rep(c("D", "C", "B", "A"), each = 2),
    N = c(2220698, 1713396, 1381339, 1274634, 2841801, 3748242, 1292953, 1743318)
)

print(gdata, row.names = FALSE)
```

```         
##    SEXO  GRUPO       N
##  <char> <char>   <num>
##  Hombre      D 2220698
##   Mujer      D 1713396
##  Hombre      C 1381339
##   Mujer      C 1274634
##  Hombre      B 2841801
##   Mujer      B 3748242
##  Hombre      A 1292953
##   Mujer      A 1743318
```

## Data Management 

Ahora necesitamos crear los minimos y máximos de los porcentajes de N respecto de cada GRUPO. La razón es que este gráfico se crea usando rectangulos de un tamaño definido por los porcentajes. Hacemos un pequeño data management.

``` r
# Calculamos el porcentaje
gdata[, PCT := N/sum(N), GRUPO]

# El valor maximo de cada geom_rect
gdata[, YMAX := cumsum(PCT), GRUPO]

# El valor mínimo
gdata[, YMIN := shift(YMAX, type = 'lag')]
gdata[, YMIN := ifelse(YMAX != 1, 0, YMIN)]

# Indice entre Mujer/Hombre
gdata[, FEM := round(PCT/YMIN, 2)]
gdata[, FEM := ifelse(FEM == Inf, NA, FEM)]

print(as.data.frame(gdata), row.names = FALSE)
```

```         
##    SEXO GRUPO       N       PCT      YMAX      YMIN  FEM
##  Hombre     D 2220698 0.5644751 0.5644751 0.0000000   NA
##   Mujer     D 1713396 0.4355249 1.0000000 0.5644751 0.77
##  Hombre     C 1381339 0.5200877 0.5200877 0.0000000   NA
##   Mujer     C 1274634 0.4799123 1.0000000 0.5200877 0.92
##  Hombre     B 2841801 0.4312265 0.4312265 0.0000000   NA
##   Mujer     B 3748242 0.5687735 1.0000000 0.4312265 1.32
##  Hombre     A 1292953 0.4258358 0.4258358 0.0000000   NA
##   Mujer     A 1743318 0.5741642 1.0000000 0.4258358 1.35
```

## Theme 

Ya con esto el grafico, pero primero el theme que suelo usar. Producto del mismo tuve que pasar muchos elementos a `element_blank()` .

``` r
# Mi tema
olito_theme <- function(){
    theme_bw() %+replace%  
        theme(
            axis.title.y = element_text(margin = margin(r = 6), 
                                        size = 14, hjust = 0, color = "gray30",
                                        angle = 90),
            axis.title.x = element_text(margin = margin(t = 8), 
                                        size = 14, hjust = 0, color = "gray30"),
            axis.text.x.top = element_blank(),
            axis.text.y.right = element_blank(),
            axis.text.x = element_text(size = 12, color = "gray30"), 
            axis.text.y = element_text(size = 12, color = "gray30", hjust = 1),
            axis.line = element_line(color ="gray30", linewidth = 0.5),
            axis.ticks.length.x.top = unit(0, "cm"),
            axis.ticks.length.y.right = unit(0, "cm"),
            axis.ticks.length.y = unit(.15, "cm"),
            axis.ticks.length.x = unit(.15, "cm"),
            panel.grid.major.x = element_line(color = "gray85"),
            panel.grid.major.y = element_line(color = "gray85"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(color = "white", fill = 'grey98', linewidth = 0.1),
            legend.position = "top", 
            legend.margin = margin(b = 0),
            legend.justification = "right",
            legend.title = element_text(size = 14, color = "gray30"),
            legend.text = element_text(size = 13, color = "gray30"),
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 12, angle = 270)
        )
}
```

## Grafico 

``` r
g <- ggplot(gdata, aes(ymax=YMAX, ymin=YMIN, xmax=4, xmin=3, fill=SEXO)) +
    geom_rect(color = 'grey30') +
    facet_grid(cols = vars(forcats::fct_rev(GRUPO))) +
    scale_fill_manual(values = c("steelblue","hotpink2"))  +
    coord_polar(theta = "y") + xlim(c(1, 4)) +
    olito_theme() +
    geom_text(aes(x = 3.5, y = (YMIN + YMAX) / 2, 
                  label = scales::percent(PCT, accuracy = 1)), 
              color = "white", size = 5, fontface = 'bold' ,
              show.legend = FALSE) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank(), panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_blank(), panel.border = element_blank(),
          strip.background = element_blank(), panel.background = element_blank(),
          axis.line = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(), strip.text.x = element_blank() ) +
    labs(fill = "Sexo") +
    geom_text(x = 0.5, y = 0.5, label = gdata$GRUPO, 
             color = "grey30", size = 12, fontface = "bold") +
    geom_text(x = 1.5, y = 0.5, label = gdata$FEM, 
              color = "grey30", size = 8, fontface = "bold")

ggsave("donut.png", plot = g, width = 1500, height = 400, 
       units = "px", dpi = 120, scale = 120/96)
```

Y Voila!

![](donut.png)
