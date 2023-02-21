library(ggplot2)
library(gridExtra)
library(viridis)


genera_graficos <- function(...) {
    # bind 
    union <-rbind(...)
    # order
    ordenado <- union
    ordenado$modelo <- with(ordenado, reorder(modelo, tasa, median))

    ordenado2 <- union
    ordenado2$modelo <- with(ordenado2, reorder(modelo, auc, median))
    
    # set colors
    nb_cols <- 18
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb_cols)
    
    # generate graphs
    plot1 <- ggplot(ordenado, aes(x=modelo, y=tasa, fill = modelo)) + 
        ggtitle ("Tasa fallos")+
        geom_boxplot(alpha=0.3) +
        theme_minimal() + 
        theme(legend.position="none") +
        scale_fill_manual(values = mycolors) +
        xlab("Modelos") + ylab("Tasa") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) 
    
    plot2 <- ggplot(ordenado2, aes(x=modelo, y=auc, fill = modelo)) + 
        ggtitle ("Area Under Curve - AUC")+
        geom_boxplot(alpha=0.3) +
        theme_minimal() + 
        theme(legend.position="none") +
        scale_fill_manual(values = mycolors) +
        xlab("Modelos") + ylab("AUC") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) 
    list(plot1, plot2)
    # a. es donde se guarda la lista si se quiere generar con grid.arrange
    # grid.arrange(grobs=a,ncol=2)
    
}


