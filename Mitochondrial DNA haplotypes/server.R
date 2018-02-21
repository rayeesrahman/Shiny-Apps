# server.R definition
server <- function(input, output){
  merge = read.table("./merge.csv", header = T, sep = "\t")
  colnames(merge) = c("X1", "Sample.name", "V1", "V2", "V3", "gender", "ana", "code", "Population.name", "scode", "Superpopulation.name")
  genomes.pl = read.table("./genomes.pl.csv", header =T, sep =  "," )
  row.names(genomes.pl)  = genomes.pl$Sample.name
  
  output$Plot4 <- renderPlotly({
    plot_ly(data = merge, 
            x = ~V1,  y = ~V3, z = ~V2 , mode = "markers", type = "scatter3d",marker = list(size = 10),
            dragmode =  "select", 
            color = ~Superpopulation.name , 
            text = ~paste('Population: ',Population.name), 
            source = "subset") %>%
      layout(dragmode = "select", plot_bgcolor = "6A446F")
  })
  # Observes the second feature input for a change
   output$Plot1 <- renderPlotly({
      plot_ly(data = merge, 
              x = ~V1,  y = ~V3, mode = "markers", type = "scatter",marker = list(size = 10),
              dragmode =  "select", 
              color = ~Superpopulation.name , 
              text = ~paste('Population: ',Population.name), 
              source = "subset") %>%
              layout(dragmode = "select", plot_bgcolor = "6A446F")
    })
  # Coupled event 1
  output$Plot2 <- renderPlotly({
    
    # Get subset based on selection
    event.data <- event_data("plotly_selected", source = "subset")
    colnames(event.data) = c("curveNumber", "pointNumber", "V1", "V3")
    subset.d = merge(event.data, merge, by=c("V1","V3"))
    genomes.sub = genomes.pl[which(genomes.pl$Sample.name %in% subset.d$Sample.name ),]
    genomes.sub$Sample.name = NULL
    i <- (colSums(genomes.sub, na.rm=T) != 0)
    genomes.sub.col <- genomes.sub[, i]
    col = ncol(genomes.sub.col)
    genomes.sub.col$Sample.name = row.names(genomes.sub.col)
    genomes.sub.merge = merge(genomes.sub.col, subset.d, by = "Sample.name")
    genomes.sub.merge$Population.name = factor(genomes.sub.merge$Population.name)
    genomes.sub.col$Sample.name = NULL
    genomes.sub.merge$Sample.name = NULL
   # If NULL dont do anything
    #if(is.null(event.data) == T) return(NULL)
    heatmaply(genomes.sub.merge[,1:col], row_side_colors = genomes.sub.merge$Population.name) 
    
#    plot_ly(plot.summ, x = ~Class, y = ~Count, type = "bar", source = "select", color = ~Class) %>%
 #     layout(title = "No. of Malignant and Benign cases <br> in Selection",
  #           plot_bgcolor = "6A446F",
   #          yaxis = list(domain = c(0, 0.9)))
  })
  
  # Coupled event 2
  output$Plot3 <- renderPlotly({
    # Get subset based on selection
    event.data <- event_data("plotly_selected", source = "subset")
    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)
    colnames(event.data) = c("curveNumber", "pointNumber", "V1", "V3")
    subset.d = merge(event.data, merge, by=c("V1","V3"))
    pp <- ggplot(data=subset.d, aes(x=Population.name, fill = factor(Population.name))) + 
      facet_wrap( ~Superpopulation.name, scales = "free" ) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), color = "black") + 
      theme_bw() + scale_y_continuous("Frequency") +  
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 15) , axis.title = element_blank()) +  scale_fill_brewer(palette="Set3")
    ggplotly(pp) %>% layout(dragmode = "select", plot_bgcolor = "6A446F")
  })
  
  output$Plot5 <- renderPlotly({
    event.data <- event_data("plotly_selected", source = "subset")
    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)
    colnames(event.data) = c("curveNumber", "pointNumber", "V1", "V3")
    subset.d = merge(event.data, merge, by=c("V1","V3"))
    genomes.sub = genomes.pl[which(genomes.pl$Sample.name %in% subset.d$Sample.name ),]
    row.names(genomes.sub) = genomes.sub$Sample.name
    genomes.sub$Sample.name = NULL
    i <- (colSums(genomes.sub, na.rm=T) != 0)
    genomes.sub.col <- genomes.sub[, i]
    cols = ncol( genomes.sub.col)
    coln = colnames(genomes.sub.col)
    genomes.sub.col$Sample.name = row.names(genomes.sub.col)
    genomes.sub.col.meta = merge(genomes.sub.col, subset.d, by = "Sample.name")
    genomes.sub.col.meta$Sample.name = NULL
    genomes.sub.col.meta$Population.name = factor(genomes.sub.col.meta$Population.name)
    genomes.sub.col.meta.sp = split(genomes.sub.col.meta, f = genomes.sub.col.meta$Population.name )
    var.pval = data.frame()
    for (i in genomes.sub.col.meta.sp)
    {
      n = unique(as.character(i$Population.name))
      comp = genomes.sub.col.meta[ which(genomes.sub.col.meta$Population.name != n ), ]
      for ( ii in 1:cols)
        {
          v = coln[ii]
          pop.with.var = sum(i[,ii])
          pop.without.var = length(which(i[,ii] == 0))
          f1 = pop.with.var/ (pop.without.var + pop.with.var )
          otherpop.with.var = sum(comp[,ii])
          otherpop.without.var =  length(which(comp[,ii] == 0))
          f2 = otherpop.with.var / ( otherpop.without.var +  otherpop.with.var)
          fc =  log10(f1/f2)
          #if (v == "CN3106C")
          #{
          #  print(paste(v, n, "freq in set:", f1, "freq in other:", f2, "fold change:", fc, sep = " "))
          #}
          #if (v == "N3107C")
          #{
          #  print(paste(v, n, "freq in set:", f1, "freq in other:", f2, "fold change:", fc, sep = " "))
          #}
          #if (is.finite(fc))
          #{
              mat =as.data.frame(cbind(c(pop.with.var, otherpop.with.var ), c(pop.without.var, otherpop.without.var)))
              mat.csq = fisher.test(mat,alternative="greater")
              df = as.data.frame(data.matrix(t(as.data.frame(c(n,v,mat.csq$p.value)))))
              df$V3 =as.numeric(mat.csq$p.value)
              var.pval = as.data.frame(rbind(var.pval,df))
          #}
       }
    }
    var.pval$V4 = -1* log10(var.pval$V3)
    mean.var.pval = mean(var.pval$V4)
    var.pval.sp = split(var.pval, f =  factor(var.pval$V2))
    var.final = data.frame()
    #for (i in var.pval.sp)
    #{
    #  n =  unique(as.character(i$V2))
    #  mean.variant = mean(i$V4)
    #  if (mean.variant > mean.var.pval)
    #  {
    #  test =  t.test(i$V4,var.pval$V4)
    #   if(test$p.value > 0)
    #   {
    #     var.final = data.frame(rbind(var.final,i))
    #   }
    #   else 
    #   {
    #     print(paste("excluded:" , n, sep = " ") )
    #   }
    #  }
    #  else {
    #    var.final = data.frame(rbind(var.final,i))
    # }
    #}
    #var.pval = var.final
    thres = -1* log10(.05/nrow(var.pval))
    p = ggplot(var.pval, aes(factor(V1), as.numeric(V4), fill = factor(V1))) + geom_point(aes(text = paste("Variant: ", V2, "<br>p-value ", V3 ), size = 3), position = position_jitter(w = 0.2, h = 0)) + geom_hline(yintercept = thres, color = "white", size = 1 ) +  theme_bw()+  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 15) , axis.title = element_blank(),panel.grid.major = element_blank())  + scale_fill_brewer(palette="Set3")
    ggplotly(p) %>% layout(dragmode = "select", plot_bgcolor = "6A446F")
  })
}