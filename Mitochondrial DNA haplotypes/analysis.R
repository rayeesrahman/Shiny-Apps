#library(ggplot2)
#library(plotly)
#library(Rtsne)
#library(randomForest)
x = read.table("./mat.csv", sep = ",", header =T )
genomes = read.table("./ALL.chrMT.phase3_callmom-v0_4.20130502.genotypes.vcf.mat", header = T, sep = ",")
row.names(genomes) = genomes$REFPOSALT
genomes$REFPOSALT = NULL
#genomes[1:10,1:5]
genomes.t = data.matrix(t(genomes))
genomes.pl  = data.frame(genomes.t)
genomes.pl$Sample.name = row.names(genomes.pl)

#genomes.t[1:10,1:5]
#snp.corr = cor(genomes.t, method = "spearman" ) 
#genome.corr = cor(genomes, method = "spearman")
#snp.corr[1:5,1:5]
#snp.corr[upper.tri(snp.corr)] <- 0
#diag(snp.corr) <- 0
#genomes.no.cor <- genomes.t[,!apply(snp.corr,2,function(x) any(x > 0.50))]
#ncol(genomes.no.cor)
#genomes.dist = vegdist(genomes.t, method = "jaccard")
#genoems.tsne =  Rtsne(genome.corr, dims = 3, perplexity = 30 , max_iter = 500, theta = 0.5, pca = T,  is_distance = F,   check_duplicates = F, verbose = T)
#genomes.tsne.2 = as.data.frame(genoems.tsne$Y) 
#head(genomes.tsne.2)
#write.table(genomes.tsne.2, file = "../genomes.tsne.csv", sep =",", quote = F, eol = "\n",row.names = F )
genomes.tsne.2 = read.table("./genomes.tsne.csv", sep = ",", header = T)
row.names(genomes.tsne.2) = row.names(genomes.t)
meta = read.table("./igsr_samples (1).tsv",header =T, sep = "\t")
genomes.tsne.2$Sample.name = row.names(genomes.tsne.2)
merge = merge(genomes.tsne.2, meta, by = "Sample.name")
#plot_ly(data = merge, x = ~V1, y = ~V2, z = ~V3, color = ~Superpopulation.name , text = ~paste('Population: ',Population.name))
