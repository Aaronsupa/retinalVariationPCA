### Import starting libraries
```{r}
library(readr)
library(tidyverse)
library(readxl)
```
### Get the Counts from our file
```{r}
counts <- read_csv(PATH)
counts = counts %>% column_to_rownames("...1")
metadata <- read_excel(PATH)
names(metadata)[names(metadata) == 'Sample.Name'] <- 'sample_id'
metadata
head(counts)
```

### Individual Data
```{r}
individualData <- read_excel(PATH, skip = 1)
view(individualData)
labels(individualData)
```

```{r}
editAge <- data.frame(individualData["Age"])
curr <- editAge[1,]

for(i in 1:nrow(editAge) - 1){
  ifelse(is.na(editAge[i,]), editAge[i,] <- curr, curr <- editAge[i,])
}

editAge <- editAge$Age
individualData <- select(individualData, !Age)
individualData <- individualData %>% mutate(Ages = c(editAge))
individualData
```
### TPM Function
```{r}
TPM <- function(counts) {
  reads_per_kilbase <- counts / 2000 #Because thats how much we can read at a time
  per_million <- sum(reads_per_kilbase, na.rm= TRUE) / 1e6
  tpms <- reads_per_kilbase / per_million
}

tpms <-counts 
head(tpms)
head(colnames(tpms))
```

### Apply TPM function
```{r}
for(i in 1:ncol(tpms)){
  tpms[,i] <- (TPM(tpms[,i]))
}
head(tpms)
```
### Log Change
```{r}
log_tpms<- log(tpms) 
log_tpms[!is.finite(as.matrix(log_tpms))] <- 0
```

```{r}
x<- log_tpms %>% t()
PC_x <- prcomp(x)

var_explained <- data.frame(PC = paste0("PC", 1:ncol(PC_x$x)), var_explained=(PC_x$sdev)^2/sum((PC_x$sdev)^2))

PC1to9_Var <- var_explained[1:9,]
PC1to9_Var
```

### Scree Plot Displaying PCAs
```{r}
x <- log_tpms %>% t() #Turn into matrix with t()

PC_x <- prcomp(x)

var_explained <- data.frame(PC = paste0("PC", 1:ncol(PC_x$x)), var_explained=(PC_x$sdev)^2/sum((PC_x$sdev)^2))

PC1to9_Var <- var_explained[1:9,]


ggplot(PC1to9_Var, aes(x=PC,y=var_explained*100, group=1))+ 
   geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot") + 
  theme_classic(base_family = "Times", 
                base_size = 14)+ ylab("Variation Explained * 100") + xlab("Component Number") + theme_grey(base_size = 12)  +
  theme(plot.title = element_text(hjust = 0.1)) +
  NULL
```
### PCA scatterplot
```{r}

PCs_x<-data.frame(PC_x$x) %>%
  rownames_to_column(var = "sample_id") #make sample IDs a column to facilitate adding other metadata

PCs_x<-left_join(PCs_x, metadata)

ggplot(data = PCs_x, aes(x =PC1, y=PC2, label = sample_id)) +
  geom_point(size = 4)+ geom_text(check_overlap = FALSE, nudge_x = 10,
  nudge_y = 10) + theme_classic(base_family = "Times", base_size = 14) +
  NULL

```

### Redo PCA with outlier removed
```{r}
log_tpms_outlierRemove<-log_tpms %>%
  select(-A2_17E1R, -D2_17E4R)

x<-log_tpms_outlierRemove %>%
  t()

PC_x<-prcomp(x)

var_explained <- data.frame(PC= paste0("PC",1:ncol(PC_x$x)),
                                var_explained=(PC_x$sdev)^2/sum((PC_x$sdev)^2)) 


PC1to9_Var<-var_explained[1:9,] 
PC1to9_Var
```

### New Scree Plot
```{r}
ggplot(PC1to9_Var, aes(x=PC,y=var_explained*100, group=1))+ 
   geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot") + 
  theme_classic(base_family = "Times", 
                base_size = 14)+ ylab("Variation Explained * 100") + xlab("Component Number") + theme_grey(base_size = 12)  +
  theme(plot.title = element_text(hjust = 0.1)) +
  NULL

```
### ScatterPlots showing different sources of variation
```{r}

PCs_x<-data.frame(PC_x$x) %>%
  rownames_to_column(var = "sample_id") #make sample IDs a column to facilitate adding other metadata

PCs_x<-left_join(PCs_x, metadata)

PCs_x<-data.frame(PC_x$x) %>%
  rownames_to_column(var = "sample_id") #make sample IDs a column to facilitate adding other metadata

PCs_x<-left_join(PCs_x, metadata)

ggplot(data = PCs_x, aes(x =PC1, y=PC2, color = Day)) +
  geom_point()+
  theme_classic(base_family = "Times", 
                base_size = 14 )+  theme_grey(base_size = 12)  +
  theme(plot.title = element_text(hjust = 0.1)) +
  NULL

ggplot(data = PCs_x, aes(x =PC2, y=PC3, color = mother)) +
  geom_point()+
  theme_classic(base_family = "Times", 
                base_size = 14 )+ theme_grey(base_size = 12)  +
  theme(plot.title = element_text(hjust = 0.1)) +
  NULL

ggplot(data = PCs_x, aes(x =PC1, y=PC3, color = Day)) +
  geom_point()+
  theme_classic(base_family = "Times", 
                base_size = 14 )+ theme_grey(base_size = 12)  +
  theme(plot.title = element_text(hjust = 0.1)) +
  NULL
```

