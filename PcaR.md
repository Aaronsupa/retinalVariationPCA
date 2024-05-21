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
![Screenshot 2024-05-21 104350](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/30ab356b-48f2-4916-a2ac-8c6bdf8f31ea)
![Screenshot 2024-05-21 104355](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/4178befd-e74a-4ab8-ac63-a8995dc53aeb)

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
![Screenshot 2024-05-21 104407](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/24aa14a4-3cdc-4148-a83f-23c0241f58cd)

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
![Screenshot 2024-05-21 104545](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/02dc78a5-32b6-48c4-90be-2210efad6677)
![Screenshot 2024-05-21 104538](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/7a97ad79-b1cc-4227-8a4d-518ea8faf78c)

### Apply TPM function
```{r}
for(i in 1:ncol(tpms)){
  tpms[,i] <- (TPM(tpms[,i]))
}
head(tpms)
```
![Screenshot 2024-05-21 104553](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/79dc880d-f42d-4ae2-be4f-0a12d9d6b8bc)

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
![Screenshot 2024-05-21 104706](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/5b0c2620-df8d-4674-be38-48d4feb90b9d)

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
![Screenshot 2024-04-18 133640](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/de947deb-385b-4859-becd-9a8032c0493a)
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
![Screenshot 2024-05-21 123809](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/ecfa1c2c-53db-4ede-b27b-d67498ade33c)

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
![Screenshot 2024-04-18 141324](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/b61bb413-fd83-4468-acce-b297ac155911)

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
![Screenshot 2024-04-18 141228](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/bbadae77-fc61-4e7b-9f0f-e5e92f219a1b)

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
![Screenshot 2024-05-21 123914](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/6c1e7dc2-271c-43b2-9fb7-945d90b63217)
![Screenshot 2024-05-21 123926](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/ba051c92-f17d-4724-b9d2-4cba4738b8aa)
![Screenshot 2024-05-21 123920](https://github.com/Aaronsupa/retinalVariationPCA/assets/77075455/c611c7f5-621e-4ca9-bcb2-4246fd01ce11)


