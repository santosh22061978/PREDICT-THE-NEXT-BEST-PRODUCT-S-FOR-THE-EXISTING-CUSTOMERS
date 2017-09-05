
##install.packages("readr")
##install.packages("data.table")
##install.packages("dplyr")
## install.packages("tidyr")
##install.packages("MASS")
#install.packages("gmodels")
#install.packages("plotly")
#install.packages("rattle")
#install.packages("RcolorBrewer")
#install.packages("quantregForest")
#install.packages("randomForest")
library(rattle)
library(RColorBrewer)

library(gmodels)
library(plotly)

library(dplyr)
library(ggplot2)
library(readr) 
library(data.table)
library(tidyr)
library(sqldf)
library(MASS)
library(randomForest)
library(quantregForest)

##install.packages("sqldf")

## set working directory
setwd("C:/Misc/R Progming/productRecomdprjt")
## verify the path
getwd()

my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

#read csv file in varaible train
train <- fread("sample_train_1percent.csv",stringsAsFactors=TRUE,nrows=-1)
set.seed(123)



## convert the dates. There's fecha_dato, the row-identifier date The table is partitioned for this column
train$fecha_dato <- as.Date(train$fecha_dato, format ="%Y-%m-%d" )
## and fecha_alta, the date that the customer joined. The date in which the customer 
## became as the first holder of a contract in the bank
train$fecha_alta <- as.Date(train$fecha_alta,format="%Y-%m-%d")

## find out is there any missing value in feature or attribute
sapply(train,function(x)any(is.na(x)))


## UNI VARIATE ANALYSIS
##################fecha_dato#################

ggplot(data=train,aes(x=fecha_dato)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("Age Distribution") + 
  my_theme

ggplot(data=train, aes(fecha_dato)) + 
  geom_bar(aes(fill=tiprel_1mes)) + 
  labs(title="Monthly Customer No") +
  labs(x="Date", y="Number of Customers") +
  scale_fill_discrete(
    name="Customer relation  ",
    labels = c("Unnoted","Active", "Inactive","N","Former","Potential")) + my_theme

######nodpers******************************
ggplot(data=train,aes(x=ncodpers)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("Customer code") + 
  my_theme
hist(train$ncodpers)


n_occur<- train %>% group_by(ncodpers) %>% summarise(nb=n()) 
count<-table(n_occur$nb)
barplot(count, main="Count of occurrences of each customer ", xlab="Number of occurrences", ylab="Number of customers", col=c("blue","red","black","yellow","green"))



ggplot(data=train,aes(x=ind_empleado)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("ind_empleado") + 
  my_theme
hist(train$ind_empleado)

#############pais_residencia################
ggplot(data=train,aes(x=pais_residencia)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("pais reidencial") + 
  my_theme

train_res <- train %>%
  group_by(pais_residencia) %>%
  summarise(count_by_res=n()) 
train_res<- data.frame(train_res)
train_res <- arrange(train_res,-count_by_res)


df2<- sqldf('SELECT pais_residencia,count(pais_residencia) as Residentiacount FROM train group by pais_residencia')

#View(df2)

ggplot(data=train,aes(x=sexo)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("Sex Distribution") + 
  my_theme


unique(train$ind_empleado)
unique(train$sexo)
unique(train$pais_residencia)
unique(train$ncodpers)
unique(train$age)
unique(train$fecha_dato)




#######################AGe######################
ggplot(data=train,aes(x=age)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("Age Distribution") + 
  my_theme

par(mfrow= c(2,4))
hist(train$age, main ="Age",xlab="") 




#######fecha_alta Analysis#######################

ggplot(data=train,aes(x=fecha_alta)) + 
  geom_bar(alpha=0.75,fill="tomato",color="blue") 
ggtitle("Fecha alta") + 
  my_theme


########ind_nuevo  Analysis#######################
## New customer Index. 1 if the customer registered in the last 6 months.


table(train$ind_nuevo)

######antiguedad################
ggplot(data=train,aes(x=antiguedad)) + 
  geom_bar(alpha=0.75,fill="tomato",color="blue") 
ggtitle("Antiguedad") + 
  my_theme


######indrel################

table(train$indrel)

#####ult_fec_cli_1t####

table(train$ult_fec_cli_1t)

#####indrel_1mes Analysis-###################

ggplot(data=train,aes(x=indrel_1mes)) + 
  geom_bar(alpha=0.75,fill="tomato",color="blue") 
ggtitle("indrel_1mes Distribution") + 
  my_theme

###############tiprel_1mes##################

ggplot(data=train,aes(x=tiprel_1mes)) + 
  geom_bar(alpha=0.75,fill="tomato",color="blue") 
ggtitle("tiprel_1mes Distribution") + 
  my_theme

var1<-table(train$tiprel_1mes)
colors <-c("red", "yellow", "green", "violet","orange", "blue", "pink", "cyan") 
barplot(var1, col=colors,xlab="Customer Relation Type", ylab="Count")

###############indresi##################

var1<-table(train$indresi)
colors <-c("red", "yellow", "green", "violet","orange", "blue", "pink", "cyan") 
barplot(var1, col=colors,xlab="Residence index", ylab="Count")                

###############indext##################

var1<-table(train$indext)

barplot(var1, col=colors,xlab="Foreigner index", ylab="Count")

###############conyuemp##################

var1<-table(train$conyuemp)


barplot(var1, col=colors, xlab="Spuse Index", ylab="Count")
table(train$conyuemp)

###############canal_entrada##################

var1<-table(train$canal_entrada)


barplot(var1, col=colors, xlab="Channel", ylab="Count")
table(train$canal_entrada)


###############indfall##################

var1<-table(train$indfall)


barplot(var1, col=colors, xlab="Deceased index", ylab="Count")
table(train$indfall)


###############tipodom##################

var1<-table(train$tipodom)


barplot(var1, col=colors, xlab="Addres type", ylab="Count")
table(train$tipodom)

###############cod_prov##################

var1<-table(train$cod_prov)


barplot(var1, col=colors, xlab="Province Code", ylab="Count")
table(train$cod_prov)

###############nomprov##################

var1<-table(train$nomprov)


barplot(var1, col=colors, xlab="Province Name", ylab="Count")
table(train$nomprov)


###############ind_actividad_cliente##################

var1<-table(train$ind_actividad_cliente)


barplot(var1, col=colors, xlab="Activity index", ylab="Count")
table(train$ind_actividad_cliente)

###############renta##################


summary(train$renta)

###############segmento##################

var1<-table(train$segmento)


barplot(var1, col=colors, xlab="Account Segment", ylab="Count")
table(train$segmento)

###################By Variate Analysis*******************

################Age and Sex##############

ggplot(data=train, aes(x=age)) + geom_bar(aes(fill=sexo)) + 
  labs(title="Customer Age and Sex") +
  labs(x="Age", y="Number of Customers") +
  scale_fill_discrete(name = "Segmentation",labels = c("Unnoted","H", "V"))


###################Age and Segmentation################

ggplot(data=train, aes(x=age)) + geom_bar(aes(fill=segmento)) + 
  labs(title="Customer Age and Segmentation") +
  labs(x="Age", y="Number of Customers") +
  scale_fill_discrete(name = "Segmentation")




################Seniority Vs Age###################

ggplot(train,aes(x=age,y=antiguedad)) +
  geom_point(alpha=0.4) +
  ggtitle("Seniority vs. Age") + 
  my_theme

########### income versus age

ggplot(train , aes(x=age,y=log(renta))) +
  geom_point(alpha=0.5) +
  ggtitle("log renta vs. Age") + 
  my_theme

###### renta versus segmento
ggplot(data=train, aes(renta)) + 
  geom_histogram(breaks=seq(1203, 155500*3, by = 2000), 
                 aes(fill=segmento)) + 
  labs(title="income of the household by Segment") +
  labs(x="income of the household", y="Count")

ggplot(data=train,aes(x=fecha_dato)) + 
  geom_bar(
    aes(fill=factor(ind_nuevo))
  ) + 
  ggtitle("customer Distribution") +
  labs(x="date", y="Number of Customers") +
  scale_fill_discrete(name = "Nuevo index", 
                      labels = c("Inactive", "Active"))

rentaDF<- sqldf('SELECT median(renta) as medianRenta,nomprov FROM train WHERE renta is  not null 
                group by nomprov ')


ggplot(rentaDF,aes(x=nomprov,y=medianRenta)) + 
  geom_point(color="Blue") + 
  guides(color=FALSE) + 
  xlab("nomprov") +
  ylab("Median Income") +  
  my_theme + 
  theme(axis.text.x=element_blank(), axis.ticks = element_blank()) + 
  geom_text(aes(x=nomprov,y=medianRenta,label=nomprov),angle=90,hjust=-.25) +
  theme(plot.background=element_rect(fill="Blue"),
        panel.background=element_rect(fill="Orange"),
        panel.grid =element_blank(),
        axis.title =element_text(color="Orange"),
        axis.text  =element_text(color="yellow"),
        plot.title =element_text(color="Orange",size=32)) +
  ylim(c(50000,200000)) +
  ggtitle("City wise Income Distribution")


##################### Gross income by segment ###################
ggplot(data = train, las =2, aes(x=segmento, y=renta)) + geom_boxplot(aes(fill=segmento))  

##################### Gross income by employee index #################
ggplot(data = train, las =2, aes(x=ind_empleado, y=renta)) + geom_boxplot(aes(fill=ind_empleado)) 


#################### Plot by year and month  ######################
dateDF<-sqldf('select fecha_dato , fecha_alta, pais_residencia from train ')
dateDF$fecha_dato <- ymd(dateDF$fecha_dato) # Format date
dateDF$fecha_alta <- ymd(dateDF$fecha_alta)
dateDF$year_dato <- year(dateDF$fecha_dato) # Extract year
dateDF$year_alta <- year(dateDF$fecha_alta)
dateDF$month_dato <- month(dateDF$fecha_dato,label=T) # Extract month
dateDF$month_alta <- month(dateDF$fecha_alta,label=T)
dateDF$weekday_alta <- wday(dateDF$fecha_alta,label=T)
dateDF<-as.data.table(dateDF)

ggplot(dateDF[year_alta>2009,.N,by=.(month_alta,year_alta)],aes(x = month_alta,y=N,fill=month_alta))+
  geom_bar(stat="identity")+ggtitle("Number of customers that became 'first holder' by month and year")+
  facet_wrap(~year_alta)

ggplot(dateDF[,.N,by=weekday_alta],aes(x = weekday_alta,y = N,fill=weekday_alta))+
  geom_bar(stat="identity")+ggtitle("Number of customers that became 'first holder' by day of week")



###############converting all blank values to NA#########################################

ind1 <- which(sapply(train, is.factor))
for (j in ind1) set(train, i = grep("^$|^ $", train[[j]]), j = j, value = NA_integer_)

ind2 <- which(sapply(train, is.character))
for (j in ind2) set(train, i = grep("^$|^ $", train[[j]]), j = j, value = NA_character_)



## DOn't select row where primary key ( ncodpers and fecha_dato is missing
## as we can not replace these values)
train1 <- sqldf('Select * from train where fecha_dato is NOT NULL and ncodpers is NOT NULL')

# drop all those empty rows which belongs to same customer 
train2 <- sqldf('Select * from train1 where ind_empleado is NOT NULL and pais_residencia is NOT NULL and sexo is NOT NULL and age is NOT NULL and ind_nuevo is NOT NULL  and indrel is NOT NULL and indrel_1mes is NOT NULL')

## Checking the variables which have NULL values
sapply(train2,function(x)any(is.na(x)))

#Below are the variables which have NULL values. This will be treated along with Blank values
#cod_prov
#renta
#ind_nomina_ult1
#ind_nom_pens_ult1


## To check how many columns has blank values 
char.cols <- names(train2)[sapply(train,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(train[[name]]))
  cat('\n')
}

## the variables which have blank values are :
#sexo : replace the blank with "U: Unknown"
#ult_fec_cli_1t : Leave it blank 
#indrel_1mes : 
#tiprel_1mes
#conyuemp
#canal_entrada
#nomprov
#segmento

##Misssing Value Replacement : 

train2$cod_prov[is.na(train2$cod_prov)]<- 100 ## Code for unknown province = 100
#train$cod_prov[is.na(train$ind_nomina_ult1)]<- 3 ## Code for unknown Payroll = 3
#train$cod_prov[is.na(train$ind_nom_pens_ult1)]<- 3 ## Code for unknown Pension = 3

####renta Missing Treatment#####




## Code for blanck  sex values assigned new sex category  U
table(train2$sexo)
train2$sexo[is.na(train2$sexo)] <- "U" 

## Code for blanck  Customer type assined as most frequent value as 1
table(train2$indrel_1mes)
train2$indrel_1mes[is.na(train2$indrel_1mes)] <- "1"

## Code for blanck  Customer Relation type assined as most frequent value as I
table(train2$tiprel_1mes)
train2$indrel_1mes[is.na(train2$tiprel_1mes)] <- "I"


## Code for blanck  Spouse Index assined as new category U
#table(train2$conyuemp)
#train2$indrel_1mes[is.na(train2$conyuemp)] <- "U" 


## Code for blanck  Channel assined as most frequent value as UNK
table(train2$canal_entrada)
train2$canal_entrada[is.na(train2$canal_entrada)] <- "UNK"


## Code for blanck  Province Name assined as most frequent value as UNKNOWN
table(train2$nomprov)
train2$nomprov[is.na(train2$nomprov)] <- "UNKNOWN"


## Code for blanck  Segmentation assined as most frequent value as 04-UNKNOWN
table(train2$segmento)
train2$segmento[is.na(train2$segmento)] <- "04-UNKNOWN"



########renta missing treatment###############

DT <- data.table(train2[is.na(train2$renta) == FALSE,c("nomprov","segmento","renta")])
DT <- DT[order(DT$nomprov,DT$segmento)]
# rentamedian <- DT[,Median:= median(DT$renta), by=list(DT$nomprov,DT$segmento)]

DT$nom_seg <- paste(DT$nomprov,DT$segmento)
DT <- data.table(DT[,c("nom_seg","renta")])
DT_rolled <- DT[, lapply(.SD, median), by = "nom_seg"]
head(DT_rolled)
# QC no of records
x <- unique(DT$nom_seg)

# add new flags for missing renta and imputed renta. also, create key as above
train2$renta_impute <- 0
train2$renta_missing <- ifelse(is.na(train2$renta) == TRUE,1,0)
train2_renta_missing <- subset(train2,train2$renta_missing == 1)
train2_renta_missing$nom_seg <- paste(train2_renta_missing$nomprov,train2_renta_missing$segmento)
# left join
train2_renta_imputed <- merge(x = train2_renta_missing, y = DT_rolled, by = "nom_seg", all.x = TRUE)
head(train2_renta_imputed)
colnames(train2_renta_imputed)
train2_renta_imputed$renta_impute <- train2_renta_imputed$renta.y
# remove extra columns
droplist <- c("nom_seg","renta.y")
train2_renta_imputed <- train2_renta_imputed[,!(names(train2_renta_imputed) %in% droplist)]
# rename columns if any after join
head(train2_renta_imputed)
colnames(train2_renta_imputed)
colnames(train2_renta_imputed)[24] <- "renta"

colnames(train2_renta_imputed)
#colnames(train2_renta_nonmissing)

# append both dataset( imputed and non missing) and delete unnessary columns
train2_renta_nonmissing <- subset(train2,train2$renta_missing == 0)
new_train2 <- rbind(train2_renta_nonmissing, train2_renta_imputed)
new_train2$renta <- ifelse(is.na(new_train2$renta) == TRUE, new_train2$renta_impute,new_train2$renta)
train2_new <- new_train2[,!names(new_train2) %in% c("renta_impute","rentaimpute")]

sapply(train2_new,function(x)any(is.na(x)))

## now looks for renta (Gross income of the household)
## there is lot's of variation between the providence 
## so instead of replacing with average value will assign based on Province 
dtnonmissing<- sqldf('SELECT median(renta) as renta,nomprov FROM train2_new WHERE renta is  not null 
                     group by nomprov order by nomprov ')

dtmissing<- sqldf('SELECT * FROM train2_new WHERE renta is  null order by nomprov ')

train2_renta_imputed1 <- merge(x = dtmissing, y = dtnonmissing, by = "nomprov", all.x = TRUE)
train2_renta_imputed1$renta_impute1 <- train2_renta_imputed1$renta.y




droplist1 <- c("renta.y")
train2_renta_imputed1 <- train2_renta_imputed1[,!(names(train2_renta_imputed1) %in% droplist1)]
colnames(train2_renta_imputed1)
colnames(train2_renta_imputed1)[24] <- "renta"
train2_new$renta_impute1 <- 0
train2_renta_nonmissing1 <- subset(train2_new,train2_new$renta_missing == 0)


new_train21 <- rbind(train2_renta_nonmissing1, train2_renta_imputed1)

new_train21$renta <- ifelse(is.na(new_train21$renta) == TRUE, new_train21$renta_impute1,new_train21$renta)
train2_new1 <- new_train21[,!names(new_train21) %in% c("renta_impute1")]

sum(is.na(train2_new1$renta))

################Age outlier Treatment#####################33


train2_new1$age[is.na(train2_new1$age)]  <- median(train2_new1$age,na.rm=TRUE)
summary(train2_new1$age)
quantile(train2_new1$age, c(.01, .02,.03,.04,.05,.95,.96,.97,.98, .99))
train2_new1$age[train2_new1$age < as.numeric(quantile(train2_new1$age, c(.01)))] <- as.numeric(quantile(train2_new1$age, c(.01))) 
train2_new1$age[train2_new1$age > as.numeric(quantile(train2_new1$age, c(.99)))] <- as.numeric(quantile(train2_new1$age, c(.99)))
train2_new1$age<- round(train2_new1$age)

ggplot(data=train2_new1,aes(x=log(age))) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("Age Distribution") + 
  my_theme

#par(mfrow= c(2,4))
hist(train2_new1$age, main ="Age",xlab="") 

summary(train2_new1$age)

##############renta outlier treatments#####################

quantile(train2_new1$renta, c(.01, .02,.03,.04,.05,.95,.96,.97,.98, .99))
train2_new1$renta[train2_new1$renta < as.numeric(quantile(train2_new1$renta, c(.01)))] <- as.numeric(quantile(train2_new1$renta, c(.01))) 
train2_new1$renta[train2_new1$renta > as.numeric(quantile(train2_new1$renta, c(.99)))] <- as.numeric(quantile(train2_new1$renta, c(.99)))

ggplot(data=train2_new1,aes(x=renta)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("renta Distribution") + 
  my_theme

train2_new1$renta<- round(train2_new1$renta)
#train2_new1$renta<- log(train2_new1$renta)
summary(train2_new1$renta)
ggplot(data=train2_new1,aes(x=renta)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") 
ggtitle("renta Distribution") + 
  my_theme
boxplot(train2_new1$renta)


##############antiguedad outlier treatments#####################

hist(train2_new1$antiguedad)
quantile(train2_new1$antiguedad, c(.01, .02,.03,.04,.05,.95,.96,.97,.98, .99))
summary(train2_new1$antiguedad)

train2_new1$antiguedad[train2_new1$antiguedad < as.numeric(quantile(train2_new1$antiguedad, c(.01)))] <- as.numeric(quantile(train2_new1$antiguedad, c(.01))) 
train2_new1$antiguedad[train2_new1$antiguedad > as.numeric(quantile(train2_new1$antiguedad, c(.99)))] <- as.numeric(quantile(train2_new1$antiguedad, c(.99)))


#There are two columns ("ult_fec_cli_1t", "conyuemp") with almost all values are missing. We are going to delete them from the dataframe

dsFinal <- train2_new1[,!names(train2_new1) %in% c("ult_fec_cli_1t","conyuemp","renta_missing","V1","tipodom","cod_prov")]

dsFinal$pais_residencia<-ifelse(dsFinal$pais_residencia=='ES',"ES","Others")

table(dsFinal$nomprov)
as.matrix((prop.table(table(dsFinal$nomprov))))
#CrossTable(dsFinal$nomprov,dsFinal$nomprov)
# merge all the categories which belongs to less than 2% data in single catgory called OThers

dsFinal$nomprov<-ifelse(dsFinal$nomprov=='ALICANTE',"ALICANTE",
                        ifelse(dsFinal$nomprov=='BARCELONA',"BARCELONA",
                               ifelse(dsFinal$nomprov=='CADIZ',"CADIZ",
                                      ifelse(dsFinal$nomprov=="CORUÃ'A, A","CORUÃ'A, A",
                                             ifelse(dsFinal$nomprov=='MADRID',"MADRID",
                                                    ifelse(dsFinal$nomprov=='MALAGA',"MALAGA",
                                                           ifelse(dsFinal$nomprov=='MURCIA',"MURCIA",
                                                                  ifelse(dsFinal$nomprov=='SEVILLA',"SEVILLA",
                                                                         ifelse(dsFinal$nomprov=='VALENCIA',"VALENCIA",
                                                                                ifelse(dsFinal$nomprov=='VALLADOLID',"VALLADOLID",
                                                                                       ifelse(dsFinal$nomprov=='ZARAGOZA',"ZARAGOZA",
                                                                                              ifelse(dsFinal$nomprov=='PONTEVEDRA',"PONTEVEDRA",
                                                                                                     "Others"))))))))))))



dsFinal$pais_residencia=as.factor(dsFinal$pais_residencia)
dsFinal$nomprov=as.factor(dsFinal$nomprov)

### canal category reduction  third product model###
table(dsFinal$canal_entrada)
as.matrix((prop.table(table(dsFinal$canal_entrada))))
CrossTable(dsFinal$canal_entrada,dsFinal$ind_ecue_fin_ult1)
ggplot(dsFinal ,aes(canal_entrada,fill=ind_ecue_fin_ult1))+geom_bar()+labs(title="Stacked Bar Chart",x="canal_entrada",y="Count")+theme_bw()


#### Treating canal_entrata.This variable has more than 120 categories but rnadom forest allows only 53 categories max so will reduce this variables categories 
# and make one new category for all categories which has very less values

dsFinal$canal_entrada<-ifelse(dsFinal$canal_entrada=='KHE',"KHE",
                              ifelse(dsFinal$canal_entrada=='KAT',"KAT",
                                     ifelse(dsFinal$canal_entrada=='KHQ',"KHQ",
                                            ifelse(dsFinal$canal_entrada=='KFC',"KFC",
                                                   ifelse(dsFinal$canal_entrada=='KHK',"KHK","Others")))))





table(dsFinal$tiprel_1mes)
as.matrix((prop.table(table(dsFinal$tiprel_1mes))))

dsFinal$tiprel_1mes<-ifelse(dsFinal$tiprel_1mes=='A',"A",
                            ifelse(dsFinal$tiprel_1mes=='I',"I","Others"))

table(dsFinal$indrel_1mes1)
dsFinal$indrel_1mes<-ifelse(dsFinal$indrel_1mes=='1',"1",
                            ifelse(dsFinal$indrel_1mes=='1.0',"1","Others"))

#want to remove multiple columns
#ind_ctop_fin_ult1,ind_recibo_ult1,ind_ecue_fin_ult1,ind_cno_fin_ult1, ind_plan_fin_ult1 

cols.dont.want <- c("indrel_1mes1","ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1",
                    "ind_ctju_fin_ult1","ind_ctma_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1",
                    "ind_dela_fin_ult1", "ind_fond_fin_ult1","ind_hip_fin_ult1",
                     "ind_pres_fin_ult1", "ind_reca_fin_ult1","ind_tjcr_fin_ult1","ind_valo_fin_ult1",                 
                    "ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1")

dsFinal <- dsFinal[, ! names(dsFinal) %in% cols.dont.want, drop = F]
as.matrix(sapply(dsFinal, function(x) sum(is.na(x))))




## convert the dates. There's fecha_dato, the row-identifier date The table is partitioned for this column
dsFinal$fecha_dato <- as.Date(dsFinal$fecha_dato, format ="%Y-%m-%d" )
## and fecha_alta, the date that the customer joined. The date in which the customer 
## became as the first holder of a contract in the bank
dsFinal$fecha_alta <- as.Date(dsFinal$fecha_alta,format="%Y-%m-%d")
#dsFinal$fecha_dato <-  format( dsFinal$fecha_dato, format="%m")

dsFinal[sapply(dsFinal, is.character)] <- lapply(dsFinal[sapply(dsFinal, is.character)], 
                                                 as.factor)
dsFinal[sapply(dsFinal, is.integer)] <- lapply(dsFinal[sapply(dsFinal, is.integer)], as.numeric)

str(dsFinal)



age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
table(age.cat(dsFinal$age, lower = 19, upper = 90, by = 10))
dsFinal$agebin<-age.cat(dsFinal$age, lower = 19, upper = 90, by = 10)
head(dsFinal)

renta.cat <- function(x, lower = 0, upper, by = 50000,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
table(renta.cat(dsFinal$renta, lower = 26000, upper = 538800, by = 50000))
dsFinal$rentabin<-renta.cat(dsFinal$renta, lower = 26000, upper = 538800, by = 50000)
str(dsFinal)
dsFinal$age<-dsFinal$agebin
dsFinal$renta<-dsFinal$rentabin

cols.dont.want.bin <- c("agebin","rentabin")

dsFinal <- dsFinal[, ! names(dsFinal) %in% cols.dont.want.bin, drop = F]

str(dsFinal)

############# Performing Chi square test for testing independece between target and categorical variales############

##########Top 5 products with highest number of counts are ind_ctop_fin_ult1,ind_recibo_ult1,ind_ecue_fin_ult1,ind_cno_fin_ult1,ind_plan_fin_ult1 



###################Tests for ind_ctop_fin_ult1#########################################

### test between ind_ctop_fin_ult1 and ind_empleado###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$ind_empleado) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$ind_empleado))


### test between ind_ctop_fin_ult1 and pais_residencia###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$pais_residencia) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$pais_residencia))

### test between ind_ctop_fin_ult1 and sexo###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$sexo) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$sexo))

### test between ind_ctop_fin_ult1 and indrel_1mes###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$indrel_1mes) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$indrel_1mes))

### test between ind_ctop_fin_ult1 and tiprel_1mes###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$tiprel_1mes) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$tiprel_1mes))


### test between ind_ctop_fin_ult1 and indresi###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$indresi) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$indresi))


### test between ind_ctop_fin_ult1 and indext###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$indext) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$indext))


### test between ind_ctop_fin_ult1 and canal_entrada###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$canal_entrada) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$canal_entrada))


### test between ind_ctop_fin_ult1 and indfall###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$indfall) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$indfall))


### test between ind_ctop_fin_ult1 and nomprov###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$nomprov) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$nomprov))


### test between ind_ctop_fin_ult1 and segmento###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$segmento) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$segmento))

### test between ind_ctop_fin_ult1 and ind_actividad_cliente###

table(dsFinal$ind_ctop_fin_ult1, dsFinal$ind_actividad_cliente) 

chisq.test(table(dsFinal$ind_ctop_fin_ult1,dsFinal$ind_actividad_cliente))


###################Tests for ind_recibo_ult1#########################################






### test between ind_recibo_ult1 and ind_empleado###

table(dsFinal$ind_recibo_ult1, dsFinal$ind_empleado) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$ind_empleado))


### test between ind_recibo_ult1 and pais_residencia###

table(dsFinal$ind_recibo_ult1, dsFinal$pais_residencia) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$pais_residencia))

### test between ind_recibo_ult1 and sexo###

table(dsFinal$ind_recibo_ult1, dsFinal$sexo) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$sexo))

### test between ind_recibo_ult1 and indrel_1mes###

table(dsFinal$ind_recibo_ult1, dsFinal$indrel_1mes) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$indrel_1mes))

### test between ind_recibo_ult1 and tiprel_1mes###

table(dsFinal$ind_recibo_ult1, dsFinal$tiprel_1mes) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$tiprel_1mes))


### test between ind_recibo_ult1 and indresi###

table(dsFinal$ind_recibo_ult1, dsFinal$indresi) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$indresi))


### test between ind_recibo_ult1 and indext###

table(dsFinal$ind_recibo_ult1, dsFinal$indext) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$indext))


### test between ind_recibo_ult1 and canal_entrada###

table(dsFinal$ind_recibo_ult1, dsFinal$canal_entrada) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$canal_entrada))


### test between ind_recibo_ult1 and indfall###

table(dsFinal$ind_recibo_ult1, dsFinal$indfall) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$indfall))


### test between ind_recibo_ult1 and nomprov###

table(dsFinal$ind_recibo_ult1, dsFinal$nomprov) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$nomprov))


### test between ind_recibo_ult1 and segmento###

table(dsFinal$ind_recibo_ult1, dsFinal$segmento) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$segmento))

### test between ind_recibo_ult1 and ind_actividad_cliente###

table(dsFinal$ind_recibo_ult1, dsFinal$ind_actividad_cliente) 

chisq.test(table(dsFinal$ind_recibo_ult1,dsFinal$ind_actividad_cliente))


###################Tests for ind_ecue_fin_ult1#########################################





### test between ind_ecue_fin_ult1 and ind_empleado###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$ind_empleado) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$ind_empleado))


### test between ind_ecue_fin_ult1 and pais_residencia###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$pais_residencia) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$pais_residencia))

### test between ind_ecue_fin_ult1 and sexo###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$sexo) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$sexo))

### test between ind_ecue_fin_ult1 and indrel_1mes###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$indrel_1mes) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$indrel_1mes))

### test between ind_ecue_fin_ult1 and tiprel_1mes###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$tiprel_1mes) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$tiprel_1mes))


### test between ind_ecue_fin_ult1 and indresi###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$indresi) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$indresi))


### test between ind_ecue_fin_ult1 and indext###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$indext) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$indext))


### test between ind_ecue_fin_ult1 and canal_entrada###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$canal_entrada) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$canal_entrada))


### test between ind_ecue_fin_ult1 and indfall###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$indfall) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$indfall))


### test between ind_ecue_fin_ult1 and nomprov###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$nomprov) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$nomprov))


### test between ind_ecue_fin_ult1 and segmento###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$segmento) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$segmento))

### test between ind_ecue_fin_ult1 and ind_actividad_cliente###

table(dsFinal$ind_ecue_fin_ult1, dsFinal$ind_actividad_cliente) 

chisq.test(table(dsFinal$ind_ecue_fin_ult1,dsFinal$ind_actividad_cliente))



############################test for 4th Product ind_cno_fin_ult1#################





### test between ind_cno_fin_ult1 and ind_empleado###

table(dsFinal$ind_cno_fin_ult1, dsFinal$ind_empleado) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$ind_empleado))


### test between ind_cno_fin_ult1 and pais_residencia###

table(dsFinal$ind_cno_fin_ult1, dsFinal$pais_residencia) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$pais_residencia))

### test between ind_cno_fin_ult1 and sexo###

table(dsFinal$ind_cno_fin_ult1, dsFinal$sexo) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$sexo))

### test between ind_cno_fin_ult1 and indrel_1mes###

table(dsFinal$ind_cno_fin_ult1, dsFinal$indrel_1mes) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$indrel_1mes))

### test between ind_cno_fin_ult1 and tiprel_1mes###

table(dsFinal$ind_cno_fin_ult1, dsFinal$tiprel_1mes) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$tiprel_1mes))


### test between ind_cno_fin_ult1 and indresi###

table(dsFinal$ind_cno_fin_ult1, dsFinal$indresi) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$indresi))


### test between ind_cno_fin_ult1 and indext###

table(dsFinal$ind_cno_fin_ult1, dsFinal$indext) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$indext))


### test between ind_cno_fin_ult1 and canal_entrada###

table(dsFinal$ind_cno_fin_ult1, dsFinal$canal_entrada) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$canal_entrada))


### test between ind_cno_fin_ult1 and indfall###

table(dsFinal$ind_cno_fin_ult1, dsFinal$indfall) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$indfall))


### test between ind_cno_fin_ult1 and nomprov###

table(dsFinal$ind_cno_fin_ult1, dsFinal$nomprov) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$nomprov))


### test between ind_cno_fin_ult1 and segmento###

table(dsFinal$ind_cno_fin_ult1, dsFinal$segmento) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$segmento))

### test between ind_cno_fin_ult1 and ind_actividad_cliente###

table(dsFinal$ind_cno_fin_ult1, dsFinal$ind_actividad_cliente) 

chisq.test(table(dsFinal$ind_cno_fin_ult1,dsFinal$ind_actividad_cliente))

###################Tests for ind_plan_fin_ult1#########################################
str(dsFinal)
### test between ind_plan_fin_ult1 and ind_empleado###

table(dsFinal$ind_plan_fin_ult1, dsFinal$ind_empleado) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$ind_empleado))


### test between ind_plan_fin_ult1 and pais_residencia###

table(dsFinal$ind_plan_fin_ult1, dsFinal$pais_residencia) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$pais_residencia))

### test between ind_plan_fin_ult1 and sexo###

table(dsFinal$ind_plan_fin_ult1, dsFinal$sexo) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$sexo))

### test between ind_plan_fin_ult1 and indrel_1mes###

table(dsFinal$ind_plan_fin_ult1, dsFinal$indrel_1mes) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$indrel_1mes))

### test between ind_plan_fin_ult1 and tiprel_1mes###

table(dsFinal$ind_plan_fin_ult1, dsFinal$tiprel_1mes) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$tiprel_1mes))


### test between ind_plan_fin_ult1 and indresi###

table(dsFinal$ind_plan_fin_ult1, dsFinal$indresi) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$indresi))


### test between ind_plan_fin_ult1 and indext###

table(dsFinal$ind_plan_fin_ult1, dsFinal$indext) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$indext))


### test between ind_plan_fin_ult1 and canal_entrada###

table(dsFinal$ind_plan_fin_ult1, dsFinal$canal_entrada) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$canal_entrada))


### test between ind_plan_fin_ult1 and indfall###

table(dsFinal$ind_plan_fin_ult1, dsFinal$indfall) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$indfall))


### test between ind_plan_fin_ult1 and nomprov###

table(dsFinal$ind_plan_fin_ult1, dsFinal$nomprov) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$nomprov))


### test between ind_plan_fin_ult1 and segmento###

table(dsFinal$ind_plan_fin_ult1, dsFinal$segmento) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$segmento))

### test between ind_plan_fin_ult1 and ind_actividad_cliente###

table(dsFinal$ind_plan_fin_ult1, dsFinal$ind_actividad_cliente) 

chisq.test(table(dsFinal$ind_plan_fin_ult1,dsFinal$ind_actividad_cliente))



####### Based on Chi square test categorical variables who have association(dependecy) with Products are below

# ind_ctop_fin_ult1(dependent) > ind_empleado,sexo,tiprel_1mes,indext,canal_entrada,indfall,nomprov,segmento,ind_actividad_cliente
#ind_ctop_fin_ult1(Independent) >indrel_1mes,pais_residencia,indresi


#ind_recibo_ult1 (dependent) > ind_empleado,sexo,indrel_1mes,tiprel_1mes,canal_entrada,indfall,nomprov,segmento,ind_actividad_cliente
#ind_recibo_ult1 (Independent) >indext,pais_residencia,indresi

#ind_ecue_fin_ult1 (dependent) > ind_empleado,pais_residencia,sexo,indrel_1mes,tiprel_1mes,indext,indresi,canal_entrada,indfall,nomprov,segmento,ind_actividad_cliente
#ind_ecue_fin_ult1 (Independent) > none

#ind_cno_fin_ult1 (dependent) > ind_empleado,sexo,indrel_1mes,tiprel_1mes,canal_entrada,indfall,nomprov,segmento,ind_actividad_cliente
#ind_cno_fin_ult1 (Independent) > pais_residencia,indresi,indext


## Model bUILDING code START








############  random forest code start

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## Building the model using Random Forest for Product ind_ctop_fin_ult1

##ind_ctop_fin_ult1,ind_recibo_ult1,ind_ecue_fin_ult1,ind_cno_fin_ult1,

## creating devlopment and testing data
set.seed(123)
n <- nrow(dsFinal)
shuffledDf <- dsFinal[sample(n),]
RFDF.dev <- shuffledDf[1:round(n*0.7),]
RFDF.holdout <- shuffledDf[(round(n*0.7)+1):n,]

#RFDF.dev$fecha_dato <-  format( RFDF.dev$fecha_dato, format="%m")
#write.csv(RFDF.dev,"RFDF.dev.csv")
#write.csv(RFDF.holdout,"RFDF.holdout.csv")

RFDF.dev.BACKUP<- RFDF.dev
RFDF.holdout.BACKUP<-RFDF.holdout

#RFDF.dev<-RFDF.dev.BACKUP
#RFDF.holdout<-RFDF.holdout.BACKUP

#ind_ctop_fin_ult1,ind_recibo_ult1,ind_ecue_fin_ult1,ind_cno_fin_ult1, ind_plan_fin_ult1 

RFDF.dev1<- RFDF.dev[,!names(RFDF.dev) %in% c("ind_recibo_ult1","ind_ecue_fin_ult1","ind_cno_fin_ult1","ind_plan_fin_ult1")]
RFDF.holdout1<- RFDF.holdout[,!names(RFDF.dev) %in% c("ind_recibo_ult1","ind_ecue_fin_ult1","ind_cno_fin_ult1","ind_plan_fin_ult1")]

RFDF.dev2<-RFDF.dev[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_ecue_fin_ult1","ind_cno_fin_ult1","ind_plan_fin_ult1")]
RFDF.holdout2<-RFDF.holdout[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_ecue_fin_ult1","ind_cno_fin_ult1","ind_plan_fin_ult1")]

RFDF.dev3<-RFDF.dev[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_recibo_ult1","ind_cno_fin_ult1","ind_plan_fin_ult1")]
RFDF.holdout3<-RFDF.holdout[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_recibo_ult1","ind_cno_fin_ult1","ind_plan_fin_ult1")]

RFDF.dev4<-RFDF.dev[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_recibo_ult1","ind_ecue_fin_ult1","ind_plan_fin_ult1")]
RFDF.holdout4<-RFDF.holdout[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_recibo_ult1","ind_ecue_fin_ult1","ind_plan_fin_ult1")]

RFDF.dev5<-RFDF.dev[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_recibo_ult1","ind_ecue_fin_ult1","ind_cno_fin_ult1","pais_residencia","indrel_1mes","indresi","indext")]
RFDF.holdout5<-RFDF.holdout[,!names(RFDF.dev) %in% c("ind_ctop_fin_ult1","ind_recibo_ult1","ind_ecue_fin_ult1","ind_cno_fin_ult1","pais_residencia","indrel_1mes","indresi","indext")]

############## First model start
rm(RFDF.dev)
rm(RFDF.holdout)
RFDF.dev<- RFDF.dev1
RFDF.holdout<-RFDF.holdout1

str(RFDF.dev)
str(RFDF.holdout)
##install.packages("randomForest")
#library(randomForest)
#Model for product ind_ctop_fin_ult1
## ?randomForest
## Calling syntax to build the Random Forest


str(RFDF.dev) 

RF <- randomForest(as.factor(ind_ctop_fin_ult1) ~ ., data = RFDF.dev[-2], 
                   ntree=100, mtry = 4, nodesize = 100,
                   importance=TRUE)
print(RF)
par(mfrow=c(4,2))
par(mar = rep(2, 4))
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]
#as.matrix(sapply(RFDF.dev, function(x) sum(is.na(x))))

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(2,21)],   #-c(2,21)]
              y=as.factor(RFDF.dev$ind_ctop_fin_ult1),
              mtryStart = 4, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize =2000, 
              importance=TRUE
)

#Evaluate variable importance
importance(tRF)
varImpPlot(tRF)

## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
#View(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
#library(data.table)
RFDF.dev$ind_ctop_fin_ult1<- as.numeric(RFDF.dev$ind_ctop_fin_ult1)
tmp_DT = data.table(RFDF.dev)

rank <- tmp_DT[, list(
  cnt = length(ind_ctop_fin_ult1), 
  cnt_resp = sum(ind_ctop_fin_ult1), 
  cnt_non_resp = sum(ind_ctop_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$ind_ctop_fin_ult1)
perf <- performance(pred, "tpr", "fpr")

#for lift chart
# Get data for ROC curve
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")



plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(ind_ctop_fin_ult1, predict.class))
auc
KS
gini



#Confusion Matrix
library(caret)
confusionMatrix(RFDF.dev$predict.class,RFDF.dev$ind_ctop_fin_ult1)


################ Validating in Holdout

## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(ind_ctop_fin_ult1, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
rank <- tmp_DT[, list(
  cnt = length(ind_ctop_fin_ult1), 
  cnt_resp = sum(ind_ctop_fin_ult1), 
  cnt_non_resp = sum(ind_ctop_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.holdout$predict.score[,2], RFDF.holdout$ind_ctop_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for ROC curve
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.holdout$predict.score[,2], type="Gini")

with(RFDF.holdout, table(ind_ctop_fin_ult1, predict.class))
auc
KS
gini

confusionMatrix(RFDF.holdout$predict.class,RFDF.holdout$ind_ctop_fin_ult1)
#View(RFDF.holdout)
#write.csv(RFDF.dev, file = "ind_ctop_fin_ult1_RFDFdev.csv",row.names=FALSE)
#write.csv(RFDF.holdout, file = "ind_ctop_fin_ult1_RFDFholdout.csv",row.names=FALSE)



######################################First model end

############## Second model start
rm(RFDF.dev)
rm(RFDF.holdout)
RFDF.dev<- RFDF.dev2
RFDF.holdout<-RFDF.holdout2

str(RFDF.dev)
str(RFDF.holdout)
##install.packages("randomForest")
#library(randomForest)
#Model for product ind_recibo_ult1
## ?randomForest
## Calling syntax to build the Random Forest


str(RFDF.dev) 

RF <- randomForest(as.factor(ind_recibo_ult1) ~ ., data = RFDF.dev[-2], 
                   ntree=100, mtry = 4, nodesize = 200,
                   importance=TRUE)
print(RF)
par(mfrow=c(4,2))
par(mar = rep(2, 4))
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]
#as.matrix(sapply(RFDF.dev, function(x) sum(is.na(x))))

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(2,21)],   #-c(2,21)]
              y=as.factor(RFDF.dev$ind_recibo_ult1),
              mtryStart = 4, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize =2800, 
              importance=TRUE
)

#Evaluate variable importance
importance(tRF)
varImpPlot(tRF)

## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
#View(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
#library(data.table)
RFDF.dev$ind_recibo_ult1<- as.numeric(RFDF.dev$ind_recibo_ult1)
tmp_DT = data.table(RFDF.dev)

rank <- tmp_DT[, list(
  cnt = length(ind_recibo_ult1), 
  cnt_resp = sum(ind_recibo_ult1), 
  cnt_non_resp = sum(ind_recibo_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$ind_recibo_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(ind_recibo_ult1, predict.class))
auc
KS
gini



#Confusion Matrix
library(caret)
confusionMatrix(RFDF.dev$predict.class,RFDF.dev$ind_recibo_ult1)


################ Validating in Holdout

## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(ind_recibo_ult1, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
rank <- tmp_DT[, list(
  cnt = length(ind_recibo_ult1), 
  cnt_resp = sum(ind_recibo_ult1), 
  cnt_non_resp = sum(ind_recibo_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.holdout$predict.score[,2], RFDF.holdout$ind_recibo_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.holdout$predict.score[,2], type="Gini")

with(RFDF.holdout, table(ind_recibo_ult1, predict.class))
auc
KS
gini

confusionMatrix(RFDF.holdout$predict.class,RFDF.holdout$ind_recibo_ult1)
#View(RFDF.holdout)
#write.csv(RFDF.dev, file = "ind_recibo_ult1_RFDFdev.csv",row.names=FALSE)
#write.csv(RFDF.holdout, file = "ind_recibo_ult1_RFDFholdout.csv",row.names=FALSE)



###################################### Second model end

############## Third model start
rm(RFDF.dev)
rm(RFDF.holdout)
RFDF.dev<- RFDF.dev3
RFDF.holdout<-RFDF.holdout3

str(RFDF.dev)
str(RFDF.holdout)
##install.packages("randomForest")
#library(randomForest)
#Model for product ind_ecue_fin_ult1
## ?randomForest
## Calling syntax to build the Random Forest


str(RFDF.dev) 

RF <- randomForest(as.factor(ind_ecue_fin_ult1) ~ ., data = RFDF.dev[-2], 
                   ntree=100, mtry = 4, nodesize = 200,
                   importance=TRUE)
print(RF)
par(mfrow=c(4,2))
par(mar = rep(2, 4))
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]
#as.matrix(sapply(RFDF.dev, function(x) sum(is.na(x))))

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(2,21)],   #-c(2,21)]
              y=as.factor(RFDF.dev$ind_ecue_fin_ult1),
              mtryStart = 4, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize =1000, 
              importance=TRUE
)

#Evaluate variable importance
importance(tRF)
varImpPlot(tRF)

## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
View(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
#library(data.table)
RFDF.dev$ind_ecue_fin_ult1<- as.numeric(RFDF.dev$ind_ecue_fin_ult1)
tmp_DT = data.table(RFDF.dev)

rank <- tmp_DT[, list(
  cnt = length(ind_ecue_fin_ult1), 
  cnt_resp = sum(ind_ecue_fin_ult1), 
  cnt_non_resp = sum(ind_ecue_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$ind_ecue_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(ind_ecue_fin_ult1, predict.class))
auc
KS
gini



#Confusion Matrix
#library(caret)
confusionMatrix(RFDF.dev$predict.class,RFDF.dev$ind_ecue_fin_ult1)


################ Validating in Holdout

## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(ind_ecue_fin_ult1, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
rank <- tmp_DT[, list(
  cnt = length(ind_ecue_fin_ult1), 
  cnt_resp = sum(ind_ecue_fin_ult1), 
  cnt_non_resp = sum(ind_ecue_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.holdout$predict.score[,2], RFDF.holdout$ind_ecue_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.holdout$predict.score[,2], type="Gini")

with(RFDF.holdout, table(ind_ecue_fin_ult1, predict.class))
auc
KS
gini

confusionMatrix(RFDF.holdout$predict.class,RFDF.holdout$ind_ecue_fin_ult1)
#View(RFDF.holdout)
write.csv(RFDF.dev, file = "ind_ecue_fin_ult1_RFDFdev.csv",row.names=FALSE)
write.csv(RFDF.holdout, file = "ind_ecue_fin_ult1_RFDFholdout.csv",row.names=FALSE)



###################################### Third model end

############## Fourth model start
rm(RFDF.dev)
rm(RFDF.holdout)
RFDF.dev<- RFDF.dev4
RFDF.holdout<-RFDF.holdout4

str(RFDF.dev)
str(RFDF.holdout)
##install.packages("randomForest")
#library(randomForest)
#Model for product ind_cno_fin_ult1
## ?randomForest
## Calling syntax to build the Random Forest


str(RFDF.dev) 

RF <- randomForest(as.factor(ind_cno_fin_ult1) ~ ., data = RFDF.dev[-2], 
                   ntree=100, mtry = 4, nodesize = 200,
                   importance=TRUE)
print(RF)
par(mfrow=c(4,2))
par(mar = rep(2, 4))
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]
#as.matrix(sapply(RFDF.dev, function(x) sum(is.na(x))))

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(2,21)],   #-c(2,21)]
              y=as.factor(RFDF.dev$ind_cno_fin_ult1),
              mtryStart = 4, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize =900, 
              importance=TRUE
)

#Evaluate variable importance
importance(tRF)
varImpPlot(tRF)

## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
View(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
#library(data.table)
RFDF.dev$ind_cno_fin_ult1<- as.numeric(RFDF.dev$ind_cno_fin_ult1)
tmp_DT = data.table(RFDF.dev)

rank <- tmp_DT[, list(
  cnt = length(ind_cno_fin_ult1), 
  cnt_resp = sum(ind_cno_fin_ult1), 
  cnt_non_resp = sum(ind_cno_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$ind_cno_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(ind_cno_fin_ult1, predict.class))
auc
KS
gini



#Confusion Matrix
#library(caret)
confusionMatrix(RFDF.dev$predict.class,RFDF.dev$ind_cno_fin_ult1)


################ Validating in Holdout

## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(ind_cno_fin_ult1, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
rank <- tmp_DT[, list(
  cnt = length(ind_cno_fin_ult1), 
  cnt_resp = sum(ind_cno_fin_ult1), 
  cnt_non_resp = sum(ind_cno_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.holdout$predict.score[,2], RFDF.holdout$ind_cno_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.holdout$predict.score[,2], type="Gini")

with(RFDF.holdout, table(ind_cno_fin_ult1, predict.class))
auc
KS
gini

confusionMatrix(RFDF.holdout$predict.class,RFDF.holdout$ind_cno_fin_ult1)
#View(RFDF.holdout)
#write.csv(RFDF.dev, file = "ind_cno_fin_ult1_RFDFdev.csv",row.names=FALSE)
#write.csv(RFDF.holdout, file = "ind_cno_fin_ult1_RFDFholdout.csv",row.names=FALSE)



###################################### Fourth model end

######################### fifth model start
rm(RFDF.dev)
rm(RFDF.holdout)
RFDF.dev<- RFDF.dev5
RFDF.holdout<-RFDF.holdout5

str(RFDF.dev)
str(RFDF.holdout)
##install.packages("randomForest")
#library(randomForest)
#Model for product ind_plan_fin_ult1
## ?randomForest
## Calling syntax to build the Random Forest


str(RFDF.dev) 

RF <- randomForest(as.factor(ind_plan_fin_ult1) ~ ., data = RFDF.dev[-2], 
                   ntree=100, mtry = 4, nodesize = 100,
                   importance=TRUE)
print(RF)
par(mfrow=c(4,2))
par(mar = rep(2, 4))
plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate
##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]
#as.matrix(sapply(RFDF.dev, function(x) sum(is.na(x))))

## Tuning Random Forest
tRF <- tuneRF(x = RFDF.dev[,-c(2,17)],   #-c(2,21)]
              y=as.factor(RFDF.dev$ind_plan_fin_ult1),
              mtryStart = 4, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize =900, 
              importance=TRUE
)



## Scoring syntax
RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
View(RFDF.dev)

## deciling
RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


## Ranking code
#library(data.table)
RFDF.dev$ind_plan_fin_ult1<- as.numeric(RFDF.dev$ind_plan_fin_ult1)
tmp_DT = data.table(RFDF.dev)

rank <- tmp_DT[, list(
  cnt = length(ind_plan_fin_ult1), 
  cnt_resp = sum(ind_plan_fin_ult1), 
  cnt_non_resp = sum(ind_plan_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$ind_plan_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")

with(RFDF.dev, table(ind_plan_fin_ult1, predict.class))
auc
KS
gini



#Confusion Matrix
#library(caret)
confusionMatrix(RFDF.dev$predict.class,RFDF.dev$ind_plan_fin_ult1)


################ Validating in Holdout

## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")
with(RFDF.holdout, table(ind_plan_fin_ult1, predict.class))

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])
tmp_DT = data.table(RFDF.holdout)
rank <- tmp_DT[, list(
  cnt = length(ind_plan_fin_ult1), 
  cnt_resp = sum(ind_plan_fin_ult1), 
  cnt_non_resp = sum(ind_plan_fin_ult1 == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
View(rank)

#library(ROCR)
pred <- prediction(RFDF.holdout$predict.score[,2], RFDF.holdout$ind_plan_fin_ult1)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Get data for lift chart
lift.obj <- performance(pred, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#library(ineq)
gini = ineq(RFDF.holdout$predict.score[,2], type="Gini")

with(RFDF.holdout, table(ind_plan_fin_ult1, predict.class))
auc
KS
gini

confusionMatrix(RFDF.holdout$predict.class,RFDF.holdout$ind_plan_fin_ult1)
#View(RFDF.holdout)
#write.csv(RFDF.dev, file = "ind_plan_fin_ult1_RFDFdev.csv",row.names=FALSE)
#write.csv(RFDF.holdout, file = "ind_plan_fin_ult1_RFDFholdout.csv",row.names=FALSE)



######################################End OF 5th product

