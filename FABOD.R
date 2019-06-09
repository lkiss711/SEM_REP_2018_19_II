# László Kiss laci.kiss@gmail.com
# The first market is commented but this comments are relevant to the other parts of the code




library("HighDimOut")
library("ggplot2")
library("plot3D")
library("plotly")
library("dplyr")
library("tidyr")
library("htmlwidgets")

# read csv
WSH_HAW_SEATT_ONE_DAY <- read.csv("c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/dataset/WSH_HAW_SEATT_ONE_DAY.csv", stringsAsFactors=FALSE)


# filter data by region3,vendor
WSH_ONE_DAY <- WSH_HAW_SEATT_ONE_DAY %>% 
  select(model,iOS_version,ruleid,region3,vendor,inc_ratio) %>% 
  filter(region3 == "WASHINGTON DC" & vendor == "Apple")




SEATT_ONE_DAY <- WSH_HAW_SEATT_ONE_DAY %>% 
  select(model,iOS_version,ruleid,region3,vendor,inc_ratio) %>% 
  filter(region3 == "SEATTLE" & vendor == "Apple")


HAW_ONE_DAY <- WSH_HAW_SEATT_ONE_DAY %>% 
  select(model,iOS_version,ruleid,region3,vendor,inc_ratio) %>% 
  filter(region3 == "HAWAII" & vendor == "Apple")




# drop region3, vendor fileds and unite the model and iOS field
WSH_ONE_DAY <- as.data.frame(cbind(paste(WSH_ONE_DAY[,1],WSH_ONE_DAY[,2]),WSH_ONE_DAY[,3],WSH_ONE_DAY[,6]),stringsAsFactors = FALSE)


# create column names
colnames(WSH_ONE_DAY) <- c("model_iOS","ruleid","inc_ratio")


# set double type to inc_ratio
WSH_ONE_DAY$inc_ratio <- mapply(as.double,WSH_ONE_DAY$inc_ratio)


# data transform (incident types to columns)
WSH_ONE_DAY <- spread(WSH_ONE_DAY,"ruleid","inc_ratio",fill = 0)


# apply to Fast ABOD to a dataset
res.FABOD_WSH <- Func.FBOD(data=WSH_ONE_DAY[,c("MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")],iter = 10,k.nn = 5)

# apply to an original ABOD to a dataset, it is commented because of the long running time
# res.ABOD_WSH <- Func.ABOD(data=WSH_ONE_DAY[,c("MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")], basic=FALSE, perc=0.2)



# create data.temp to plot


data.temp <- WSH_ONE_DAY[,c("model_iOS","MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")]


# add new column to identify the outliers
data.temp$Ind <- NA
# determine the first 5 element as an outlier
data.temp[order(res.FABOD_WSH, decreasing = FALSE)[1:5],"Ind"] <- "Outlier"


# determine the others as an inlier
data.temp[is.na(data.temp$Ind),"Ind"] <- "Inlier"


# factorise the data.temp$Ind column (it is necessary to plot categorized by this column)
data.temp$Ind <- factor(data.temp$Ind)


# ggplot(data = data.temp) + geom_point(aes(x = MUTING_VOLTE_VOICE, y = GARBLING_VOLTE_VOICE, color=Ind, shape=Ind))
# scatter3D(x = data.temp$MUTING_VOLTE_VOICE, y = data.temp$GARBLING_VOLTE_VOICE,z = data.temp$SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS)


# View(res.FABOD_WSH)
# am <- WSH_ONE_DAY$model_iOS


# fill the data.temp$Ind column when the row of matrix represent an outlier
data.temp[order(res.FABOD_WSH, decreasing = FALSE)[1:5],"Ind"] <- "Outlier"


# create a matrix just for the outlier
abod_outlier_WSH <- data.temp %>% 
  filter(Ind == "Outlier")


# plot just the raw data https://plot.ly/r
p_by_dim_WSH <- plot_ly(data.temp, x = ~model_iOS, y = ~MUTING_VOLTE_VOICE,name = 'MUTING', type = 'bar') %>% 
  add_trace(y = ~GARBLING_VOLTE_VOICE, name = 'GARBLING') %>% 
  add_trace(y = ~SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS, name = 'SOFT_DROP') %>% 
  layout(
    yaxis = list(title = 'inc_ratio_by_rule')
  )


p_by_dim_WSH

saveWidget(p_by_dim_WSH,file = "c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/plots/p_by_dim_WSH.html")


# plot the inliers and outliers by top 3 incident in one market https://plot.ly/r
p_WSH <- plot_ly(data.temp, x = ~MUTING_VOLTE_VOICE, y = ~GARBLING_VOLTE_VOICE,
                 z = ~SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS, color = ~Ind, 
                 colors = c('#BF382A', '#0C4B8E'),
                 text = WSH_ONE_DAY$model_iOS) %>%
  add_markers() %>%
  layout(title = 'Apple devices in top 30 model with iOS verison by 3 top incidents (WASHINGTON DC market)' ,
         scene = list(xaxis = list(title = 'MUTING_VOLTE_VOICE'),
                      yaxis = list(title = 'GARBLING_VOLTE_VOICE'),
                      zaxis = list(title = 'SOFT_DROP_DUE')))
p_WSH

saveWidget(p_WSH,file = "c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/plots/p_WSH.html")



SEATT_ONE_DAY <- as.data.frame(cbind(paste(SEATT_ONE_DAY[,1],SEATT_ONE_DAY[,2]),SEATT_ONE_DAY[,3],SEATT_ONE_DAY[,6]),stringsAsFactors = FALSE)


colnames(SEATT_ONE_DAY) <- c("model_iOS","ruleid","inc_ratio")


SEATT_ONE_DAY$inc_ratio <- mapply(as.double,SEATT_ONE_DAY$inc_ratio)


SEATT_ONE_DAY <- spread(SEATT_ONE_DAY,"ruleid","inc_ratio",fill = 0)


res.FABOD_SEATT <- Func.FBOD(data=SEATT_ONE_DAY[,c("MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")], iter = 10,k.nn = 5)
# res.ABOD_SEATT <- Func.ABOD(data=SEATT_ONE_DAY[,c("MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")], basic=FALSE, perc=0.2)

data.temp <- SEATT_ONE_DAY[,c("model_iOS","MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")]




data.temp$Ind <- NA
data.temp[order(res.FABOD_SEATT, decreasing = FALSE)[1:5],"Ind"] <- "Outlier"
data.temp[is.na(data.temp$Ind),"Ind"] <- "Inlier"
data.temp$Ind <- factor(data.temp$Ind)
# ggplot(data = data.temp) + geom_point(aes(x = MUTING_VOLTE_VOICE, y = GARBLING_VOLTE_VOICE, color=Ind, shape=Ind))
# scatter3D(x = data.temp$MUTING_VOLTE_VOICE, y = data.temp$GARBLING_VOLTE_VOICE,z = data.temp$SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS)




# am <- SEATT_ONE_DAY$model_iOS




data.temp[order(res.FABOD_SEATT, decreasing = FALSE)[1:5],"Ind"] <- "Outlier"


abod_outlier_SEATT <- data.temp %>% 
  filter(Ind == "Outlier")




p_by_dim_SEATT <- plot_ly(data.temp, x = ~model_iOS, y = ~MUTING_VOLTE_VOICE,name = 'MUTING', type = 'bar') %>% 
  add_trace(y = ~GARBLING_VOLTE_VOICE, name = 'GARBLING') %>% 
  add_trace(y = ~SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS, name = 'SOFT_DROP') %>% 
  layout(
    yaxis = list(title = 'inc_ratio_by_rule')
  )


p_by_dim_SEATT

saveWidget(p_by_dim_SEATT,file = "c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/plots/p_by_dim_SEATT.html")









p_SEATT <- plot_ly(data.temp, x = ~MUTING_VOLTE_VOICE, y = ~GARBLING_VOLTE_VOICE,
                   z = ~SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS, color = ~Ind, 
                   colors = c('#BF382A', '#0C4B8E'),
                   text = SEATT_ONE_DAY$model_iOS) %>%
  add_markers() %>%
  layout(title = 'Apple devices in top 30 model with iOS verison by 3 top incidents (SEATTLE market)' ,
         scene = list(xaxis = list(title = 'MUTING_VOLTE_VOICE'),
                      yaxis = list(title = 'GARBLING_VOLTE_VOICE'),
                      zaxis = list(title = 'SOFT_DROP_DUE')))
p_SEATT

saveWidget(p_SEATT,file = "c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/plots/p_SEATT.html")



HAW_ONE_DAY <- as.data.frame(cbind(paste(HAW_ONE_DAY[,1],HAW_ONE_DAY[,2]),HAW_ONE_DAY[,3],HAW_ONE_DAY[,6]),stringsAsFactors = FALSE)


colnames(HAW_ONE_DAY) <- c("model_iOS","ruleid","inc_ratio")


HAW_ONE_DAY$inc_ratio <- mapply(as.double,HAW_ONE_DAY$inc_ratio)


HAW_ONE_DAY <- spread(HAW_ONE_DAY,"ruleid","inc_ratio",fill = 0)


res.FABOD_HAW <- Func.FBOD(data=HAW_ONE_DAY[,c("MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")], iter = 10, k.nn = 5)
#res.ABOD_HAW <- Func.ABOD(data=HAW_ONE_DAY[,c("MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")], basic=FALSE, perc=0.2)

data.temp <- HAW_ONE_DAY[,c("model_iOS","MUTING_VOLTE_VOICE","GARBLING_VOLTE_VOICE","SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS")]


data.temp$Ind <- NA
data.temp[order(res.FABOD_HAW, decreasing = FALSE)[1:5],"Ind"] <- "Outlier"
data.temp[is.na(data.temp$Ind),"Ind"] <- "Inlier"
data.temp$Ind <- factor(data.temp$Ind)
# ggplot(data = data.temp) + geom_point(aes(x = MUTING_VOLTE_VOICE, y = GARBLING_VOLTE_VOICE, color=Ind, shape=Ind))
# scatter3D(x = data.temp$MUTING_VOLTE_VOICE, y = data.temp$GARBLING_VOLTE_VOICE,z = data.temp$SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS)




# am <- HAW_ONE_DAY$model_iOS




data.temp[order(res.FABOD_HAW, decreasing = FALSE)[1:5],"Ind"] <- "Outlier"


abod_outlier_HAW <- data.temp %>% 
  filter(Ind == "Outlier")




p_by_dim_HAW <- plot_ly(data.temp, x = ~model_iOS, y = ~MUTING_VOLTE_VOICE,name = 'MUTING', type = 'bar') %>% 
  add_trace(y = ~GARBLING_VOLTE_VOICE, name = 'GARBLING') %>% 
  add_trace(y = ~SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS, name = 'SOFT_DROP') %>% 
  layout(
    yaxis = list(title = 'inc_ratio_by_rule')
  )


p_by_dim_HAW

saveWidget(p_by_dim_HAW,file = "c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/plots/p_by_dim_HAW.html")









p_HAW <- plot_ly(data.temp, x = ~MUTING_VOLTE_VOICE, y = ~GARBLING_VOLTE_VOICE,
                 z = ~SOFT_DROP_DUE_TO_MEDIA_STOP_VOLTE_IMS, color = ~Ind, 
                 colors = c('#BF382A', '#0C4B8E'),
                 text = HAW_ONE_DAY$model_iOS) %>%
  add_markers() %>%
  layout(title = 'Apple devices in top 30 model with iOS verison by 3 top incidents (HAWAII market)' ,
         scene = list(xaxis = list(title = 'MUTING_VOLTE_VOICE'),
                      yaxis = list(title = 'GARBLING_VOLTE_VOICE'),
                      zaxis = list(title = 'SOFT_DROP_DUE')))
p_HAW

saveWidget(p_HAW,file = "c:/DE_IDI/SEMESTER_REPORT_2018_19_II/SEM_REP/plots/p_HAW.html")
