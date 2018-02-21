##Loading required libraries.

library(h2o)
library(data.table)
library(dplyr)
library(mlr)
library(ade4)
library(ranger)

##setting working directory.

setwd("C:\\Users\\rishabh.parihar\\Downloads\\DataTales")

##loading the datasets.

train = fread("train.csv"  ,  na.strings = c("" , " " , "NA" , "missing"))
test = fread("test.csv"  ,  na.strings = c("" , " " , "NA" , "missing"))

##Performing variable cleaning.

train$AREA = ifelse(train$AREA %in% c("Karapakkam" , "Karapakam") , "Karapakkam" ,
                    ifelse(train$AREA %in% c("Anna Nagar" , "Ana Nagar" , "Ann Nagar") , "Anna_Nagar" , 
                           ifelse(train$AREA %in% c("Adyar" , "Adyr") , "Adyar" , 
                                  ifelse(train$AREA %in% c("Velachery" , "Velchery") , "Velchery" , 
                                         ifelse(train$AREA %in% c("Chrompet" , "Chormpet" , "Chrompt" , "Chrmpet") , "Chrompet" , 
                                                ifelse(train$AREA %in% c("T Nagar" , "TNagar") , "T_Nagar" , "KK_Nagar"))))))

test$AREA = ifelse(test$AREA %in% c("Karapakkam" , "Karapakam") , "Karapakkam" ,
                   ifelse(test$AREA %in% c("Anna Nagar" , "Ana Nagar" , "Ann Nagar") , "Anna_Nagar" , 
                          ifelse(test$AREA %in% c("Adyar" , "Adyr") , "Adyar" , 
                                 ifelse(test$AREA %in% c("Velachery" , "Velchery") , "Velchery" , 
                                        ifelse(test$AREA %in% c("Chrompet" , "Chormpet" , "Chrompt" , "Chrmpet") , "Chrompet" , 
                                               ifelse(test$AREA %in% c("T Nagar" , "TNagar") , "T_Nagar" , "KK_Nagar"))))))

##converting DATE_BUILD , DATE_SALE to date format.

train$DATE_SALE = as.Date(train$DATE_SALE , format = "%d-%m-%Y")
test$DATE_SALE = as.Date(test$DATE_SALE , format = "%d-%m-%Y")

train$DATE_BUILD = as.Date(train$DATE_BUILD , format = "%d-%m-%Y")
test$DATE_BUILD = as.Date(test$DATE_BUILD , format = "%d-%m-%Y")

train$SALE_COND = ifelse(train$SALE_COND %in% c("Abnormal" , "Ab Normal") , "Abnormal" , 
                         ifelse(train$SALE_COND %in% c("Partial" , "Partiall" , "PartiaLl") , "Partial" , 
                                ifelse(train$SALE_COND %in% c("AdjLand" , "Adj Land") , "Adj_Land" , 
                                       ifelse(train$SALE_COND %in% c("Family") , "Family" , "Normal"))))


test$SALE_COND = ifelse(test$SALE_COND %in% c("Abnormal" , "Ab Normal") , "Abnormal" , 
                        ifelse(test$SALE_COND %in% c("Partial" , "Partiall" , "PartiaLl") , "Partial" , 
                               ifelse(test$SALE_COND %in% c("AdjLand" , "Adj Land") , "Adj_Land" , 
                                      ifelse(test$SALE_COND %in% c("Family") , "Family" , "Normal"))))



train$PARK_FACIL = ifelse(train$PARK_FACIL %in% c("No" , "Noo") , "No" , "Yes")
test$PARK_FACIL = ifelse(test$PARK_FACIL %in% c("No" , "Noo") , "No" , "Yes")

train$BUILDTYPE = ifelse(train$BUILDTYPE %in% c("Commercial" , "Comercial" , "Commercil") , "Commercial" , 
                         ifelse(train$BUILDTYPE %in% c("Others" , "Other") , "Other" , 
                                ifelse(train$BUILDTYPE %in% c("Others" , "Other") , "Other" , "House")))

test$BUILDTYPE = ifelse(test$BUILDTYPE %in% c("Commercial" , "Comercial" , "Commercil") , "Commercial" , 
                        ifelse(test$BUILDTYPE %in% c("Others" , "Other") , "Other" , 
                               ifelse(test$BUILDTYPE %in% c("Others" , "Other") , "Other" , "House")))

train$UTILITY_AVAIL = ifelse(train$UTILITY_AVAIL %in% c("AllPub" , "All Pub") , "All_Pub" , train$UTILITY_AVAIL)

test$UTILITY_AVAIL = ifelse(test$UTILITY_AVAIL %in% c("AllPub" , "All Pub") , "All_Pub" ,test$UTILITY_AVAIL)

train$STREET = ifelse(train$STREET %in% c("Paved" , "Pavd") , "Paved" , 
                      ifelse(train$STREET %in% c("NoAccess" , "No Access") , "No_Access" , "Gravel"))

test$STREET = ifelse(test$STREET %in% c("Paved" , "Pavd") , "Paved" , 
                      ifelse(test$STREET %in% c("NoAccess" , "No Access") , "No_Access" , "Gravel"))

##imputation

train$N_BEDROOM[is.na(train$N_BEDROOM)] = 2
train$N_BATHROOM[is.na(train$N_BATHROOM)] = 1

#let's try to impute QS overall variable.

QS = c("QS_ROOMS" , "QS_BATHROOM" , "QS_BEDROOM" , "QS_OVERALL")
QS_overall_tr = train[!is.na(train$QS_OVERALL) , ][,QS , with=FALSE]

imp_model = lm(QS_OVERALL ~ . , data = QS_overall_tr)
summary(imp_model)

train$QS_OVERALL[is.na(train$QS_OVERALL)] = predict(imp_model , train[is.na(train$QS_OVERALL) , ][,QS[-4] , with=FALSE])
test$QS_OVERALL[is.na(test$QS_OVERALL)] = predict(imp_model , test[is.na(test$QS_OVERALL) , ][,QS[-4] , with=FALSE])

names(train)
names(test)

##Creating the count of the facilities available.

train$facilities_count = ifelse(train$UTILITY_AVAIL == "All_Pub" , 4 , ifelse(train$UTILITY_AVAIL == "NoSewr" , 3 , 
                                                         ifelse(train$UTILITY_AVAIL == "NoSeWa" , 2 , 1)))

train$facilities_count = train$facilities_count + as.numeric(train$PARK_FACIL == "Yes")


test$facilities_count = ifelse(test$UTILITY_AVAIL == "All_Pub" , 4 , ifelse(test$UTILITY_AVAIL == "NoSewr" , 3 , 
                                                                            ifelse(test$UTILITY_AVAIL == "NoSeWa" , 2 , 1)))
test$facilities_count = test$facilities_count + as.numeric(test$PARK_FACIL == "Yes")

train$Distance_facil_ratio = train$DIST_MAINROAD/train$facilities_count
test$Distance_facil_ratio = test$DIST_MAINROAD/test$facilities_count

sum(is.na(test$Distance_footage_ratio))

train$Distance_footage_ratio = train$DIST_MAINROAD/train$INT_SQFT
test$Distance_footage_ratio = test$DIST_MAINROAD/test$INT_SQFT

train$PARK_FACIL = as.numeric(ifelse(train$PARK_FACIL == "Yes" , 1 , 0))
test$PARK_FACIL = as.numeric(ifelse(test$PARK_FACIL == "Yes" , 1 , 0))

#Creating new features.

train$Prop_Age = as.integer((train$DATE_SALE - train$DATE_BUILD)/360)
test$Prop_Age = as.integer((test$DATE_SALE - test$DATE_BUILD)/360)

train$Distance_age_ratio = train$DIST_MAINROAD/train$Prop_Age
test$Distance_age_ratio = test$DIST_MAINROAD/test$Prop_Age

train$Prop_age_cat = as.character(cut(train$Prop_Age , breaks = c(4,10,20,30,40,50 ,55) ,
                    labels = c("age_cat_1" , "age_cat_2" , "age_cat_3" , "age_cat_4" , "age_cat_5" , "age_cat_6") ,
                    include.lowest = TRUE , right = FALSE))
test$Prop_age_cat = as.character(cut(test$Prop_Age , breaks = c(4,10,20,30,40,50 ,55) ,
                    labels = c("age_cat_1" , "age_cat_2" , "age_cat_3" , "age_cat_4" , "age_cat_5" , "age_cat_6") ,
                    include.lowest = TRUE , right = FALSE))

train$Extra_rooms = (train$N_ROOM - (train$N_BEDROOM + train$N_BATHROOM))
test$Extra_rooms = (test$N_ROOM - (test$N_BEDROOM + test$N_BATHROOM))

train$Footage_cat = as.character(cut(train$INT_SQFT , breaks = c(500 , 1000 , 1500 , 2000 , 2500) , 
         include.lowest = TRUE , right = FALSE , labels = c("Footage_Cat_1" , "Footage_Cat_2" , "Footage_Cat_3" , 
                                                            "Footage_Cat_4")))

test$Footage_cat = as.character(cut(test$INT_SQFT , breaks = c(500 , 1000 , 1500 , 2000 , 2500) , 
                        include.lowest = TRUE , right = FALSE , labels = c("Footage_Cat_1" , "Footage_Cat_2" , "Footage_Cat_3" , 
                                                                           "Footage_Cat_4")))

train$Room_area = 0.8*train$INT_SQFT/train$N_ROOM
test$Room_area = 0.8*test$INT_SQFT/test$N_ROOM

train$Room_area_cat = as.character(cut(train$Room_area  , breaks = c(200 , 250 , 300 , 360) ,labels = c("Room_area_cat_1" ,
                      "Room_area_cat_2" , "Room_area_cat_3") , include.lowest = TRUE , right = FALSE))

test$Room_area_cat = as.character(cut(test$Room_area  , breaks = c(200 , 250 , 300 , 360) ,labels = c("Room_area_cat_1",
                     "Room_area_cat_2" , "Room_area_cat_3") , include.lowest = TRUE , right = FALSE))

train$Bedroom_area = train$N_BEDROOM*train$Room_area
test$Bedroom_area = test$N_BEDROOM*test$Room_area

train$Bedroom_area_cat = as.character(cut(train$Bedroom_area , breaks = c(200 , 400 , 800 , 1100 , 1400) ,
                         labels = c("Bedroom_area_cat_1" , "Bedroom_area_cat_2" , "Bedroom_area_cat_3" , 
                                    "Bedroom_area_cat_4") , include.lowest = TRUE , right = FALSE))

test$Bedroom_area_cat = as.character(cut(test$Bedroom_area , breaks = c(200 , 400 , 800 , 1100 , 1400) ,
                         labels = c("Bedroom_area_cat_1" , "Bedroom_area_cat_2" , "Bedroom_area_cat_3" , 
                                    "Bedroom_area_cat_4") , include.lowest = TRUE , right = FALSE))

train$QS_BEDROOM_cat = as.character(cut(train$QS_BEDROOM , breaks = c(2 , 3 , 4 , 5) ,
                       labels = c("qs_bedroom_cat_1" , "qs_bedroom_cat_2" , "qs_bedroom_cat_3") ,
                       include.lowest = TRUE , right = FALSE))

test$QS_BEDROOM_cat = as.character(cut(test$QS_BEDROOM , breaks = c(2 , 3 , 4 , 5) ,
                                        labels = c("qs_bedroom_cat_1" , "qs_bedroom_cat_2" , "qs_bedroom_cat_3") ,
                                        include.lowest = TRUE , right = FALSE))

train$QS_BATHROOM_cat = as.character(cut(train$QS_BATHROOM , breaks = c(2 , 3 , 4 , 5) ,
                                         labels = c("qs_bathroom_cat_1" , "qs_bathroom_cat_2" , "qs_bathroom_cat_3") ,
                                         include.lowest = TRUE , right = FALSE))

test$QS_BATHROOM_cat = as.character(cut(test$QS_BATHROOM , breaks = c(2 , 3 , 4 , 5) ,
                                        labels = c("qs_bathroom_cat_1" , "qs_bathroom_cat_2" , "qs_bathroom_cat_3") ,
                                        include.lowest = TRUE , right = FALSE))

train$QS_ROOMS_cat = as.character(cut(train$QS_ROOMS , breaks = c(2 , 3 , 4 , 5) ,
                                      labels = c("qs_rooms_cat_1" , "qs_rooms_cat_2" , "qs_rooms_cat_3") ,
                                      include.lowest = TRUE , right = FALSE))

test$QS_ROOMS_cat = as.character(cut(test$QS_ROOMS , breaks = c(2 , 3 , 4 , 5) ,
                                     labels = c("qs_rooms_cat_1" , "qs_rooms_cat_2" , "qs_rooms_cat_3") ,
                                     include.lowest = TRUE , right = FALSE))

train$QS_OVERALL_cat = as.character(cut(train$QS_OVERALL , breaks = c(2 , 3 , 4 , 5) ,
                                        labels = c("qs_overall_cat_1" , "qs_overall_cat_2" , "qs_overall_cat_3") ,
                                        include.lowest = TRUE , right = FALSE))

test$QS_OVERALL_cat = as.character(cut(test$QS_OVERALL , breaks = c(2 , 3 , 4 , 5) ,
                                       labels = c("qs_overall_cat_1" , "qs_overall_cat_2" , "qs_overall_cat_3") ,
                                       include.lowest = TRUE , right = FALSE))

train$Rating_gap = (train$QS_OVERALL - (train$QS_ROOMS + train$QS_BEDROOM + train$QS_BATHROOM)/3)
test$Rating_gap = (test$QS_OVERALL - (test$QS_ROOMS + test$QS_BEDROOM + test$QS_BATHROOM)/3)

train$Overhead_cost = train$REG_FEE + train$COMMIS
test$Overhead_cost = test$REG_FEE + test$COMMIS

train$Overhead_per_unit_area = train$Overhead_cost/train$INT_SQFT
test$Overhead_per_unit_area = test$Overhead_cost/test$INT_SQFT

train$Overhead_per_room = train$Overhead_cost/train$N_ROOM
test$Overhead_per_room = test$Overhead_cost/test$N_ROOM

rating_combs = train[,(Avg_sales_price = mean(SALES_PRICE)) , by = c("QS_BEDROOM_cat" , "QS_BATHROOM_cat" , "QS_ROOMS_cat")]
rating_combs = rating_combs[,-4 , with=FALSE]
rating_combs$rating_com_cats = seq(1,27 , 1)

train = merge(train , rating_combs , by = c("QS_BEDROOM_cat" , "QS_BATHROOM_cat" , "QS_ROOMS_cat") , all.x = TRUE ,
              all.y = FALSE)
test = merge(test , rating_combs , by = c("QS_BEDROOM_cat" , "QS_BATHROOM_cat" , "QS_ROOMS_cat") , all.x = TRUE ,
              all.y = FALSE)

char_vars = names(train)[sapply(train , class) == "character"]
char_vars = char_vars[-4]
target_var = "SALES_PRICE"
num_vars = names(train)[sapply(train , class) %in% c("numeric" , "integer")]
num_vars = num_vars[!num_vars %in% target_var]

train = as.data.frame(train)
test = as.data.frame(test)

##code to perform ohe in R .

for (f in char_vars){
  train_dummy = acm.disjonctif(train[f])
  train[f] = NULL
  train = cbind(train, train_dummy)
}

for (f in char_vars){
  test_dummy = acm.disjonctif(test[f])
  test[f] = NULL
  test = cbind(test, test_dummy)
}

names(train)
names(test)

test$SALE_COND.Abnormal = 0

test$SALES_PRICE = mean(train$SALES_PRICE)

vars = names(train)[!names(train) %in% c("DATE_BUILD" , "DATE_SALE")]


##appending train and test sets.

all = rbind(train[,vars] , test[,vars])

names(all)

clus_vars = names(all)[c(2:84)]

##selecting optimal number of clusters.

within_ss = numeric()

for(i in seq(1,20)){
  
  Cluster = kmeans(all[,clus_vars[-13]] , i)
  within_ss[i] = mean(Cluster$withinss) 
  
}

plot(within_ss , type = "b")

##let's make 5 clusters.

Cluster <- kmeans(all[,clus_vars[-13]], 10)

all$Group = as.factor(Cluster$cluster)

char_vars = c("Group")

##doing ohe of group variable.

for (f in char_vars){
  all_dummy = acm.disjonctif(all[f])
  all[f] = NULL
  all = cbind(all, all_dummy)
}

train = all[1:(nrow(train)) , ]
test = all[-(1:(nrow(train))) , ]

##Initialising h2o.

localH2O <- h2o.init(nthreads = -1)
h2o.init()

train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

colnames(train.h2o)

x.indep = c(2:13 , 15:94)
y.dep = 14

system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 500,
                                      hidden = c(300,300 , 200),
                                      activation = "Rectifier",
                                      seed = 1122 , 
                                      keep_cross_validation_predictions = TRUE , 
                                      keep_cross_validation_fold_assignment = TRUE , 
                                      nfolds = 5 , 
                                      fold_assignment = "AUTO"
  )
)

h2o.performance(dlearning.model)

predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))

sub_dlearning <- data.frame(PRT_ID = test$PRT_ID, SALES_PRICE = predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new_more_features_14_01_last.csv", row.names = F)

#create a data frame and writing submission file

bst_dl = fread("sub_dlearning_new_more_features_1.csv")
sec_bst_dl = fread("sub_dlearning_new_1.csv")
sec_last_14 = fread("sub_dlearning_new_more_features_14_01_2.csv")
last_14 = fread("sub_dlearning_new_more_features_14_01_last.csv")
one_more = fread("sub_dlearning_new.csv")
sub_dlearning <- data.frame(PRT_ID = test$PRT_ID, SALES_PRICE = predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new_more_features_14_01.csv", row.names = F)

sub_dlearning <- data.frame(PRT_ID = test$PRT_ID, SALES_PRICE = predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new_more_features_14_01_2.csv", row.names = F)

ensemble_sub = data.frame(PRT_ID = test$PRT_ID, SALES_PRICE = (bst_dl$SALES_PRICE + sec_bst_dl$SALES_PRICE +
                            sec_last_14$SALES_PRICE + one_more$SALES_PRICE)/4)
write.csv(ensemble_sub, file = "sub_ensemble_4_models_2.csv", row.names = F)

best_ensemble = fread("C:\\Users\\rishabh.parihar\\Downloads\\DataTales\\best_soln\\sub_dlearning_best_2_ensemble_3.csv")
anoth = fread("sub_ensemble_4_models_1.csv")

ensemble_sub = data.frame(PRT_ID = test$PRT_ID, SALES_PRICE = (best_ensemble$SALES_PRICE + anoth$SALES_PRICE)/2)
write.csv(ensemble_sub, file = "sub_ensemble_2_best_ensembles.csv", row.names = F)


############################################END################################################################################3

