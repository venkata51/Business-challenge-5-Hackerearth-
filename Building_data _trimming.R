setwd("C:\\Users\\Venkatasubramanian\\Desktop\\dataset\\Dataset")
train=read.csv("train.csv",header = T,stringsAsFactors = F)
test=read.csv("test.csv",header = T,stringsAsFactors = F)
Building_Ownership_Use=read.csv("Building_Ownership_Use.csv",header = T,stringsAsFactors = F)
Building_Structure=read.csv("Building_Structure.csv",header = T,stringsAsFactors = F)

sum(is.na(train$has_repair_started))
str(train)
train_ID=train$damage_grade
test_ID=test$damage_grade


train_new=merge(x =train, y = Building_Ownership_Use, by = "building_id", all.x = TRUE)
train_final=merge(x =train_new, y = Building_Structure, by = "building_id", all.x = TRUE)
test_new=merge(x =test, y = Building_Ownership_Use, by = "building_id", all.x = TRUE,sort = FALSE)
test_final=merge(x =test_new, y = Building_Structure, by = "building_id", all.x = TRUE,sort = FALSE)

test_final$count_families
colnames(train_final)

str(train_final)

train_final$district_id.x<-NULL
train_final$district_id.y<-NULL
train_final$vdcmun_id.x<-NULL
train_final$vdcmun_id.y<-NULL
train_final$ward_id.x<-NULL
train_final$ward_id.y<-NULL

test_final$district_id.x<-NULL
test_final$district_id.y<-NULL
test_final$vdcmun_id.x<-NULL
test_final$vdcmun_id.y<-NULL
test_final$ward_id.x<-NULL
test_final$ward_id.y<-NULL
train_final$vdcmun_id<-NULL
test_final$vdcmun_id<-NULL
train_final$district_id<-NULL
test_final$district_id<-NULL
#train_final$has_geotechnical_risk<-NULL
#test_final$has_geotechnical_risk<-NULL
#train_final$has_secondary_use<-NULL
#test_final$has_secondary_use<-NULL

sum(is.na(train_final))
str(train_final)

prop.table(table(train_final$damage_grade))

colnames(train_final)


columns_factor<-c(
"has_geotechnical_risk_fault_crack",
"has_geotechnical_risk_flood",
"has_geotechnical_risk_land_settlement",
"has_geotechnical_risk_landslide",
"has_geotechnical_risk_liquefaction",
"has_geotechnical_risk_other",
"has_geotechnical_risk_rock_fall",
"has_secondary_use_agriculture",
"has_secondary_use_hotel",
"has_secondary_use_rental",
"has_secondary_use_institution",
"has_secondary_use_school",
"has_secondary_use_industry",
"has_secondary_use_health_post",
"has_secondary_use_gov_office",
"has_secondary_use_use_police",
"has_secondary_use_other",
"has_superstructure_adobe_mud",
"has_superstructure_mud_mortar_stone",
"has_superstructure_stone_flag",
"has_superstructure_cement_mortar_ston",
"has_superstructure_mud_mortar_brick",
"has_superstructure_cement_mortar_bric",
"has_superstructure_timber",
"has_superstructure_bamboo",
"has_superstructure_rc_non_engineered",
"has_superstructure_rc_engineered",
"has_superstructure_other",
"area_assesed",
"damage_grade",
"legal_ownership_status",
"foundation_type",                       
"roof_type",                            
"ground_floor_type",                    
"other_floor_type",                   
"position",                            
"plan_configuration",
"condition_post_eq",
"land_surface_condition")

str(train_final)

for (c in columns_factor){
  train_final[,colnames(train_final)==c]=as.factor(train_final[,colnames(train_final)==c])
  test_final[,colnames(test_final)==c]=as.factor(test_final[,colnames(test_final)==c])
  
}

str(train_final)

train_final$has_superstructure_cement_mortar_brick<-as.factor(train_final$has_superstructure_cement_mortar_brick)
test_final$has_superstructure_cement_mortar_brick<-as.factor(test_final$has_superstructure_cement_mortar_brick)
train_final$has_superstructure_cement_mortar_stone<-as.factor(train_final$has_superstructure_cement_mortar_stone)
test_final$has_superstructure_cement_mortar_stone<-as.factor(test_final$has_superstructure_cement_mortar_stone)
train_final$count_families<-as.factor(train_final$count_families)
test_final$count_families<-as.factor(test_final$count_families)


feature_classes <- sapply(names(train_final[,!colnames(train_final)%in% "building_id"]),function(x){class(train_final[[x]])})
numeric_feats <- names(feature_classes[feature_classes != "factor"])
Categorical_feats <- names(feature_classes[feature_classes == "factor"])

write.csv(Categorical_feats,"cat.csv")

colSums(is.na(train_final))
colSums(is.na(test_final))

colnames(train_final)
str(train_final)

levels(train_final$condition_post_eq)

train_final[train_final$building_id==i,]

str(train_final)
i
is.na(train_final[train_final$building_id==i,]$has_repair_started)



levels(train_final$condition_post_eq)


Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

Covered_by_landslide<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Covered by landslide",]$has_repair_started)))
Damaged_Not_used<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Damaged-Not used",]$has_repair_started)))
Damaged_Repaired_and_used<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Damaged-Repaired and used",]$has_repair_started)))
Damaged_Rubble_clear<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Damaged-Rubble clear",]$has_repair_started)))
Damaged_Rubble_Clear_New_building_built<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Damaged-Rubble Clear-New building built",]$has_repair_started)))
Damaged_Rubble_unclear<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Damaged-Rubble unclear",]$has_repair_started)))
Damaged_Used_in_risk<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Damaged-Used in risk",]$has_repair_started)))
Not_damaged<-as.numeric(as.character(Mode(train_final[train_final$condition_post_eq=="Not damaged",]$has_repair_started)))



    
    x$has_repair_started<-ifelse(x$condition_post_eq=="Covered by landslide",Covered_by_landslide,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Damaged-Not used",Damaged_Not_used,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Damaged-Repaired and used",Damaged_Repaired_and_used,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Damaged-Rubble clear",Damaged_Rubble_clear,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Damaged-Rubble Clear-New building built",Damaged_Rubble_Clear_New_building_built,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Damaged-Rubble unclear",Damaged_Rubble_unclear,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Damaged-Used in risk",Damaged_Used_in_risk,x$has_repair_started)
    x$has_repair_started<-ifelse(x$condition_post_eq=="Not damaged",0,x$has_repair_started)
    
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Covered by landslide" && is.na(train_final$has_repair_started) ,Covered_by_landslide,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Damaged-Not used",Damaged_Not_used,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Damaged-Repaired and used",Damaged_Repaired_and_used,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Damaged-Rubble clear",Damaged_Rubble_clear,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Damaged-Rubble Clear-New building built",Damaged_Rubble_Clear_New_building_built,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Damaged-Rubble unclear",Damaged_Rubble_unclear,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Damaged-Used in risk",Damaged_Used_in_risk,train_final$has_repair_started)
train_final$has_repair_started<-ifelse(train_final$condition_post_eq=="Not damaged",0,train_final$has_repair_started)

sum(is.na(train_final))

train_final<-na.omit(train_final)

test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Covered by landslide" && is.na(test_final$has_repair_started) ,Covered_by_landslide,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Damaged-Not used",Damaged_Not_used,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Damaged-Repaired and used",Damaged_Repaired_and_used,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Damaged-Rubble clear",Damaged_Rubble_clear,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Damaged-Rubble Clear-New building built",Damaged_Rubble_Clear_New_building_built,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Damaged-Rubble unclear",Damaged_Rubble_unclear,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Damaged-Used in risk",Damaged_Used_in_risk,test_final$has_repair_started)
test_final$has_repair_started<-ifelse(test_final$condition_post_eq=="Not damaged",0,test_final$has_repair_started)

sum(is.na(test_final))

train_final$has_repair_started<-as.factor(train_final$has_repair_started)
test_final$has_repair_started<-as.factor(test_final$has_repair_started)
str(train_final)

str(train)

table(train_final$plan_configuration)

table(train_final[train$district_id==9,]$damage_grade)

table(train$vdcmun_id)
table(train[train$district_id==7,]$damage_grade)

table(train_final[train_final$plan_configuration=='Rectangular',]$damage_grade)
slevels(train_final$land_surface_condition)

table(train$gelogical)



x<-c("has_geotechnical_risk",
"has_geotechnical_risk_fault_crack",
"has_geotechnical_risk_flood",
"has_geotechnical_risk_land_settlement",
"has_geotechnical_risk_landslide",
"has_geotechnical_risk_liquefaction",
"has_geotechnical_risk_other",
"has_geotechnical_risk_rock_fall")

x<-c("has_secondary_use_agriculture",
"has_secondary_use_hotel",
"has_secondary_use_rental",
"has_secondary_use_institution",
"has_secondary_use_school",
"has_secondary_use_industry",
"has_secondary_use_health_post",
"has_secondary_use_gov_office",
"has_secondary_use_use_police",
"has_secondary_use_other")
Building_Structure

train$gelogical<-(train$has_geotechnical_risk+train$has_geotechnical_risk_fault_crack+train$has_geotechnical_risk_flood+train$has_geotechnical_risk_land_settlement+train$has_geotechnical_risk_landslide+train$has_geotechnical_risk_liquefaction+train$has_geotechnical_risk_other+train$has_geotechnical_risk_rock_fall)
train_final$secondary<-as.numeric(as.character(train_final$has_secondary_use_agriculture))+
  as.numeric(as.character(train_final$has_secondary_use_hotel))+
  as.numeric(as.character(train_final$has_secondary_use_rental))+
  as.numeric(as.character(train_final$has_secondary_use_institution))+
  as.numeric(as.character(train_final$has_secondary_use_school))+
  as.numeric(as.character(train_final$has_secondary_use_industry))+
  as.numeric(as.character(train_final$has_secondary_use_health_post))+
  as.numeric(as.character(train_final$has_secondary_use_gov_office))+
  as.numeric(as.character(train_final$has_secondary_use_use_police))+
  as.numeric(as.character(train_final$has_secondary_use_other))

x<-as.data.frame(train_final$secondary)
table(x)
