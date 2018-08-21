

train_final$gelogical<-
  #as.numeric(as.character(train_final$has_geotechnical_risk))+
  as.numeric(as.character(train_final$has_geotechnical_risk_fault_crack))+
  as.numeric(as.character(train_final$has_geotechnical_risk_flood))+
  as.numeric(as.character(train_final$has_geotechnical_risk_land_settlement))+
  as.numeric(as.character(train_final$has_geotechnical_risk_landslide))+
  as.numeric(as.character(train_final$has_geotechnical_risk_liquefaction))+
  as.numeric(as.character(train_final$has_geotechnical_risk_other)) +
  as.numeric(as.character(train_final$has_geotechnical_risk_rock_fall))

train_final$secondary<-
  #as.numeric(as.character(train_final$has_geotechnical_risk))+
  as.numeric(as.character(train_final$has_secondary_use_agriculture))+
  as.numeric(as.character(train_final$has_secondary_use_hotel))+
  as.numeric(as.character(train_final$has_secondary_use_rental))+
  as.numeric(as.character(train_final$has_secondary_use_institution))+
  as.numeric(as.character(train_final$has_secondary_use_school))+
  as.numeric(as.character(train_final$has_secondary_use_industry))+
  as.numeric(as.character(train_final$has_secondary_use_health_post))+
  as.numeric(as.character(train_final$has_secondary_use_gov_office))+
  as.numeric(as.character(train_final$has_secondary_use_use_police))+
  as.numeric(as.character(train_final$has_secondary_use_other))

train_final$superstructure<-
  as.numeric(as.character(train_final$has_superstructure_adobe_mud))+
  as.numeric(as.character(train_final$has_superstructure_mud_mortar_stone))+
  as.numeric(as.character(train_final$has_superstructure_stone_flag))+
  as.numeric(as.character(train_final$has_superstructure_cement_mortar_ston))+
  as.numeric(as.character(train_final$has_superstructure_mud_mortar_brick))+
  as.numeric(as.character(train_final$has_superstructure_cement_mortar_brick))+
  as.numeric(as.character(train_final$has_superstructure_timber))+
  as.numeric(as.character(train_final$has_superstructure_bamboo))+
  as.numeric(as.character(train_final$has_superstructure_rc_non_engineered))+
  as.numeric(as.character(train_final$has_superstructure_rc_engineered))+
  as.numeric(as.character(train_final$has_superstructure_other))


   train_final$has_geotechnical_risk_fault_crack<-NULL
   train_final$has_geotechnical_risk_flood<-NULL
   train_final$has_geotechnical_risk_land_settlement<-NULL
   train_final$has_geotechnical_risk_landslide<-NULL
   train_final$has_geotechnical_risk_liquefaction<-NULL
   train_final$has_geotechnical_risk_other <-NULL
   train_final$has_geotechnical_risk_rock_fall<-NULL


  train_final$has_secondary_use_agriculture<-NULL
  train_final$has_secondary_use_hotel<-NULL
  train_final$has_secondary_use_rental<-NULL
  train_final$has_secondary_use_institution<-NULL
  train_final$has_secondary_use_school<-NULL
  train_final$has_secondary_use_industry<-NULL
  train_final$has_secondary_use_health_post<-NULL
  train_final$has_secondary_use_gov_office<-NULL
  train_final$has_secondary_use_use_police<-NULL
  train_final$has_secondary_use_other<-NULL


  train_final$has_superstructure_adobe_mud<-NULL
  train_final$has_superstructure_mud_mortar_stone<-NULL
  train_final$has_superstructure_stone_flag<-NULL
  train_final$has_superstructure_cement_mortar_stone<-NULL
  train_final$has_superstructure_mud_mortar_brick<-NULL
  train_final$has_superstructure_cement_mortar_brick<-NULL
  train_final$has_superstructure_timber<-NULL
  train_final$has_superstructure_bamboo<-NULL
  train_final$has_superstructure_rc_non_engineered<-NULL
  train_final$has_superstructure_rc_engineered<-NULL
  train_final$has_superstructure_other<-NULL
  train_final$floors_present<-train_final$count_floors_pre_eq
  #train_final$count_floors_pre_eq<-NULL
  #train_final$count_floors_pre_eq<-NULL
  #train_final$floors_present
#str(train_final)

train_final$district_id<-train$district_id
train_final$district_id<-NULL

table(train_final)


str(train_final)

table(train_final$condition_post_eq)
train_final$district_id<-NULL
train_final$floors_present<-NULL
train_final$buldinghight_decreased<-NULL


train_final$has_geotechnical_risk<-as.factor(train_final$has_geotechnical_risk)
train_final$has_repair_started<-as.factor(train_final$has_repair_started)
train_final$has_geotechnical_risk<-as.factor(train_final$has_geotechnical_risk)

train_final$count_families<-as.numeric(as.character(train_final$count_families))
str(train_final)

