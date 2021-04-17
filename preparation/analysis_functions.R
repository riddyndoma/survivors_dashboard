library(lubridate)
library(dplyr)

##########################GET FOLLOWERS BY WEEKS########################################
getFollowersByWeeks <- function(df,week_val,elem) {
 answer=NULL
      if (elem=="vas") {
        answer=df %>%
          filter(week(date_of_followup)==week_val & status == "Vu avec signe") %>%
          count()
        
      }else if (elem=="vss"){
        answer=df %>%
          filter(week(date_of_followup)==week_val & status == "Vu sans signe") %>%
          count()
        
      }else if (elem=="all"){
        answer=df %>%
          filter(week(date_of_followup)==week_val & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
          count()
        
      }
 
  answer=as.data.frame(answer)
  rownames(answer)=c(paste0("Sem-",week_val)) #Renommage ligne pour porter numero de semaine
  return(answer)
}

##########################GET FOLLOWERS BY WEEKS, BY SEXE ########################################

getFollowersBySex <- function(df,week_val,elem) {
  answer=NULL
  if (elem=="m") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & gender == "Homme" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="f") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & gender == "Femme" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  } else {
      answer=df %>%
        filter(week(date_of_followup)==week_val & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
        count()
  }
  
  answer=as.data.frame(answer)
  if (week_val < 10) {
    rownames(answer)=c(paste0("Sem-0",week_val)) #Renommage ligne pour porter numero de semaine
  }else{
    rownames(answer)=c(paste0("Sem-",week_val)) #Renommage ligne pour porter numero de semaine
  }
 
  return(answer)
}


######################GET SURVIVORS BY SEX########################################################

getSurvivorsBySex <- function(df,elem) {
  answer=NULL
  if (elem=="m") {
    answer=df %>%
      filter(gender == "Homme") %>%
      distinct(uuid) %>%
      count()
    
  }else if (elem=="f") {
    answer=df %>%
      filter(gender == "Femme") %>%
      distinct(uuid) %>%
      count()
    
  }else{
    answer=df %>%
      filter(is.na(gender)) %>%
      distinct(uuid) %>%
      count()
  }
  
  answer=as.data.frame(answer)
 # rownames(answer)=c(paste0("Sem-",week_val)) #Renommage ligne pour porter numero de semaine
  return(answer)
}

##########################GET FOLLOWUPS RATE########################################
getFollowersRateByWeeks <- function(df,val) {
  # current_week=week(database_date)
  answer=NULL
  num=NULL
  denom=NULL
  if (val==0) {
    num=df %>%
      filter(week(date_of_followup)==current_week & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    denom=df %>%
      filter(week(date_of_followup)==current_week ) %>%
      count()
    #answer=(num/denom)*100
    answer=(num/285)*100
    
  }else {
    num=df %>%
      filter(week(date_of_followup)==current_week - val & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    denom=df %>%
      filter(week(date_of_followup)==current_week - val ) %>%
      count()
   # answer=(num/denom)*100
    answer=(num/285)*100
    
  }
  
  answer=as.data.frame(answer)
  rownames(answer)=c(paste0("Sem-",current_week-val)) #Renommage ligne pour porter numero de semaine
  return(round(answer,2))
}

#############################GET SPERME GenExpert RESULTAT CURRENT MONTH ######################
getSpermeGenExpertResultCurrentMonth <- function(df,val) {
  
  #current_month <- formatDate <- format(Sys.Date(), "%Y-%m")
  answer = NULL
  if (is.null(val) ){ #Count all tested
    answer= df %>%
      filter(sperme=="test_gp") %>%
      filter(format(date_of_followup, "%Y-%m") ==current_month) %>%
      distinct(person_id) %>%
      count()
  }else{
    #count positive or negative
    answer= df %>%
    filter(sperme=="test_gp") %>% 
      filter(format(date_of_followup, "%Y-%m") ==current_month) %>%
      filter(sperme=="test_gp") %>%
      filter(sperme_genexpert_resultat==val) %>%
      distinct(person_id) %>%
      count()
  }
  
  return(answer)
  
}

##############################PSYCHO FUNCTIONS###########################################"
getPsychoEvaluationByElem <- function(df,week_val,elem) {
  answer=NULL
  if (elem=="tris") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & pyscho_tristesse_douleur_morale == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="iso") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & psycho_retrait_social_isolement == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  } else if(elem=="suicid") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & psycho_pensees_suicidaires == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  } else if(elem=="honte") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & psycho_fort_sentiment_de_culpabilite_honte == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="psom_rec") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & psycho_problemes_sommeil_recurrents == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }
  
  answer=as.data.frame(answer)
  if (week_val < 10) {
    rownames(answer)=c(paste0("Sem-0",week_val)) #Renommage ligne pour porter numero de semaine
  }else{
    rownames(answer)=c(paste0("Sem-",week_val)) #Renommage ligne pour porter numero de semaine
  }
 
  return(answer)
}

##############################CLINICAL FUNCTIONS###########################################"
getClinicalEvaluationByElem <- function(df,week_val,elem) {
  answer=NULL
  if (elem=="doul_osta") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_cmd_douleurs_osteo_articulaires == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="doul_mus") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_douleurs_musculaires == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  } else if(elem=="doul_lomb") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_douleurs_lombaires == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  } else if(elem=="doul_ocu") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_douleurs_ocuiaires == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="bais_vis") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_baisse_de_la_vision == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="oel_rg") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_oeil_rouge == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="larmoie") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_larmoiement == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="photo") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_photophobie == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="bais_aud") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_baisse_de_laudition == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="bourd_oreil") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_bourdonnement_doreille == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="troub_erect") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_trouble_erectile == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="doul_test") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_douleurs_testiculaires == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="amenorrhee") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_amenorrhee == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="bais_lib") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_baisse_de_la_libido == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="leucorrhees") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_leucorrhees == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else if(elem=="hemorragie") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & clin_hemorragie  == "oui" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }
  
  
  answer=as.data.frame(answer)
  if (week_val < 10) {
    rownames(answer)=c(paste0("Sem-0",week_val)) #Renommage ligne pour porter numero de semaine
  }else{
    rownames(answer)=c(paste0("Sem-",week_val)) #Renommage ligne pour porter numero de semaine
  }
  
  return(answer)
}




















