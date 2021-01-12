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

getFollowersBySexe <- function(df,week_val,elem) {
  answer=NULL
  if (elem=="m") {
    answer=df %>%
      filter(week(date_of_followup)==week_val & gender == "Homme" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }else {
    answer=df %>%
      filter(week(date_of_followup)==week_val & gender == "Femme" & (status == "Vu sans signe" | status == "Vu avec signe")) %>%
      count()
    
  }
  
  answer=as.data.frame(answer)
  rownames(answer)=c(paste0("Sem-",week_val)) #Renommage ligne pour porter numero de semaine
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
  current_week=week(database_date)
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
  
  current_month <- formatDate <- format(Sys.Date(), "%Y-%m")
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
