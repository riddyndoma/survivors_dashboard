######################################################################
followups_base_columns <- c(
  'questionnaireAnswers.lait_matenel',
  'questionnaireAnswers.genex_date',
  'questionnaireAnswers.gen_gp',
  'questionnaireAnswers.gen_np',
  'questionnaireAnswers.exa_biochimie',
  'questionnaireAnswers.hematologie',
  'questionnaireAnswers.urine',
  'questionnaireAnswers.sperme',
  'questionnaireAnswers.gene_date',
  'questionnaireAnswers.gene_resultat',
  'questionnaireAnswers.cmd_np',
  #'questionnaireAnswers.var5_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante',
  'questionnaireAnswers.test_vih',
  'questionnaireAnswers.tdr_palu',
  'questionnaireAnswers.grossesse',
  'questionnaireAnswers.refere_pour_des_soins_appropries',
  'questionnaireAnswers.le_geuris_est_il_refere',
  'questionnaireAnswers.cest_une_consultation_medicale_durgence',
  'questionnaireAnswers.le_gueris_a_t_il_beneficie_dun_coseil_depistage_vih',
  'questionnaireAnswers.le_gueris_a_t_il_accepte_de_se_faire_teste',
  'questionnaireAnswers.le_gueris_presente_t_il_des_troubles_psychologiques',
  'questionnaireAnswers.evolution_psychologique_du_patient_depuis_la_derniere_visite'
)

# followups_base_clean_columns <- c(
#   'questionnaireanswers_lait_matenel_value',
#   'questionnaireanswers_genex_date_value',
#   'questionnaireanswers_gen_gp_value',
#   'questionnaireanswers_gen_np_value',
#   'questionnaireanswers_sperme_value',
#   'questionnaireanswers_gene_date_value',
#   'questionnaireanswers_gene_resulta_value',
#   'questionnaireanswers_cmd_np_value',
#   'questionnaireAnswers_refere_pour_des_soins_appropries_value'
# )

######################################################################
#Add missed columns in the tibble
AddingColumns <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}

#Check and correct columns for artificial columns added
correctingColumns <- function(data,c){
  #data=as_tibble(data)
  nc <- str_split(c,"_questionnaire")
  nc <- nc[[1]][1]
  nc <- paste0(nc,"_value")
  
  if(c %in% colnames(data)){
    names(data)[names(data) == c] <- nc
  }
  return(data)
}