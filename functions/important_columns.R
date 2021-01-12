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
  'questionnaireAnswers.var5_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante',
  'questionnaireAnswers.test_vih',
  'questionnaireAnswers.tdr_palu',
  'questionnaireAnswers.grossesse',
  'questionnaireAnswers.lait_matenel_value',
  'questionnaireAnswers.genex_date_value',
  'questionnaireAnswers.gen_gp_value',
  'questionnaireAnswers.gen_np_value'
)

######################################################################
#Add missed columns in the tibble
AddingColumns <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}