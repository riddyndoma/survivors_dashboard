## This script is for cleaning the Go data data, revised to use the collections pulled directly from API !
library(dplyr)
library(tidyr)
library(rio)
library(linelist)

options(max.print=1000000)

# check to see which of these are actually needed, take out those that are not.
path_to_functions <- here::here("functions")
scripts_files <- dir(path_to_functions, pattern = ".R$", full.names=TRUE)
for (file in scripts_files) source(file, local = TRUE)

# LOAD DATA FROM API R AS TIBBLES###############################################

database_date <- Sys.Date()

followups <- followups %>%
  as_tibble()

cases <- cases %>%
  as_tibble()


contacts <- contacts %>%
  as_tibble()

relationships <- relationships %>%
  as_tibble()

teams <- teams %>%
  as_tibble()

users <- users %>%
  as_tibble()

#########################MAKE COPY#################################################

copy_followups <- followups
copy_cases <- cases
copy_contacts <- contacts
copy_relationships <- relationships
copy_teams <- teams
copy_users <- users

#-----Data recovery---------
# followups <- copy_followups
# cases <- copy_cases
# contacts <- copy_contacts
# relationships <- copy_relationships
# teams <- copy_teams
# users <- copy_users

#################### UNNEST TIBBLES ############################################

#Cases data, under follow up, active, not deleted, also unnest all nested fields

colnames(cases)=str_replace(colnames(cases),"3277482e-e1a3-4426-a0a7-91b448ff93ab","sexe")
names(cases)=tolower(names(cases)) #Columns in lowercase
unnested_cases <- cases %>%
  filter(deleted == FALSE) %>%
  mutate(dateranges = map_if(dateranges, is.null, ~ tibble())) %>% #Replace null by a list
  unnest(c( 
    addresses,
    vaccinesreceived,
    questionnaireanswers.age,
    questionnaireanswers.sexe,
    questionnaireanswers.resultats),
    keep_empty = TRUE, names_sep = "_")


followups_base_checked=AddingColumns(followups,followups_base_columns) #Adding some important columns who can missed

#Follow up data, under follow up, active, not deleted, also unnest all nested fields
unnested_followups <- followups_base_checked %>%
  filter(deleted == FALSE) %>%
  filter(contact.active == TRUE) %>%
  unnest(c(
    contact.addresses,
    contact.followUpHistory,
    questionnaireAnswers.quel_formulaire_de_rapport_initial_utiliser,
    questionnaireAnswers.ta_mm_hg,
    questionnaireAnswers.medecine_generale,
    questionnaireAnswers.perte_dequilibre,
    questionnaireAnswers.rhumato,
    questionnaireAnswers.ophtamo,
    questionnaireAnswers.orl,
    questionnaireAnswers.reproduction,
    questionnaireAnswers.complications_au_cte,
    questionnaireAnswers.2_maladies_anterieures_connues,
    questionnaireAnswers.4_traitement_habituel_medicament_posologie,
    questionnaireAnswers.vaccination_connue,
    questionnaireAnswers.vision,
    questionnaireAnswers.audition_nutrition_uro_genital,
    questionnaireAnswers.nutrition,
    questionnaireAnswers.uro_genital,
    questionnaireAnswers.articulaire,
    questionnaireAnswers.abdominal,
    questionnaireAnswers.6_cat,
    questionnaireAnswers.marche,
    questionnaireAnswers.tonus_musculaire,
    questionnaireAnswers.1_bicipital,
    questionnaireAnswers.2_tricipital,
    questionnaireAnswers.3_rotuliens,
    questionnaireAnswers.4_achilleens,
    questionnaireAnswers.si_trouble_de_force_musculaire,
    questionnaireAnswers.troubles_sensitifs,
    questionnaireAnswers.troubles_de_lequilibre_et_coordination_des_mouvements,
    questionnaireAnswers.troubies_du_sommeil,
    questionnaireAnswers.trouble_de_langage,
    questionnaireAnswers.autres_signes_neurologiques_ou_somatiques_si_presents,
    questionnaireAnswers.si_oui_quel_a_ete_le_resultat_pour_chaque_grossesse_depuis_votre_sortie_remplir_le_tableau_ci_dessous,
    questionnaireAnswers.avez_vous_allaite_un_ou_plusieurs_de_vos_enfants_depuis_votre_sortie_du_cte,
    questionnaireAnswers.quest_ce_qui_decrit_le_mieux_votre_cycle_menstruel_sur_lannee_avant_que_vous_ayez_eu_ebola,
    questionnaireAnswers.si_vous_naviez_pas_vos_regles_avant_davoir_ebola_quelle_etait_la_raison,
    questionnaireAnswers.quest_ce_qui_decrit_le_mieux_votre_cycle_menstruel_sur_lannee_apres_que_vous_ayez_eu_ebola,
    questionnaireAnswers.apres_votre_sortie_du_cte_quand_est_ce_que_vos_regles_ont_recommence,
    questionnaireAnswers.si_vous_naviez_pas_vos_regles_apres_davoir_eu_ebola_quelle_etait_la_raison,
    questionnaireAnswers.fievre,
    questionnaireAnswers.fatigue,
    questionnaireAnswers.maux_de_tete,
    questionnaireAnswers.anorexie,
    questionnaireAnswers.nausee,
    questionnaireAnswers.diarrhee,
    questionnaireAnswers.vomissement,
    questionnaireAnswers.mal_de_gorge,
    questionnaireAnswers.douleurs_thoraciques,
    questionnaireAnswers.difficulte_respiratoire,
    questionnaireAnswers.difficulte_a_avaler,
    questionnaireAnswers.douleurs_abdominales,
    questionnaireAnswers.troubles_du_sommeil,
    questionnaireAnswers.engourdssement_des_extremites,
    questionnaireAnswers.sueur_froide,
    questionnaireAnswers.stress,
    questionnaireAnswers.cmd_douleurs_osteo_articulaires,
    questionnaireAnswers.var_douleurs_musculaires,
    questionnaireAnswers.douleurs_lombaires,
    questionnaireAnswers.douleurs_ocuiaires,
    questionnaireAnswers.baisse_de_la_vision,
    questionnaireAnswers.oeil_rouge,
    questionnaireAnswers.larmoiement,
    questionnaireAnswers.et_photophobie,
    questionnaireAnswers.baisse_de_laudition,
    questionnaireAnswers.bourdonnement_doreille,
    questionnaireAnswers.trouble_erectile,
    questionnaireAnswers.douleurs_testiculaires,
    questionnaireAnswers.amenorrhee,
    questionnaireAnswers.baisse_de_la_libido,
    questionnaireAnswers.leucorrhees,
    questionnaireAnswers.hemorragie,
    questionnaireAnswers.abdomen_sensible,
    questionnaireAnswers.cardio_vasculaire,
    questionnaireAnswers.pulmonaire,
    questionnaireAnswers.cutane,
    questionnaireAnswers.1_ere_grossesse,
    questionnaireAnswers.2_eqrossesse,
    questionnaireAnswers.reponses,
    questionnaireAnswers.gonflement_articulaire,
    questionnaireAnswers.vos_regles_ont_elles_change_de_couleur,
    questionnaireAnswers.le_flux_de_vos_regles_est_il_different,
    questionnaireAnswers.var_serologie_ebola,
    questionnaireAnswers.sperme,
    questionnaireAnswers.gene_date,
    questionnaireAnswers.gene_resultat,
    questionnaireAnswers.cmd_np,
    questionnaireAnswers.lait_matenel,
    questionnaireAnswers.genex_date,
    questionnaireAnswers.gen_gp,
    questionnaireAnswers.gen_np,
    questionnaireAnswers.exa_biochimie,
    questionnaireAnswers.hematologie,
    questionnaireAnswers.urine,
    questionnaireAnswers.q1_maux_de_tete,
    questionnaireAnswers.maux_destomac,
    questionnaireAnswers.lbl1_fatigue,
    questionnaireAnswers.var_tristesse_douleur_morale,
    questionnaireAnswers.var4_retrait_social_isolement,
    questionnaireAnswers.pensees_suicidaires,
    questionnaireAnswers.fort_sentiment_de_culpabilite_honte,
    questionnaireAnswers.problemes_de_sommeil_recurrents,
    questionnaireAnswers.difficulte_dendormissement,
    questionnaireAnswers.manque_dappetit,
    questionnaireAnswers.perturbations_sexuelles,
    questionnaireAnswers.symptomes_danxiete_frequents_palpitations_sensations_detouffement_corps_ou_tete_qui_chauffe_mains_moites,
    questionnaireAnswers.var18_sentiment_que_lavenir_est_bouche,
    questionnaireAnswers.var19_19difficulte_pour_reprendre_ses_activites,
    questionnaireAnswers.var25_hyper_vigilance_attention_exageree,
    questionnaireAnswers.comportement_agite,
    questionnaireAnswers.souvenirs_repetitifs_de_situations_liees_a_ebola,
    questionnaireAnswers.reves_repetitifs_lies_a_ebola,
    questionnaireAnswers.abus_dalcool_ou_de_substance,
    # questionnaireAnswers.vous_sentez_vous_stigmatise_par_la_communaute,
    questionnaireAnswers.evenement_traumatique,
    questionnaireAnswers.symptomatologie_actuelle,
    # questionnaireAnswers.vr_vous_sentez_vous_limite_dans_vos_activites_en_raison_dun_probleme_de_sante,
    questionnaireAnswers.refere_pour_des_soins_appropries,
    questionnaireAnswers.socia_structure_de_sante
    # questionnaireAnswers.var1_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var2_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var3_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var4_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var5_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var6_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var7_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var9_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var10_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var13_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var20_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var25_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var26_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var27_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var28_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante,
    # questionnaireAnswers.var29_est_ce_un_probleme_que_vous_avez_eu_avant_ebola_sinon_completer_la_colonne_suivante
    ),

    keep_empty = TRUE, names_sep = "_")


#Active contacts, currently under follow up (using follow up end date in case the status is off, as it sometimes is.); also unnest all nested fields

unnested_contacts <- contacts %>%
  filter(deleted == FALSE) %>%
  filter(active == TRUE) %>%
  filter(as.Date.character(followUp.endDate) >= database_date) %>%
  filter(followUp.status == "LNG_REFERENCE_DATA_CONTACT_FINAL_FOLLOW_UP_STATUS_TYPE_UNDER_FOLLOW_UP") %>%
  unnest(c(
    addresses,
    vaccinesReceived,
    followUpHistory,
    relationshipsRepresentation
  ),
  keep_empty = TRUE, names_sep = "_")

#Teams
colnames(teams)=tolower(colnames(teams))
unnested_teams <- teams %>%
  filter(deleted == FALSE) %>%
  unnest(userids, keep_empty = TRUE) %>%
  unnest(locationids, keep_empty = TRUE)
 
#Users
unnested_users <- users %>%
  filter(deleted == FALSE) %>%
  filter(activeOutbreakId %contains% outbreak_id) %>%
  unnest(roleIds, keep_empty = TRUE)
colnames(unnested_users)=tolower(colnames(unnested_users)) 

#Relationships
colnames(relationships)=tolower(colnames(relationships))
relationships <- relationships %>%
  filter(deleted == FALSE)

#Locations - rearrange via joins to get into more usable hierarchy format
colnames(locations)=tolower(colnames(locations))
locations <- locations %>%
  filter(deleted == FALSE)

####### STANDARDISE THE DATA ###################################################

## make variables lower cases and have underscore in names
## remove french character encoding.
## retain original location ids so that joins work
# to add later - dateranges_typeid 0 and 1....right now they dont appear bc they are all null i guess
cleaned_cases <- clean_data(unnested_cases, guess_dates = FALSE)  ## cleaning variable names

cleaned_contacts <- clean_data(unnested_contacts, guess_dates = FALSE)

cleaned_followups <- clean_data(unnested_followups, guess_dates = FALSE)

cleaned_locations <- clean_data(locations, guess_dates = FALSE) 

cleaned_teams <- clean_data(unnested_teams, guess_dates = FALSE)

cleaned_users <- clean_data(unnested_users, guess_dates = FALSE)

cleaned_relationships <- clean_data(relationships, guess_dates = FALSE)

#########################DISTINCT BY ID TO REMOVE DUPLICATE ####################

cleaned_cases <- cleaned_cases %>%
  distinct_at( vars(id),.keep_all = TRUE)
 
cleaned_contacts <- cleaned_contacts %>%
  distinct_at( vars(id),.keep_all = TRUE)

cleaned_followups <- cleaned_followups %>%
  distinct_at( vars(id),.keep_all = TRUE)

##############RENAME SOME VARIABLES AND SOME CONTENTS IN UPPERLOW###############
cleaned_cases = cleaned_cases %>%
  rename(id_cases = id) %>%
  mutate(firstname=toupper(firstname),
         lastname=toupper(lastname),
         middlename=toupper(middlename),
         addresses_city=toupper(addresses_city))

cleaned_contacts = cleaned_contacts %>%
  rename(id_contacts = id) %>%
  mutate(firstname=toupper(firstname),
         lastname=toupper(lastname),
         middlename=toupper(middlename),
         addresses_city=toupper(addresses_city))

cleaned_followups = cleaned_followups %>%
  rename(id_followups = id)

cleaned_teams = cleaned_teams %>%
  rename(id_teams = id) %>%
  mutate(name=toupper(name))

cleaned_users = cleaned_users %>%
  rename(id_users = id) %>%
  mutate(firstname=toupper(firstname),
         lastname=toupper(lastname))

cleaned_locations = cleaned_locations %>%
  rename(id_locations = id) %>%
  mutate(name=toupper(name))


cleaned_relationships = cleaned_relationships %>%
  rename(id_relationships = id) %>%
  mutate(source_first_name=toupper(source_first_name),
         source_last_name=toupper(source_last_name),
         source_middle_name=toupper(source_middle_name),
         target_first_name=toupper(target_first_name),
         target_middle_name=toupper(target_middle_name),
         target_last_name=toupper(target_last_name))


#--------Contacts per case--------------------------------------------

contacts_per_case <- cleaned_relationships %>%
  group_by(source_case_contact_id,source_uid,source_first_name,source_last_name) %>%
  tally() %>%
  select(firstname_case=source_first_name,
          lastname_case=source_last_name,
                  source_case_contact_id,
                  source_uid,
                  nbre_contacts = n)

survivors_with_more_contacts= contacts_per_case %>%
  filter(nbre_contacts > 1)

#---------------------------------------------------------------------
## Converts dates so that R recognises them
## other renaming
## choosing only variables we need so it is less cluttered

cleaned_cases <- cleaned_cases %>%
  mutate(date_of_reporting = guess_dates(dateofreporting),
         date_of_data_entry = guess_dates(createdat),
         date_of_onset = guess_dates(dateofonset),
         date_of_outcome = guess_dates(dateofoutcome),
         date_of_last_contact = guess_dates(dateoflastcontact),
         date_become_case = guess_dates(datebecomecase)) %>%
  
  mutate(classification = case_when(
    tolower(classification) == "lng_reference_data_category_case_classification_confirmed" ~ "Confirmé",
    tolower(classification) == "lng_reference_data_category_case_classification_suspect"  ~ "Suspect",
    tolower(classification) == "lng_reference_data_category_case_classification_probable" ~ "Probable"
  )) %>%
  mutate(gender = case_when(
    tolower(gender) == "lng_reference_data_category_gender_female" ~ "Femme",
    tolower(gender) == "lng_reference_data_category_gender_male" ~ "Homme"
  )) %>%
  
  mutate(occupation = case_when(
    tolower(occupation) == "lng_reference_data_category_occupation_business_person" ~ "Personne d'affaires",
    tolower(occupation) == "lng_reference_data_category_occupation_child" ~ "Enfant",
    tolower(occupation) == "lng_reference_data_category_occupation_farmer" ~ "Agriculteur(fermier)",
    tolower(occupation) == "lng_reference_data_category_occupation_health_care_worker" ~ "Professionnel de santé",
    tolower(occupation) == "lng_reference_data_category_occupation_housewife" ~ "Femme au foyer",
    tolower(occupation) == "lng_reference_data_category_occupation_miner" ~ "Mineur",
    tolower(occupation) == "lng_reference_data_category_occupation_other" ~ "Autre",
    tolower(occupation) == "lng_reference_data_category_occupation_religious_leader" ~ "Chef religieux",
    tolower(occupation) == "lng_reference_data_category_occupation_student" ~ "Etudiant",
    tolower(occupation) == "lng_reference_data_category_occupation_taxi_driver" ~ "Chauffeur de taxi",
    tolower(occupation) == "lng_reference_data_category_occupation_unknown"~ "Inconnu",
    tolower(occupation) == "lng_reference_data_category_occupation_teacher"~ "Enseignant",
    tolower(occupation) == "lng_reference_data_category_occupation_traditional_healer"~ "Guérisseur traditionnel",
    tolower(occupation) == "lng_reference_data_category_occupation_butcher"~ "Boucher",
    tolower(occupation) == "lng_reference_data_category_occupation_femme_daffaire"~ "Personne d'affaires",
    tolower(occupation) == "enfant"~ "Enfant",
    tolower(occupation) == "lng_reference_data_category_occupation_civil_servant"~ "Fonctionnaire",
    tolower(occupation) == "lng_reference_data_category_occupation_health_laboratory_worker"~ "Laborantin"
  )) %>%
  
  mutate(full_name = paste0(str_replace(firstname,"_"," "), " ", str_replace(lastname,"_"," ")))  %>%
  mutate(out_come_id = case_when(
    tolower(outcomeid) == "lng_reference_data_category_outcome_alive" ~ "Vivant",
    tolower(outcomeid) == "lng_reference_data_category_outcome_deceased" ~ "Décédé"
  )) %>%

  select(uuid = id_cases,
         visualid,
         full_name,
         location_id = addresses_locationid,
         city = addresses_city,
         address = addresses_addressline1,
         lat = addresses_geolocation_lat,
         long = addresses_geolocation_lng,
         phone = addresses_phonenumber,
         classification,
         out_come_id,
         #age_months,
         #age_years,
         age=questionnaireanswers_age_value,
         gender=questionnaireanswers_sexe_value,
         date_of_data_entry=createdat,
         date_become_case=datebecomecase,
         date_of_last_contact=dateoflastcontact,
         date_of_onset=dateofonset,
         date_of_outcome=dateofoutcome,
         date_of_reporting=dateofreporting,
         occupation,
         safeburial,
         transferrefused
  )

#--------------------------------------------------------------------------------------------
cleaned_cases <- cleaned_cases %>%
  left_join(contacts_per_case, by = c("uuid" = "source_uid")) %>%
  select(-source_case_contact_id) %>%
  left_join(select(cleaned_relationships, source_uid, target_uid, source_case_contact_id), by = c("uuid" = "target_uid"))

#--------------------------------------------------------------------------------------------

cleaned_contacts <- cleaned_contacts %>%
 mutate_at(vars(contains("date")), as.character) %>%
  mutate(date_of_reporting = guess_dates(dateofreporting),
         date_of_data_entry = guess_dates(createdat),
         date_of_last_contact = guess_dates(dateoflastcontact),
         date_of_followup_start = guess_dates(followup_startdate),
        date_of_followup_end = followup_enddate
  ) %>%
  mutate(followup_status = case_when(
    tolower(followup_status) == "lng_reference_data_contact_final_follow_up_status_type_follow_up_completed" ~ "completed",
    tolower(followup_status) == "lng_reference_data_contact_final_follow_up_status_type_under_follow_up" ~ "under_follow_up"
  )) %>%
  mutate(gender = case_when(
    tolower(gender) == "lng_reference_data_category_gender_female" ~ "Femme",
    tolower(gender) == "lng_reference_data_category_gender_male" ~ "Homme"
  )) %>%

  mutate(full_name = paste0(str_replace(firstname,"_"," "), " ", str_replace(lastname,"_"," "))) %>%
  mutate(occupation = case_when(
    tolower(occupation) == "lng_reference_data_category_occupation_business_person" ~ "Personne d'affaires",
    tolower(occupation) == "lng_reference_data_category_occupation_child" ~ "Enfant",
    tolower(occupation) == "lng_reference_data_category_occupation_farmer" ~ "Agriculteur(fermier)",
    tolower(occupation) == "lng_reference_data_category_occupation_health_care_worker" ~ "Professionnel de santé",
    tolower(occupation) == "lng_reference_data_category_occupation_housewife" ~ "Femme au foyer",
    tolower(occupation) == "lng_reference_data_category_occupation_miner" ~ "Mineur",
    tolower(occupation) == "lng_reference_data_category_occupation_other" ~ "Autre",
    tolower(occupation) == "lng_reference_data_category_occupation_religious_leader" ~ "Chef religieux",
    tolower(occupation) == "lng_reference_data_category_occupation_student" ~ "Etudiant",
    tolower(occupation) == "lng_reference_data_category_occupation_taxi_driver" ~ "Chauffeur de taxi",
    tolower(occupation) == "lng_reference_data_category_occupation_unknown"~ "Inconnu",
    tolower(occupation) == "lng_reference_data_category_occupation_teacher"~ "Enseignant",
    tolower(occupation) == "lng_reference_data_category_occupation_traditional_healer"~ "Guérisseur traditionnel",
    tolower(occupation) == "lng_reference_data_category_occupation_butcher"~ "Boucher",
    tolower(occupation) == "lng_reference_data_category_occupation_femme_daffaire"~ "Personne d'affaires",
    tolower(occupation) == "enfant"~ "Enfant",
    tolower(occupation) == "lng_reference_data_category_occupation_civil_servant"~ "Fonctionnaire",
    tolower(occupation) == "lng_reference_data_category_occupation_health_laboratory_worker"~ "Laborantin"
  )) %>%

  select(uuid = id_contacts,
         visualid,
         full_name,
         location_id = addresses_locationid,
         city = addresses_city,
         address = addresses_addressline1,
         phone = addresses_phonenumber,
         age_months,
         age_years,
         gender,
         date_of_data_entry,
         date_of_last_contact,
         date_of_reporting,
         followup_status,
         occupation,
         safeburial,
         wascase)

#-------------------------------------------------------------------------------------------------------

cleaned_followups <- clean_data(cleaned_followups,
                              clean_data = FALSE,
                              guess_dates = FALSE)


#cleaned_followups=AddingColumns(cleaned_followups,followups_base_clean_columns)#Check to add missed columns
########################Correcting columns added manually##########################################################
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_lait_matenel_questionnaireanswers_lait_matenel")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_genex_date_questionnaireanswers_genex_date")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_gen_gp_questionnaireanswers_gen_gp")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_gen_np_questionnaireanswers_gen_np")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_exa_biochimie_questionnaireanswers_exa_biochimie")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_hematologie_questionnaireanswers_hematologie")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_urine_questionnaireanswers_urine")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_sperme_questionnaireanswers_sperme")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_gene_questionnaireanswers_gene_date")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_gene_resultat_questionnaireanswers_gene_resultat")
cleaned_followups=correctingColumns(cleaned_followups,"questionnaireanswers_cmd_np_questionnaireanswers_cmd_np")
######################################################################################################################


cleaned_followups <- cleaned_followups %>%
  mutate(date_of_followup = guess_dates(date),
         date_of_data_entry = guess_dates(createdat),
         date_suvi_evaluation_clinique =guess_dates(questionnaireanswers_date),
         questionnaireanswers_date_de_decharge=guess_dates(questionnaireanswers_date_de_decharge),
         questionnaireanswers_vardate_prochaine_visite=guess_dates(questionnaireanswers_vardate_prochaine_visite),
         date_resultat_labo_suivi_clinique = guess_dates(questionnaireanswers_test_date),
         date_suivi_psychologique=guess_dates(questionnaireanswers_socia_date)
          ) %>%
 
  mutate(status = case_when(
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_decede" ~ "Décédé",
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_missed" ~ "Perdu de vue",
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_not_seen" ~ "not_seen",
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_not_attempted" ~ "Pas de données",
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_not_performed" ~ "Pas de données",
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_seen_not_ok" ~ "Vu avec signe",
    tolower(statusid) == "lng_reference_data_contact_daily_follow_up_status_type_seen_ok" ~ "Vu sans signe"
  )) %>%
  
  mutate(performed = case_when(
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_decede" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_missed" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_not_seen" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_seen_not_ok" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_seen_ok" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_not_attempted" ~ "FALSE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_not_performed" ~ "FALSE"
  )) %>%
  
  mutate(seen = case_when(
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_decede" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_missed" ~ "FALSE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_not_seen" ~ "FALSE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_seen_not_ok" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_seen_ok" ~ "TRUE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_not_attempted" ~ "FALSE",
     tolower(statusid) =="lng_reference_data_contact_daily_follow_up_status_type_not_performed" ~ "FALSE"
  )) %>% 
  
  mutate(gender = case_when(
    contact_gender =="lng_reference_data_category_gender_female" ~ "Femme",
    contact_gender =="lng_reference_data_category_gender_male" ~ "Homme"
  )) %>%
  select(uuid = id_followups,
         person_id = personid,
         location_id = address_locationid,
         address = address_addressline1,
         phone = address_phonenumber,
         gender,
         city = address_city,
         status,
         performed,
         seen,
         date_of_data_entry,
         date_of_followup,
         updated_at = updatedat,
         date_suvi_evaluation_clinique,
         date_resultat_labo_suivi_clinique,
         test_vih_labo=questionnaireanswers_test_vih,
         tdr_suivi_cliniaue=questionnaireanswers_tdr_palu,
         grossesse_suivi_clinique=questionnaireanswers_grossesse,
         lait_maternel=questionnaireanswers_lait_matenel_value,
         lait_maternel_genexpert_date=questionnaireanswers_genex_date_value,
         lait_maternel_genexpert_gp=questionnaireanswers_gen_gp_value,
         lait_maternel_genexpert_np=questionnaireanswers_gen_np_value,
         sperme=questionnaireanswers_sperme_value,
         sperme_genexpert_date=questionnaireanswers_gene_date_value,
         sperme_genexpert_resultat=questionnaireanswers_gene_resultat_value,
         sperme_genexpert_cmd_np=questionnaireanswers_cmd_np_value,
         resultat_serologie_ebola=questionnaireanswers_sero_resultat,
         nom_laboratoire=questionnaireanswers_name_laboratoire,
         date_suivi_psychologique,
         structure_suivi_psychologique=questionnaireanswers_socia_structure_de_sante_value,
         refere_pour_soins_psycho=questionnaireanswers_refere_pour_des_soins_appropries_value,
         refere_suivi_clin=questionnaireanswers_le_geuris_est_il_refere,
         consultation_urgence_suivi_clin=questionnaireanswers_cest_une_consultation_medicale_durgence,
         conseil_depistage_vih_suivi_clin=questionnaireanswers_le_gueris_a_t_il_beneficie_dun_coseil_depistage_vih,
         acceptance_test_vih_suivi_clin=questionnaireanswers_le_gueris_a_t_il_accepte_de_se_faire_teste,
         trouble_psycho=questionnaireanswers_le_gueris_presente_t_il_des_troubles_psychologiques,
         follow_up_number = index,
         team_id = teamid)

#---------------LOCATIONS---------------------------------------------------------

cleaned_locations <- cleaned_locations %>%
  select(location_id = id_locations,
         admin_level = geographicallevelid,
         name,
         parent_location_id = parentlocationid)

#------------------TEAMS----------------------------------------------------------

cleaned_teams <- cleaned_teams %>%
  select(uuid = id_teams,
         name,
         userids,
         locationids)

#--------------USERS------------------------------------------------------------

cleaned_users <- cleaned_users %>%
  select(uuid = id_users,
         firstname,
         lastname)
#-------------------------------------------------------------------------------

## Standardise the ages so all are in years

# cleaned_cases <- cleaned_cases %>%
#   mutate(age_years = case_when(
#     is.na(age_years) ~ as.double(age_months / 12),
#     TRUE ~ as.double(age_years)))

cleaned_contacts <- cleaned_contacts %>%
  mutate(age_years = case_when(
    is.na(age_years) ~ as.double(age_months / 12),
    TRUE ~ as.double(age_years)))

cleaned_cases <- cleaned_cases %>%
  mutate(date_of_reporting=guess_dates(date_of_reporting)) %>% #Se rassurer que le format de date est ok
  mutate(epiweek_report_label = aweek::date2week(date_of_reporting,
                                                 week_start = "Monday",
                                                 floor_day = TRUE),
         epiweek_report = aweek::week2date(epiweek_report_label,
                                           week_start = "Monday")) %>%
  mutate(
    # age = as.numeric(age_years),
    age = as.numeric(age),
    age_class = factor(
      case_when(
        age <= 5 ~ "<=5",
        age <= 10 ~ "6-10",
        age <= 17 ~ "11-17",
        age <= 25 ~ "18-25",
        age <= 35 ~ "26-35",
        age <= 45 ~ "36-45",
        age <= 55 ~ "46-55",
        age <= 65 ~ "56-65",
        is.finite(age) ~ "66+",
        TRUE ~ "unknown"
      ), levels = c(
        "<=5",
        "6-10",
        "11-17",
        "18-25",
        "26-35",
        "36-45",
        "46-55",
        "56-65",
        "66+",
        "unknown"
      )),
    age_class_plot = factor(
      age_class,
      levels = rev(levels(age_class))))

cleaned_contacts <- cleaned_contacts %>%
  mutate(epiweek_report_label = aweek::date2week(date_of_reporting,
                                                 week_start = "Monday",
                                                 floor_day = TRUE),
         epiweek_report = aweek::week2date(epiweek_report_label,
                                           week_start = "Monday")) %>%
  mutate(
    age = as.numeric(age_years),
    age_class = factor(
      case_when(
        age <= 5 ~ "<=5",
        age <= 10 ~ "6-10",
        age <= 17 ~ "11-17",
        age <= 25 ~ "18-25",
        age <= 35 ~ "26-35",
        age <= 45 ~ "36-45",
        age <= 55 ~ "46-55",
        age <= 65 ~ "56-65",
        is.finite(age) ~ "66+",
        TRUE ~ "unknown"
      ), levels = c(
        "<=5",
        "6-10",
        "11-17",
        "18-25",
        "26-35",
        "36-45",
        "46-55",
        "56-65",
        "66+",
        "unknown"
      )),
    age_class_plot = factor(
      age_class,
      levels = rev(levels(age_class))))

#################################################################

## Clean locations
## Look in clean_locations script for this

#clean_locations <- here::here('location_hierarchy')
#source(clean_locations)
path_to_functions <- here::here("hierarchy/clean_locations.R")
#scripts_files <- dir(path_to_functions, pattern = ".R$", full.names=TRUE)
path_to_functions
source(path_to_functions)

###############################################################

## Save files with a date of database
clean_folder <- here::here("data", "clean")
library(rio)

## export cases file
cases_rds_file_name <- sprintf(
  "%sclean_%s.rds",
  "cases_",
  format(database_date, "%Y-%m-%d"))

cases_rds_file_name
rio::export(cases_join,
            file.path(clean_folder, cases_rds_file_name))


## export contacts file
contacts_rds_file_name <- sprintf(
  "%sclean_%s.rds",
  "contacts_",
  format(database_date, "%Y-%m-%d"))

contacts_rds_file_name
rio::export(contacts_join,
            file.path(clean_folder, contacts_rds_file_name))


## export followups file
followups_rds_file_name <- sprintf(
  "%sclean_%s.rds",
  "followups_",
  format(database_date, "%Y-%m-%d"))

followups_rds_file_name
rio::export(followups_join,
            file.path(clean_folder, followups_rds_file_name))

## export locations file
location_rds_file_name <- sprintf(
  "%sclean_%s.rds",
  "location_",
  format(database_date, "%Y-%m-%d"))

location_rds_file_name
rio::export(hierarchy_join_renamed,
            file.path(clean_folder, location_rds_file_name))


## export cases file as csv
cases_csv_file_name <- sprintf(
  "%sclean_%s.csv",
  "cases_",
  format(database_date, "%Y-%m-%d"))

cases_csv_file_name
rio::export(cases_join,
            file.path(clean_folder, cases_csv_file_name))


## export contacts file as csv
contacts_csv_file_name <- sprintf(
  "%sclean_%s.csv",
  "contacts_",
  format(database_date, "%Y-%m-%d"))

contacts_csv_file_name
rio::export(contacts_join,
            file.path(clean_folder, contacts_csv_file_name))


## export followups file as csv
followups_csv_file_name <- sprintf(
  "%sclean_%s.csv",
  "followups_",
  format(database_date, "%Y-%m-%d"))

followups_csv_file_name
rio::export(followups_join,
            file.path(clean_folder, followups_csv_file_name))


## export locations file as csv
location_csv_file_name <- sprintf(
  "%sclean_%s.csv",
  "location_",
  format(database_date, "%Y-%m-%d"))

location_csv_file_name
rio::export(hierarchy_join_renamed,
            file.path(clean_folder, location_csv_file_name))


## export team file as csv
teams_csv_file_name <- sprintf(
  "%sclean_%s.csv",
  "teams_",
  format(database_date, "%Y-%m-%d"))

teams_csv_file_name
rio::export(cleaned_teams,
            file.path(clean_folder, teams_csv_file_name))
