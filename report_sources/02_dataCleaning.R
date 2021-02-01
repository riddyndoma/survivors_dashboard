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
    questionnaireAnswers.socia_structure_de_sante,
    questionnaireAnswers.evolution_psychologique_du_patient_depuis_la_derniere_visite
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
