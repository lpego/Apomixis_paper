excelnames <- as.character(c("Achillea_clavennae", "Achillea_erba-rotta", "Achillea_millefolium", "Achillea_millefolium", 
  "Achillea_millefolium", "Achillea_millefolium", "Achillea_millefolium", "Achillea_millefolium", 
  "Achillea_millefolium", "Achillea_millefolium", "Achillea_nana", "Achillea_nana", 
  "Achillea_nana", "Achillea_nana", "Achillea_nobilis", "Achillea_oxyloba", 
  "Adenostyles_alliariae", "Adenostyles_alliariae", "Adenostyles_alliariae", "Adenostyles_alpina", 
  "Adenostyles_leucophylla", "Adenostyles_leucophylla", "Adenostyles_leucophylla", "Adenostyles_leucophylla", "Andryala_integrifolia", "Antennaria_carpatica", "Antennaria_carpatica", "Antennaria_carpatica", "Antennaria_dioica", "Antennaria_dioica", "Antennaria_dioica", "Antennaria_dioica", "Antennaria_dioica", "Antennaria_dioica", "Aposeris_foetida", "Arctium_lappa", "Arctium_lappa", "Arctium_lappa", "Arctium_lappa", "Arctium_minus", "Arctium_minus", "Arctium_tomentosum", "Arnica_montana", "Arnica_montana", "Arnica_montana", "Arnica_montana", "Arnica_montana", "Arnica_montana", "Arnica_montana", "Artemisia_absinthium", "Artemisia_absinthium", "Artemisia_genipi", "Artemisia_glacialis", 
  "Artemisia_nitida", "Artemisia_nitida", "Artemisia_umbelliformis_subsp._eriantha", "Artemisia_umbelliformis_subsp._umbelliformis", "Artemisia_umbelliformis_subsp._umbelliformis", "Artemisia_vulgaris", "Aster_alpinus", "Aster_alpinus", "Aster_alpinus", "Aster_alpinus", "Aster_alpinus", "Aster_alpinus", "Aster_alpinus", "Bellidastrum_michelii", "Bellis_perennis", "Berardia_lanuginosa", "Berardia_lanuginosa", "Bidens_bipinnata", "Bidens_frondosa", "Buphthalmum_salicifolium", "Buphthalmum_salicifolium", "Buphthalmum_salicifolium", "Buphthalmum_salicifolium", "Buphthalmum_salicifolium", "Calendula_arvensis", "Calendula_arvensis", "Calendula_tripterocarpa", "Carduus_acanthoides", "Carduus_crispus", "Carduus_defloratus", "Carduus_defloratus_subsp._carlinifolius", "Carduus_defloratus_subsp._summanus", "Carduus_personata", "Carduus_personata", "Carlina_acanthifolia", "Carlina_acaulis", "Carlina_acaulis", "Carlina_corymbosa", "Carlina_vulgaris",
  "Catananche_caerulea", "Catananche_caerulea", "Catananche_caerulea", "Catananche_caerulea", "Centaurea_benedicta", "Centaurea_jacea", "Centaurea_jacea", "Centaurea_jacea", "Centaurea_jacea_subsp._gaudinii", "Centaurea_jacea_subsp._gaudinii", "Centaurea_leucophaea", "Centaurea_splendens", "Centaurea_nervosa", "Centaurea_nervosa", "Centaurea_nigra", "Centaurea_pectinata", "Centaurea_rhaetica", "Centaurea_rupestris", "Centaurea_scabiosa_subsp._alpestris", "Centaurea_scabiosa_subsp._alpestris", "Centaurea_scabiosa_subsp._alpestris", "Centaurea_scabiosa_subsp._alpestris", "Centaurea_scabiosa_subsp._alpestris", "Centaurea_scabiosa_subsp._grinensis", "Centaurea_uniflora", "Centaurea_uniflora", "Centaurea_uniflora", "Centaurea_vallesiaca", "Chondrilla_juncea", "Chondrilla_juncea", "Cirsium_acaule", "Cirsium_acaule", "Cirsium_acaule", "Cirsium_acaule", "Cirsium_acaule", "Cirsium_alsophilum", "Cirsium_alsophilum", "Cirsium_alsophilum", "Cirsium_arvense", "Cirsium_arvense", "Cirsium_carniolicum", "Cirsium_carniolicum", "Cirsium_eriophorum", "Cirsium_erisithales", "Cirsium_erisithales", "Cirsium_heterophyllum", "Cirsium_monspessulanum", "Cirsium_monspessulanum", "Cirsium_monspessulanum", "Cirsium_oleraceum", "Cirsium_oleraceum", "Cirsium_pannonicum", "Cirsium_rivulare", "Cirsium_spinosissimum", "Cirsium_spinosissimum", "Cirsium_tuberosum", "Cirsium_vulgare", "Cirsium_vulgare", "Cota_tinctoria", "Cota_tinctoria", "Cota_triumfettii", "Crepis_aurea", "Crepis_aurea", "Crepis_capillaris", "Crepis_conyzifolia", "Crepis_conyzifolia", "Crepis_kerneri", "Crepis_paludosa", "Crepis_paludosa", "Crepis_pontana", "Crepis_pontana", "Crepis_pygmaea", "Crepis_pygmaea", "Crepis_pyrenaica", "Crepis_pyrenaica", "Crepis_rhaetica", "Crepis_sancta", "Crepis_tectorum", "Crepis_terglouensis", "Crepis_vesicaria", "Crepis_vesicaria", "Crepis_vesicaria", "Crepis_vesicaria", "Crupina_vulgaris", "Cyanus_montanus", 
  "Cyanus_montanus", "Cyanus_segetum", "Cyanus_segetum", "Cyanus_triumfettii", "Dittrichia_graveolens", "Doronicum_austriacum", "Doronicum_clusii", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_grandiflorum", "Doronicum_pardalianches", "Echinops_exaltatus", "Echinops_ritro", "Echinops_sphaerocephalus", "Echinops_sphaerocephalus", "Erigeron_acris_subsp._acris", "Erigeron_acris_subsp._acris", "Erigeron_alpinus", "Erigeron_alpinus", "Erigeron_alpinus", "Erigeron_alpinus", "Erigeron_annuus", "Erigeron_atticus", "Erigeron_bonariensis", "Erigeron_canadensis", "Erigeron_canadensis", "Erigeron_glabratus", "Erigeron_karvinskianus", "Erigeron_schleicheri", "Erigeron_schleicheri", "Erigeron_uniflorus", "Erigeron_uniflorus", "Eupatorium_cannabinum", "Galinsoga_parviflora", "Galinsoga_quadriradiata", "Gnaphalium_hoppeanum", "Gnaphalium_hoppeanum", "Gnaphalium_hoppeanum", "Gnaphalium_hoppeanum", "Gnaphalium_supinum", "Gnaphalium_supinum", "Gnaphalium_supinum", "Gnaphalium_sylvaticum", "Helianthus_tuberosus", "Helichrysum_italicum", "Helichrysum_italicum", "Helichrysum_italicum", "Hieracium_alpinum", "Hieracium_amplexicaule", "Hieracium_amplexicaule", "Hieracium_bifidum", "Hieracium_cydoniifolium", "Hieracium_froelichianum", "Hieracium_glaucopsis", "Hieracium_glaucum", "Hieracium_humile", "Hieracium_lawsonii", "Hieracium_murorum", "Hieracium_murorum", "Hieracium_murorum", "Hieracium_murorum", "Hieracium_murorum", "Hieracium_piliferum", "Hieracium_piliferum", "Hieracium_prenanthoides", "Hieracium_ramosissimums_subsp._lactucifolium", "Hieracium_tomentosum", "Hieracium_valdepilosum", "Hieracium_villosum", "Hieracium_villosum", "Hieracium_villosum", "Homogyne_alpina", "Homogyne_alpina", "Homogyne_alpina", "Homogyne_discolor", "Homogyne_sylvestris", "Hypochaeris_maculata", "Hypochaeris_maculata", "Hypochaeris_maculata", "Hypochaeris_radicata", "Hypochaeris_radicata", "Hypochaeris_uniflora", "Hypochaeris_uniflora", "Hypochaeris_uniflora", "Inula_bifrons", "Inula_conyzae", "Inula_conyzae", "Inula_helenium", "Inula_helvetica", "Inula_montana", "Inula_oculus-christi", "Inula_salicina", "Inula_spiraefolia", "Jacobaea_abrotanifolia_subsp._abrotanifolia", "Jacobaea_abrotanifolia_subsp._abrotanifolia", "Jacobaea_abrotanifolia_subsp._abrotanifolia", "Jacobaea_alpina_subsp._alpina", "Jacobaea_alpina_subsp._alpina", "Jacobaea_aquatica", "Jacobaea_incana_subsp._carniolica", "Jacobaea_incana_subsp._carniolica", "Jacobaea_erucifolia", "Jacobaea_erucifolia", "Jacobaea_erucifolia",
  "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_incana_subsp._incana", "Jacobaea_subalpina", "Jurinea_mollis", "Klasea_lycopifolia", "Cicerbita_alpina", "Cicerbita_alpina", "Lactuca_muralis", "Lactuca_muralis", "Lactuca_perennis", "Lactuca_perennis", "Lactuca_perennis", "Lactuca_serriola", "Lactuca_serriola", "Laphangium_luteoalbum", "Lapsana_communis", "Lapsana_communis", "Leontodon_crispus", "Leontodon_hirtus", "Leontodon_hispidus", "Leontodon_hispidus", "Leontodon_incanus_subsp._tenuiflorus", "Leontopodium_nivale_subsp._alpinum", "Leontopodium_nivale_subsp._alpinum", "Leontopodium_nivale_subsp._alpinum", "Leontopodium_nivale_subsp._alpinum", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemopsis_alpina", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_adustum", "Leucanthemum_atratum", "Leucanthemum_atratum", "Leucanthemum_coronopifolium", "Leucanthemum_coronopifolium", "Leucanthemum_coronopifolium", "Leucanthemum_halleri", "Leucanthemum_pallens", "Leucanthemum_platylepis", "Matricaria_chamomila", "Onopordium_acanthium", "Onopordium_acanthium", "Pallenis_spinosa", "Petasites_albus", "Petasites_paradoxus", "Phagnalon_saxatile", "Picris_hieracioides", "Picris_hieracioides", "Picris_hieracioides", "Pilosella_aurantiaca", "Pilosella_aurantiaca", "Pilosella_cymosa", "Pilosella_glacialis", "Pilosella_hoppeana", "Hieracium_lactucella", "Pilosella_officinarum", "Pilosella_peleteriana", "Pilosella_peleteriana", "Pilosella_piloselloides", "Scorzonera_laciniata", "Podospermum_purpureum", "Prenanthes_purpurea", "Pulicaria_dysenterica", "Pulicaria_dysenterica", "Rhaponticum_heleniifolium_subsp._bicknellii", "Rhaponticum_heleniifolium_subsp._helenifolium", "Rhaponticum_scariosum", "Saussurea_alpina", "Saussurea_alpina", "Schlagintweitia_huteri_subsp._lantoscana", "Schlagintweitia_intybacea", "Scorzonera_humilis", "Scorzoneroides_autumnalis", "Scorzoneroides_crocea", "Scorzoneroides_crocea", "Scorzoneroides_montana", "Scorzoneroides_montana", "Scorzoneroides_montana", "Senecio_doria", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_doronicum", "Senecio_inaequidens", 
  "Senecio_nemorensis_subsp._jacquinianus", "Senecio_ovatus", "Senecio_squalidus_subsp._rupestris", 
  "Senecio_squalidus_subsp._rupestris", 
  "Senecio_viscosus", "Senecio_viscosus", "Senecio_vulgaris", "Senecio_vulgaris", "Senecio_vulgaris", "Senecio_vulgaris", "Serratula_tinctoria", "Sigesbeckia_orientalis", "Solidago_virgaurea", "Solidago_virgaurea", "Solidago_virgaurea", "Solidago_virgaurea", "Solidago_virgaurea", "Solidago_virgaurea", "Solidago_virgaurea_subsp._minuta",
  "Sonchus_oleraceus", "Sonchus_oleraceus", "Sonchus_tenerrimus", "Staehelina_dubia", "Staehelina_dubia", "Aster_squamatus", "Tanacetum_corymbosum", "Tanacetum_macrophyllum", "Tanacetum_parthenium", "Taraxacum_officinale", "Taraxacum_officinale", "Taraxacum_officinale", "Taraxacum_officinale", "Telekia_speciosa", "Telekia_speciosa", "Tephroseris_integrifolia", "Tephroseris_integrifolia_subsp._capitata", "Tephroseris_longifolia_subsp._gaudinii", "Tolpis_staticifolia", "Tolpis_staticifolia", "Tragopogon_crocifolius", "Tragopogon_dubius", "Tragopogon_pratensis", "Tragopogon_pratensis", "Tragopogon_pratensis", "Tragopogon_pratensis", "Tragopogon_pratensis", "Tragopogon_pratensis_subsp._orientalis", "Tripleurospermum_inodorum", "Tripleurospermum_inodorum", "Tripleurospermum_inodorum", "Tussilago_farfara", "Urospermum_dalechampii", "Urospermum_dalechampii", "Urospermum_picroides", "Xanthium_italicum", 
  "Xanthium_italicum", "Xeranthemum_annuum", "Xerolekia_speciosissima"))
excelnames
length(excelnames)
unique(excelnames)

JanTree4$tip.label
unique(JanTree4$tip.label)
length(JanTree4$tip.label)

data_red$animal
unique(data_red$animal)
length(data_red$animal)

setdiff(data_red$animal, JanTree4_red$tip.label)
setdiff(JanTree4_red$tip.label, data_red$animal)

setdiff(JanTree4$tip.label, excelnames)
setdiff(excelnames, JanTree4$tip.label)

setdiff(data_red$animal, excelnames)
setdiff(excelnames, data_red$animal)

# cbind.fill(excelnames, data_red$animal, JanTree4_red$tip.label)

nrow(DATA_CC)
setdiff(DATA_CC_red$SpeciesName, data_red$animal)

View(DATA_CC[DATA_CC$SpeciesName %in% setdiff(DATA_CC$SpeciesName, data_red$animal), ])
DATA_CC

is.ultrametric(JanTree4)
is.binary(JanTree4)
testtree <- multi2di(JanTree4)
is.binary(testtree)
testtree$edge.length[testtree$edge.length == 0]
testtree <- compute.brlen(testtree, method = "Grafen")
is.ultrametric(testtree)
is.binary(testtree)
is.binary(testtree)
testtree$edge.length[testtree$edge.length == 0]

geiger::name.check(testtree, data_red$animal)

### 

plot(JanTree, cex = .25)

library(phytools)
facetoface3 <- cophylo(JanTree, JanTree4) # original VS added tips
plot(facetoface3, fsize = .3, pts = F)
facetoface4 <- cophylo(JanTree, JanTree4_red) # added tips VS renamed & dropped tips
plot(facetoface4, fsize = .3, pts = F)

### 

data_red$animal

read.csv(file = "DATA_CC_red.csv", header = T)[, 3]

### 

as.character(data_red$animal)
unique(vapply(strsplit(as.character(data_red$animal), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
unique(vapply(strsplit(as.character(data_red$animal), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))[order(unique(vapply(strsplit(as.character(data_red$animal), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))))]


supptable <- c("Achillea clavennae", "Achillea erba-rotta", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", 
               "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", 
               "Achillea nana", "Achillea nana", "Achillea nana", "Achillea nana", "Achillea nobilis", "Achillea oxyloba", 
               "Adenostyles alliariae", "Adenostyles alliariae", "Adenostyles alliariae", "Adenostyles alpina", "Adenostyles leucophylla", 
               "Adenostyles leucophylla", "Adenostyles leucophylla", "Adenostyles leucophylla", "Andryala integrifolia", 
               "Antennaria carpatica", "Antennaria carpatica", "Antennaria carpatica", "Antennaria dioica", "Antennaria dioica", 
               "Antennaria dioica", "Antennaria dioica", "Antennaria dioica", "Antennaria dioica", "Aposeris foetida", "Arctium lappa", 
               "Arctium lappa", "Arctium lappa", "Arctium lappa", "Arctium minus", "Arctium minus", "Arctium tomentosum", "Arnica montana", 
               "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", 
               "Artemisia absinthium", "Artemisia absinthium", "Artemisia genipi", "Artemisia glacialis", "Artemisia nitida", 
               "Artemisia nitida", "Artemisia umbelliformis subsp. eriantha", "Artemisia umbelliformis subsp. umbelliformis", 
               "Artemisia umbelliformis subsp. umbelliformis", "Artemisia vulgaris", "Aster alpinus", "Aster alpinus", "Aster alpinus", 
               "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Bellidiastrum michelii", "Bellis perennis", 
               "Berardia lanuginosa", "Berardia lanuginosa", "Bidens bipinnatus", "Bidens frondosus", "Buphthalmum salicifolium", 
               "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", 
               "Calendula arvensis", "Calendula arvensis", "Calendula tripterocarpa", "Carduus acanthoides", "Carduus crispus", 
               "Carduus defloratus", "Carduus defloratus subsp. carlinifolius", "Carduus defloratus subsp. summanus", "Carduus personata", 
               "Carduus personata", "Carlina acanthifolia", "Carlina acaulis", "Carlina acaulis", "Carlina corymbosa", "Carlina vulgaris", 
               "Catananche caerulea", "Catananche caerulea", "Catananche caerulea", "Catananche caerulea", "Centaurea benedicta", 
               "Centaurea jacea", "Centaurea jacea", "Centaurea jacea", "Centaurea jacea subsp. gaudinii", "Centaurea jacea subsp. gaudinii", 
               "Centaurea leucophaea", "Centaurea margaritacea", "Centaurea nervosa", "Centaurea nervosa", "Centaurea nigra", 
               "Centaurea pectinata", "Centaurea rhaetica", "Centaurea rupestris", "Centaurea scabiosa subsp. alpestris", 
               "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", 
               "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. grinensis", "Centaurea uniflora", "Centaurea uniflora", 
               "Centaurea uniflora", "Centaurea valesiaca", "Chondrilla juncea", "Chondrilla juncea", "Cirsium acaulon", 
               "Cirsium acaulon", "Cirsium acaulon", "Cirsium acaulon", "Cirsium acaulon", "Cirsium alsophilum", "Cirsium alsophilum", 
               "Cirsium alsophilum", "Cirsium arvense", "Cirsium arvense", "Cirsium carniolicum", "Cirsium carniolicum", 
               "Cirsium eriophorum", "Cirsium erisithales", "Cirsium erisithales", "Cirsium heterophyllum", "Cirsium monspessulanum", 
               "Cirsium monspessulanum", "Cirsium monspessulanum", "Cirsium oleraceum", "Cirsium oleraceum", "Cirsium pannonicum", 
               "Cirsium rivulare", "Cirsium spinosissimum", "Cirsium spinosissimum", "Cirsium tuberosum", "Cirsium vulgare", 
               "Cirsium vulgare", "Cota tinctoria", "Cota tinctoria", "Cota triumfettii", "Crepis aurea", "Crepis aurea", 
               "Crepis capillaris", "Crepis conyzifolia", "Crepis conyzifolia", "Crepis jacquinii subsp. kerneri", "Crepis paludosa", 
               "Crepis paludosa", "Crepis pontana", "Crepis pontana", "Crepis pygmaea", "Crepis pygmaea", "Crepis pyrenaica", 
               "Crepis pyrenaica", "Crepis rhaetica", "Crepis sancta", "Crepis tectorum", "Crepis terglouensis", "Crepis vesicaria", 
               "Crepis vesicaria", "Crepis vesicaria", "Crepis vesicaria", "Crupina vulgaris", "Cyanus montanus", "Cyanus montanus", 
               "Cyanus segetum", "Cyanus segetum", "Cyanus triumfettii", "Dittrichia graveolens", "Doronicum austriacum", "Doronicum clusii", 
               "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", 
               "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", 
               "Doronicum grandiflorum", "Doronicum pardalianches", "Echinops exaltatus", "Echinops ritro", "Echinops sphaerocephalus", 
               "Echinops sphaerocephalus", "Erigeron acris subsp. acris", "Erigeron acris subsp. acris", "Erigeron alpinus", 
               "Erigeron alpinus", "Erigeron alpinus", "Erigeron alpinus", "Erigeron annuus", "Erigeron atticus", "Erigeron bonariensis", 
               "Erigeron canadensis", "Erigeron canadensis", "Erigeron glabratus", "Erigeron karvinskianus", "Erigeron schleicheri", 
               "Erigeron schleicheri", "Erigeron uniflorus", "Erigeron uniflorus", "Eupatorium cannabinum", "Galinsoga parviflora", 
               "Galinsoga quadriradiata", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", 
               "Gnaphalium supinum", "Gnaphalium supinum", "Gnaphalium supinum", "Gnaphalium sylvaticum", "Helianthus tuberosus", 
               "Helichrysum italicum", "Helichrysum italicum", "Helichrysum italicum", "Hieracium alpinum", "Hieracium amplexicaule", 
               "Hieracium amplexicaule", "Hieracium bifidum", "Hieracium cydoniifolium", "Hieracium froelichianum", "Hieracium glaucopsis", 
               "Hieracium glaucum", "Hieracium humile", "Hieracium lawsonii", "Hieracium murorum", "Hieracium murorum", 
               "Hieracium murorum", "Hieracium murorum", "Hieracium murorum", "Hieracium piliferum", "Hieracium piliferum", 
               "Hieracium prenanthoides", "Hieracium ramosissimum subsp. lactucifolium", "Hieracium tomentosum", "Hieracium valdepilosum", 
               "Hieracium villosum", "Hieracium villosum", "Hieracium villosum", "Homogyne alpina", "Homogyne alpina", "Homogyne alpina", 
               "Homogyne discolor", "Homogyne sylvestris", "Hypochaeris maculata", "Hypochaeris maculata", "Hypochaeris maculata", 
               "Hypochaeris radicata", "Hypochaeris radicata", "Hypochaeris uniflora", "Hypochaeris uniflora", "Hypochaeris uniflora", 
               "Inula bifrons", "Inula conyzae", "Inula conyzae", "Inula helenium", "Inula helvetica", "Inula montana", 
               "Inula oculus-christi", "Inula salicina", "Inula spiraefolia", "Jacobaea abrotanifolia subsp. abrotanifolia", 
               "Jacobaea abrotanifolia subsp. abrotanifolia", "Jacobaea abrotanifolia subsp. abrotanifolia", "Jacobaea alpina subsp. alpina", 
               "Jacobaea alpina subsp. alpina", "Jacobaea aquatica", "Jacobaea carniolica", "Jacobaea carniolica", "Jacobaea erucifolia", 
               "Jacobaea erucifolia", "Jacobaea erucifolia", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", 
               "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea subalpina", "Jurinea mollis", 
               "Klasea lycopifolia", "Lactuca alpina ", "Lactuca alpina ", "Lactuca muralis", "Lactuca muralis", "Lactuca perennis", 
               "Lactuca perennis", "Lactuca perennis", "Lactuca serriola", "Lactuca serriola", "Laphangium luteoalbum", "Lapsana communis", 
               "Lapsana communis", "Leontodon crispus", "Leontodon hirtus", "Leontodon hispidus", "Leontodon hispidus", 
               "Leontodon tenuiflorus ", "Leontopodium nivale subsp. alpinum", "Leontopodium nivale subsp. alpinum", 
               "Leontopodium nivale subsp. alpinum", "Leontopodium nivale subsp. alpinum", "Leucanthemopsis alpina", "Leucanthemopsis alpina", 
               "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", 
               "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", 
               "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", 
               "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum atratum", "Leucanthemum atratum", "Leucanthemum coronopifolium", 
               "Leucanthemum coronopifolium", "Leucanthemum coronopifolium", "Leucanthemum halleri", "Leucanthemum pallens", 
               "Leucanthemum platylepis", "Matricaria chamomilla", "Onopordum acanthium", "Onopordum acanthium", "Pallenis spinosa", 
               "Petasites albus", "Petasites paradoxus", "Phagnalon saxatile", "Picris hieracioides", "Picris hieracioides", 
               "Picris hieracioides", "Pilosella aurantiaca", "Pilosella aurantiaca", "Pilosella cymosa", "Pilosella glacialis", 
               "Pilosella hoppeana", "Pilosella lactucella", "Pilosella officinarum", "Pilosella peleteriana", "Pilosella peleteriana", 
               "Pilosella piloselloides", "Podospermum laciniatum", "Podospermum purpureum", "Prenanthes purpurea", "Pulicaria dysenterica", 
               "Pulicaria dysenterica", "Rhaponticum heleniifolium subsp. bicknellii", "Rhaponticum heleniifolium subsp. heleniifolium", 
               "Rhaponticum scariosum", "Saussurea alpina", "Saussurea alpina", "Schlagintweitia huteri subsp. lantoscana", 
               "Schlagintweitia intybacea", "Scorzonera humilis", "Scorzoneroides autumnalis", "Scorzoneroides crocea", 
               "Scorzoneroides crocea", "Scorzoneroides montana", "Scorzoneroides montana", "Scorzoneroides montana", "Senecio doria", 
               "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", 
               "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", 
               "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio inaequidens", "Senecio nemorensis subsp. jacquinianus", 
               "Senecio ovatus", "Senecio squalidus subsp. rupestris", "Senecio squalidus subsp. rupestris", "Senecio viscosus", 
               "Senecio viscosus", "Senecio vulgaris", "Senecio vulgaris", "Senecio vulgaris", "Senecio vulgaris", "Serratula tinctoria", 
               "Sigesbeckia orientalis", "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea", 
               "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea subsp. minuta", "Sonchus oleraceus", "Sonchus oleraceus", 
               "Sonchus tenerrimus", "Staehelina dubia", "Staehelina dubia", "Symphyotrichum squamatum", "Tanacetum corymbosum", 
               "Tanacetum macrophyllum", "Tanacetum parthenium", "Taraxacum officinale", "Taraxacum officinale", "Taraxacum officinale", 
               "Taraxacum officinale", "Telekia speciosa", "Telekia speciosa", "Tephroseris integrifolia", 
               "Tephroseris integrifolia subsp. capitata", "Tephroseris longifolia subsp. gaudinii", "Tolpis staticifolia", 
               "Tolpis staticifolia", "Tragopogon crocifolius", "Tragopogon dubius", "Tragopogon pratensis", "Tragopogon pratensis", 
               "Tragopogon pratensis", "Tragopogon pratensis", "Tragopogon pratensis", "Tragopogon pratensis subsp. orientalis", 
               "Tripleurospermum inodorum", "Tripleurospermum inodorum", "Tripleurospermum inodorum", "Tussilago farfara", 
               "Urospermum dalechampii", "Urospermum dalechampii", "Urospermum picroides", "Xanthium orientale subsp. italicum ", 
               "Xanthium orientale subsp. italicum", "Xeranthemum annuum", "Xerolekia speciosissima")
supptable
unique(supptable)

unique(vapply(strsplit(as.character(supptable), '(?<=[a-z])\\s(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))

setdiff(unique(vapply(strsplit(as.character(data_red$animal), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))), 
        unique(vapply(strsplit(as.character(supptable), '(?<=[a-z])\\s(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
)

setdiff(unique(vapply(strsplit(as.character(supptable), '(?<=[a-z])\\s(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))), 
        unique(vapply(strsplit(as.character(data_red$animal), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
)




##### Checking taxa numbers across different versions of the dataset ##### 
### Extended dataset
vapply(strsplit(as.character(unique(DATA$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))
genera_all <- unique(vapply(strsplit(as.character(unique(DATA$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
genera_all
### Srictly Alps
vapply(strsplit(as.character(unique(DATA_StrictlyAlps$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))
genera_alps <- unique(vapply(strsplit(as.character(unique(DATA_StrictlyAlps$SpeciesName)), '(?<=[a-z])_(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
genera_alps
### Apomixis paper Supplementary Table v5
table_v5 <- c("Achillea clavennae", "Achillea erba-rotta", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", 
          "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea millefolium", "Achillea nana", "Achillea nana", "Achillea nana", 
          "Achillea nana", "Achillea nobilis", "Achillea oxyloba", "Adenostyles alliariae", "Adenostyles alliariae", "Adenostyles alliariae", "Adenostyles alpina", 
          "Adenostyles leucophylla", "Adenostyles leucophylla", "Adenostyles leucophylla", "Adenostyles leucophylla", "Andryala integrifolia", 
          "Antennaria carpatica", "Antennaria carpatica", "Antennaria carpatica", "Antennaria dioica", "Antennaria dioica", "Antennaria dioica", 
          "Antennaria dioica", "Antennaria dioica", "Antennaria dioica", "Aposeris foetida", "Arctium lappa", "Arctium lappa", "Arctium lappa", "Arctium lappa", 
          "Arctium minus", "Arctium minus", "Arctium tomentosum", "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", "Arnica montana", 
          "Arnica montana", "Arnica montana", "Artemisia absinthium", "Artemisia absinthium", "Artemisia genipi", "Artemisia glacialis", "Artemisia nitida", 
          "Artemisia nitida", "Artemisia umbelliformis subsp. eriantha", "Artemisia umbelliformis subsp. umbelliformis", "Artemisia umbelliformis subsp. umbelliformis", 
          "Artemisia vulgaris", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", "Aster alpinus", 
          "Bellidiastrum michelii", "Bellis perennis", "Berardia lanuginosa", "Berardia lanuginosa", "Bidens bipinnatus", "Bidens frondosus", 
          "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", "Buphthalmum salicifolium", 
          "Calendula arvensis", "Calendula arvensis", "Calendula tripterocarpa", "Carduus acanthoides", "Carduus crispus", "Carduus defloratus", 
          "Carduus defloratus subsp. carlinifolius", "Carduus defloratus subsp. summanus", "Carduus personata", "Carduus personata", "Carlina acanthifolia", 
          "Carlina acaulis", "Carlina acaulis", "Carlina corymbosa", "Carlina vulgaris", "Catananche caerulea", "Catananche caerulea", "Catananche caerulea", 
          "Catananche caerulea", "Centaurea benedicta", "Centaurea jacea", "Centaurea jacea", "Centaurea jacea", "Centaurea jacea subsp. gaudinii", 
          "Centaurea jacea subsp. gaudinii", "Centaurea leucophaea", "Centaurea margaritacea", "Centaurea nervosa", "Centaurea nervosa", "Centaurea nigra", 
          "Centaurea pectinata", "Centaurea rhaetica", "Centaurea rupestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", 
          "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. alpestris", "Centaurea scabiosa subsp. grinensis", 
          "Centaurea uniflora", "Centaurea uniflora", "Centaurea uniflora", "Centaurea valesiaca", "Chondrilla juncea", "Chondrilla juncea", "Cirsium acaulon", 
          "Cirsium acaulon", "Cirsium acaulon", "Cirsium acaulon", "Cirsium acaulon", "Cirsium alsophilum", "Cirsium alsophilum", "Cirsium alsophilum", 
          "Cirsium arvense", "Cirsium arvense", "Cirsium carniolicum", "Cirsium carniolicum", "Cirsium eriophorum", "Cirsium erisithales", "Cirsium erisithales", 
          "Cirsium heterophyllum", "Cirsium monspessulanum", "Cirsium monspessulanum", "Cirsium monspessulanum", "Cirsium oleraceum", "Cirsium oleraceum", 
          "Cirsium pannonicum", "Cirsium rivulare", "Cirsium spinosissimum", "Cirsium spinosissimum", "Cirsium tuberosum", "Cirsium vulgare", "Cirsium vulgare", 
          "Cota tinctoria", "Cota tinctoria", "Cota triumfettii", "Crepis aurea", "Crepis aurea", "Crepis capillaris", "Crepis conyzifolia", "Crepis conyzifolia", 
          "Crepis jacquinii subsp. kerneri", "Crepis paludosa", "Crepis paludosa", "Crepis pontana", "Crepis pontana", "Crepis pygmaea", "Crepis pygmaea", 
          "Crepis pyrenaica", "Crepis pyrenaica", "Crepis rhaetica", "Crepis sancta", "Crepis tectorum", "Crepis terglouensis", "Crepis vesicaria", "Crepis vesicaria", 
          "Crepis vesicaria", "Crepis vesicaria", "Crupina vulgaris", "Cyanus montanus", "Cyanus montanus", "Cyanus segetum", "Cyanus segetum", 
          "Cyanus triumfettii", "Dittrichia graveolens", "Doronicum austriacum", "Doronicum clusii", "Doronicum grandiflorum", "Doronicum grandiflorum", 
          "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", "Doronicum grandiflorum", 
          "Doronicum grandiflorum", "Doronicum pardalianches", "Echinops exaltatus", "Echinops ritro", "Echinops sphaerocephalus", "Echinops sphaerocephalus", 
          "Erigeron acris subsp. acris", "Erigeron acris subsp. acris", "Erigeron alpinus", "Erigeron alpinus", "Erigeron alpinus", "Erigeron alpinus", 
          "Erigeron annuus", "Erigeron atticus", "Erigeron bonariensis", "Erigeron canadensis", "Erigeron canadensis", "Erigeron glabratus", "Erigeron karvinskianus", 
          "Erigeron schleicheri", "Erigeron schleicheri", "Erigeron uniflorus", "Erigeron uniflorus", "Eupatorium cannabinum", "Galinsoga parviflora", 
          "Galinsoga quadriradiata", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium hoppeanum", "Gnaphalium supinum", 
          "Gnaphalium supinum", "Gnaphalium supinum", "Gnaphalium sylvaticum", "Helianthus tuberosus", "Helichrysum italicum", "Helichrysum italicum", 
          "Helichrysum italicum", "Hieracium alpinum", "Hieracium amplexicaule", "Hieracium amplexicaule", "Hieracium bifidum", "Hieracium cydoniifolium", 
          "Hieracium froelichianum", "Hieracium glaucopsis", "Hieracium glaucum", "Hieracium humile", "Hieracium lawsonii", "Hieracium murorum", 
          "Hieracium murorum", "Hieracium murorum", "Hieracium murorum", "Hieracium murorum", "Hieracium piliferum", "Hieracium piliferum", "Hieracium prenanthoides", 
          "Hieracium ramosissimum subsp. lactucifolium", "Hieracium tomentosum", "Hieracium valdepilosum", "Hieracium villosum", "Hieracium villosum", 
          "Hieracium villosum", "Homogyne alpina", "Homogyne alpina", "Homogyne alpina", "Homogyne discolor", "Homogyne sylvestris", "Hypochaeris maculata", 
          "Hypochaeris maculata", "Hypochaeris maculata", "Hypochaeris radicata", "Hypochaeris radicata", "Hypochaeris uniflora", "Hypochaeris uniflora", 
          "Hypochaeris uniflora", "Inula bifrons", "Inula conyzae", "Inula conyzae", "Inula helenium", "Inula helvetica", "Inula montana", "Inula oculus-christi", 
          "Inula salicina", "Inula spiraefolia", "Jacobaea abrotanifolia subsp. abrotanifolia", "Jacobaea abrotanifolia subsp. abrotanifolia", 
          "Jacobaea abrotanifolia subsp. abrotanifolia", "Jacobaea alpina subsp. alpina", "Jacobaea alpina subsp. alpina", "Jacobaea aquatica", "Jacobaea carniolica", 
          "Jacobaea carniolica", "Jacobaea erucifolia", "Jacobaea erucifolia", "Jacobaea erucifolia", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", 
          "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea incana", "Jacobaea subalpina", "Jurinea mollis", "Klasea lycopifolia", 
          "Lactuca alpina ", "Lactuca alpina ", "Lactuca muralis", "Lactuca muralis", "Lactuca perennis", "Lactuca perennis", "Lactuca perennis", "Lactuca serriola", 
          "Lactuca serriola", "Laphangium luteoalbum", "Lapsana communis", "Lapsana communis", "Leontodon crispus", "Leontodon hirtus", "Leontodon hispidus", 
          "Leontodon hispidus", "Leontodon tenuiflorus ", "Leontopodium nivale subsp. alpinum", "Leontopodium nivale subsp. alpinum", 
          "Leontopodium nivale subsp. alpinum", "Leontopodium nivale subsp. alpinum", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", 
          "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemopsis alpina", "Leucanthemum adustum", 
          "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", 
          "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum adustum", "Leucanthemum atratum", "Leucanthemum atratum", "Leucanthemum coronopifolium", 
          "Leucanthemum coronopifolium", "Leucanthemum coronopifolium", "Leucanthemum halleri", "Leucanthemum pallens", "Leucanthemum platylepis", 
          "Matricaria chamomilla", "Onopordum acanthium", "Onopordum acanthium", "Pallenis spinosa", "Petasites albus", "Petasites paradoxus", "Phagnalon saxatile", 
          "Picris hieracioides", "Picris hieracioides", "Picris hieracioides", "Pilosella aurantiaca", "Pilosella aurantiaca", "Pilosella cymosa", 
          "Pilosella glacialis", "Pilosella hoppeana", "Pilosella lactucella", "Pilosella officinarum", "Pilosella peleteriana", "Pilosella peleteriana", 
          "Pilosella piloselloides", "Podospermum laciniatum", "Podospermum purpureum", "Prenanthes purpurea", "Pulicaria dysenterica", "Pulicaria dysenterica", 
          "Rhaponticum heleniifolium subsp. bicknellii", "Rhaponticum heleniifolium subsp. heleniifolium", "Rhaponticum scariosum", "Saussurea alpina", 
          "Saussurea alpina", "Schlagintweitia huteri subsp. lantoscana", "Schlagintweitia intybacea", "Scorzonera humilis", "Scorzoneroides autumnalis", 
          "Scorzoneroides crocea", "Scorzoneroides crocea", "Scorzoneroides montana", "Scorzoneroides montana", "Scorzoneroides montana", "Senecio doria", 
          "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", 
          "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", "Senecio doronicum", 
          "Senecio doronicum", "Senecio inaequidens", "Senecio nemorensis subsp. jacquinianus", "Senecio ovatus", "Senecio squalidus subsp. rupestris", 
          "Senecio squalidus subsp. rupestris", "Senecio viscosus", "Senecio viscosus", "Senecio vulgaris", "Senecio vulgaris", "Senecio vulgaris", "Senecio vulgaris", 
          "Serratula tinctoria", "Sigesbeckia orientalis", "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea", 
          "Solidago virgaurea", "Solidago virgaurea", "Solidago virgaurea subsp. minuta", "Sonchus oleraceus", "Sonchus oleraceus", "Sonchus tenerrimus", 
          "Staehelina dubia", "Staehelina dubia", "Symphyotrichum squamatum", "Tanacetum corymbosum", "Tanacetum macrophyllum", "Tanacetum parthenium", 
          "Taraxacum officinale", "Taraxacum officinale", "Taraxacum officinale", "Taraxacum officinale", "Telekia speciosa", "Telekia speciosa", 
          "Tephroseris integrifolia", "Tephroseris integrifolia subsp. capitata", "Tephroseris longifolia subsp. gaudinii", "Tolpis staticifolia", 
          "Tolpis staticifolia", "Tragopogon crocifolius", "Tragopogon dubius", "Tragopogon pratensis", "Tragopogon pratensis", "Tragopogon pratensis", 
          "Tragopogon pratensis", "Tragopogon pratensis", "Tragopogon pratensis subsp. orientalis", "Tripleurospermum inodorum", "Tripleurospermum inodorum", 
          "Tripleurospermum inodorum", "Tussilago farfara", "Urospermum dalechampii", "Urospermum dalechampii", "Urospermum picroides", 
          "Xanthium orientale subsp. italicum ", "Xanthium orientale subsp. italicum", "Xeranthemum annuum", "Xerolekia speciosissima"
          )
table_v5
vapply(strsplit(table_v5, '(?<=[a-z])\\s+(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1))
genera_table_v5 <- unique(vapply(strsplit(table_v5, '(?<=[a-z])\\s+(?=[a-z])', perl = T), `[`, 1, FUN.VALUE = character(1)))
genera_table_v5

setdiff(genera_table_v5, genera_all)
setdiff(genera_all, genera_table_v5)

setdiff(genera_table_v5, genera_alps)
setdiff(genera_alps, genera_table_v5)

#### How many apomictic taxa? ####
### Extended dataset
DATA_CC_mean_red
table(DATA_CC_mean_red$Repr_mode_summ)

DATA_CC_mean_red[DATA_CC_mean_red$Repr_mode_summ == "Apomictic", 
                 "SpeciesName"]

DATA_CC_mean_red[DATA_CC_mean_red$Repr_mode_summ == "Mixed", 
                 "SpeciesName"]

### Strictly Alps
DATA_StrictlyAlps_mean_red
table(DATA_StrictlyAlps_mean_red$Repr_mode_summ)

DATA_StrictlyAlps_mean_red[DATA_StrictlyAlps_mean_red$Repr_mode_summ == "Apomictic", 
                 "SpeciesName"]

DATA_StrictlyAlps_mean_red[DATA_StrictlyAlps_mean_red$Repr_mode_summ == "Mixed", 
                           "SpeciesName"]

##### How many apomictic taxa per ploidy level? ##### 
### Extended dataset
with(DATA, table(Ploidy, Repr_mode))
sum(with(DATA, table(Ploidy, Repr_mode))) == nrow(DATA) # sanity check

### Strictly Alps
with(DATA_StrictlyAlps, table(Ploidy, Repr_mode))
sum(with(DATA_StrictlyAlps, table(Ploidy, Repr_mode))) == nrow(DATA_StrictlyAlps) # sanity check

##### Elevation range? #### 
### Extended dataset
DATA$Altitude

min(DATA$Altitude, na.rm = T)
max(DATA$Altitude, na.rm = T)
mean(DATA$Altitude, na.rm = T)
boxplot(DATA$Altitude)



##### Checking Online Resource 2 species names ##### 
online <- read.csv(file = "Pegoraro_et_al_Apomixis-Online_Resource-03-10-2019.csv")
# View(online)
colnames(DATA)

nrow(DATA) == nrow(online)

setdiff(as.character(DATA$ID_FloraAlpina), as.character(online$ID_FloraAlpina))
setdiff(as.character(online$ID_FloraAlpina), as.character(DATA$ID_FloraAlpina))
cbind(as.character(DATA$ID_FloraAlpina), as.character(DATA$SpeciesName), 
      as.character(online$ID_FloraAlpina), as.character(online$SpeciesName))

# View(cbind(as.character(DATA$ID_FloraAlpina), as.character(DATA$SpeciesName),
#            as.character(online$ID_FloraAlpina), as.character(online$SpeciesName))
#      )

colnames(DATA)
# online <- merge(online, DATA[, c(1:6,8:11,119)], by = "ID_FloraAlpina", all.x = T)
# online[, c(2,15)]

nrow(DATA)
DATA$Init.month
DATA[is.na(DATA$Init.month), 1:3]

DATA$ID_FloraAlpina[order(DATA$ID_FloraAlpina)]
online$ID_FloraAlpina[order(online$ID_FloraAlpina)]

setdiff(DATA$ID_FloraAlpina[order(DATA$ID_FloraAlpina)], online$ID_FloraAlpina[order(online$ID_FloraAlpina)])
setdiff(online$ID_FloraAlpina[order(online$ID_FloraAlpina)], DATA$ID_FloraAlpina[order(DATA$ID_FloraAlpina)])

### Need to recalculate the variables
DATA_online <- DATA
### Substitute the IDs with the ones referring to the subspecies (as chosen above)
### Put RegEx in a cycle
i = 1
ids <- DATA_online$ID_FloraAlpina
while (i <= length(old_IDs_regex)) {
  ids <- gsub(old_IDs[i], new_IDs[i], ids)
  i = i+1
}
cbind(as.character(DATA_online$ID_FloraAlpina), ids)
setdiff(DATA_online$ID_FloraAlpina, ids)

length(ids) == length(DATA_online$ID_FloraAlpina) # sanity check
DATA_online$ID_FloraAlpina2 <- ids # substitute modified IDs onto dataset

### Merge databases
DATA_online <- merge(DATA_online, FloraAlpina, by.x = "ID_FloraAlpina2", by.y = "ID_FloraAlpina", all.x = T)
### Check that missing values are only due to taxa that are not matchable to others in Flora Alpina
setdiff(DATA_online$ID_FloraAlpina2, FloraAlpina$ID_FloraAlpina)
DATA_online[grep('124$', DATA_online$ID_FloraAlpina2), 1:4] # Neophyte
DATA_online[grep('124.51$', DATA_online$ID_FloraAlpina2), 1:4] # Neophyte
DATA_online[grep('124.88$', DATA_online$ID_FloraAlpina2), 1:4] # Not in the Alps
DATA_online[grep('124.99$', DATA_online$ID_FloraAlpina2), 1:4] # Hieracium taxa not in Flora Alpina

DATA_online$Endemic.x
DATA_online$Endemic.y

DATA_online <- DATA_online[, -c(13:128)]

### Frickin column names...
colnames(DATA_online) <- gsub('\\.y$', '', colnames(DATA_online))

### Use data for similar species
DATA_online[DATA_online$SpeciesName == "Hieracium_valdepilosum", 13:118] <- DATA_online[DATA_online$SpeciesName == "Hieracium_villosum", 13:118][1,]
DATA_online[DATA_online$SpeciesName == "Hieracium_glaucopsis", 13:118] <- DATA_online[DATA_online$SpeciesName == "Hieracium_villosum", 13:118][1,]
DATA_online[DATA_online$SpeciesName == "Hieracium_cydoniifolium", 13:118] <- DATA_online[DATA_online$SpeciesName == "Hieracium_villosum", 13:118][1,]
DATA_online[DATA_online$SpeciesName == "Hieracium_ramosissimums_subsp._lactucifolium", 13:118] <- DATA_online[DATA_online$SpeciesName == "Hieracium_amplexicaule", 13:118][1,]
DATA_online[DATA_online$SpeciesName == "Schlagintweitia_huteri_subsp._lantoscana", 13:118] <- DATA_online[DATA_online$SpeciesName == "Schlagintweitia_intybacea", 13:118][1,]
DATA_online[DATA_online$SpeciesName == "Aremisia_nitida", 13:118] <- DATA_online[DATA_online$SpeciesName == "Artemisia_glacialis", 13:118][1,] # doesn't have altitude in any case
DATA_online[DATA_online$SpeciesName == "Sonchus_tenerrimus", 13:118] <- DATA_online[DATA_online$SpeciesName == "Sonchus_oleraceus", 13:118][1,]
DATA_online[DATA_online$SpeciesName == "Calendula_tripterocarpa", 13:118] <- FloraAlpina[grep('Calendula arvensis', FloraAlpina$CompleteName), 2:107]

### Missing values? 
DATA_online[is.na(DATA_online$Endemic), ]

### Need to reshape some data formats in order to use them
### Phytosociology 
DATA_online$Phytosociology <- gsub("\\(", "", DATA_online$Phytosociology)
DATA_online$Phytosociology <- gsub("\\)", "", DATA_online$Phytosociology)
DATA_online$Phytosociology <- gsub("\\(", "", DATA_online$Phytosociology)
DATA_online$Phytosociology <- gsub(" - ", "", DATA_online$Phytosociology)
DATA_online$Phytosociology <- gsub("\\[", "NEO_", DATA_online$Phytosociology)
DATA_online$Phytosociology <- gsub("\\]", "", DATA_online$Phytosociology)
DATA_online$Phytosociology

### Phenology
library(dplyr)
Phen <- select(DATA_online, c(1:4, 27:38))
Phen[,5:16] <- apply(Phen[,5:16], 2, function(y) as.numeric(gsub('x', '1', y)))
Phen$Tot.months <- apply(Phen[,5:16], 1, function(y) sum(y, na.rm=T))
Phen$Init.month <- apply(Phen[,5:16], 1, function (y) first(which(y == '1')))
Phen$End.month <- apply(Phen[,5:16], 1, function (y) last(which(y == '1')))

DATA_online$Tot.months <- Phen$Tot.months
DATA_online$Init.month <- Phen$Init.month
DATA_online$End.month <- Phen$End.month 

### pH and N
pH <- NULL
i = 1
while (i<=nrow(DATA_online)) {
  pH[i] <- factorize_pH(DATA_online, i)
  i <- i+1
}

DATA_online$pH <- pH

N <- NULL
i = 1
while (i<=nrow(DATA_online)) {
  N[i] <- factorize_N(DATA_online, i)
  i <- i+1
}

DATA_online$N <- N

### Water availability 
W <- NULL
i = 1
while (i<=nrow(DATA_online)) {
  W[i] <- factorize_Water(DATA_online, i)
  i <- i+1
}

DATA_online$Water <- W

### Geographic occurrences 
Occ <- NULL
i = 1
while (i<=nrow(DATA_online)) {
  Occ[i] <- sum(DATA_online[i, 42:96]=="+")
  i <- i+1
}
Occ
### the apply alternative:
DATA_online$Sect_Occ <- apply(DATA_online[, 42:96]=="+", 1, sum)
### sanity check:
Occ == apply(DATA_online[, 42:96]=="+", 1, sum)
rbind(Occ, apply(DATA_online[, 42:96]=="+", 1, sum))

### Odd VS Even ploidy levels
DATA_online$PloidyEvenOdd <- sapply(DATA_online$Ploidy, factorize_Ploidy)

DATA_online$SpeciesName <- as.factor(DATA_online$SpeciesName)

str(which(unique(DATA_online$SpeciesName)[i] == DATA_online$SpeciesName))
droplevels(as.factor(DATA_online[which(unique(DATA_online$SpeciesName)[i] == DATA_online$SpeciesName), "Repr_mode"]))

### Adding a level for "mixed" reproductive mode
i = 1
Repr_mode_summ <- factor(rep(1, length(unique(DATA_online$SpeciesName))))
levels(Repr_mode_summ) <- c("Apomictic", "Sexual", "Mixed")
Repr_mode_summ <- data.frame(Repr_mode_summ, "SpeciesName" = unique(DATA_online$SpeciesName))
while (i <= length(DATA_online$Repr_mode)) {
  if (length(levels(droplevels(as.factor(DATA_online[which(unique(DATA_online$SpeciesName)[i] == DATA_online$SpeciesName), "Repr_mode"])))) >= 2) {
    Repr_mode_summ$Repr_mode_summ[i] <- "Mixed"} else {
      Repr_mode_summ$Repr_mode_summ[i] <- as.character(first(DATA_online[which(unique(DATA_online$SpeciesName)[i] == DATA_online$SpeciesName), "Repr_mode"]))
    }
  i = i + 1
}
Repr_mode_summ

Repr_mode_summ <- Repr_mode_summ[order(Repr_mode_summ[, "SpeciesName"]), ]
Repr_mode_summ_ext <- merge(DATA_online, Repr_mode_summ, by = "SpeciesName", all.x = T)[c(1,125)]

DATA_online$Init.month
colnames(DATA_online)[39:43]

### Calculate mean elevation
DATA_online$rowsum <- rowSums(DATA_online[, 39:43])
DATA_online[, 39] <- DATA_online[, 39]*350 # collineen
DATA_online[, 40] <- DATA_online[, 40]*1050 # montagnard
DATA_online[, 41] <- DATA_online[, 41]*1750 # subalpin
DATA_online[, 42] <- DATA_online[, 42]*2450 # alpin
DATA_online[, 43] <- DATA_online[, 43]*3650 # nival

DATA_online$Elevation <- sapply(1:nrow(DATA_online), function(x) sum(DATA_online[x, 39:43])/DATA_online[x, "rowsum"])


##### Need to find an order in which to paste them and then do a cbind
online$SpeciesName
DATA_online$SpeciesName

match(gsub('_', ' ', DATA_online$SpeciesName), online$SpeciesName)
match(DATA_online$ID_FloraAlpina, online$ID_FloraAlpina)

DATA_online <- DATA_online[as.numeric(match(as.character(online$ID_FloraAlpina), as.character(DATA_online$ID_FloraAlpina))), ]

Online <- cbind(online, DATA_online)
write.csv(Online, file = "Online.csv")



##### Checking AGAIN for potential messups in the Online resource table... ##### 
Online_v7 <- read.csv(file = "Pegoraro_et_al_Apomixis-Online_Resource_1-v7 -JP_LP_OH2_LP.csv")
DATA

nrow(Online_v7)
nrow(DATA)

DATA[is.na(match(DATA$ID_FloraAlpina, Online_v7$ID_FloraAlpina)), 1:3]
Online_v7[is.na(match(Online_v7$ID_FloraAlpina, DATA$ID_FloraAlpina)), 1:3]
### It's intended that there are no more "unspecified subspecies" Flora Alpina IDs:
### I selected manually the most representative subspecies for each species to be able to use Flora Alpina data. 
### Changing the IDs locally for Online resource table to check correspondences

setdiff(DATA$ID_FloraAlpina, Online_v7$ID_FloraAlpina)
setdiff(Online_v7$ID_FloraAlpina, DATA$ID_FloraAlpina)

Online_v7$ID_FloraAlpina <- gsub('124\\.31\\.2\\.0', '124\\.31\\.2\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.31\\.19\\.0', '124\\.31\\.19\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.56\\.3\\.0', '124\\.56\\.3\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.60\\.9\\.0', '124\\.60\\.9\\.3', Online_v7$ID_FloraAlpina) # this is Carduus defloratus, chose subsp carlinifolius, 124.60.9.3
Online_v7$ID_FloraAlpina <- gsub('124\\.60\\.4\\.0', '124\\.60\\.4\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.68\\.25\\.0', '124\\.68\\.25\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.61\\.3\\.0', '124\\.61\\.3\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.46\\.8\\.0', '124\\.46\\.8\\.2', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.6\\.1\\.0', '124\\.6\\.1\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.96\\.1\\.0', '124\\.96\\.1\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.39\\.2\\.0', '124\\.39\\.2\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.39\\.8\\.0', '124\\.39\\.8\\.3', Online_v7$ID_FloraAlpina) # this is Leucanthemum atratum, chose subsp coronopifolium, 124.39.8.3
Online_v7$ID_FloraAlpina <- gsub('124\\.67\\.1\\.0', '124\\.67\\.1\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.48\\.14\\.0', '124\\.48\\.14\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.65\\.1\\.0', '124\\.65\\.1\\.2', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.2\\.1\\.0', '124\\.2\\.1\\.2', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.35\\.2\\.0', '124\\.35\\.2\\.1', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.86\\.4\\.0', '124\\.86\\.4\\.3', Online_v7$ID_FloraAlpina)
Online_v7$ID_FloraAlpina <- gsub('124\\.31\\.11\\.0', '124\\.31\\.11\\.1', Online_v7$ID_FloraAlpina)

setdiff(DATA$ID_FloraAlpina, Online_v7$ID_FloraAlpina)
setdiff(Online_v7$ID_FloraAlpina, DATA$ID_FloraAlpina)
### It's fine that Sigesbeckia is not in Online resource, as we decided to drop it

# Online_v7 <- merge(DATA, Online_v7, by = "ID_FloraAlpina", all.x = T)
# colnames(Online_v7)
# 
# Online_v7[, c(1:3, 14:16)]

library(rowr)
cbind.fill(Online_v7$ID_FloraAlpina, DATA$ID_FloraAlpina, fill = NA)

cbind(as.character(Online_v7$ID_FloraAlpina), as.character(sort(Online_v7$ID_FloraAlpina)))
cbind.fill(as.character(sort(Online_v7$ID_FloraAlpina)), as.character(sort(DATA$ID_FloraAlpina)), fill = NA)

Online_v7 <- cbind.fill(DATA[order(DATA$ID_FloraAlpina), c(1:18, 117:129)], 
                        Online_v7[order(Online_v7$ID_FloraAlpina), ], 
                        fill = NA)
Online_v7
colnames(Online_v7)
Online_v7[, c(2,32)]
write.csv(Online_v7, file = "Online_v7_manual.csv")


# check <- read.csv(file ="NewApomixisData_FloraAlpinaID.csv")
# check$SpeciesName
# NewApomixisData_FloraAlpinaID$SpeciesName
# intersect(check$SpeciesName, NewApomixisData_FloraAlpinaID$SpeciesName)
# setdiff(NewApomixisData_FloraAlpinaID$SpeciesName, check$SpeciesName)
# 
# check$ID
# NewApomixisData_FloraAlpinaID$ID
# intersect(check$ID, NewApomixisData_FloraAlpinaID$ID)
# setdiff(NewApomixisData_FloraAlpinaID$ID, check$ID)
# setdiff(check$ID, NewApomixisData_FloraAlpinaID$ID)



#### checking
testDATA <- DATA[order(DATA$ID_FloraAlpina), ]
nrow(testDATA)

testNewApomixisData_FloraAlpinaID_manual <- NewApomixisData_FloraAlpinaID_manual[order(NewApomixisData_FloraAlpinaID_manual$ID_FloraAlpina), ]
nrow(testNewApomixisData_FloraAlpinaID_manual)


##### Author names for botanical names #### 
authnames <- read.csv(file = "/home/luca/Dropbox/Royal Botanic Gardens of Kew/ApomixisPaper/Apomixis_v8/Pegoraro_et_al_Apomixis-Online_Resource_1_v9_27Jan20.csv")
authnames <- as.character(authnames$Species..name.author.publication.)
authnames

strsplit(authnames, '\\,', perl = T)
cbind(vapply(strsplit(authnames, '\\,', perl = T), `[`, 1, FUN.VALUE = character(1)), 
      vapply(strsplit(authnames, '\\,', perl = T), `[`, 2, FUN.VALUE = character(1))
      )

write.table(
  vapply(strsplit(authnames, '\\,', perl = T), `[`, 1, FUN.VALUE = character(1)), 
  file = "names_authors.txt"
  )

writeLines(vapply(strsplit(authnames, '\\,', perl = T), `[`, 1, FUN.VALUE = character(1)), con = "names_authors.txt")

write.table(
  writeLines(vapply(strsplit(authnames, '\\,', perl = T), `[`, 1, FUN.VALUE = character(1))), 
  file = "names_authors.txt"
)

authnames <- vapply(strsplit(authnames, '\\,', perl = T), `[`, 1, FUN.VALUE = character(1))

vapply(strsplit(authnames, '[A-z]\\s[a-z]<=\\s', perl = T), `[`, 1, FUN.VALUE = character(1))

grep('(?<=[A-z]\\s[a-z]) ', authnames, perl = T, value = T)


onlyauth <- vapply(strsplit(authnames, '(?<=[A-z])\\s[a-z]+\\s', perl = T), `[`, 2, FUN.VALUE = character(1))
onlyauth

unlist(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T))[unlist(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T)) != "subsp."]
vapply(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T), `[`, 1, FUN.VALUE = character(1))

cbind(unlist(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T))[unlist(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T)) != "subsp."], 
      vapply(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T), `[`, 1, FUN.VALUE = character(1)))

onlyauth2 <- unlist(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T))[unlist(strsplit(onlyauth, '(?<=subsp\\.)\\s[a-z]+\\s', perl = T)) != "subsp."]

writeLines(onlyauth2, con = "only_authors.txt")

cbind()

# library(stringr)
# word(authnames, 3)
