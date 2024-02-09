rga23_tape <- readCSV("rga23_tape.csv")
rga23_prodVegetales <- readCSV("rga23_prodVegetales.csv")
rga23_prodAnimales <- readCSV("rga23_prodAnimales.csv")
rga23_exploitations <- readCSV("rga23_exploitations.csv")
rga23_surfacesCultures <- readCSV("rga23_surfacesCultures.csv")

# Vente
# Auto-consommation familiale.....................................1/1
# Alimentation des animaux .......................................2/2
# Dons (à la famille, des amis)...................................3/3
# Echange.........................................................4/4
# Vente directe au particulier ...................................5/5
# Vente par internet (Facebook ou autre site).....................6/6
# Vente à un commerçant, artisan ou revendeur.....................7/7
# Vente à un grossiste............................................8/8
# Vente à un transformateur ou préparateur (y compris abattoir)...9/9
# Vente à la coopérative ou au syndicat...........................10/10
# Vente à la restauration collective..............................11/11
# Vente aux restaurants (hors collectifs) / hôtels................12/12
# Sans objet (pas de production de ce type).......................13/13

rga23 <- rga23_prodVegetales |> 
  mutate(
    partVendueMaraic = PartComMaraic__5 + PartComMaraic__6 + PartComMaraic__7 + PartComMaraic__8 + PartComMaraic__9 + PartComMaraic__10 + PartComMaraic__11 + PartComMaraic__12,
    partVenduePlantes = PartComPlantes__5 + PartComPlantes__6 + PartComPlantes__7 + PartComPlantes__8 + PartComPlantes__9 + PartComPlantes__10 + PartComPlantes__11 + PartComPlantes__12,
    partVendueFlorale = PartComFlorale__5 + PartComFlorale__6 + PartComFlorale__7 + PartComFlorale__8 + PartComFlorale__9 + PartComFlorale__10 + PartComFlorale__11 + PartComFlorale__12,
    partVendueVivri = PartComVivri__5 + PartComVivri__6 + PartComVivri__7 + PartComVivri__8 + PartComVivri__9 + PartComVivri__10 + PartComVivri__11 + PartComVivri__12,
    partVendueFruit = PartComFruit__5 + PartComFruit__6 + PartComFruit__7 + PartComFruit__8 + PartComFruit__9 + PartComFruit__10 + PartComFruit__11 + PartComFruit__12,
    partVenduePepinieres = PartComPepinieres__5 + PartComPepinieres__6 + PartComPepinieres__7 + PartComPepinieres__8 + PartComPepinieres__9 + PartComPepinieres__10 + PartComPepinieres__11 + PartComPepinieres__12,
    partVendueFourrages = PartComFourrages__5 + PartComFourrages__6 + PartComFourrages__7 + PartComFourrages__8 + PartComFourrages__9 + PartComFourrages__10 + PartComFourrages__11 + PartComFourrages__12
  )

rga23 <- rga23_prodAnimales |>
  mutate(
    partVendueOeufs = PartComOeufs__5 + PartComOeufs__6 + PartComOeufs__7 + PartComOeufs__8 + PartComOeufs__9 + PartComOeufs__10 + PartComOeufs__11 + PartComOeufs__12,
    partVendueMiel = PartComMiel__5 + PartComMiel__6 + PartComMiel__7 + PartComMiel__8 + PartComMiel__9 + PartComMiel__10 + PartComMiel__11 + PartComMiel__12,
    partVendueViande = PartComViande__5 + PartComViande__6 + PartComViande__7 + PartComViande__8 + PartComViande__9 + PartComViande__10 + PartComViande__11 + PartComViande__12
  )

source("tape/1_Diversite.R")
source("tape/2_Synergies.R")
source("tape/3_Efficience.R")
source("tape/4_Recyclage.R")
source("tape/5_Resilience.R")
source("tape/6_CultureTraditions.R")
source("tape/7_Cocreation.R")
source("tape/8_ValeursHumainesSociales.R")
source("tape/9_EconomieCirculaire.R")
