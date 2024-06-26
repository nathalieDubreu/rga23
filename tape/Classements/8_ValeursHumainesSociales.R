# 8. VALEURS HUMAINES & SOCIALES
#
# ÉMANCIPATION DES FEMMES
# > 0 - Les femmes n’ont normalement pas voix au chapitre dans les décisions, ni dans la famille, ni dans la communauté. Aucune organisation pour l’autonomisation des femmes n’existe.
# > 1 - Les femmes peuvent avoir voix au sein de leur ménage mais pas dans la communauté. Certaines associations de femmes existent mais ne sont pas très fonctionnelles.
# > 2 - Les femmes peuvent influencer la prise de décision, tant au niveau du ménage que de la communauté, mais ne sont pas des décideurs. Ils n’ont pas accès aux ressources. Et / ou certaines formes d’associations de femmes existent mais ne sont pas pleinement fonctionnelles.
# > 3 - Les femmes participent pleinement aux processus décisionnels mais n’ont toujours pas pleinement accès aux ressources. Et / ou des organisations de femmes existent et sont utilisées.
# > 4 - Les femmes sont complètement emancipées en termes de prise de décision et d’accès aux ressources. Et / ou des organisations de femmes existent, sont fonctionnelles et opérationnelles.
#
# TRAVAIL (CONDITIONS DE PRODUCTION, INÉGALITÉS SOCIALES)
# > 0 – Les chaînes d’approvisionnement agricoles sont intégrées et gérées par l’agro-industrie. Il existe une distance sociale et économique entre les propriétaires fonciers et les travailleurs. Et/ou les travailleurs n’ont pas de conditions de travail décentes, font de bas salaires et sont très exposés aux risques.
# > 1 – Les conditions de travail sont difficiles, les travailleurs ont un salaire moyen pour le contexte local et peuvent être exposés à des risques.
# > 2 - L’agriculture est principalement basée sur l’exploitation familiale mais les producteurs ont un accès limité aux capitaux et aux processus de prise de décision. Les travailleurs ont des conditions de travail décentes minimales.
# > 3 - L’agriculture est principalement basée sur l’exploitation familiale et les producteurs (hommes et femmes) ont accès au capital et aux processus décisionnels. Les travailleurs ont des conditions de travail décentes.
# > 4 - L’agriculture est basée sur des exploitations familiaux qui ont pleinement accès au capital et aux processus de prise de décision en matière d’équité entre les sexes. Il existe une proximité sociale et économique entre agriculteurs et salariés.

# - PropTravailPenibleExpl : Quelle proportion de votre temps de travail est consacrée à du travail pénible physiquement ?
# 0 à 25% du temps.......1
# 25 à 50% du temps......2
# Plus de 50% du temps...3

# - TypePhytosanit : De quel(s) type(s) sont les produits phytosanitaires que vous utilisez ?
# Synthétique (chimique)...1
# Biologique...............2

# (Si produits phyto chimiques) FormationPhytosanit : Avez-vous suivi une formation à l’utilisation des produits phytosanitaires ?

# - UtilisationGlyphosate : Utilisez-vous certains de ces produits : Herbosate , Robust, Roundup, Glyphosate 360 ou autre produit à base de glyphosate ?

# - BesoinsSatisf : Vos besoins en nourriture et autres produits essentiels sont-ils satisfaits par votre production ou les revenus de votre production agricole ?

# - Economies : Les revenus de votre production agricole vous permettent-ils de réaliser des économies ?
# Oui, de façon régulière.......1
# Oui, de façon occasionnelle...2
# Non...........................3

# TpsTravailChefExpl
# Moins de 1/2 temps.................1/1
# 1/2 temps..........................2/2
# Entre 1/2 temps et temps complet...3/3
# Temps complet......................4/4

score_2_Travail <- left_join(rga23_tape,
  rga23_exploitations |> select(interview__key, TypePhytosanit__1, FormationPhytosanit, UtilisationGlyphosate),
  by = "interview__key"
) |>
  left_join(rga23_mainOeuvre |> select(interview__key, TpsTravailChefExpl),
    by = "interview__key"
  ) |>
  mutate(
    score = case_when(
      # 0
      PropTravailPenibleExpl == 3 & (TypePhytosanit__1 == 1 | UtilisationGlyphosate == 1) & BesoinsSatisf == 2 ~ 0,
      TpsTravailChefExpl <= 2 & NoteSatisfactionExpl <= 4 & BesoinsSatisf == 2 ~ 0,
      # 1
      PropTravailPenibleExpl >= 2 & (TypePhytosanit__1 == 1 | UtilisationGlyphosate == 1) & (BesoinsSatisf == 2 | Economies == 3) ~ 1,
      TpsTravailChefExpl <= 2 & (NoteSatisfactionExpl == 5 | NoteSatisfactionExpl == 6) & BesoinsSatisf == 2 ~ 1,
      PropTravailPenibleExpl <= 2 & NoteSatisfactionExpl >= 7 & BesoinsSatisf == 2 ~ 1,
      PropTravailPenibleExpl == 3 & BesoinsSatisf == 2 & is.na(TypePhytosanit__1) ~ 1,
      # 2
      PropTravailPenibleExpl < 3 & (is.na(FormationPhytosanit) | FormationPhytosanit == 1) & Economies == 3 ~ 2,
      PropTravailPenibleExpl == 3 & NoteSatisfactionExpl >= 7 ~ 2,
      TpsTravailChefExpl <= 2 & NoteSatisfactionExpl >= 7 & BesoinsSatisf == 2 ~ 2,
      # 3
      PropTravailPenibleExpl < 3 & (is.na(FormationPhytosanit) | FormationPhytosanit == 1) & Economies < 3 ~ 3,
      TRUE ~ 55
    )
  )

score_2_Travail |>
  group_by(score) |>
  count()

# ÉMANCIPATION DE LA JEUNESSE ET ÉMIGRATION
# > 0 - Les jeunes ne voient aucun avenir dans l’agriculture et sont impatients d’émigrer.
# > 1 - La plupart des jeunes pensent que l’agriculture est trop difficile et plusieurs souhaitent émigrer.
# > 2 - La plupart des jeunes ne veulent pas émigrer, malgré des conditions de travail difficiles, et souhaitent améliorer leurs conditions de vie et de travail au sein de la communauté.
# > 3 - La plupart des jeunes (autant les garçons que les filles) sont satisfaits des conditions de travail et ne veulent pas émigrer.
# > 4 - Les jeunes (autant les garçons que les filles) voient leur avenir dans l’agriculture et sont désireux de continuer et d’améliorer l’activité de leurs parents.
#
# BIEN-ÊTRE ANIMAL [SI APPLICABLE]
# > 0 - Les animaux souffrent de la faim, de la soif, du stress et des maladies toute l’année et sont abattus sans éviter de douleur inutile.
# > 1 - Les animaux souffrent périodiquement/de façon saisonnière de la faim, de la soif, du stress ou des maladies, et sont abattus sans éviter de douleur inutile.
# > 2 - Les animaux ne souffrent ni de faim ni de soif, mais souffrent de stress, peuvent être sujets à des maladies et peuvent souffrir de douleurs à l’abattage.
# > 3 - Les animaux ne souffrent pas de faim, de soif ou de maladies mais peuvent souffrir de stress, notamment lors de l’abattage.
# > 4 - Les animaux ne souffrent pas de stress, de faim, de soif, de douleur ou de maladies et sont abattus de manière à éviter toute douleur inutile.

# - ManqueEauAliments : Y-a-t-il eu des moments de l'année où vos animaux n'ont pas eu assez d'eau ou d'aliments ?

# - RaisonsManque : Pour quelle(s) raison(s) ?
# Rupture d'approvisionnement.........................1
# Sécheresse..........................................2
# Paturage insuffisant (herbe pas assez abondante)....3
# Manque de main d'œuvre (congé, maladie, décès, …)...4
# Autre...............................................5

# - VisiteursElevages : Vos élevages sont-ils visités par :
# Des techniciens..........1
# Des vétérinaires.........2
# Autre public.............3
# Aucun de ces visiteurs...4

score_4_BienEtreAnimal <- left_join(rga23_tape,
  rga23_exploitations |> select(interview__key, ArchipelExploitation, IleExploitation),
  by = "interview__key"
) |>
  mutate(
    score = case_when(
      # OUI moments sans eau ou aliments pour 2 raisons différentes OU pour cause de manque de main d'oeuvre + pas de visites véto ni technicien ni autre public
      ManqueEauAliments == 1 &
        (RaisonsManque__4 == 1 | (RaisonsManque__1 + RaisonsManque__2 + RaisonsManque__3 + RaisonsManque__4 + RaisonsManque__5 >= 2)) &
        VisiteursElevages__4 == 1 ~ 0,
      # OUI moments sans eau ou aliments pour 1 seule raison + pas de visites véto ni technicien ni autre public
      ManqueEauAliments == 1 &
        (RaisonsManque__1 + RaisonsManque__2 + RaisonsManque__3 + RaisonsManque__4 + RaisonsManque__5 == 1) &
        VisiteursElevages__4 == 1 ~ 1,
      # Pas de moments de privation + visite véto ET technicien Mais pas d'abattoir (donc hors Tahiti)
      ManqueEauAliments == 2 & VisiteursElevages__1 == 1 & VisiteursElevages__2 == 1 & IleExploitation != "Tahiti" ~ 3,
      # Pas de moments de privation + visite véto OU technicien Mais pas d'abattoir (donc hors Tahiti)
      ManqueEauAliments == 2 & (VisiteursElevages__1 == 1 | VisiteursElevages__2 == 1) & IleExploitation != "Tahiti" ~ 2,
      # Pas de moments de privation + visite véto ou technicien + abattoir (donc Tahiti)
      ManqueEauAliments == 2 & (VisiteursElevages__1 == 1 | VisiteursElevages__2 == 1) & IleExploitation == "Tahiti" ~ 4,
      # Pas d'élevages
      RaisonsRecensement__2 == 0 ~ 99,
      TRUE ~ 55
    )
  )

score_4_BienEtreAnimal |>
  group_by(score) |>
  count()

nonClasses <- score_4_BienEtreAnimal |>
  filter(score == 55) |>
  mutate(
    nombreRaisonsManque = RaisonsManque__1 + RaisonsManque__2 + RaisonsManque__3 + RaisonsManque__4 + RaisonsManque__5
  ) |>
  group_by(RaisonsRecensement__1, VisiteursElevages__4, nombreRaisonsManque, ArchipelExploitation) |>
  count()
