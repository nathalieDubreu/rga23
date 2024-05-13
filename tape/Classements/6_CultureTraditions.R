# 6. CULTURE & TRADITIONS ALIMENTAIRES
#
# RÉGIME ALIMENTAIRE APPROPRIÉ ET CONSCIENCE NUTRITIONNELLE
# > 0 - L’alimentation est systématiquement insuffisante pour répondre aux besoins nutritionnels et méconnaissance des bonnes pratiques nutritionnelles.
# > 1 - L’alimentation est périodiquement insuffisante pour répondre aux besoins nutritionnels et ou le régime alimentaire est basée sur un nombre limité de groupes alimentaires. Manque de sensibilisation aux bonnes pratiques nutritionnelles.
# > 2 - Sécurité alimentaire globale au fil du temps, mais diversité insuffisante des groupes alimentaires. De bonnes pratiques nutritionnelles sont connues mais pas toujours appliquées.
# > 3 - La nourriture est suffisante et variée. De bonnes pratiques nutritionnelles sont connues mais pas toujours appliquées.
# > 4 - Alimentation saine, nutritive et diversifiée. Les bonnes pratiques nutritionnelles sont bien connues et appliquées.

# FruitsLocaux, PoissonsLocaux, LegumesLocaux, VivriLocaux
# Tous les jours...............1/1
# Plusieurs fois par semaine...2/2
# Une fois par semaine ........3/3
# Une fois par mois............4/4
# Moins fréquemment............5/5
# Jamais.......................6/6

score_1_regimeAlimentaire <- rga23_tape |> mutate(
  score = case_when(
    # 3 des 4 types sont consommés quotidiennement et le dernier au moins plusieurs fois par semaine
    FruitsLocaux + PoissonsLocaux + LegumesLocaux + VivriLocaux <= 5 ~ 0,
    # Les 4 types d'aliments sont consommés en moyenne plusieurs fois par semaine
    FruitsLocaux + PoissonsLocaux + LegumesLocaux + VivriLocaux <= 8 ~ 1,
    # Les 4 types d'aliments sont consommés en moyenne une fois par semaine
    FruitsLocaux + PoissonsLocaux + LegumesLocaux + VivriLocaux <= 12 ~ 2,
    # Les 4 types d'aliments sont consommés en moyenne une fois par mois
    FruitsLocaux + PoissonsLocaux + LegumesLocaux + VivriLocaux <= 16 ~ 3,
    # Les 4 types d'aliments sont consommés moins d'une fois par mois en moyenne
    FruitsLocaux + PoissonsLocaux + LegumesLocaux + VivriLocaux > 16 ~ 4,
    TRUE ~ 55
  )
)

score_1_regimeAlimentaire |>
  group_by(score) |>
  count()


# IDENTITÉ ET CONSCIENCE LOCALES OU TRADITIONNELLES (PAYSANNES/INDIGÈNES)
# > 0 - Aucune identité locale ou traditionnelle (paysanne / indigène) n’est ressentie.
# > 1 - Peu de conscience de l’identité locale ou traditionnelle.
# > 2 - Identité locale ou traditionnelle ressentie en partie, ou qui concerne seulement une partie du ménage.
# > 3 - Bonne conscience de l’identité locale ou traditionnelle et respect des traditions ou des rituels en général.
# > 4 - Identité locale ou traditionnelle fortement ressentie et protégée, grand respect des traditions et/ou des rituels.
#
# UTILISATION DE VARIETÉS/RACES LOCALES ET CONNAISSANCES TRADITIONNELLES (PAYSANNES / INDIGÈNES) POUR LA PRÉPARATION DES ALIMENTS
# > 0 - Aucune utilisation de variétés/races locales ni de connaissances traditionnelles pour la préparation des aliments.
# > 1 – La majorité des variétés/races consommées sont exotiques/introduites, ou les connaissances et les pratiques traditionnelles pour la préparation des aliments sont peu utilisées.
# > 2 - Des variétés/races locales et exotiques/introduites sont produites et consommées. Les connaissances et pratiques locales ou traditionnelles pour la préparation des aliments sont identifiées mais pas toujours appliquées.
# > 3 – La majorité de la nourriture consommée provient de variétés/races locales et les connaissances et les pratiques traditionnelles pour la préparation des aliments sont mises en œuvre.
# > 4 – Un certain nombre de variétés/races locales sont produites et consommées. Les connaissances et pratiques traditionnelles pour la préparation des aliments sont identifiées, appliquées et reconnues dans des cadres officiels et/ou des événements spécifiques.
