# 10. GOUVERNANCE RESPONSABLE

# 10.1 ÉMANCIPATION DES PRODUCTEURS
# Avec perspective de genre
# # > 0 - Les droits des producteurs ne sont pas respectés.
# Ils n’ont aucun pouvoir de négociation et n’ont pas les moyens d’améliorer leurs moyens de subsistance et de développer leurs compétences.
# # > 1 - Les droits des producteurs sont reconnus mais pas toujours respectés.
# Ils ont un faible pouvoir de négociation et peu de moyens pour améliorer leurs moyens de subsistance et/ou développer leurs compétences.
# # > 2 - Les droits des producteurs sont reconnus et respectés tant pour les hommes que pour les femmes.
# Ils ont un faible pouvoir de négociation mais ne sont pas encouragés à améliorer leurs moyens de subsistance et/ou à développer leurs compétences.
# # > 3 - Les droits des producteurs sont reconnus et respectés tant pour les hommes que pour les femmes.
# Ils ont la capacité et les moyens d’améliorer leurs moyens de subsistance et sont parfois incités à développer leurs compétences.
# # > 4 - Les droits des producteurs sont reconnus et respectés tant pour les hommes que pour les femmes.
# Ils ont la capacité et les moyens d’améliorer leurs moyens de subsistance et de développer leurs compétences.

# ProbActivite
# Oui en me regroupant avec d'autres agriculteurs qui me ressemblent...1
# Oui via les élus, les politiques, ...................................2
# Oui via la CAPL......................................................3
# Non..................................................................4

# MoyensCompetences
# Oui....................................................1
# Oui, mais pas d'incitation à le faire..................2
# Oui, en m'associant avec d'autres producteurs..........3
# Non, peu de moyens (financier, organisationnel, ...)...4
# Non, pas de moyens (financier, organisationnel, ...)...5

score_1_Emancipation <- left_join(rga23_tape,
  rga23_mainOeuvre |> select(interview__key, FormationContinue),
  by = "interview__key"
) |>
  mutate(score = case_when(
    # NON à la question "ProbActivite" et non PAS de moyens de développer les compétences
    ProbActivite__4 == 1 & MoyensCompetences == 5 ~ 0,
    # NON à la question "ProbActivite" ou en se regroupant avec d'autres agriculteurs et non PEU de moyens de développer les compétences
    (ProbActivite__4 == 1 | ProbActivite__1 == 1) & MoyensCompetences == 4 ~ 1,
    # OUI à la question "ProbActivite" et pas d'incitation ou peu de moyens
    ProbActivite__4 == 0 & (MoyensCompetences == 2 | MoyensCompetences == 4) ~ 2,
    # OUI à la question "ProbActivite" et aurait les moyens de le faire (seul ou en s'associant)
    ProbActivite__4 == 0 & (MoyensCompetences == 1 | MoyensCompetences == 3) ~ 3,
    # OUI à la question "ProbActivite" et FormationContinue
    ProbActivite__4 == 0 & FormationContinue == 1 ~ 4,
    TRUE ~ 55
  ))

score_1_Emancipation |>
  group_by(score) |>
  count()

score_1_Emancipation |>
  filter(score == 55) |>
  group_by(ProbActivite__4, MoyensCompetences, FormationContinue) |>
  count()
