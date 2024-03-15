library(testthat)

source("champs/champCAPL.R")

donneesTestsElevage <- data.frame(
  interview__key = c(1, 2, 3, 4, 5),
  NbRuchesPourProduire = c(2, NA, NA, NA, NA),
  NbRuchettes = c(3, NA, NA, NA, NA),
  NbJeunesEngrLait = c(NA, 1, NA, NA, NA),
  NbJeunesEngrViande = c(NA, 1, NA, NA, NA),
  NbTaureauxLait = c(NA, 1, NA, NA, NA),
  NbTaureauxViande = c(NA, 1, NA, NA, NA),
  NbVachesLait = c(NA, 1, NA, NA, NA),
  NbVachesViande = c(NA, 1, NA, NA, NA),
  NbBoucs = c(NA, NA, NA, NA, NA),
  ChevresLait = c(2, NA, NA, NA, NA),
  NbChevres = c(1, NA, NA, NA, NA),
  EquidesJeunesBat = c(NA, NA, NA, NA, NA),
  EquidesJeunesSportsLoisirs = c(NA, NA, NA, NA, NA),
  EtalonsBat = c(NA, NA, NA, NA, NA),
  EtalonsSportsLoisirs = c(NA, NA, NA, NA, NA),
  HongresBat = c(NA, NA, NA, NA, NA),
  HongresSportsLoisirs = c(NA, NA, NA, NA, NA),
  JumentsPonettesBat = c(NA, NA, NA, NA, NA),
  JumentsPonettesSportsLoisirs = c(NA, NA, NA, NA, NA),
  NbLapinesMeres = c(NA, NA, NA, NA, NA),
  NbLapinsSevresEngrais = c(NA, NA, NA, NA, NA),
  NbBeliers = c(NA, NA, NA, NA, NA),
  NbBrebis = c(NA, NA, NA, NA, NA),
  NbPorcsEngraissement = c(NA, NA, 10, NA, NA),
  NbTruiesGestVides = c(NA, NA, 10, NA, NA),
  NbTruiesMaternite = c(NA, NA, 10, NA, NA),
  NbVerrats = c(NA, NA, 10, NA, NA),
  NbAutresVolailles = c(NA, NA, NA, NA, NA),
  NbCailles = c(NA, NA, NA, NA, NA),
  NbCanards = c(NA, NA, NA, 100, NA),
  NbDindesDindons = c(NA, NA, NA, NA, NA),
  NbOies = c(NA, NA, NA, NA, NA),
  NbPintades = c(NA, NA, NA, NA, NA),
  NbPouletsChairCoqs = c(NA, NA, NA, NA, NA),
  NombrePoules0 = c(NA, NA, NA, NA, NA),
  NombrePoules1 = c(NA, NA, NA, 100, NA),
  NombrePoules3 = c(NA, NA, NA, NA, NA)
)

# Tests
test_that("Calcul points elevages", {
  expected_points <- c(110, 280, 2700, 500, 0)
  result <- calculPointsCAPLElevage(donneesTestsElevage)
  expect_equal(result$nombrePointsElevages, expected_points)
})

donneesTestsJardinsOceaniens <- data.frame(
  interview__key = c(1, 2, 3),
  SurfaceJardins = c(100, NA, 200),
  SurfaceIrrigJardins = c(100, NA, NA)
)

test_that("Calcul points jardins oceaniens", {
  expected_points <- c(20, 0, 20)
  result <- calculPointsCAPLJardinsOceaniens(donneesTestsJardinsOceaniens)
  expect_equal(result$nombrePointsJardinsOceaniens, expected_points)
})

