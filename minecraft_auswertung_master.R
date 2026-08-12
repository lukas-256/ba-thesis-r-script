# Auswertung der Minecraft-User-Study

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

master_file <- "master-excel.xlsx"
pre_file <- "pre-eval.xlsx"
post_file <- "post-eval.xlsx"
study_file <- "user-study-minecraft.xlsx"

text_norm <- function(x) str_to_lower(str_squish(as.character(x)))
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

time_to_min <- function(x) {
  if (length(x) != 1 || is.na(x)) return(NA_real_)
  if (inherits(x, "POSIXt")) {
    return(as.numeric(format(x, "%H")) * 60 +
             as.numeric(format(x, "%M")) +
             as.numeric(format(x, "%S")) / 60)
  }
  s <- str_squish(as.character(x))
  if (is.na(s) || s == "") return(NA_real_)
  if (str_detect(s, "\\d{1,2}:\\d{2}")) {
    t <- str_extract(s, "\\d{1,2}:\\d{2}(?::\\d{2}(?:\\.\\d+)?)?")
    p <- as.numeric(strsplit(t, ":", fixed = TRUE)[[1]])
    if (length(p) == 2) return(p[1] * 60 + p[2])
    if (length(p) == 3) return(p[1] * 60 + p[2] + p[3] / 60)
  }
  n <- suppressWarnings(as.numeric(s))
  if (is.na(n)) return(NA_real_)
  if (abs(n) <= 1) return(n * 24 * 60)
  n
}

count_entry <- function(x) {
  s <- str_squish(as.character(x))
  as.integer(!is.na(x) && !is.na(s) && s != "")
}

desc <- function(x) {
  x <- x[!is.na(x)]
  data.frame(
    n = length(x), M = mean(x), SD = sd(x), Median = median(x),
    Q1 = unname(quantile(x, .25)), Q3 = unname(quantile(x, .75)),
    Minimum = min(x), Maximum = max(x)
  )
}

cronbach_alpha <- function(x) {
  x <- as.data.frame(x)
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x)
  (k / (k - 1)) * (1 - sum(sapply(x, var)) / var(rowSums(x)))
}

rank_biserial <- function(diff) {
  diff <- diff[!is.na(diff) & diff != 0]
  ranks <- rank(abs(diff), ties.method = "average")
  w_pos <- sum(ranks[diff > 0])
  w_neg <- sum(ranks[diff < 0])
  (w_pos - w_neg) / (w_pos + w_neg)
}

wilcoxon_paired <- function(x, y) {
  diff <- x - y
  nz <- diff[!is.na(diff) & diff != 0]
  ranks <- rank(abs(nz), ties.method = "average")
  w_pos <- sum(ranks[nz > 0])
  w_neg <- sum(ranks[nz < 0])
  test <- wilcox.test(x, y, paired = TRUE, exact = FALSE, correct = FALSE)
  data.frame(W = min(w_pos, w_neg), p = test$p.value,
             r = rank_biserial(diff))
}

mcnemar_exact <- function(pre, post) {
  b <- sum(pre == 0 & post == 1, na.rm = TRUE)
  c_count <- sum(pre == 1 & post == 0, na.rm = TRUE)
  p <- if (b + c_count == 0) 1 else binom.test(
    min(b, c_count), b + c_count, p = .5, alternative = "two.sided"
  )$p.value
  c(falsch_richtig = b, richtig_falsch = c_count, p = p)
}


# Master-Datensatz ------------------------------------------------------------

master <- read_excel(master_file, sheet = 1, range = "A1:Z21")

names(master) <- c(
  "id", "alter", "geschlecht", "abschluss",
  "programmiererfahrung", "selbst_verstaendnis", "selbst_sicherheit",
  "videospiele_regelmaessig", "minecraft_erfahrung", "spass_videospiele",
  "pre_gesamt", "post_gesamt",
  paste0("ueq_", 1:8, "_raw"),
  "welt_dauer", "welt_fehler", "welt_hilfe",
  "ueq_pragmatisch", "ueq_hedonisch", "ueq_gesamt"
)

numeric_cols <- c(
  "alter", "programmiererfahrung", "selbst_verstaendnis",
  "selbst_sicherheit", "videospiele_regelmaessig",
  "minecraft_erfahrung", "spass_videospiele", "pre_gesamt", "post_gesamt",
  paste0("ueq_", 1:8, "_raw"),
  "welt_dauer", "welt_fehler", "welt_hilfe",
  "ueq_pragmatisch", "ueq_hedonisch", "ueq_gesamt"
)

master <- master |>
  mutate(
    id = as.character(id),
    across(all_of(numeric_cols), as.numeric),
    programmier_selbstindex = rowMeans(
      across(c(programmiererfahrung, selbst_verstaendnis, selbst_sicherheit))
    )
  )


# Stichprobe ------------------------------------------------------------------

cat("\n--- Stichprobe ---\n")
print(data.frame(
  n = nrow(master),
  alter_M = mean(master$alter),
  alter_SD = sd(master$alter),
  alter_min = min(master$alter),
  alter_max = max(master$alter)
))
print(table(master$geschlecht))
print(table(master$abschluss))

print(data.frame(
  Variable = c(
    "Programmiererfahrung", "Verständnis Programmierkonzepte",
    "Sicherheit bei Programmieraufgaben", "Minecraft-Erfahrung",
    "Freude an Videospielen"
  ),
  M = c(
    mean(master$programmiererfahrung),
    mean(master$selbst_verstaendnis),
    mean(master$selbst_sicherheit),
    mean(master$minecraft_erfahrung),
    mean(master$spass_videospiele)
  ),
  SD = c(
    sd(master$programmiererfahrung),
    sd(master$selbst_verstaendnis),
    sd(master$selbst_sicherheit),
    sd(master$minecraft_erfahrung),
    sd(master$spass_videospiele)
  )
))


# Wissenstest -----------------------------------------------------------------

cat("\n--- Wissenstest ---\n")
print(rbind(
  Pretest = desc(master$pre_gesamt),
  Posttest = desc(master$post_gesamt)
))

print(table(factor(
  sign(master$post_gesamt - master$pre_gesamt),
  levels = c(-1, 0, 1),
  labels = c("Verschlechtert", "Unverändert", "Verbessert")
)))

print(wilcoxon_paired(master$post_gesamt, master$pre_gesamt))


# Einzelne Wissensfragen ------------------------------------------------------

pre <- read_excel(pre_file, sheet = 1, skip = 1)
names(pre)[1:13] <- c(
  "id", "alter", "geschlecht", "abschluss", "programmiererfahrung",
  "selbst_verstaendnis", "selbst_sicherheit", "videospiele_regelmaessig",
  "minecraft_erfahrung", "spass_videospiele",
  "pre_bedingung_antwort", "pre_schleife_antwort", "pre_bool_antwort"
)

pre <- pre |>
  filter(str_detect(as.character(id), "^ID\\d+$")) |>
  transmute(
    id = as.character(id),
    bedingung = as.integer(text_norm(pre_bedingung_antwort) == "ja"),
    schleife = as.integer(text_norm(pre_schleife_antwort) == "nein"),
    bool = as.integer(text_norm(pre_bool_antwort) == "false")
  )

post <- read_excel(post_file, sheet = 1, skip = 1)
names(post)[1:12] <- c(
  "id", "post_bedingung_antwort", "post_schleife_antwort",
  "post_bool_antwort", paste0("ueq_", 1:8, "_raw")
)

post <- post |>
  filter(str_detect(as.character(id), "^ID\\d+$")) |>
  transmute(
    id = as.character(id),
    bedingung = as.integer(text_norm(post_bedingung_antwort) == "b"),
    schleife = as.integer(to_num(post_schleife_antwort) == 3),
    bool = as.integer(text_norm(post_bool_antwort) == "false")
  )

wissen <- inner_join(pre, post, by = "id", suffix = c("_pre", "_post"))

konzepte <- list(
  "Boolesche Algebra" = c("bool_pre", "bool_post"),
  "Verzweigungen" = c("bedingung_pre", "bedingung_post"),
  "Schleifen" = c("schleife_pre", "schleife_post")
)

mcnemar_ergebnisse <- lapply(names(konzepte), function(k) {
  vars <- konzepte[[k]]
  pre_x <- wissen[[vars[1]]]
  post_x <- wissen[[vars[2]]]
  test <- mcnemar_exact(pre_x, post_x)
  data.frame(
    Konzept = k,
    Pre_korrekt = sum(pre_x),
    Post_korrekt = sum(post_x),
    falsch_zu_richtig = test["falsch_richtig"],
    richtig_zu_falsch = test["richtig_falsch"],
    p = test["p"]
  )
}) |>
  bind_rows() |>
  mutate(p_Holm = p.adjust(p, method = "holm"))

print(mcnemar_ergebnisse)


# Bearbeitung der Lernumgebung ------------------------------------------------

study <- read_excel(
  study_file, sheet = 1, range = "A1:I184", col_names = FALSE
)

id_rows <- which(str_detect(as.character(study[[1]]), "^(ID|D)\\d+$"))

parse_person <- function(start_row) {
  id <- as.character(study[[1]][start_row])
  if (id == "D06") id <- "ID06"
  if (id == "D13") id <- "ID13"
  if (id == "D14") id <- "ID14"

  rows <- study[(start_row + 1):(start_row + 3), ]
  test_end <- vapply(rows[[2]], time_to_min, numeric(1))
  lesson_end <- vapply(rows[[3]], time_to_min, numeric(1))
  previous_end <- c(0, lesson_end[1:2])

  out <- data.frame(
    id = id,
    lektion = 1:3,
    testkammer = test_end - previous_end,
    hauptaufgabe = lesson_end - test_end,
    gesamt = lesson_end - previous_end,
    fehler_test = to_num(rows[[4]]),
    fehler_haupt = to_num(rows[[5]]),
    hilfe_test = vapply(rows[[8]], count_entry, integer(1)),
    hilfe_haupt = vapply(rows[[9]], count_entry, integer(1))
  )

  out$fehler_test[is.na(out$fehler_test)] <- 0
  out$fehler_haupt[is.na(out$fehler_haupt)] <- 0

  # In diesen beiden Zellen stehen jeweils zwei getrennte Hilfestellungen.
  if (id == "ID09") out$hilfe_test[out$lektion == 2] <- 2
  if (id == "ID11") out$hilfe_test[out$lektion == 2] <- 2

  out$fehler <- out$fehler_test + out$fehler_haupt
  out$hilfen <- out$hilfe_test + out$hilfe_haupt
  out
}

lektionen <- lapply(id_rows, parse_person) |> bind_rows()

cat("\n--- Bearbeitungszeiten und Fehlversuche ---\n")
print(desc(master$welt_dauer))

print(lektionen |>
  group_by(lektion) |>
  summarise(
    Testkammer_M = mean(testkammer),
    Testkammer_SD = sd(testkammer),
    Hauptaufgabe_M = mean(hauptaufgabe),
    Hauptaufgabe_SD = sd(hauptaufgabe),
    Gesamt_M = mean(gesamt),
    Gesamt_SD = sd(gesamt),
    Fehler_M = mean(fehler),
    Fehler_SD = sd(fehler),
    Hilfen = sum(hilfen),
    .groups = "drop"
  ))

zeit_matrix <- lektionen |>
  select(id, lektion, gesamt) |>
  pivot_wider(names_from = lektion, values_from = gesamt) |>
  arrange(id) |>
  select(`1`, `2`, `3`) |>
  as.matrix()

fehler_matrix <- lektionen |>
  select(id, lektion, fehler) |>
  pivot_wider(names_from = lektion, values_from = fehler) |>
  arrange(id) |>
  select(`1`, `2`, `3`) |>
  as.matrix()

friedman_zeit <- friedman.test(zeit_matrix)
friedman_fehler <- friedman.test(fehler_matrix)

cat("\nFriedman-Test Bearbeitungszeit:\n")
print(friedman_zeit)
cat("Kendall-W:", unname(friedman_zeit$statistic) / (nrow(zeit_matrix) * 2), "\n")

cat("\nFriedman-Test Fehlversuche:\n")
print(friedman_fehler)
cat("Kendall-W:", unname(friedman_fehler$statistic) / (nrow(fehler_matrix) * 2), "\n")

paarweise_tests <- function(matrix) {
  paare <- list(c(1, 2), c(1, 3), c(2, 3))
  out <- lapply(paare, function(paar) {
    test <- wilcoxon_paired(matrix[, paar[2]], matrix[, paar[1]])
    data.frame(
      Vergleich = paste0("Lektion ", paar[1], " vs. ", paar[2]),
      W = test$W,
      p = test$p
    )
  }) |> bind_rows()
  out$p_Holm <- p.adjust(out$p, method = "holm")
  out
}

cat("\nPaarweise Vergleiche Bearbeitungszeit:\n")
print(paarweise_tests(zeit_matrix))

cat("\nPaarweise Vergleiche Fehlversuche:\n")
print(paarweise_tests(fehler_matrix))


# UEQ-S -----------------------------------------------------------------------

cat("\n--- UEQ-S ---\n")

ueq_skalen <- data.frame(
  Skala = c("Pragmatisch", "Hedonisch", "Gesamt"),
  M = c(
    mean(master$ueq_pragmatisch),
    mean(master$ueq_hedonisch),
    mean(master$ueq_gesamt)
  ),
  SD = c(
    sd(master$ueq_pragmatisch),
    sd(master$ueq_hedonisch),
    sd(master$ueq_gesamt)
  )
)

ueq_skalen$KI_unten <- ueq_skalen$M -
  1.96 * ueq_skalen$SD / sqrt(nrow(master))
ueq_skalen$KI_oben <- ueq_skalen$M +
  1.96 * ueq_skalen$SD / sqrt(nrow(master))

print(ueq_skalen)

cat(
  "Cronbachs Alpha pragmatisch:",
  cronbach_alpha(master[, paste0("ueq_", 1:4, "_raw")]), "\n"
)
cat(
  "Cronbachs Alpha hedonisch:",
  cronbach_alpha(master[, paste0("ueq_", 5:8, "_raw")]), "\n"
)

ueq_items <- master |>
  select(all_of(paste0("ueq_", 1:8, "_raw"))) - 4

item_ergebnisse <- data.frame(
  Item = 1:8,
  M = sapply(ueq_items, mean),
  SD = sapply(ueq_items, sd)
)

item_ergebnisse$KI_unten <- item_ergebnisse$M -
  1.96 * item_ergebnisse$SD / sqrt(nrow(master))
item_ergebnisse$KI_oben <- item_ergebnisse$M +
  1.96 * item_ergebnisse$SD / sqrt(nrow(master))

print(item_ergebnisse)


# Explorative Korrelationen ---------------------------------------------------

cat("\n--- Explorative Korrelationen ---\n")

cat(
  "Cronbachs Alpha programmierbezogener Selbsteinschätzungsindex:",
  cronbach_alpha(master[, c(
    "programmiererfahrung",
    "selbst_verstaendnis",
    "selbst_sicherheit"
  )]),
  "\n"
)

korrelationen <- data.frame(
  variable_1 = c(
    "programmier_selbstindex", "programmier_selbstindex",
    "programmier_selbstindex", "programmier_selbstindex",
    "minecraft_erfahrung", "spass_videospiele",
    "minecraft_erfahrung", "pre_gesamt"
  ),
  variable_2 = c(
    "pre_gesamt", "post_gesamt", "welt_dauer", "ueq_pragmatisch",
    "ueq_pragmatisch", "ueq_hedonisch", "welt_dauer", "welt_dauer"
  )
)

korrelations_ergebnisse <- lapply(seq_len(nrow(korrelationen)), function(i) {
  test <- cor.test(
    master[[korrelationen$variable_1[i]]],
    master[[korrelationen$variable_2[i]]],
    method = "spearman",
    exact = FALSE,
    alternative = "two.sided"
  )
  data.frame(
    Zusammenhang = paste(
      korrelationen$variable_1[i],
      korrelationen$variable_2[i],
      sep = " - "
    ),
    rho = unname(test$estimate),
    p = test$p.value
  )
}) |> bind_rows()

korrelations_ergebnisse$p_Holm <-
  p.adjust(korrelations_ergebnisse$p, method = "holm")

print(korrelations_ergebnisse)
