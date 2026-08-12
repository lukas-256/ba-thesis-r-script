# Auswertung der Minecraft-User-Study

Dieses Repository enthält das R-Skript zur statistischen Auswertung der im Rahmen der Bachelorarbeit

**„Vermittlung grundlegender Programmierlogik durch Minecraft: Ein spielerischer Lernansatz“**

durchgeführten User Study.

Das Skript berechnet unter anderem:

- deskriptive Statistiken zur Stichprobe
- Pre-/Post-Vergleiche des Wissenstests
- exakte McNemar-Tests für die einzelnen Programmierkonzepte
- Auswertungen zu Bearbeitungszeiten, Fehlversuchen und Hilfestellungen
- UEQ-S-Kennwerte und interne Konsistenzen
- explorative Spearman-Rangkorrelationen mit Holm-Korrektur

## Voraussetzungen

Benötigt werden R sowie die Pakete:

- `readxl`
- `dplyr`
- `tidyr`
- `stringr`

Die zugrunde liegenden Rohdaten sind aus Datenschutzgründen nicht Bestandteil dieses Repositories.

## Ausführung

Das Skript erwartet die verwendeten Datendateien im selben Verzeichnis. Die Dateinamen können bei Bedarf am Anfang des Skripts angepasst werden.

Die statistische Auswertung entspricht dem in Kapitel 4.3 der Bachelorarbeit beschriebenen Vorgehen.
