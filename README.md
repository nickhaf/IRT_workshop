# IRT Workshop

Quarto-Website für den Workshop **„Anwendung von Modellen der Item-Response-Theorie in R"** (Sebastian Weirich & Nicklas Hafiz).

## Struktur

```
├── index.qmd            # Startseite mit Zeitplan (Listing-Grids pro Tag)
├── slides/              # Ein Ordner pro Foliensatz, nummeriert in Präsentationsreihenfolge
│   ├── index.qmd        # Übersicht aller Foliensätze
│   └── <NN>_<Thema>/
│       ├── index.qmd    # Übersichtsseite: Zusammenfassung, eingebettete Folien, Link zur Übung
│       └── slides.qmd   # Reveal.js-Foliensatz (bzw. PDF/PPTX bei importierten Folien)
├── exercises/           # Übungen (eigener Bereich mit Sidebar-Navigation)
│   ├── index.qmd        # Übersicht aller Übungen
│   ├── index_<thema>.qmd
│   └── <thema>/         # Teilübungen zu umfangreicheren Themen
├── R/                   # Hilfsfunktionen (Plots, IRT-Formeln)
├── raw_data/            # Datensätze für die Übungen
├── _extensions/         # letterbox (Reveal.js-Theme), embedio (Folien-/PDF-Einbettung)
├── styles.scss          # Theme für die Folien
└── styles_html.scss     # Theme für die HTML-Seiten
```

Navigation und Sidebar sind in [`_quarto.yml`](_quarto.yml) konfiguriert: Die Navbar enthält Dropdowns pro Workshop-Tag mit den Foliensatz-Seiten sowie Links auf die Folien- und Übungsübersicht.

## Rendern

```bash
quarto render    # baut die Website nach _site/
quarto preview   # lokale Vorschau mit Live-Reload
```

Die R-Abhängigkeiten werden über [renv](https://rstudio.github.io/renv/) verwaltet (`renv::restore()`). Veröffentlicht wird die Seite über GitHub Actions auf GitHub Pages.
