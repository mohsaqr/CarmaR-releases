# carmar — releases

The public release channel for **CarmaR**, an R IDE that runs on your own machine.

This repository serves two purposes:

1. **The `carmar` R package source** (this tree) — built into macOS and
   Windows binaries by [mohsaqr.r-universe.dev](https://mohsaqr.r-universe.dev).
   Install with:

   ```r
   install.packages("carmar",
     repos = c("https://mohsaqr.r-universe.dev", "https://cloud.r-project.org"))
   carmar::run()
   ```

2. **Release assets** — each GitHub release carries the current notebook
   (`carmar_V*.html`, the fast update channel that `carmar::upgrade()` polls)
   and the installer script (`Install-CarmaR.R`).

CarmaR's source is developed in a separate repository; this one holds only
what installations consume. Install options: https://lacarm.com/carmar/
