# tools/run_publish.R
# Launch the publish app. Source this file from the project root (pts/).
# This ensures the working directory is set correctly so www/ paths resolve.

library(here)

shiny::runApp(here("app_publish"),
              launch.browser = getOption("shiny.launch.browser", interactive()))
