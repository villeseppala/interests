# tools/run_author.R
# Launch the author app with restart support.
# Source this file once to start; clicking "Restart" in the app will stop it,
# re-source all code, and relaunch — picking up any changes made to app.R or
# shared files.

library(here)

restart_flag <- here("app_author", ".restart")

repeat {
  if (file.exists(restart_flag)) file.remove(restart_flag)
  shiny::runApp(here("app_author"),
                launch.browser = getOption("shiny.launch.browser", interactive()))
  if (file.exists(restart_flag)) {
    file.remove(restart_flag)
    message("Restarting author app...")
  } else {
    break
  }
}
