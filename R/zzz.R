.onLoad <- function(...) {
  try(check_si_up_to_date())
}

check_si_up_to_date <- function() {
  rem <- structure(remotes:::package2remote("sorensonimpact"), class = "github_remote")
  if(is.na(rem[["sha"]])) return()
  rem <- remotes:::remote_sha(rem)

  loc <- remotes:::local_sha("sorensonimpact")

  if(loc != rem) {
    cli::cli_alert_info(cli::bg_red(cli::col_white(cli::style_bold("A newer version of \`sorensonimpact\` is available.  Run \`si_update()\` to update."))))
  }

}
