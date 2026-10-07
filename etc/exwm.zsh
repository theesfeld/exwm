# Source this from ~/.zshrc.
# Reports $PWD to EXWM at each prompt.  Does nothing unless EXWM
# exported INSIDE_EXWM.  The directory is escaped for an Emacs string
# and is not expanded by the shell a second time.

exwm_report_cwd() {
  local dir=${PWD//\\/\\\\}
  dir=${dir//\"/\\\"}
  dir=${dir//$'\n'/\\n}
  emacsclient -e "$(printf '(exwm-report-cwd %s "%s")' "$$" "$dir")" \
    >/dev/null 2>&1 || true
}

if [[ -n ${INSIDE_EXWM-} ]]; then
  if [[ -z ${precmd_functions[(Ie)exwm_report_cwd]-} || ${precmd_functions[(Ie)exwm_report_cwd]} == 0 ]]; then
    precmd_functions+=(exwm_report_cwd)
  fi
fi
