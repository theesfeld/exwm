# Source this from ~/.bashrc.
# Reports $PWD to EXWM at each prompt.  Does nothing unless EXWM
# exported INSIDE_EXWM.  The directory is escaped for an Emacs string
# and is not expanded by the shell a second time.

exwm_report_cwd() {
  local dir
  dir=${PWD//\\/\\\\}
  dir=${dir//\"/\\\"}
  dir=${dir//$'\n'/\\n}
  emacsclient -e "$(printf '(exwm-report-cwd %s "%s")' "$$" "$dir")" \
    >/dev/null 2>&1 || true
}

if [[ -n ${INSIDE_EXWM-} ]]; then
  case " $(declare -p PROMPT_COMMAND 2>/dev/null) " in
    *'declare -a'*)
      if [[ " ${PROMPT_COMMAND[*]-} " != *" exwm_report_cwd "* ]]; then
        PROMPT_COMMAND+=(exwm_report_cwd)
      fi
      ;;
    *)
      case ";${PROMPT_COMMAND-};" in
        *";exwm_report_cwd;"*) ;;
        *) PROMPT_COMMAND="exwm_report_cwd${PROMPT_COMMAND:+;$PROMPT_COMMAND}" ;;
      esac
      ;;
  esac
fi
