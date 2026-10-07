# Source this from ~/.config/fish/config.fish.
# Reports $PWD to EXWM at each prompt.  Does nothing unless EXWM
# exported INSIDE_EXWM.  The directory is escaped for an Emacs string
# and is not expanded by the shell a second time.

if set -q INSIDE_EXWM
    if not functions -q exwm_report_cwd
        function exwm_report_cwd --on-event fish_prompt
            set -l dir (string replace -a '\\' '\\\\' -- $PWD)
            set dir (string replace -a '"' '\\"' -- $dir)
            set dir (string replace -a \n '\\n' -- $dir)
            emacsclient -e (printf '(exwm-report-cwd %s "%s")' $fish_pid $dir) \
                >/dev/null 2>&1
        end
    end
end
