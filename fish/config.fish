if status is-interactive
    # Commands to run in interactive sessions can go here
end
alias nv="nvim"
alias dc='docker-compose'
# Some tmux-related shell aliases
alias t='tmux attach || tmux new-session'
alias ta='tmux attach -t'
alias tn='tmux new-session'
alias tl='tmux list-sessions'

# work related stuff
function fetch_endpoint
    set endpoint $argv[1]
    set access_token $ACCESS_TOKEN
    curl -H -s "Authorization: Bearer $access_token" $endpoint | jq .
end
alias fetch=fetch_endpoint

function fzf_fd
    set selected (fd --type f --type d | fzf)

    if test -d "$selected"
        cd "$selected"
    else
        hx "$selected"
    end
end
alias f=fzf_fd


eval (tmuxifier init - fish)
starship init fish | source
zoxide init fish | source
rvm default
