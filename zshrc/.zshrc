fpath+=("$(brew --prefix)/share/zsh/site-functions")

autoload -U promptinit; promptinit

# change the path color
zstyle :prompt:pure:path color red

prompt pure


alias nv="nvim"
