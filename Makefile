default: link-config set-shell sync-vim

link-config:
	stow --restow `ls -d */`

set-shell:
	chsh -s `which fish`

sync-vim:
	./neovim/.config/nvim/sync.sh
