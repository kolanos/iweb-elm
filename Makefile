build:
	elm-package install --yes
	elm-make main.elm --output=www/js/main.js