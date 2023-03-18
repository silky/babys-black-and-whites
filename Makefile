# ~ Targets
.PHONY: img $(MAKECMDGOALS)

build: ## Just build the project
	cabal build

img: build ## Make an output image
	fswatch --event Updated -o ./src/ | xargs -I{} cabal run baby-black-and-whites -- -w 500 -h 500 -o a.png

help: ## List all available targets
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := img
