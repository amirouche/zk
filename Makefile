help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

dev: ## Setup dev environment, you still need Chez Scheme
	git submodule update --init
	cd upstream/termbox/ && ./waf configure && ./waf
	akku install
	@echo "\n  Now you can do: source .akku/bin/activate"
	@echo "\n  Then run the editor with: scheme zk.scm"
	@echo "\n  You can quit the editor by typing: Ctrl+Q\n"

devrun:  ## Fail safe run
	scheme zk.scm || reset
	cat out.log
