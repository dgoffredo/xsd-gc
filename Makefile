
BUILD_DIR = build/$(shell uname)
SOURCES = $(shell find . -type f -name '*.rkt')

$(BUILD_DIR)/bin/xsd-gc: $(SOURCES)
	mkdir -p $(BUILD_DIR)
	raco exe -o $(BUILD_DIR)/xsd-gc info.rkt
	raco distribute $(BUILD_DIR) $(BUILD_DIR)/xsd-gc
	rm $(BUILD_DIR)/xsd-gc

.PHONY: build test package clean

## Create self-contained distribution
build: $(BUILD_DIR)/bin/xsd-gc

## Run all of the unit tests
test:
	raco test --quiet --quiet-program .

## `raco pkg install` xsd-gc library
package: $(SOURCES)
	-2>/dev/null raco pkg remove xsd-gc
	raco pkg install

## Remove build and all build/run artifacts
clean:
	if [ -d build ]; then rm -r build; fi
	find . -type d -name 'compiled'    -exec rm -r {} +

# The "help" target and its associated code was copied from crifan's
# December 7, 2017 comment on <https://gist.github.com/prwhite/8168133>,
# accessed July 23, 2018.

# COLORS
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
WHITE  := $(shell tput -Txterm setaf 7)
RESET  := $(shell tput -Txterm sgr0)

TARGET_MAX_CHAR_NUM=20
## Show help
help:
	@echo ''
	@echo 'Usage:'
	@echo '  ${YELLOW}make${RESET} ${GREEN}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

