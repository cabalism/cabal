CABALBUILD := cabal build

# API generation

API_GHC=9.10.1
API_FLAGS:=-w ghc-$(API_GHC) --write-ghc-environment-files=always --disable-documentation --project-file=cabal.release.project --builddir=dist-newstyle.apigen

.PHONY: generate-api
generate-api: generate-api-cabal-syntax generate-api-cabal generate-api-cabal-hooks generate-api-cabal-install-solver

.PHONY: check-api
check-api: check-api-cabal-syntax check-api-cabal check-api-cabal-hooks check-api-cabal-install-solver

.PHONY: update-api
update-api: generate-api
	mv Cabal-syntax-$(API_GHC).api Cabal-syntax/Cabal-syntax-$(API_GHC).api
	mv Cabal-$(API_GHC).api Cabal/Cabal-$(API_GHC).api
	if test -f Cabal-hooks-$(API_GHC).api; then mv Cabal-hooks-$(API_GHC).api Cabal-hooks/Cabal-hooks-$(API_GHC).api; fi
	mv cabal-install-solver-$(API_GHC).api cabal-install-solver/cabal-install-solver-$(API_GHC).api

.PHONY: check-api-cabal-syntax
check-api-cabal-syntax: generate-api-cabal-syntax
	diff -c Cabal-syntax/Cabal-syntax-$(API_GHC).api Cabal-syntax-$(API_GHC).api

.PHONY: check-api-cabal
check-api-cabal: generate-api-cabal
	diff -c Cabal/Cabal-$(API_GHC).api Cabal-$(API_GHC).api

.PHONY: check-api-cabal-hooks
check-api-cabal-hooks: generate-api-cabal-hooks
	if test -d Cabal-hooks; then diff -c Cabal-hooks/Cabal-hooks-$(API_GHC).api Cabal-hooks-$(API_GHC).api; fi

.PHONY: check-api-cabal-install-solver
check-api-cabal-install-solver: generate-api-cabal-install-solver
	diff -c cabal-install-solver/cabal-install-solver-$(API_GHC).api cabal-install-solver-$(API_GHC).api

# NB. currently print-api has no way to specify a target ghc version itself.
# The dependency on ghcup should be removed once it has one; nix users, among
# others, won't be very happy with it.

.PHONY: generate-api-cabal-syntax
generate-api-cabal-syntax:
	$(CABALBUILD) Cabal-syntax $(API_FLAGS)
	ghcup run --ghc $(API_GHC) -- print-api --package-name Cabal-syntax | sed 's/\([( ]\)[Cc]abal-[-0-9.][-0-9]*:/\1/g' >Cabal-syntax-$(API_GHC).api

.PHONY: generate-api-cabal
generate-api-cabal:
	$(CABALBUILD) Cabal $(API_FLAGS)
	ghcup run --ghc $(API_GHC) -- print-api --package-name Cabal | sed 's/\([( ]\)[Cc]abal-[-0-9.][-0-9]*:/\1/g' >Cabal-$(API_GHC).api

.PHONY: generate-api-cabal-hooks
generate-api-cabal-hooks:
	if test \! -d Cabal-hooks; then \
		:; \
	else \
		$(CABALBUILD) Cabal-hooks $(API_FLAGS); \
		ghcup run --ghc $(API_GHC) -- print-api --package-name Cabal-hooks | sed 's/\([( ]\)[Cc]abal-[-0-9.][-0-9]*:/\1/g' >Cabal-hooks-$(API_GHC).api; \
	fi

.PHONY: generate-api-cabal-install-solver
generate-api-cabal-install-solver:
	$(CABALBUILD) cabal-install-solver $(API_FLAGS)
	ghcup run --ghc $(API_GHC) -- print-api --package-name cabal-install-solver | sed 's/\([( ]\)[Cc]abal-[-0-9.][-0-9]*:/\1/g' >cabal-install-solver-$(API_GHC).api

.DEFAULT_GOAL := all

PRINT_API_VERSION ?= eedf83e6f828217ba7946ca8bfaf4ab4062c2363
PRINT_API_URL := https://github.com/Kleidukos/print-api/archive/${PRINT_API_VERSION}.tar.gz

print-api:
	rm -rf print-api
	curl -sSL ${PRINT_API_URL} | tar -xz
	mv print-api-* print-api
	chmod +x $$(grep -RIl '^#!' print-api)