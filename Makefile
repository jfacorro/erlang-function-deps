PROJECT = functions_deps

DEPS = lager sync zipper katana

dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_sync = git https://github.com/rustyio/sync.git master
dep_zipper = git https://github.com/inaka/zipper 0.1.0
dep_katana =  git https://github.com/inaka/erlang-katana 0.2.0

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Builds the elvis escript.

shell: app
	erl -pa ebin -pa deps/*/ebin -s sync -s lager
