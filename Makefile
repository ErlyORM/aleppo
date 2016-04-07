PROJECT = aleppo

TEST_DEPS = katana_test mixer

dep_katana_test = git https://github.com/inaka/katana-test.git 0.0.5
dep_mixer       = git https://github.com/inaka/mixer.git       0.1.5

BUILD_DEPS = inaka_mk hexer_mk
DEP_PLUGINS = inaka_mk hexer_mk

dep_inaka_mk    = git https://github.com/inaka/inaka.mk.git    1.0.0
dep_hexer_mk    = git https://github.com/inaka/hexer.mk.git    1.1.0

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

include erlang.mk
