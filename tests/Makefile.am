SCM_TESTS = \
	web/proxy/common.scm		\
	web/proxy/connection.scm	\
	web/proxy/interceptor.scm

TESTS = ${SCM_TESTS}

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = \
	abs_top_srcdir="$(abs_top_srcdir)"; export abs_top_srcdir; \
	abs_top_builddir="$(abs_top_builddir)"; export abs_top_builddir; \
	ORIGTERM=${TERM}; export ORIGTERM; \
	TERM=xterm; export TERM; \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE; \
	GUILE_LOAD_COMPILED_PATH="$(abs_top_srcdir)"; export GUILE_LOAD_COMPILED_PATH;

# LOG_COMPILER was introduced in Automake 1.12; don't expect "make
# check" or "make distcheck" to work with earlier versions.
SCM_LOG_COMPILER = \
	$(GUILE)

AM_SCM_LOG_FLAGS = \
	-L "$(top_srcdir)/" \
	-L "$(abs_top_builddir)/" \
	-s

EXTRA_DIST = \
	${SCM_TESTS}

CLEANFILES = \
	*.log
