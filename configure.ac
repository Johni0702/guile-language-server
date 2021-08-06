
dnl -*- Autoconf -*-

AC_INIT(guile-language-server, 0.1)
AC_SUBST(HVERSION, "\"0.1\"")
AC_SUBST(AUTHOR, "\"Jonas Herzig\"")
AC_SUBST(COPYRIGHT, "'(2018)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(language-server.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/guile-ls],[chmod +x scripts/guile-ls])
dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl Hall auto-generated guile-module dependencies
GUILE_MODULE_REQUIRED([json])

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
