AC_INIT([murrayc-compiler-experiments],[0.0.1])
AC_PREREQ(2.63)

AC_CONFIG_SRCDIR([src/chapter_02/section_2_4_3_subset_construction.cc])
AC_CONFIG_MACRO_DIR([macros])
$AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([1.10 -Wno-portability no-define nostdinc tar-ustar])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AM_MAINTAINER_MODE
AC_ARG_VAR([ACLOCAL_FLAGS], [aclocal flags, e.g. -I <macro dir>])

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CXX

AX_CXX_COMPILE_STDCXX([17], [noext], [mandatory])

PKG_CHECK_MODULES([PROJECT], [sigc++-3.0])

# See http://www.gnu.org/software/autoconf-archive/ax_boost_base.html
# Boost System is needed by Boost Timer
AX_BOOST_BASE
AX_BOOST_SYSTEM
AX_BOOST_TIMER

# Allow use of mm-common macros for the warnings option.
MM_PREREQ([0.9.5])
#MM_INIT_MODULE([test-0-1])

AC_LANG([C++])
MM_ARG_ENABLE_WARNINGS([PROJECT_WXXFLAGS],
                       [-Wall],
                       [-pedantic -Wall -Wextra -Wsuggest-override -Wshadow -Wformat-security -Wno-missing-field-initializers -DGSEAL_ENABLE],
                       [G GDK GDK_PIXBUF GTK GDA GLIBMM GDKMM CAIROMM PANGOMM GTKMM SIGCXX LIBGDAMM])


AC_CONFIG_FILES([Makefile])

AC_OUTPUT
