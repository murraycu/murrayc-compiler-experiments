# Put compiled object files into the same sub-directory as their sources
AUTOMAKE_OPTIONS = subdir-objects

# Point to our macro directory and pick up user flags from the environment
ACLOCAL_AMFLAGS  = -I macros ${ACLOCAL_FLAGS}

# Make warnings always fatal at distcheck time, and disable execution
# of tools that change files not installed by the package itself
DISTCHECK_CONFIGURE_FLAGS = --enable-warnings=fatal

#SUBDIRS =

AM_CXXFLAGS = $(PROJECT_WXXFLAGS)

# TODO: Why do we still need to specify this for each executable that we build?
COMMON_CXXFLAGS = $(PROJECT_CFLAGS) \
  $(PROJECT_WXXFLAGS) \
  $(BOOST_CPPFLAGS)

COMMON_LIBS = $(PROJECT_LIBS) \
  $(BOOST_SYSTEM_LIB) \
  $(BOOST_TIMER_LIB)

check_PROGRAMS = \
  section_2_4_3_subset_construction

TESTS = $(check_PROGRAMS)

#List of source files needed to build the executable:
section_2_4_3_subset_construction_SOURCES = \
	src/chapter_02/state.h \
	src/chapter_02/section_2_4_3_subset_construction.cc
section_2_4_3_subset_construction_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_2_4_3_subset_construction_LDADD = \
	$(COMMON_LIBS)

