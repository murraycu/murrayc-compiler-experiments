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
  section_2_4_3_subset_construction \
  section_2_4_4_minimal_dfa_with_hopcrofts_algorithm \
  section_2_5_1_table_driven_scanner \
  section_2_5_1_table_driven_scanner_avoiding_excess_rollback \
  section_3_3_1_top_down_parser_leftmost \
  section_3_3_1_top_down_parser_leftmost_backtrack_free \
  section_3_3_3_top_down_table_driven_ll1_parser \
  section_3_4_1_bottom_up_lr1_parser \
  section_3_4_2_bottom_up_lr1_parser_with_generated_tables

TESTS = $(check_PROGRAMS)

#List of source files needed to build the executable:
section_2_4_3_subset_construction_SOURCES = \
	src/chapter_02/state.h \
	src/chapter_02/section_2_4_3_subset_construction.cc
section_2_4_3_subset_construction_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_2_4_3_subset_construction_LDADD = \
	$(COMMON_LIBS)

section_2_4_4_minimal_dfa_with_hopcrofts_algorithm_SOURCES = \
	src/chapter_02/section_2_4_4_minimal_dfa_with_hopcrofts_algorithm.cc
section_2_4_4_minimal_dfa_with_hopcrofts_algorithm_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_2_4_4_minimal_dfa_with_hopcrofts_algorithm_LDADD = \
	$(COMMON_LIBS)

section_2_5_1_table_driven_scanner_SOURCES = \
	src/chapter_02/section_2_5_1_table_driven_scanner.cc
section_2_5_1_table_driven_scanner_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_2_5_1_table_driven_scanner_LDADD = \
	$(COMMON_LIBS)

section_2_5_1_table_driven_scanner_avoiding_excess_rollback_SOURCES = \
	src/chapter_02/section_2_5_1_table_driven_scanner_avoiding_excess_rollback.cc
section_2_5_1_table_driven_scanner_avoiding_excess_rollback_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_2_5_1_table_driven_scanner_avoiding_excess_rollback_LDADD = \
	$(COMMON_LIBS)

section_3_3_1_top_down_parser_leftmost_SOURCES = \
	src/chapter_03/grammar.h \
	src/chapter_03/symbol.h \
	src/chapter_03/tree.h \
	src/chapter_03/section_3_3_1_top_down_parser_leftmost.cc
section_3_3_1_top_down_parser_leftmost_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_3_3_1_top_down_parser_leftmost_LDADD = \
	$(COMMON_LIBS)

section_3_3_1_top_down_parser_leftmost_backtrack_free_SOURCES = \
	src/chapter_03/grammar.h \
	src/chapter_03/symbol.h \
	src/chapter_03/tree.h \
	src/chapter_03/build_sets.h \
	src/chapter_03/section_3_3_1_top_down_parser_leftmost_backtrack_free.cc
section_3_3_1_top_down_parser_leftmost_backtrack_free_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_3_3_1_top_down_parser_leftmost_backtrack_free_LDADD = \
	$(COMMON_LIBS)

section_3_3_3_top_down_table_driven_ll1_parser_SOURCES = \
	src/chapter_03/grammar.h \
	src/chapter_03/symbol.h \
	src/chapter_03/build_sets.h \
	src/chapter_03/section_3_3_3_top_down_table_driven_ll1_parser.cc
section_3_3_3_top_down_table_driven_ll1_parser_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_3_3_3_top_down_table_driven_ll1_parser_LDADD = \
	$(COMMON_LIBS)

section_3_4_1_bottom_up_lr1_parser_SOURCES = \
	src/chapter_03/grammar.h \
	src/chapter_03/symbol.h \
	src/chapter_03/section_3_4_1_bottom_up_lr1_parser.cc
section_3_4_1_bottom_up_lr1_parser_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_3_4_1_bottom_up_lr1_parser_LDADD = \
	$(COMMON_LIBS)

section_3_4_2_bottom_up_lr1_parser_with_generated_tables_SOURCES = \
	src/chapter_03/grammar.h \
	src/chapter_03/symbol.h \
	src/chapter_03/section_3_4_2_bottom_up_lr1_parser_with_generated_tables.cc
section_3_4_2_bottom_up_lr1_parser_with_generated_tables_CXXFLAGS = \
	$(COMMON_CXXFLAGS)
section_3_4_2_bottom_up_lr1_parser_with_generated_tables_LDADD = \
	$(COMMON_LIBS)
