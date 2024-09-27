include guile.am

SOURCES =                           \
  commonmark/utils.scm              \
  commonmark/common.scm             \
  commonmark/parser.scm             \
  commonmark/entities.scm           \
  commonmark/node.scm               \
  commonmark/blocks.scm             \
  commonmark/inlines.scm            \
  commonmark/html.scm               \
  commonmark/sxml.scm               \
  commonmark.scm

TESTS =	                                       \
  tests/node.scm                               \
  tests/tabs.scm                               \
  tests/blocks/paragraphs.scm                  \
  tests/blocks/atx-headings.scm                \
  tests/blocks/block-quotes.scm                \
  tests/blocks/code-blocks.scm                 \
  tests/blocks/fenced-code.scm                 \
  tests/blocks/link-reference-definitions.scm  \
  tests/blocks/list-items.scm                  \
  tests/blocks/lists.scm                       \
  tests/blocks/setext-headings.scm             \
  tests/blocks/thematic-breaks.scm             \
  tests/inlines/code-spans.scm                 \
  tests/inlines/emphasis.scm                   \
  tests/inlines/backslash-escape.scm           \
  tests/inlines/softbreak.scm                  \
  tests/inlines/hardbreak.scm                  \
  tests/inlines/links.scm                      \
  tests/inlines/images.scm                     \
  tests/inlines/autolinks.scm                  \
  tests/inlines/entities.scm                   \
  tests/html.scm

TEST_EXTENSIONS = .scm

SCM_LOG_DRIVER =                               \
  $(top_builddir)/pre-inst-env                 \
  $(GUILE) --no-auto-compile -e main           \
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

include $(top_srcdir)/doc/local.mk

EXTRA_DIST = README.md                        \
             pre-inst-env.in                  \
             tests/utils.scm                  \
             build-aux/test-driver.scm        \
             $(TESTS)
