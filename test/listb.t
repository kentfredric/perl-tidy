# Some examples of lists which are "broken" by comments and/or blank lines.
# For such lists, perltidy uses the original line breaks, rather than
# attempting to use its default formatting.  This can be a good thing.

# Here is my favorite example of a "broken" list, from latex2html.  It wouldn't
# be possible to automatically format this with a fixed number of fields.
# It would be a mess if formatted with the default formatting.

unless(GetOptions(\%opt, # all non-linked options go into %opt
        # option                linkage (optional)
        'help|h',
        'version|V',
        'split=s',
        'link=s',
        'toc_depth=i',          \$TOC_DEPTH,
        'toc_stars!',           \$TOC_STARS,
        'short_extn!',          \$SHORTEXTN,
        'iso_language=s',       \$ISO_LANGUAGE,
        'validate!',            \$HTML_VALIDATE,
        'latex!',
        'djgpp!',               \$DJGPP,
        'fork!',                \$CAN_FORK,
        'external_images!',     \$EXTERNAL_IMAGES,
        'ascii_mode!',          \$ASCII_MODE,
        'lcase_tags!',          \$LOWER_CASE_TAGS,
        'ps_images!',           \$PS_IMAGES,
        'font_size=s',          \$FONT_SIZE,
        'tex_defs!',            \$TEXDEFS,
        'navigation!',
        'top_navigation!',      \$TOP_NAVIGATION,
        'bottom_navigation!',   \$BOTTOM_NAVIGATION,
        'auto_navigation!',     \$AUTO_NAVIGATION,
        'index_in_navigation!', \$INDEX_IN_NAVIGATION,
        'contents_in_navigation!', \$CONTENTS_IN_NAVIGATION,
        'next_page_in_navigation!', \$NEXT_PAGE_IN_NAVIGATION,
        'previous_page_in_navigation!', \$PREVIOUS_PAGE_IN_NAVIGATION,
        'footnode!',
        'numbered_footnotes!',  \$NUMBERED_FOOTNOTES,
        'prefix=s',             \$PREFIX,
        'auto_prefix!',         \$AUTO_PREFIX,
        'long_titles=i',        \$LONG_TITLES,
        'custom_titles!',       \$CUSTOM_TITLES,
        'title|t=s',            \$TITLE,
        'rooted!',              \$ROOTED,
        'rootdir=s',
        'dir=s',                \$FIXEDDIR,
        'mkdir',                \$MKDIR,
        'address=s',            \$ADDRESS,
        'noaddress',
        'subdir!',
        'info=s',               \$INFO,
        'noinfo',
        'auto_link!',
        'reuse=i',              \$REUSE,
        'noreuse',
        'antialias_text!',      \$ANTI_ALIAS_TEXT,
        'antialias!',           \$ANTI_ALIAS,
        'transparent!',         \$TRANSPARENT_FIGURES,
        'white!',               \$WHITE_BACKGROUND,
        'discard!',             \$DISCARD_PS,
        'image_type=s',         \$IMAGE_TYPE,
        'images!',
        'accent_images=s',      \$ACCENT_IMAGES,
        'noaccent_images',
        'style=s',              \$STYLESHEET,
        'parbox_images!',
        'math!',
        'math_parsing!',
        'latin!',
        'entities!',            \$USE_ENTITY_NAMES,
        'local_icons!',         \$LOCAL_ICONS,
        'scalable_fonts!',      \$SCALABLE_FONTS,
        'images_only!',         \$IMAGES_ONLY,
        'show_section_numbers!',\$SHOW_SECTION_NUMBERS,
        'show_init!',           \$SHOW_INIT_FILE,
        'init_file=s',          \$INIT_FILE,
        'up_url=s',             \$EXTERNAL_UP_LINK,
        'up_title=s',           \$EXTERNAL_UP_TITLE,
        'down_url=s',           \$EXTERNAL_DOWN_LINK,
        'down_title=s',         \$EXTERNAL_DOWN_TITLE,
        'prev_url=s',           \$EXTERNAL_PREV_LINK,
        'prev_title=s',         \$EXTERNAL_PREV_TITLE,
        'index=s',              \$EXTERNAL_INDEX,
        'biblio=s',             \$EXTERNAL_BIBLIO,
        'contents=s',           \$EXTERNAL_CONTENTS,
        'external_file=s',      \$EXTERNAL_FILE,
        'short_index!',         \$SHORT_INDEX,
        'unsegment!',           \$UNSEGMENT,
        'debug!',               \$DEBUG,
        'tmp=s',                \$TMP,
        'ldump!',               \$LATEX_DUMP,
        'timing!',              \$TIMING,
        'verbosity=i',          \$VERBOSITY,
        'html_version=s',       \$HTML_VERSION,
        'strict!',              \$STRICT_HTML,
        'test_mode!' # undocumented switch
       )) {
    &usage();
    exit 1;
    }

# Another example of a broken list (from aclocal).  This has one column,
# which is not usually used by the default formatting.
@obsolete_macros = (
  'AC_FEATURE_CTYPE',
  'AC_FEATURE_ERRNO',
  'AC_FEATURE_EXIT',
  'AC_SYSTEM_HEADER',
  'fp_C_PROTOTYPES',
  'fp_FUNC_FNMATCH',
  'fp_PROG_CC_STDC',
  'fp_PROG_INSTALL',
  'fp_WITH_DMALLOC',
  'fp_WITH_REGEX',
  'gm_PROG_LIBTOOL',
  'jm_MAINTAINER_MODE',
  'md_TYPE_PTRDIFF_T',
  'ud_PATH_LISPDIR',
  'ud_GNU_GETTEXT',

  # Now part of autoconf proper, under a different name.
  'AM_FUNC_FNMATCH',
  'AM_SANITY_CHECK_CC',
  'AM_PROG_INSTALL',
  'AM_EXEEXT',
  'AM_CYGWIN32',
  'AM_MINGW32',

  # These aren't quite obsolete.
  #      'md_PATH_PROG',
  #      'ud_LC_MESSAGES',
  #      'ud_WITH_NLS'
);

    # Here is another list with a side comment (from latex2html).
    # Perltidy will therefore use the old line breaks, and thus keep
    # the leading commas.
    %html_specials_inv = (
    		 ';SPMlt;' ,'<'
		, ';SPMgt;','>'
		, ';SPMamp;','&'
		, ';SPMquot;','"'
		, ';SPMldquo;','``'
		, ';SPMrdquo;',"''"
		, ';SPMdollar;', '$'	# for alltt
		, ';SPMpct;', '%'
		, ';SPMtilde;', '&#126;'
		);

    # Here is the same list with the side comment deleted
    # Perltidy will therefore use its default formatting
    %html_specials_inv = (
    		 ';SPMlt;' ,'<'
		, ';SPMgt;','>'
		, ';SPMamp;','&'
		, ';SPMquot;','"'
		, ';SPMldquo;','``'
		, ';SPMrdquo;',"''"
		, ';SPMdollar;', '$'
		, ';SPMpct;', '%'
		, ';SPMtilde;', '&#126;'
		);

# This has no comments and will be formatted in the default manner
my $optval =
  GetOptions( "help|h", \$args->{"help"}, "version|v", \$args->{"version"},
  "license|L", \$args->{"license"}, "quiet|q", \$args->{"quiet"}, "report|r",
  \$args->{"report"}, "recursive|R", \$args->{"recursive"}, "libs|l",
  \$args->{"libs"}, "bare|b", \$args->{"bare"}, "err|e", \$args->{"err"},
  "force|f=s", \$args->{"force"}, "nosym|n", \$args->{"nosym"} );
