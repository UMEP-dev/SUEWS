# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/stable/config

# -- Path setup --------------------------------------------------------------

import os
import platform

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from time import strptime

import pandas as pd
import sphinxcontrib.bibtex
from pybtex.plugin import register_plugin
from pybtex.style.formatting import BaseStyle, toplevel
from pybtex.style.formatting.unsrt import Style as UnsrtStyle
from pybtex.style.formatting.unsrt import Text, date, pages
from pybtex.style.sorting.author_year_title import SortingStyle as Sorter
from pybtex.style.template import (
    field,
    first_of,
    href,
    join,
    names,
    optional,
    optional_field,
    sentence,
    tag,
    words,
)

import supy

# -- Git version information --------------------------------------------------------

# Import get_ver_git module
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
try:
    from get_ver_git import get_version_from_git, get_commit_info

    git_version_string = get_version_from_git()
    is_dev_version = ".dev" in git_version_string
    git_commit_short, git_commit_full = get_commit_info()
except ImportError:
    # Fallback if functions not available
    git_version_string = "unknown"
    is_dev_version = False
    git_commit_short = "unknown"
    git_commit_full = "unknown"

# Use short version for display
git_commit = git_commit_short

print(f"Building docs for version: {git_version_string} (commit: {git_commit})")
print(f"Development version: {is_dev_version}")

# -- processing code --------------------------------------------------------


# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
sys.path.insert(0, os.path.abspath("."))
sys.path.insert(0, os.path.abspath("_ext"))

print(r"this build is made by:", "\n", sys.version)
# determine if in RTD environment
read_the_docs_build = os.environ.get("READTHEDOCS", None) == "True"
if read_the_docs_build:
    # update `today`
    dt_today = datetime.today()
    pass
else:
    dt_today = datetime.today()
    print(r"this build is for:", "\n")
    supy.show_version()


def subprocess_cmd(command):
    process = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    proc_stdout = process.communicate()[0].strip()
    print(proc_stdout.decode())


# run script to generate rst files for df_{group}
# subprocess_cmd('cd supy/proc_var_info; python3 gen_rst.py')


# load all csv as a whole df
def load_df_csv(path_csv):
    if not path_csv.exists():
        print(str(path_csv), "not existing!")
        sys.exit()

    # get list of all csv file names
    list_csv = list(path_csv.glob("*csv"))
    df_csv = pd.concat(
        {
            csv.stem: pd.read_csv(csv, skipinitialspace=True, quotechar='"')
            for csv in list_csv
        },
        sort=False,
    )
    return df_csv


# retrieve description from rst files
def load_df_opt_desc(file_options):
    ser_opts = pd.read_csv(
        file_options,
        sep=r"\n",
        skipinitialspace=True,
    )
    ser_opts = ser_opts.iloc[:, 0]
    ind_opt = ser_opts.index[ser_opts.str.contains(".. option::")]
    ser_opt_name = ser_opts[ind_opt].str.replace(".. option::", "").str.strip()
    ser_opt_desc = ser_opts[ind_opt + 2].str.strip()
    df_opt_desc = pd.DataFrame(
        {"desc": ser_opt_desc.values}, index=ser_opt_name.rename("option")
    )
    return df_opt_desc


# generate dataframe for a specific SUEWS table `csv_suews`
def gen_df_suews(df_csv, df_opt_desc, csv_suews):
    print("\t", csv_suews + ".csv")
    df_csv_suews = df_csv.loc[csv_suews].dropna(axis=1).copy()
    df_csv_suews.loc[:, "No."] = df_csv_suews.loc[:, "No."].astype(int)
    for ind, row in df_csv_suews.iterrows():
        var = row.loc["Column Name"].strip("`")
        if var in df_opt_desc.index:
            print(f"\t\t {var} found...")
            df_csv_suews.at[ind, "Description"] = df_opt_desc.loc[var].values[0]
        else:
            print(f"\t\t {var} NOT found...")
            sys.exit(0)
    print(f"\n")
    return df_csv_suews


# save all re-generated CSV files to `path_csv`
def gen_csv_suews(path_csv):
    print("re-generating summary tables ...")
    # load all csv as a whole df
    df_csv = load_df_csv(path_csv)

    # retrieve description from rst files
    file_options = path_csv.parent / "Input_Options.rst"
    df_opt_desc = load_df_opt_desc(file_options)

    list_csv_suews = df_csv.index.levels[0].to_series().filter(like="SUEWS")
    for csv_suews in list_csv_suews:
        if "Profiles" not in csv_suews:
            df_csv_suews = gen_df_suews(df_csv, df_opt_desc, csv_suews)
            df_csv_suews.to_csv(path_csv / (csv_suews + ".csv"), index=False)

    return list_csv_suews


# determine latest version and release
path_source = Path(".").resolve()
list_ver = sorted([
    x.stem
    for x in list((path_source / "version-history").glob("v*rst"))
    if "version" not in x.stem
])


# The short X.Y version
version = list_ver[-1]
# The full version, including alpha/beta/rc tags
release = list_ver[-1]

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
# today = ''
# Else, today_fmt is used as the format for a strftime call.
today_fmt = r"%Y-%m-%d"

html_last_updated_fmt = today_fmt

# determine if in RTD environment
read_the_docs_build = os.environ.get("READTHEDOCS", None) == "True"

if read_the_docs_build:
    # run doxygen
    subprocess.call("doxygen", shell=True)

    # generate summary tables using info in `Input_Options.rst`
    path_csv = path_source / "inputs/tables/SUEWS_SiteInfo/csv-table"
    gen_csv_suews(path_csv)

    # update `today`
    dt_today = datetime.today()
else:
    dt_today = datetime.today()  # Use current date for local builds too
    # subprocess.call("doxygen", shell=True)
    pass


# -- Project information ----------------------------------------------------
project = "SUEWS"
doc_name = "SUEWS Documentation"
copyright = f"2018 – {dt_today.year}"
author = "SUEWS dev team led by Prof Sue Grimmond"

# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Python 3.13 compatibility: imghdr module was removed in Python 3.13
# This affects Sphinx < 6.0 which tries to import imghdr for image handling
# Both local dev (uv with Python 3.13) and ReadTheDocs (conda with Python 3.13) need this
import sys

if sys.version_info >= (3, 13):
    # Create a more complete dummy imghdr module for Python 3.13+
    import types

    imghdr_module = types.ModuleType("imghdr")
    # Add the 'tests' attribute that Sphinx expects
    imghdr_module.tests = []
    # Add what_file function that returns None
    imghdr_module.what = lambda file, h=None: None
    sys.modules["imghdr"] = imghdr_module

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "sphinx.ext.mathjax",
    "sphinx.ext.ifconfig",
    "sphinx.ext.viewcode",
    # 'rinoh.frontend.sphinx',
    "sphinx.ext.autosectionlabel",
    # 'sphinxfortran.fortran_autodoc',
    # 'sphinxfortran.fortran_domain',
    "sphinxcontrib.bibtex",
    # "sphinxcontrib.email",
    "sphinx.ext.githubpages",
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "sphinx.ext.intersphinx",
    "sphinx.ext.extlinks",
    "sphinx_comments",
    "recommonmark",
    "nbsphinx",
    "sphinx.ext.mathjax",
    # "breathe",
    "sphinx_panels",
    "sphinx_last_updated_by_git",
    "sphinx_click.ext",
    # 'exhale'
    "sphinx.ext.napoleon",
    # 'suews_config_editor',  # Our custom extension
    # 'sphinx-jsonschema', # to genenrate docs based JSON Schema from SUEWSConfig
]

# email_automode = True

breathe_projects = {"SUEWS": "./doxygenoutput/xml"}
breathe_default_project = "SUEWS"

# sphinx_last_updated_by_git options
git_last_updated_metatags = True

# sphinx comments
# https://sphinx-comments.readthedocs.io/
comments_config = {
    "hypothesis": True,
    "utterances": {
        "repo": "UMEP-dev/SUEWS",
        "issue-term": "title",
        #   "optional": "config",
    },
}


# exhale_args = {
#     # These arguments are required
#     "containmentFolder": "./api",
#     "rootFileName": "library_root.rst",
#     "rootFileTitle": "API",
#     "doxygenStripFromPath": "..",
#     # Suggested optional arguments
#     "createTreeView": True,
#     # TIP: if using the sphinx-bootstrap-theme, you need
#     "treeViewIsBootstrap": True,
#     "exhaleExecutesDoxygen": True,
#     "exhaleUseDoxyfile": True,
#     # "exhaleDoxygenStdin":    '''INPUT = ../../../SUEWS-SourceCode\n
#     #                            GENERATE_HTML  = YES
#     #                            '''
# }

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
source_suffix = [".rst", ".md"]
# source_suffix = '.rst'

# fortran source code for `fortran_autodoc` and `fortran_domain`
fortran_src = [
    os.path.abspath("../fortran-src"),
]
fortran_ext = ["f90", "F90", "f95", "F95"]

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# The master toctree document.
master_doc = "index"
master_doc_latex = "index_latex"

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = "en"

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path .

exclude_patterns = [
    "_build",
    "**.ipynb_checkpoints",
    "build",
]
# tags.add('html')
# if tags.has('html'):
#     exclude_patterns = ['references.rst']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = "sphinx"

# default interpretation of `role` markups
default_role = "any"

# some text replacement defintions and version info
dev_banner = ""
if is_dev_version:
    dev_banner = f"""
.. warning::

   **Development Version**: This documentation was built from a development version
   ({git_version_string}, commit: {git_commit}). Features described here may be
   unstable or subject to change. For stable documentation, please visit the
   `latest release <https://suews.readthedocs.io/stable/>`_.

"""

rst_prolog = f"""
.. |git_version| replace:: {git_version_string}
.. |git_commit| replace:: {git_commit}

{dev_banner}
.. |km^-1| replace:: km\\ :sup:`-1`
.. |mm^-1| replace:: mm\\ :sup:`-1`
.. |m^-1| replace:: m\\ :sup:`-1`
.. |m^-2| replace:: m\\ :sup:`-2`
.. |m^-3| replace:: m\\ :sup:`-3`
.. |m^2| replace:: m\\ :sup:`2`
.. |m^3| replace:: m\\ :sup:`3`
.. |s^-1| replace:: s\\ :sup:`-1`
.. |kg^-1| replace:: kg\\ :sup:`-1`
.. |K^-1| replace:: K\\ :sup:`-1`
.. |J^-1| replace:: J\\ :sup:`-1`
.. |W^-1| replace:: W\\ :sup:`-1`
.. |h^-1| replace:: h\\ :sup:`-1`
.. |day^-1| replace:: day\\ :sup:`-1`
.. |cap^-1| replace:: cap\\ :sup:`-1`
.. |ha^-1| replace:: ha\\ :sup:`-1`
.. |QF| replace:: Q\\ :sub:`F`
.. |Qstar| replace:: Q\\ :sup:`*`
.. |d^-1| replace:: d\\ :sup:`-1`
.. |d^-2| replace:: d\\ :sup:`-2`
.. |)^-1| replace:: )\\ :sup:`-1`
.. |Recmd| replace:: **Recommended in this version.**
.. |EXP| replace:: **Experimental in this version.**
.. |NotRecmd| replace:: **Not recommended in this version.**
.. |NotAvail| replace:: **Not available in this version.**
.. |NotUsed| replace:: **Not used in this version.**

.. _Zenodo page: https://doi.org/10.5281/zenodo.5723970

.. only:: html

    .. tip::

      1. Need help? Please let us know in the `UMEP Community`_.
      2. Please report issues with the manual on the `GitHub Issues`_.
      3. Please cite SUEWS with proper information from our `Zenodo page`_.

.. _UMEP Community : https://github.com/UMEP-dev/UMEP/discussions/
.. _SUEWS download page: https://forms.office.com/r/4qGfYu8LaR

"""

# Version info will be added to HTML footer below

# -- Options for HTML output -------------------------------------------------
# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
# html_theme = "sphinx_rtd_theme"
html_theme = "sphinx_book_theme"

# Search configuration
html_search_language = "en"
html_search_options = {
    "type": "default",
    "includehidden": True,
}
# html_theme_path = ["_themes"]
html_context = {
    "repository_url": "https://github.com/{your-docs-url}",
    "display_github": True,  # Integrate GitHub
    "github_user": "UMEP-dev",  # Username
    "github_repo": "SUEWS",  # Repo name
    "github_version": "master",  # Version
    "conf_py_path": "/source/",  # Path in the checkout to the docs root
    "git_version": git_version_string,  # Add version info
    "git_commit": git_commit,  # Add commit hash
    "is_dev_version": is_dev_version,  # Add dev flag
}

# check every link in this project is working
nitpicky = True


# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
# today = ''
# Else, today_fmt is used as the format for a strftime call.
today_fmt = r"%Y-%m-%d"

html_last_updated_fmt = today_fmt

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
html_theme_options = dict(
    # analytics_id=''  this is configured in rtfd.io
    # canonical_url="",
    repository_url="https://github.com/UMEP-dev/SUEWS",
    repository_branch="master",
    path_to_docs="docs/source",
    use_edit_page_button=True,
    use_repository_button=True,
    use_issues_button=True,
    home_page_in_toc=False,
    extra_navbar="",
    navbar_footer_text="",
    logo_only=True,
    extra_footer=f"""<p>Version: {git_version_string} | 
    Commit: <a href="https://github.com/UMEP-dev/SUEWS/commit/{git_commit_full}">{git_commit_short}</a></p>""",
    # twitter_url="https://twitter.com/xarray_devs",
)

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
#
html_static_path = ["_static", "doxygenoutput"]
# html_context = {
#     'css_files': [
#         '_static/theme_overrides.css',  # override wide tables in RTD theme
#         ],
#      }

# html_extra_path = ['doxygenoutput']

# Copy React build output to _static
html_extra_path = ["../suews-config-ui/dist"]

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
# html_sidebars = {}
numfig = True
html_logo = "images/logo/SUEWS_LOGO-display.png"
# html_theme_options = {
# "logo_only": True,
#     "display_version": True,
# }


# -- Options for HTMLHelp output ---------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = "SUEWSdoc"


# -- Options for LaTeX output ------------------------------------------------
# this can be one of ['pdflatex', 'xelatex', 'lualatex', 'platex']
if platform.system() == "Darwin":
    latex_engine = "lualatex"
else:
    latex_engine = "pdflatex"

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',
    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',
    # Additional stuff for the LaTeX preamble.
    #
    "preamble": r"""
\usepackage[titles]{tocloft}
\usepackage{ragged2e}
\addto\captionsenglish{\renewcommand{\bibname}{References}}
\cftsetpnumwidth {1.25cm}\cftsetrmarg{1.5cm}
\setlength{\cftchapnumwidth}{0.75cm}
\setlength{\cftsecindent}{\cftchapnumwidth}
\setlength{\cftsecnumwidth}{1.25cm}
\newcolumntype{T}{L}
\setlength{\tymin}{40pt}
""",
    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

latex_show_pagerefs = False

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, "SUEWS.tex", doc_name, author, "manual"),
]
# latex_logo = 'assets/img/SUEWS_LOGO.png'

# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, "suews", "SUEWS Documentation", [author], 1),
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (
        master_doc,
        "SUEWS",
        "SUEWS Documentation",
        author,
        "SUEWS",
        "One line description of project.",
        "Miscellaneous",
    ),
]


# -- Options for Epub output -------------------------------------------------
# DISABLED: epub3 builder is incompatible with Python 3.13 (imghdr module removed)
# See: https://github.com/sphinx-doc/sphinx/issues/11780

# # Bibliographic Dublin Core info.
# epub_title = project
# epub_author = author
# epub_publisher = author
# epub_copyright = copyright

# # The unique identifier of the text. This can be a ISBN number
# # or the project homepage.
# #
# # epub_identifier = ''

# # A unique identification for the text.
# #
# # epub_uid = ''

# # A list of files that should not be packed into the epub file.
# epub_exclude_files = ["search.html"]


# -- Extension configuration -------------------------------------------------
# rinoh_documents = [('index',            # top-level file (index.rst)
#                     'target',           # output (target.pdf)
#                     'Document Title',   # document title
#                     'John A. Uthor')]   # document author


import urllib.parse


def source_read_handler(app, docname, source):
    if app.builder.format != "html":
        return
    src = source[0]
    # base location for `docname`
    if ('"metadata":' in src) and ('"nbformat":' in src):
        # consider this as an ipynb
        # and do nothing
        return

    # Add deprecation warning to table-based input documentation
    deprecation_warning = ""
    if docname.startswith("inputs/tables/") and not docname.endswith("index"):
        deprecation_warning = """
.. warning::

   **DEPRECATED**: This table-based input format is deprecated as of 2025.
   Please use the modern :ref:`YAML format <yaml_input>` instead.
   See our :doc:`transition guide </inputs/transition_guide>` for migration help.

"""

    # modify the issue link to provide page specific URL
    str_base = "docs/source"
    str_repo = html_context["github_repo"]

    # encode body query to be URL compliant
    str_body = f"""## Issue
<!-- Please describe the issue below this line -->

## Links
[source doc](https://github.com/UMEP-dev/{str_repo}/blob/master/{str_base}/{docname}.rst)
[RTD page](https://suews.readthedocs.org/en/latest/{docname}.html)
"""
    str_query_body = urllib.parse.urlencode({"body": str_body})
    str_url = f"https://github.com/UMEP-dev/SUEWS/issues/new?assignees=&labels=docs&template=docs-issue-report.md&{str_query_body}&title=[Docs]{docname}"
    str_GHPage = f"""
.. _GitHub Issues: {str_url}
"""
    rendered = "\n".join([str_GHPage, deprecation_warning, src])
    source[0] = rendered.rstrip("\n")


# Fix for scrolling tables in the RTD-theme
# https://rackerlabs.github.io/docs-rackspace/tools/rtd-tables.html
def setup(app):
    app.connect("source-read", source_read_handler)
    app.add_css_file("theme_overrides.css")
    # Fix equation formatting in the RTD-theme
    app.add_css_file("fix-eq.css")


# Example configuration for intersphinx: refer to the Python standard library.
intersphinx_mapping = {
    "python": ("https://docs.python.org/3/", None),
    "pandas": ("http://pandas.pydata.org/pandas-docs/stable/", None),
    "xarray": ("http://xarray.pydata.org/en/stable/", None),
    "numpy": ("https://docs.scipy.org/doc/numpy/", None),
    "supy": ("https://supy.readthedocs.io/en/latest/", None),
}


# -- sphinxcontrib.bibtex configuration -------------------------------------------------
bibtex_bibfiles = [
    "assets/refs/refs-SUEWS.bib",
    "assets/refs/refs-others.bib",
]
bibtex_default_style = "refs"
bibtex_reference_style = "author_year_round"


# https://sphinxcontrib-bibtex.readthedocs.io/en/latest/usage.html#custom-inline-citation-references
import dataclasses

import sphinxcontrib.bibtex.plugin
from sphinxcontrib.bibtex.style.referencing import BracketStyle
from sphinxcontrib.bibtex.style.referencing.author_year import AuthorYearReferenceStyle

my_bracket_style = BracketStyle(
    left="(",
    right=")",
)


# @dataclasses.dataclass
class MyReferenceStyle(AuthorYearReferenceStyle):
    bracket_parenthetical: BracketStyle = my_bracket_style
    bracket_textual: BracketStyle = my_bracket_style
    bracket_author: BracketStyle = my_bracket_style
    bracket_label: BracketStyle = my_bracket_style
    bracket_year: BracketStyle = my_bracket_style


sphinxcontrib.bibtex.plugin.register_plugin(
    "sphinxcontrib.bibtex.style.referencing", "author_year_round", MyReferenceStyle
)

import unicodedata

###############################################################################################
# ref: https://titanwolf.org/Network/Articles/Article?AID=1463c485-6603-4a3c-ac34-d68c95e67f0a
from collections import Counter

from pybtex.style.labels import BaseLabelStyle


def is_japanese(string):
    for c in string:
        name = unicodedata.name(c)
        if "CJK UNIFIED" in name or "HIRAGANA" in name or "KATAKANA" in name:
            return True
    return False


class AuthorYearLabelStyle(BaseLabelStyle):
    def format_labels(self, sorted_entries):
        labels = [self.format_label(entry) for entry in sorted_entries]

        # Key to the same entry to the Unique
        counter = Counter(labels)
        counted = Counter()

        for label in labels:
            if counter[label] == 1:
                yield label
            else:
                yield label + chr(ord("a") + counted[label])
                counted.update([label])

    def format_label(self, entry):
        # Author also no year entry
        if not "author" in entry.persons and not "year" in entry.fields:
            return entry.key

        # Label is first author of Last Name Tasu year
        if "author" in entry.persons:
            num_author = len(entry.persons["author"])
            author_first = entry.persons["author"][0]
            if num_author > 2:
                if is_japanese(
                    "".join(author_first.first_names + author_first.last_names)
                ):
                    author = "".join(author_first.first_names)
                else:
                    author = "".join(author_first.last_names) + " et al."
            elif num_author == 2:
                author_second = entry.persons["author"][1]
                # print(author_first.last_names)
                if is_japanese(
                    "".join(author_first.first_names + author_first.last_names)
                ):
                    author = " and ".join(
                        [" ".join(author_first.first_names)] + author_second.first_names
                    )
                else:
                    author = " and ".join(
                        [" ".join(author_first.last_names)] + author_second.last_names
                    )
            else:
                if is_japanese(
                    "".join(author_first.first_names + author_first.last_names)
                ):
                    author = "".join(author_first.first_names)
                else:
                    author = "".join(author_first.last_names)

        else:
            author = ""

        if "year" in entry.fields:
            yeartail = entry.fields["year"][-2:]
            year = entry.fields["year"]
        else:
            yeartail = ""
            year = ""

        # return "%s %s" % (author, yeartail)
        return f"{author} {year}"


class JsaiStyle(UnsrtStyle):
    default_label_style = "author_year_label"


register_plugin("pybtex.style.labels", "author_year_label", AuthorYearLabelStyle)
register_plugin("pybtex.style.formatting", "jsai", JsaiStyle)
###############################################################################################


from pybtex.plugin import register_plugin

# https://github.com/mcmtroffaes/sphinxcontrib-bibtex/blob/develop/test/roots/test-bibliography_style_label_2/conf.py
from pybtex.style.formatting.unsrt import Style as UnsrtStyle
from pybtex.style.labels.alpha import LabelStyle as AlphaLabelStyle


class LabelStyle_APA(AlphaLabelStyle):
    def format_label(self, entry):
        return "APA"


class ApaStyle(UnsrtStyle):
    default_label_style = "apa"


register_plugin("pybtex.style.labels", "apa", LabelStyle_APA)
register_plugin("pybtex.style.formatting", "apastyle", ApaStyle)


from pybtex.plugin import register_plugin
from pybtex.style.formatting.unsrt import Style as UnsrtStyle
from pybtex.style.labels import BaseLabelStyle


# a simple label style which uses the bibtex keys for labels
class LabelStyle_key(BaseLabelStyle):
    def format_labels(self, sorted_entries):
        for entry in sorted_entries:
            yield entry.key


class LabelStyle_key(UnsrtStyle):
    default_label_style = LabelStyle_key


register_plugin("pybtex.style.formatting", "style_key", LabelStyle_key)
register_plugin("pybtex.style.labels", "style_key", LabelStyle_key)


# Custom bibliography stuff for sphinxcontrib.bibtex
class MySort_year_author_title(Sorter):
    def sort(self, entries):
        entry_dict = dict((self.sorting_key(entry), entry) for entry in entries)

        sorted_keys = sorted(entry_dict, reverse=True)
        sorted_entries = [entry_dict[key] for key in sorted_keys]
        return sorted_entries

    def sorting_key(self, entry):
        if entry.type in ("book", "inbook"):
            author_key = self.author_editor_key(entry)
        elif "author" in entry.persons:
            author_key = self.persons_key(entry.persons["author"])
        else:
            author_key = ""

        name_mon = entry.fields.get("month", "")
        try:
            num_mon = strptime(name_mon, "%b").tm_mon
        except:
            try:
                num_mon = strptime(name_mon, "%B").tm_mon
            except:
                print(entry)
                num_mon = 1

        return (
            entry.fields.get("year", ""),
            str(num_mon),
            # entry.fields.get("month", ""),
            author_key,
            entry.fields.get("title", ""),
        )


class MyStyle_author_year(UnsrtStyle):
    default_sorting_style = "author_year_title"
    # default_sorting_style = "year_author_title"
    default_name_style = "lastfirst"
    # default_label_style = "alpha"
    # default_label_style = 'style_key'
    default_label_style = "author_year_label"

    def format_web_refs(self, e):
        # based on urlbst output.web.refs
        return sentence[optional[self.format_doi(e)],]

    def get_book_template(self, e):
        template = toplevel[
            self.format_author_or_editor(e),
            self.format_btitle(e, "title"),
            self.format_volume_and_series(e),
            sentence[field("publisher"), self.format_edition(e), date],
            optional[sentence[self.format_isbn(e)]],
            self.format_web_refs(e),
            # tag('strong')[optional_field('note')],
        ]

        return template


register_plugin("pybtex.style.formatting", "refs", MyStyle_author_year)


class MyStyle_year_author(UnsrtStyle):
    default_sorting_style = "year_author_title"
    default_name_style = "lastfirst"
    default_label_style = "alpha"

    def format_web_refs(self, e):
        # based on urlbst output.web.refs
        return sentence[optional[self.format_doi(e)],]

    def get_book_template(self, e):
        template = toplevel[
            self.format_author_or_editor(e),
            self.format_btitle(e, "title"),
            self.format_volume_and_series(e),
            sentence[field("publisher"), self.format_edition(e), date],
            optional[sentence[self.format_isbn(e)]],
            self.format_web_refs(e),
            # tag('strong')[optional_field('note')],
        ]

        return template


register_plugin("pybtex.style.formatting", "refs_recent", MyStyle_year_author)


# reading list style
class MyStyle_author_year_note(UnsrtStyle):
    default_sorting_style = "author_year_title"
    default_label_style = "number"
    default_name_style = "lastfirst"

    def format_web_refs(self, e):
        # based on urlbst output.web.refs
        return sentence[optional[self.format_doi(e)],]

    def get_book_template(self, e):
        template = toplevel[
            self.format_author_or_editor(e),
            self.format_btitle(e, "title"),
            self.format_volume_and_series(e),
            sentence[field("publisher"), self.format_edition(e), date],
            optional[sentence[self.format_isbn(e)]],
            optional[sentence[self.format_web_refs(e)]],
            tag("strong")[optional_field("note")],
        ]

        return template

    # format_online = format_article
    # format_book = format_article


# Add the path to custom extensions
register_plugin("pybtex.style.formatting", "rl", MyStyle_author_year_note)
register_plugin("pybtex.style.sorting", "year_author_title", MySort_year_author_title)


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".

# These paths are either relative to html_static_path or fully qualified paths (eg. https://...)
html_css_files = [
    "suews-config-ui/main.css",
]

html_js_files = [
    "suews-config-ui/main.js",
]
