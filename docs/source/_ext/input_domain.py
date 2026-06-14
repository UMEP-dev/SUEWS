"""
Sphinx extension for SUEWS input configuration options.

This creates a separate namespace for input configuration options,
preventing collisions with output variable directives in the documentation.

See GitHub issue #1031 for context.
"""

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, Index, ObjType
from sphinx.roles import XRefRole
from sphinx.util.nodes import make_refnode
from typing import Any


class InputOption(ObjectDescription):
    """Directive for input configuration options."""

    has_content = True
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False

    def handle_signature(self, sig, signode):
        """Handle the signature (option name)."""
        signode += addnodes.desc_name(sig, sig)
        return sig

    def add_target_and_index(self, name, sig, signode):
        """Add target and index entries."""
        targetname = f"input-option-{sig}"
        if targetname not in self.state.document.ids:
            signode["ids"].append(targetname)
            self.state.document.note_explicit_target(signode)

            # Add to domain data
            domain = self.env.get_domain("input")
            domain.add_option(sig, self.env.docname)

            # Add index entry
            indextext = f"{sig} (input configuration)"
            inode = addnodes.index(
                entries=[("single", indextext, targetname, "", None)]
            )
            return [inode, signode]
        return []


class InputOptionIndex(Index):
    """Index for input options."""

    name = "option"
    localname = "Input Option Index"
    shortname = "Input options"

    def generate(self, docnames=None):
        """Generate the index."""
        content = {}
        options = self.domain.data["options"]

        for name, docname in options.items():
            letter = name[0].upper()
            entries = content.setdefault(letter, [])
            entries.append((
                name,  # name
                0,  # subtype
                docname,  # docname
                f"input-option-{name}",  # target
                "",  # extra
                "",  # qualifier
                "",  # description
            ))

        # Sort entries
        for letter in content:
            content[letter].sort(key=lambda x: x[0].lower())

        # Return content and collapse flag (True = collapse same sub-entries)
        return sorted(content.items()), True


class InputDomain(Domain):
    """Domain for input configuration."""

    name = "input"
    label = "Input Configuration"

    object_types = {
        "option": ObjType("option", "option"),
    }

    directives = {
        "option": InputOption,
    }

    roles = {
        "option": XRefRole(),
    }

    indices = [InputOptionIndex]

    initial_data: dict[str, Any] = {
        "options": {},  # name -> docname
    }

    def add_option(self, name, docname):
        """Add an option to the domain."""
        self.data["options"][name] = docname

    def resolve_xref(self, env, fromdocname, builder, typ, target, node, contnode):
        """Resolve cross-references."""
        if typ == "option":
            docname = self.data["options"].get(target)
            if docname:
                targetid = f"input-option-{target}"
                return make_refnode(
                    builder, fromdocname, docname, targetid, contnode, target
                )
        return None

    def resolve_any_xref(self, env, fromdocname, builder, target, node, contnode):
        """Resolve any cross-references.

        Returns empty list to prevent conflicts with other directives.
        Input options should be referenced explicitly using :input:option:`name`
        rather than bare backticks.
        """
        return []

    def clear_doc(self, docname):
        """Clear all options from a document."""
        for name, doc in list(self.data["options"].items()):
            if doc == docname:
                del self.data["options"][name]

    def merge_domaindata(self, docnames, otherdata):
        """Merge data from parallel builds."""
        for name, docname in otherdata["options"].items():
            if docname in docnames:
                self.data["options"][name] = docname


def setup(app):
    """Setup the extension."""
    app.add_domain(InputDomain)

    return {
        "version": "1.0",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
