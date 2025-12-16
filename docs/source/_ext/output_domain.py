"""
Sphinx extension for SUEWS output variables.

This creates a separate namespace for output variables,
preventing collisions with YAML configuration options in the documentation.

See GitHub issue #1031 for context.
"""

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, Index, ObjType
from sphinx.roles import XRefRole
from sphinx.util.nodes import make_refnode
from typing import Any


class OutputVariable(ObjectDescription):
    """Directive for SUEWS output variables."""

    has_content = True
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False

    def handle_signature(self, sig, signode):
        """Handle the signature (variable name)."""
        signode += addnodes.desc_name(sig, sig)
        return sig

    def add_target_and_index(self, name, sig, signode):
        """Add target and index entries."""
        targetname = f"output-variable-{sig}"
        if targetname not in self.state.document.ids:
            signode["ids"].append(targetname)
            self.state.document.note_explicit_target(signode)

            # Add to domain data
            domain = self.env.get_domain("output")
            domain.add_variable(sig, self.env.docname)

            # Add index entry
            indextext = f"{sig} (output variable)"
            inode = addnodes.index(
                entries=[("single", indextext, targetname, "", None)]
            )
            return [inode, signode]
        return []


class OutputVariableIndex(Index):
    """Index for output variables."""

    name = "variable"
    localname = "Output Variable Index"
    shortname = "Output variables"

    def generate(self, docnames=None):
        """Generate the index."""
        content = {}
        variables = self.domain.data["variables"]

        for name, docname in variables.items():
            letter = name[0].upper()
            entries = content.setdefault(letter, [])
            entries.append((
                name,  # name
                0,  # subtype
                docname,  # docname
                f"output-variable-{name}",  # target
                "",  # extra
                "",  # qualifier
                "",  # description
            ))

        # Sort entries
        for letter in content:
            content[letter].sort(key=lambda x: x[0].lower())

        # Return content and collapse flag (True = collapse same sub-entries)
        return sorted(content.items()), True


class OutputDomain(Domain):
    """Domain for SUEWS output variables."""

    name = "output"
    label = "Output Variables"

    object_types = {
        "variable": ObjType("variable", "variable"),
    }

    directives = {
        "variable": OutputVariable,
    }

    roles = {
        "variable": XRefRole(),
    }

    indices = [OutputVariableIndex]

    initial_data: dict[str, Any] = {
        "variables": {},  # name -> docname
    }

    def add_variable(self, name, docname):
        """Add a variable to the domain."""
        self.data["variables"][name] = docname

    def resolve_xref(self, env, fromdocname, builder, typ, target, node, contnode):
        """Resolve cross-references."""
        if typ == "variable":
            docname = self.data["variables"].get(target)
            if docname:
                targetid = f"output-variable-{target}"
                return make_refnode(
                    builder, fromdocname, docname, targetid, contnode, target
                )
        return None

    def resolve_any_xref(self, env, fromdocname, builder, target, node, contnode):
        """Resolve any cross-references.

        Returns empty list to prevent conflicts with other directives.
        Output variables should be referenced explicitly using :output:variable:`name`
        rather than bare backticks.
        """
        return []

    def clear_doc(self, docname):
        """Clear all variables from a document."""
        for name, doc in list(self.data["variables"].items()):
            if doc == docname:
                del self.data["variables"][name]

    def merge_domaindata(self, docnames, otherdata):
        """Merge data from parallel builds."""
        for name, docname in otherdata["variables"].items():
            if docname in docnames:
                self.data["variables"][name] = docname


def setup(app):
    """Setup the extension."""
    app.add_domain(OutputDomain)

    return {
        "version": "1.0",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
