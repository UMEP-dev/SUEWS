"""
Sphinx extension for YAML configuration options.

This creates a separate namespace for YAML configuration options,
preventing collisions with other option directives in the documentation.
"""

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, Index
from sphinx.roles import XRefRole
from sphinx.util.nodes import make_refnode
from sphinx.util.docutils import SphinxDirective
from docutils import nodes
from docutils.parsers.rst import directives
from typing import Any, Dict, List, Tuple


class YAMLOption(ObjectDescription):
    """Directive for YAML configuration options."""
    
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
        targetname = f"yaml-option-{sig}"
        if targetname not in self.state.document.ids:
            signode['ids'].append(targetname)
            self.state.document.note_explicit_target(signode)
            
            # Add to domain data
            domain = self.env.get_domain('yaml')
            domain.add_option(sig, self.env.docname)
            
            # Add index entry
            indextext = f"{sig} (YAML configuration)"
            inode = addnodes.index(entries=[('single', indextext, targetname, '', None)])
            return [inode, signode]
        return []


class YAMLOptionIndex(Index):
    """Index for YAML options."""
    
    name = 'option'
    localname = 'YAML Option Index'
    shortname = 'YAML options'
    
    def generate(self, docnames=None):
        """Generate the index."""
        content = {}
        options = self.domain.data['options']
        
        for name, docname in options.items():
            letter = name[0].upper()
            entries = content.setdefault(letter, [])
            entries.append((
                name,  # name
                0,     # subtype
                docname,  # docname
                f'yaml-option-{name}',  # target
                '',    # extra
                '',    # qualifier
                ''     # description
            ))
            
        # Sort entries
        for letter in content:
            content[letter].sort(key=lambda x: x[0].lower())
            
        return sorted(content.items())


class YAMLDomain(Domain):
    """Domain for YAML configuration."""
    
    name = 'yaml'
    label = 'YAML Configuration'
    
    object_types = {
        'option': {'name': 'option', 'label': 'YAML option'},
    }
    
    directives = {
        'option': YAMLOption,
    }
    
    roles = {
        'option': XRefRole(),
    }
    
    indices = [YAMLOptionIndex]
    
    initial_data = {
        'options': {},  # name -> docname
    }
    
    def add_option(self, name, docname):
        """Add an option to the domain."""
        self.data['options'][name] = docname
    
    def resolve_xref(self, env, fromdocname, builder, typ, target, node, contnode):
        """Resolve cross-references."""
        if typ == 'option':
            docname = self.data['options'].get(target)
            if docname:
                targetid = f'yaml-option-{target}'
                return make_refnode(
                    builder,
                    fromdocname,
                    docname,
                    targetid,
                    contnode,
                    target
                )
        return None
    
    def resolve_any_xref(self, env, fromdocname, builder, target, node, contnode):
        """Resolve any cross-references."""
        results = []
        if target in self.data['options']:
            docname = self.data['options'][target]
            targetid = f'yaml-option-{target}'
            results.append((
                'yaml:option',
                make_refnode(builder, fromdocname, docname, targetid, contnode, target)
            ))
        return results
    
    def clear_doc(self, docname):
        """Clear all options from a document."""
        for name, doc in list(self.data['options'].items()):
            if doc == docname:
                del self.data['options'][name]
    
    def merge_domaindata(self, docnames, otherdata):
        """Merge data from parallel builds."""
        for name, docname in otherdata['options'].items():
            if docname in docnames:
                self.data['options'][name] = docname


def setup(app):
    """Setup the extension."""
    app.add_domain(YAMLDomain)
    
    return {
        'version': '1.0',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }