from docutils import nodes
from docutils.parsers.rst import Directive
import os


class SUEWSConfigEditor(Directive):
    has_content = True
    required_arguments = 0
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}

    def run(self):
        # Create a div that will contain our React app
        container = nodes.container(classes=["suews-config-editor"])

        # Add the necessary scripts and styles
        scripts = [
            nodes.raw(
                "",
                '<script type="module" crossorigin src="_static/suews-config-ui/main.js"></script>',
                format="html",
            ),
            nodes.raw(
                "",
                '<link rel="stylesheet" href="_static/suews-config-ui/main.css">',
                format="html",
            ),
        ]

        container.extend(scripts)
        return [container]


def setup(app):
    app.add_directive("suews-config-editor", SUEWSConfigEditor)

    # Add static path for the React build output
    static_path = os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", "_static")
    )
    app.config.html_static_path.append(static_path)

    return {
        "version": "0.1",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
