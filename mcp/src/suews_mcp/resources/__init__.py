"""Read-only MCP resources under the ``suews://`` URI scheme."""

from .docs import read_doc
from .examples import read_example_resource
from .knowledge import (
    read_knowledge_manifest_resource,
    read_knowledge_query_resource,
)
from .runs import read_run_resource
from .schema import read_schema_resource

__all__ = [
    "read_doc",
    "read_example_resource",
    "read_knowledge_manifest_resource",
    "read_knowledge_query_resource",
    "read_run_resource",
    "read_schema_resource",
]
