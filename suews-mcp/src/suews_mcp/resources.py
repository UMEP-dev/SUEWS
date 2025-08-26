"""
Resource management for SUEWS MCP server.

This module provides functionality to discover, load, and manage
templates, examples, and other resources for SUEWS workflows.
"""

import json
import logging
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)


class ResourceManager:
    """
    Manages SUEWS MCP resources including templates, examples, and documentation.
    """
    
    def __init__(self, templates_dir: Optional[Path] = None):
        """
        Initialize resource manager.
        
        Parameters
        ----------
        templates_dir : Path, optional
            Path to templates directory. If None, uses default location.
        """
        if templates_dir is None:
            # Default to templates directory in suews-mcp
            self.templates_dir = Path(__file__).parent.parent.parent / "templates"
        else:
            self.templates_dir = Path(templates_dir)
            
        self.catalog_path = self.templates_dir / "resource_catalog.json"
        self._catalog = None
        
    def load_catalog(self) -> Dict[str, Any]:
        """
        Load resource catalog from JSON file.
        
        Returns
        -------
        dict
            Resource catalog data
        """
        if self._catalog is None:
            try:
                with open(self.catalog_path, 'r') as f:
                    self._catalog = json.load(f)
                logger.info(f"Loaded resource catalog from {self.catalog_path}")
            except FileNotFoundError:
                logger.warning(f"Resource catalog not found at {self.catalog_path}")
                self._catalog = {"resources": {}}
            except json.JSONDecodeError as e:
                logger.error(f"Error parsing resource catalog: {e}")
                self._catalog = {"resources": {}}
                
        return self._catalog
    
    def list_resources(self, resource_type: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        List available resources, optionally filtered by type.
        
        Parameters
        ----------
        resource_type : str, optional
            Filter by resource type (config_templates, examples, etc.)
            
        Returns
        -------
        list
            List of resource information dictionaries
        """
        catalog = self.load_catalog()
        resources = catalog.get("resources", {})
        
        if resource_type and resource_type in resources:
            return resources[resource_type]
        elif resource_type:
            logger.warning(f"Unknown resource type: {resource_type}")
            return []
        else:
            # Return all resources as flat list
            all_resources = []
            for res_type, res_list in resources.items():
                for resource in res_list:
                    resource["resource_type"] = res_type
                    all_resources.append(resource)
            return all_resources
    
    def find_resources(self, 
                      tags: Optional[List[str]] = None,
                      difficulty: Optional[str] = None,
                      domain: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Find resources matching specified criteria.
        
        Parameters
        ----------
        tags : list of str, optional
            Tags to match
        difficulty : str, optional
            Difficulty level (beginner, intermediate, advanced)
        domain : str, optional
            Domain/application area
            
        Returns
        -------
        list
            Matching resources
        """
        all_resources = self.list_resources()
        matching = []
        
        for resource in all_resources:
            # Check tags
            if tags:
                resource_tags = resource.get("tags", [])
                if not any(tag in resource_tags for tag in tags):
                    continue
                    
            # Check difficulty
            if difficulty:
                resource_difficulty = resource.get("difficulty", resource.get("complexity", ""))
                if resource_difficulty != difficulty:
                    continue
                    
            # Check domain
            if domain:
                resource_domain = resource.get("domain", "")
                if resource_domain != domain:
                    continue
                    
            matching.append(resource)
            
        return matching
    
    def get_resource(self, resource_path: str) -> Optional[str]:
        """
        Load resource content from file.
        
        Parameters
        ----------
        resource_path : str
            Path to resource file (relative to templates directory)
            
        Returns
        -------
        str or None
            Resource content, or None if not found
        """
        full_path = self.templates_dir / resource_path
        
        try:
            with open(full_path, 'r', encoding='utf-8') as f:
                content = f.read()
            logger.info(f"Loaded resource: {resource_path}")
            return content
        except FileNotFoundError:
            logger.warning(f"Resource not found: {full_path}")
            return None
        except Exception as e:
            logger.error(f"Error loading resource {resource_path}: {e}")
            return None
    
    def get_resource_info(self, resource_id: str) -> Optional[Dict[str, Any]]:
        """
        Get detailed information about a specific resource.
        
        Parameters
        ----------
        resource_id : str
            Resource identifier
            
        Returns
        -------
        dict or None
            Resource information, or None if not found
        """
        all_resources = self.list_resources()
        
        for resource in all_resources:
            if resource.get("id") == resource_id:
                return resource
                
        logger.warning(f"Resource not found: {resource_id}")
        return None
    
    def get_usage_pattern(self, pattern_name: str) -> List[str]:
        """
        Get recommended resource usage pattern.
        
        Parameters
        ----------
        pattern_name : str
            Name of usage pattern (e.g., 'beginner_workflow')
            
        Returns
        -------
        list of str
            List of resource paths in recommended order
        """
        catalog = self.load_catalog()
        patterns = catalog.get("usage_patterns", {})
        
        return patterns.get(pattern_name, [])
    
    def search_by_tags(self, search_category: str, tag: str) -> List[str]:
        """
        Search resources using catalog tag system.
        
        Parameters
        ----------
        search_category : str
            Category to search in (e.g., 'by_difficulty', 'by_domain')
        tag : str
            Tag within category
            
        Returns
        -------
        list of str
            Resource IDs matching the tag
        """
        catalog = self.load_catalog()
        search_tags = catalog.get("search_tags", {})
        
        if search_category not in search_tags:
            return []
            
        category_tags = search_tags[search_category]
        return category_tags.get(tag, [])
    
    def get_documentation_links(self) -> Dict[str, str]:
        """
        Get links to external documentation.
        
        Returns
        -------
        dict
            Dictionary of documentation name -> URL
        """
        catalog = self.load_catalog()
        return catalog.get("documentation_links", {})
    
    def validate_resource_paths(self) -> Dict[str, Any]:
        """
        Validate that all resources in catalog exist on filesystem.
        
        Returns
        -------
        dict
            Validation results with missing/found resources
        """
        catalog = self.load_catalog()
        resources = catalog.get("resources", {})
        
        results = {
            "found": [],
            "missing": [],
            "total_checked": 0
        }
        
        for resource_type, resource_list in resources.items():
            for resource in resource_list:
                path = resource.get("path", "")
                if not path:
                    continue
                    
                results["total_checked"] += 1
                full_path = self.templates_dir / path
                
                if full_path.exists():
                    results["found"].append(path)
                else:
                    results["missing"].append(path)
                    
        return results
    
    def create_resource_index(self) -> Dict[str, Any]:
        """
        Create searchable index of all resources.
        
        Returns
        -------
        dict
            Resource index with metadata for quick lookup
        """
        catalog = self.load_catalog()
        resources = catalog.get("resources", {})
        
        index = {
            "by_id": {},
            "by_type": {},
            "by_tag": {},
            "by_difficulty": {},
            "by_domain": {}
        }
        
        for resource_type, resource_list in resources.items():
            index["by_type"][resource_type] = []
            
            for resource in resource_list:
                resource_id = resource.get("id")
                if resource_id:
                    index["by_id"][resource_id] = resource
                    index["by_type"][resource_type].append(resource_id)
                    
                # Index by tags
                for tag in resource.get("tags", []):
                    if tag not in index["by_tag"]:
                        index["by_tag"][tag] = []
                    index["by_tag"][tag].append(resource_id)
                    
                # Index by difficulty
                difficulty = resource.get("difficulty", resource.get("complexity"))
                if difficulty:
                    if difficulty not in index["by_difficulty"]:
                        index["by_difficulty"][difficulty] = []
                    index["by_difficulty"][difficulty].append(resource_id)
                    
                # Index by domain
                domain = resource.get("domain")
                if domain:
                    if domain not in index["by_domain"]:
                        index["by_domain"][domain] = []
                    index["by_domain"][domain].append(resource_id)
                    
        return index


# Convenience functions for MCP server integration

def get_resource_manager() -> ResourceManager:
    """Get default resource manager instance."""
    return ResourceManager()


def list_all_resources() -> List[Dict[str, Any]]:
    """List all available resources."""
    manager = get_resource_manager()
    return manager.list_resources()


def find_resources_for_use_case(use_case: str) -> List[str]:
    """
    Find resources appropriate for a specific use case.
    
    Parameters
    ----------
    use_case : str
        Use case description (matched against tags and descriptions)
        
    Returns
    -------
    list of str
        Recommended resource IDs
    """
    manager = get_resource_manager()
    
    # Simple keyword matching for now - could be enhanced with NLP
    use_case_lower = use_case.lower()
    
    # Define keyword mappings
    keyword_mappings = {
        "park": ["park", "urban_park_study"],
        "green": ["park", "urban_park_study"], 
        "building": ["commercial", "building_energy"],
        "energy": ["building_energy", "commercial"],
        "residential": ["residential", "basic_simulation"],
        "beginner": manager.search_by_tags("by_difficulty", "beginner"),
        "advanced": manager.search_by_tags("by_difficulty", "advanced"),
        "validation": ["validation_workflow"],
        "sensitivity": ["sensitivity_analysis"]
    }
    
    recommended = set()
    
    for keyword, resource_ids in keyword_mappings.items():
        if keyword in use_case_lower:
            recommended.update(resource_ids)
            
    return list(recommended)


def get_quick_start_resources() -> List[str]:
    """Get resources for quick start workflow."""
    manager = get_resource_manager()
    return manager.get_usage_pattern("beginner_workflow")