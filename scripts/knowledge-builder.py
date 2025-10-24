#!/usr/bin/env python3
"""
Hugo Knowledge Base Builder
Generates search index, backlinks, and knowledge graph data for Hugo static site
"""

import os
import json
import re
from pathlib import Path
from datetime import datetime
from collections import defaultdict
import frontmatter

class HugoKnowledgeBuilder:
    def __init__(self, site_dir):
        self.site_dir = Path(site_dir)
        self.content_dir = self.site_dir / "content"
        self.public_dir = self.site_dir / "public"
        self.pages = []
        self.links = defaultdict(list)
        self.backlinks = defaultdict(list)
        
    def build(self):
        """Main build process"""
        print("Building Hugo knowledge base...")
        
        # Scan all content files
        self.scan_content()
        
        # Generate backlinks
        self.generate_backlinks()
        
        # Create search index
        self.create_search_index()
        
        # Generate knowledge graph data
        self.create_knowledge_graph()
        
        # Update page frontmatter with backlinks
        self.update_page_frontmatter()
        
        print("Knowledge base build complete!")
        
    def scan_content(self):
        """Scan all content files and extract metadata"""
        print("Scanning content files...")
        
        for file_path in self.content_dir.rglob("*.md"):
            if file_path.name.startswith('.'):
                continue
                
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    post = frontmatter.load(f)
                    
                # Extract page data
                page_data = {
                    'file': str(file_path.relative_to(self.site_dir)),
                    'title': post.get('title', ''),
                    'content': post.content,
                    'tags': post.get('tags', []),
                    'categories': post.get('categories', []),
                    'concepts': post.get('concepts', []),
                    'date': post.get('date', ''),
                    'org_roam_id': post.get('org_roam_id', ''),
                    'slug': post.get('slug', ''),
                    'url': self.get_url_from_file(file_path)
                }
                
                # Extract internal links
                internal_links = self.extract_internal_links(post.content)
                page_data['links'] = internal_links
                
                self.pages.append(page_data)
                
                # Build link index
                for link in internal_links:
                    self.links[page_data['url']].append(link)
                    
            except Exception as e:
                print(f"Error processing {file_path}: {e}")
                
    def extract_internal_links(self, content):
        """Extract internal links from markdown content"""
        links = []
        
        # Match Hugo internal links: [text]({{< ref "path" >}})
        hugo_ref_pattern = r'\[([^\]]+)\]\(\{\{<\s*ref\s+"([^"]+)"\s*>\}\}\)'
        for match in re.finditer(hugo_ref_pattern, content):
            links.append({
                'text': match.group(1),
                'target': match.group(2),
                'type': 'hugo_ref'
            })
            
        # Match org-roam style links: [[id:...]]
        org_roam_pattern = r'\[\[id:([^\]]+)\]\]'
        for match in re.finditer(org_roam_pattern, content):
            links.append({
                'text': match.group(1),
                'target': match.group(1),
                'type': 'org_roam'
            })
            
        # Match markdown links to other pages
        markdown_pattern = r'\[([^\]]+)\]\(([^)]+)\)'
        for match in re.finditer(markdown_pattern, content):
            target = match.group(2)
            if not target.startswith('http') and not target.startswith('#'):
                links.append({
                    'text': match.group(1),
                    'target': target,
                    'type': 'markdown'
                })
                
        return links
        
    def generate_backlinks(self):
        """Generate backlinks for each page"""
        print("Generating backlinks...")
        
        for page in self.pages:
            page_url = page['url']
            backlinks = []
            
            # Find pages that link to this page
            for other_page in self.pages:
                if other_page['url'] == page_url:
                    continue
                    
                for link in other_page['links']:
                    if self.links_to_page(link, page):
                        backlinks.append({
                            'title': other_page['title'],
                            'url': other_page['url'],
                            'text': link['text']
                        })
                        
            self.backlinks[page_url] = backlinks
            
    def links_to_page(self, link, target_page):
        """Check if a link points to the target page"""
        target_url = target_page['url']
        target_id = target_page.get('org_roam_id', '')
        
        if link['type'] == 'org_roam' and link['target'] == target_id:
            return True
        elif link['type'] == 'hugo_ref' and link['target'] in target_url:
            return True
        elif link['type'] == 'markdown' and link['target'] in target_url:
            return True
            
        return False
        
    def create_search_index(self):
        """Create search index JSON file"""
        print("Creating search index...")
        
        search_data = {
            'pages': [],
            'indexed_at': datetime.now().isoformat()
        }
        
        for page in self.pages:
            # Clean content for search
            content = re.sub(r'[#*`]', '', page['content'])
            content = re.sub(r'\s+', ' ', content)
            
            search_data['pages'].append({
                'title': page['title'],
                'content': content[:5000],  # Limit content length
                'url': page['url'],
                'tags': page['tags'],
                'categories': page['categories'],
                'concepts': page['concepts'],
                'date': str(page['date']) if page['date'] else None,
                'slug': page['slug']
            })
            
        # Write search index
        search_file = self.public_dir / "search-index.json"
        search_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(search_file, 'w', encoding='utf-8') as f:
            json.dump(search_data, f, indent=2, ensure_ascii=False)
            
    def create_knowledge_graph(self):
        """Create knowledge graph data"""
        print("Creating knowledge graph...")
        
        nodes = []
        links = []
        
        # Create nodes for each page
        for page in self.pages:
            node = {
                'id': page.get('org_roam_id', page['url']),
                'title': page['title'],
                'url': page['url'],
                'tags': page['tags'],
                'connections': len(page['links']) + len(self.backlinks[page['url']])
            }
            nodes.append(node)
            
        # Create links between pages
        for page in self.pages:
            page_id = page.get('org_roam_id', page['url'])
            
            for link in page['links']:
                # Find target page
                target_page = self.find_page_by_link(link)
                if target_page:
                    target_id = target_page.get('org_roam_id', target_page['url'])
                    links.append({
                        'source': page_id,
                        'target': target_id,
                        'value': 1,
                        'type': link['type']
                    })
                    
        graph_data = {
            'nodes': nodes,
            'links': links,
            'generated_at': datetime.now().isoformat()
        }
        
        # Write graph data
        graph_file = self.public_dir / "api" / "graph-data.json"
        graph_file.parent.mkdir(parents=True, exist_ok=True)
        
        with open(graph_file, 'w', encoding='utf-8') as f:
            json.dump(graph_data, f, indent=2, ensure_ascii=False)
            
    def find_page_by_link(self, link):
        """Find page that matches a link target"""
        for page in self.pages:
            if link['type'] == 'org_roam' and page.get('org_roam_id') == link['target']:
                return page
            elif link['type'] == 'hugo_ref' and link['target'] in page['url']:
                return page
            elif link['type'] == 'markdown' and link['target'] in page['url']:
                return page
        return None
        
    def update_page_frontmatter(self):
        """Update page frontmatter with backlinks"""
        print("Updating page frontmatter...")
        
        for page in self.pages:
            file_path = self.site_dir / page['file']
            if not file_path.exists():
                continue
                
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    post = frontmatter.load(f)
                    
                # Add backlinks to frontmatter
                post['backlinks'] = self.backlinks[page['url']]
                
                # Add related articles (pages with similar tags)
                related = self.find_related_pages(page)
                post['related'] = related
                
                # Write updated file
                with open(file_path, 'w', encoding='utf-8') as f:
                    content = frontmatter.dumps(post)
                    f.write(content)
                    
            except Exception as e:
                print(f"Error updating {file_path}: {e}")
                
    def find_related_pages(self, page, limit=5):
        """Find related pages based on tags and content similarity"""
        related = []
        page_tags = set(page['tags'])
        
        for other_page in self.pages:
            if other_page['url'] == page['url']:
                continue
                
            other_tags = set(other_page['tags'])
            similarity = len(page_tags.intersection(other_tags))
            
            if similarity > 0:
                related.append({
                    'title': other_page['title'],
                    'url': other_page['url'],
                    'similarity': similarity
                })
                
        # Sort by similarity and return top results
        related.sort(key=lambda x: x['similarity'], reverse=True)
        return related[:limit]
        
    def get_url_from_file(self, file_path):
        """Generate URL from file path"""
        relative_path = file_path.relative_to(self.content_dir)
        
        # Remove .md extension
        url_path = str(relative_path).replace('.md', '')
        
        # Convert to URL format
        url_path = url_path.replace('\\', '/')
        
        return f"/{url_path}/"

def main():
    """Main function"""
    import sys
    
    if len(sys.argv) > 1:
        site_dir = sys.argv[1]
    else:
        site_dir = "."
        
    builder = HugoKnowledgeBuilder(site_dir)
    builder.build()

if __name__ == "__main__":
    main()
