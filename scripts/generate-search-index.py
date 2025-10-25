#!/usr/bin/env python3
"""
Search Index Generator for Hugo Site
Generates a searchable index of all content for client-side search functionality.
"""

import os
import json
import frontmatter
import re
from pathlib import Path
from datetime import datetime

def clean_text(text):
    """Clean and normalize text for indexing."""
    # Remove markdown syntax
    text = re.sub(r'#{1,6}\s+', '', text)  # Headers
    text = re.sub(r'\*\*(.*?)\*\*', r'\1', text)  # Bold
    text = re.sub(r'\*(.*?)\*', r'\1', text)  # Italic
    text = re.sub(r'`(.*?)`', r'\1', text)  # Code
    text = re.sub(r'\[([^\]]+)\]\([^\)]+\)', r'\1', text)  # Links
    text = re.sub(r'!\[([^\]]*)\]\([^\)]+\)', r'\1', text)  # Images
    text = re.sub(r'```.*?```', '', text, flags=re.DOTALL)  # Code blocks
    text = re.sub(r'`.*?`', '', text)  # Inline code
    text = re.sub(r'\n+', ' ', text)  # Multiple newlines to space
    text = re.sub(r'\s+', ' ', text)  # Multiple spaces to single space
    return text.strip()

def extract_keywords(text):
    """Extract potential keywords from text."""
    # Remove common words and extract meaningful terms
    stop_words = {
        'the', 'a', 'an', 'and', 'or', 'but', 'in', 'on', 'at', 'to', 'for', 'of', 'with', 'by',
        'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'do', 'does', 'did',
        'will', 'would', 'could', 'should', 'may', 'might', 'must', 'can', 'this', 'that', 'these',
        'those', 'i', 'you', 'he', 'she', 'it', 'we', 'they', 'me', 'him', 'her', 'us', 'them'
    }
    
    words = re.findall(r'\b[a-zA-Z]{3,}\b', text.lower())
    keywords = [word for word in words if word not in stop_words]
    return list(set(keywords))  # Remove duplicates

def process_content_file(file_path, base_path):
    """Process a single content file and extract searchable data."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            post = frontmatter.load(f)
        
        # Extract basic info
        title = post.get('title', 'Untitled')
        date_raw = post.get('date', datetime.now())
        
        # Handle different date formats
        if isinstance(date_raw, str):
            try:
                date = datetime.fromisoformat(date_raw.replace('Z', '+00:00')).isoformat()
            except:
                date = datetime.now().isoformat()
        elif hasattr(date_raw, 'isoformat'):
            date = date_raw.isoformat()
        else:
            date = datetime.now().isoformat()
            
        tags = post.get('tags', [])
        categories = post.get('categories', [])
        topics = post.get('topics', [])
        domains = post.get('domains', [])
        concepts = post.get('concepts', [])
        draft = post.get('draft', False)
        
        # Skip draft posts
        if draft:
            return None
        
        # Clean content
        content = clean_text(post.content)
        
        # Extract keywords
        keywords = extract_keywords(content)
        
        # Generate URL (assuming Hugo permalink structure)
        relative_path = os.path.relpath(file_path, base_path)
        if relative_path.startswith('posts/'):
            # Use title-based permalink
            url_title = re.sub(r'[^a-zA-Z0-9\s-]', '', title.lower())
            url_title = re.sub(r'\s+', '-', url_title)
            url = f"/{url_title}/"
        else:
            # Use path-based URL
            url = f"/{relative_path.replace('.md', '').replace('index', '')}/"
        
        return {
            'title': title,
            'url': url,
            'content': content,
            'excerpt': content[:200] + '...' if len(content) > 200 else content,
            'date': date,
            'tags': tags,
            'categories': categories,
            'topics': topics,
            'domains': domains,
            'concepts': concepts,
            'keywords': keywords,
            'file_path': relative_path
        }
    
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return None

def generate_search_index(content_dir, output_file):
    """Generate search index for all content files."""
    print("🔍 Generating search index...")
    
    search_data = {
        'generated_at': datetime.now().isoformat(),
        'total_pages': 0,
        'pages': []
    }
    
    # Process all markdown files
    for root, dirs, files in os.walk(content_dir):
        for file in files:
            if file.endswith('.md'):
                file_path = os.path.join(root, file)
                page_data = process_content_file(file_path, content_dir)
                
                if page_data:
                    search_data['pages'].append(page_data)
                    search_data['total_pages'] += 1
                    print(f"  ✓ Indexed: {page_data['title']}")
    
    # Sort by date (newest first)
    search_data['pages'].sort(key=lambda x: x['date'], reverse=True)
    
    # Write search index
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(search_data, f, indent=2, ensure_ascii=False)
    
    print(f"✅ Search index generated: {output_file}")
    print(f"📊 Total pages indexed: {search_data['total_pages']}")
    
    return search_data

if __name__ == "__main__":
    content_dir = "content"
    output_file = "static/search-index.json"
    
    # Ensure output directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    
    generate_search_index(content_dir, output_file)
