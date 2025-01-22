
#!/usr/bin/env python3
import os
import datetime
from pathlib import Path

def create_post(title):
    # Convert title to filename format
    filename = title.lower().replace(' ', '-')
    date = datetime.datetime.now().strftime('%Y-%m-%dT%H:%M:%SZ')
    
    # Create post content
    content = f"""---
title: "{title}"
date: {date}
draft: false
---

Write your content here.
"""
    
    # Ensure posts directory exists
    posts_dir = Path('content/posts')
    posts_dir.mkdir(parents=True, exist_ok=True)
    
    # Create the post file
    post_path = posts_dir / f"{filename}.md"
    with open(post_path, 'w') as f:
        f.write(content)
    
    print(f"Created new post: {post_path}")

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python post.py 'Your Post Title'")
        sys.exit(1)
    
    title = sys.argv[1]
    create_post(title)
