# Content Creation and Publishing Workflow

This guide covers the complete workflow for creating, editing, and publishing content on your IP Spark knowledge base.

## 🚀 Quick Start

### 1. Create New Content

#### Option A: Using the Python Script (Recommended)
```bash
# Navigate to your project directory
cd /home/ago/kDrive/1_Projects/notes_agostinodeangelis_da15e11b/

# Create a new post using the provided script
python3 scripts/post.py "Your Post Title Here"
```

#### Option B: Manual Creation
```bash
# Create a new markdown file in content/posts/
touch content/posts/your-post-title.md
```

### 2. Edit Your Content

Your posts should follow this structure:

```markdown
---
title: "Your Post Title"
date: 2025-01-XX
draft: false
tags: ["tag1", "tag2", "tag3"]
categories: ["category1"]
concepts: ["concept1", "concept2"]
org_roam_id: "optional-id"
slug: "your-post-slug"
---

# Your Content Here

Write your content using Markdown. You can:

- Use **bold** and *italic* text
- Create lists and tables
- Add internal links using Hugo refs: [Link Text]({{< ref "other-post" >}})
- Add org-roam style links: [[id:some-id]]
- Include code blocks with syntax highlighting

## Linking to Other Content

You can create connections between your posts using:

1. **Hugo References**: `[Link Text]({{< ref "other-post-title" >}})`
2. **Org-roam Links**: `[[id:unique-identifier]]`
3. **Markdown Links**: `[Link Text](relative/path)`

## Tags and Categories

- **tags**: For flexible topic organization
- **categories**: For broader content classification  
- **concepts**: For knowledge graph connections
- **mocs**: For Maps of Content (overview pages)
```

### 3. Build and Preview Locally
```bash
# Install dependencies (first time only)
make install-deps

# Start development server
make serve

# Your site will be available at http://localhost:1313
```

### 4. Build Knowledge Base Features
```bash
# Build search index, backlinks, and knowledge graph
make knowledge-build

# Or do a complete build
make full-build
```

### 5. Deploy to Production
```bash
# Deploy to your Hostinger VPS
make deploy

# Or push to GitHub for automated deployment
git add .
git commit -m "Add new post: Your Post Title"
git push origin main
```

## 📝 Content Structure Guidelines

### File Organization
- **Posts**: `content/posts/` - Main content articles
- **MOCs**: `content/mocs/` - Maps of Content (overview pages)
- **About**: `content/about/` - Static pages

### Frontmatter Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `title` | string | ✅ | Post title |
| `date` | date | ✅ | Publication date |
| `draft` | boolean | ❌ | Set to `false` to publish |
| `tags` | array | ❌ | Topic tags for organization |
| `categories` | array | ❌ | Broader content classification |
| `concepts` | array | ❌ | For knowledge graph connections |
| `org_roam_id` | string | ❌ | Unique identifier for linking |
| `slug` | string | ❌ | URL-friendly version of title |

### Linking System

Your site supports three types of internal links:

1. **Hugo References**: `{{< ref "post-title" >}}`
   ```markdown
   [Link Text]({{< ref "other-post-title" >}})
   ```

2. **Org-roam Links**: `[[id:unique-id]]`
   ```markdown
   [[id:unique-identifier]]
   ```

3. **Markdown Links**: `[text](path)`
   ```markdown
   [Link Text](relative/path)
   ```

## 🔄 Automated Features

When you run `make knowledge-build`, the system automatically:

1. **Scans all content** for internal links
2. **Generates backlinks** - shows which pages link to each post
3. **Creates search index** - enables site-wide search
4. **Builds knowledge graph** - visual network of connected content
5. **Updates frontmatter** - adds backlinks and related posts

## 🚀 Deployment Options

### Manual Deployment
```bash
# Complete build and deploy
make full-build
make deploy
```

### Automated Deployment (GitHub Actions)
```bash
git add .
git commit -m "Add new content"
git push origin main
# GitHub Actions will automatically build and deploy
```

## 📊 Content Types

### 1. Regular Posts
- Articles, thoughts, notes
- Located in `content/posts/`
- Use standard frontmatter

### 2. MOCs (Maps of Content)
- Overview pages that organize related content
- Located in `content/mocs/`
- Help users navigate complex topics

### 3. Static Pages
- About, contact, etc.
- Located in `content/about/`
- Use for non-blog content

## 🎯 Best Practices

### Content Creation
1. **Use meaningful tags** to help with discovery
2. **Create internal links** to build knowledge connections
3. **Add org_roam_id** for consistent linking
4. **Write descriptive titles** and slugs
5. **Use the draft flag** to work on content before publishing

### Organization
1. **Group related content** using categories
2. **Create MOCs** for complex topics
3. **Use consistent naming** conventions
4. **Link related posts** to build knowledge networks

### Publishing
1. **Preview locally** before publishing
2. **Test all links** and functionality
3. **Build knowledge features** after content changes
4. **Deploy consistently** using the established workflow

## 🔍 Preview and Testing

### Local Development
```bash
# Start development server
make serve
# Site available at http://localhost:1313
```

### Test Build
```bash
# Test production build
make test
# Test server at http://localhost:8000
```

### Production Build
```bash
# Create production build
make prod
# Files ready in public/ directory
```

## 🛠️ Available Commands

### Make Commands
- `make help` - Show available commands
- `make install-deps` - Install Python dependencies
- `make knowledge-build` - Build knowledge base features
- `make build` - Build Hugo site
- `make serve` - Start development server
- `make clean` - Clean build artifacts
- `make deploy` - Deploy to Hostinger VPS
- `make full-build` - Complete build with all features
- `make dev` - Development workflow
- `make prod` - Production build
- `make test` - Test build

### Python Scripts
- `python3 scripts/post.py "Title"` - Create new post
- `python3 scripts/knowledge-builder.py` - Build knowledge features

## 📚 Content Examples

### Example Post Structure
```markdown
---
title: "Understanding Intellectual Property in the Digital Age"
date: 2025-01-15T10:00:00Z
draft: false
tags: ["intellectual-property", "digital-transformation", "innovation"]
categories: ["business-strategy"]
concepts: ["IP-strategy", "digital-rights", "innovation-management"]
org_roam_id: "ip-digital-age-2025"
slug: "intellectual-property-digital-age"
---

# Understanding Intellectual Property in the Digital Age

The digital transformation has fundamentally changed how we think about intellectual property...

## Key Concepts

- **Digital Rights Management**: [[id:digital-rights-management]]
- **Innovation Strategy**: {{< ref "business-strategy" >}}
- **Patent Landscape**: [Patent Analysis](patent-analysis)

## Related Topics

This connects to several other areas of study...
```

### Example MOC (Map of Content)
```markdown
---
title: "Intellectual Property Strategy MOC"
date: 2025-01-15T10:00:00Z
draft: false
tags: ["moc", "intellectual-property", "strategy"]
categories: ["maps-of-content"]
concepts: ["IP-strategy", "knowledge-management"]
org_roam_id: "ip-strategy-moc"
slug: "ip-strategy-moc"
---

# Intellectual Property Strategy MOC

This Map of Content organizes all resources related to intellectual property strategy.

## Core Concepts

- [[id:ip-digital-age-2025]] - Understanding IP in the Digital Age
- {{< ref "business-strategy" >}} - Business Strategy Fundamentals
- [Patent Strategy](patent-strategy) - Patent-specific strategies

## Implementation Guides

- IP Portfolio Management
- Digital Rights Strategy
- Innovation Protection Methods

## Case Studies

- Technology Company IP Strategies
- Startup IP Protection
- Open Source vs. Proprietary Models
```

## 🚨 Troubleshooting

### Common Issues

1. **Post not appearing**
   - Check `draft: false` in frontmatter
   - Verify file is in correct directory
   - Run `make full-build` to rebuild

2. **Links not working**
   - Check link syntax (Hugo refs, org-roam, markdown)
   - Verify target files exist
   - Run `make knowledge-build` to update links

3. **Search not finding content**
   - Run `make knowledge-build` to rebuild search index
   - Check if search-index.json exists in public/

4. **Deployment issues**
   - Check SSH connection to VPS
   - Verify rsync command in Makefile
   - Check VPS directory permissions

### Getting Help

1. Check the logs: `make serve` shows build errors
2. Verify configuration in `config.toml`
3. Test locally before deploying
4. Check GitHub Actions logs for automated deployments

## 🎉 Success Checklist

Before publishing new content:

- [ ] Content is written and edited
- [ ] Frontmatter is complete and correct
- [ ] Internal links are working
- [ ] Tags and categories are appropriate
- [ ] Draft flag is set to `false`
- [ ] Local preview looks good
- [ ] Knowledge base features are built
- [ ] Content is committed to git
- [ ] Deployment is successful
- [ ] Live site is updated

---

Your IP Spark knowledge base is now live at `https://notes.agostinodeangelis.com` with full search functionality, knowledge graph, and backlinks working automatically! 🚀

For questions or issues, refer to the main [README.md](README.md) or [DEPLOYMENT.md](DEPLOYMENT.md) files.
