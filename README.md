# IP Spark - Networked Knowledge Base & Blog

A powerful Hugo-based static website that combines the best of networked thinking (like Obsidian/org-roam) with seamless publishing from Emacs. This project transforms your static blog into a dynamic knowledge base with bidirectional links, intelligent search, and visual knowledge graphs.

## 🌟 Features

### Networked Thinking
- **Bidirectional Links**: Automatic backlink generation like Obsidian
- **Knowledge Graph**: Interactive D3.js visualization of article relationships
- **Map of Content (MOC)**: Organize complex topics with structured content maps
- **Internal Linking**: Smart shortcodes for seamless internal references

### Powerful Search
- **Multi-modal Search**: Search by tags, keywords, full-text content, and concepts
- **Real-time Results**: Instant search with highlighting and suggestions
- **Semantic Discovery**: Find related articles and concepts automatically
- **Keyboard Shortcuts**: Ctrl/Cmd+K to focus search, Escape to close

### Seamless Publishing
- **Emacs Integration**: Write in org-roam/denote, publish to Hugo automatically
- **ox-hugo Export**: Custom templates preserve org-roam IDs and metadata
- **Automated Workflow**: One-command publishing from Emacs to live site
- **Version Control**: Git-based content management with automated deployments

### Enhanced UX
- **Responsive Design**: Optimized for desktop and mobile
- **Dark/Light Mode**: Automatic theme switching based on system preferences
- **Fast Loading**: Static site generation with optimized assets
- **SEO Optimized**: Structured data and meta tags for search engines

## 🚀 Quick Start

### Prerequisites
- Hugo (extended version)
- Python 3.11+
- Emacs with org-roam/denote
- Git

### Installation

1. **Clone the repository**
   ```bash
   git clone <your-repo-url>
   cd ip-spark
   ```

2. **Install dependencies**
   ```bash
   make install-deps
   ```

3. **Configure Emacs** (add to your init.el)
   ```elisp
   (load-file ".emacs.d/ox-hugo-config.el")
   ```

4. **Build the site**
   ```bash
   make full-build
   ```

5. **Start development server**
   ```bash
   make serve
   ```

## 📝 Writing Workflow

### From Emacs to Published Blog

1. **Create a new blog post**
   ```
   M-x org-roam-node-find
   Choose "Blog Post" template
   ```

2. **Write your content** with org-roam links:
   ```org
   #+title: My New Article
   #+filetags: :innovation:technology:
   
   This article explores [[id:20250122T100000--ai-copyright__legal_ai]] concepts.
   
   See also [[id:20250122T110000--ip-strategy__business_strategy]].
   ```

3. **Export to Hugo**
   ```
   C-c C-e H h  (export current node to Hugo)
   ```

4. **Build and deploy**
   ```bash
   make deploy
   ```

### Using Shortcodes

**Internal Links**
```markdown
{{< link target="posts/my-article" text="Read more about this topic" >}}
```

**Map of Content**
```markdown
{{< moc title="AI and Copyright" tags="ai,legal,copyright" concepts="intellectual-property,artificial-intelligence" >}}
```

## 🛠 Configuration

### Hugo Configuration

The `config.toml` includes:
- Enhanced taxonomies (tags, categories, concepts, MOCs)
- Custom content types with metadata
- Search and graph visualization settings
- Optimized permalinks and menus

### Emacs Configuration

The `.emacs.d/ox-hugo-config.el` provides:
- ox-hugo integration with org-roam
- Custom export templates
- Automatic front matter generation
- Blog post capture templates

### Search Configuration

Search is powered by:
- Client-side JavaScript search engine
- Generated search index (`search-index.json`)
- Real-time filtering and highlighting
- Keyboard shortcuts and accessibility

## 📊 Knowledge Graph

The knowledge graph visualization shows:
- **Nodes**: Articles sized by connection count
- **Links**: Relationships between articles
- **Colors**: Different content types or tags
- **Interactivity**: Click to navigate, hover for previews

### Graph Data Structure
```json
{
  "nodes": [
    {
      "id": "article-id",
      "title": "Article Title",
      "url": "/article-url/",
      "tags": ["tag1", "tag2"],
      "connections": 5
    }
  ],
  "links": [
    {
      "source": "source-id",
      "target": "target-id",
      "value": 1,
      "type": "internal-link"
    }
  ]
}
```

## 🔍 Search Features

### Search Types
- **Full-text**: Search article content
- **Tag-based**: Filter by tags and categories
- **Semantic**: Find related concepts
- **MOC-based**: Navigate through content maps

### Search Interface
- Real-time suggestions
- Result highlighting
- Advanced filters
- Keyboard navigation

## 🚀 Deployment

### Automated Deployment

The project includes GitHub Actions for automated deployment to Hostinger VPS:

1. **Set up secrets** in GitHub repository:
   - `HOSTINGER_HOST`: Your VPS IP address
   - `HOSTINGER_USERNAME`: SSH username
   - `HOSTINGER_SSH_KEY`: Private SSH key

2. **Push to main branch** triggers automatic deployment

### Manual Deployment

```bash
make deploy
```

### Traefik Configuration

For Hostinger VPS with Traefik [[memory:8175396]], ensure your `docker-compose.yml` includes:

```yaml
services:
  traefik:
    image: traefik:v2.10
    command:
      - --api.dashboard=true
      - --providers.docker=true
      - --entrypoints.web.address=:80
      - --entrypoints.websecure.address=:443
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
      - ./traefik:/etc/traefik
```

## 📁 Project Structure

```
ip-spark/
├── .emacs.d/
│   └── ox-hugo-config.el          # Emacs configuration
├── .github/workflows/
│   └── deploy.yml                 # GitHub Actions deployment
├── content/
│   ├── posts/                     # Blog posts
│   └── mocs/                      # Map of Content pages
├── layouts/
│   ├── _default/
│   │   └── single.html            # Enhanced single page template
│   └── shortcodes/
│       ├── link.html              # Internal link shortcode
│       └── moc.html                # MOC shortcode
├── static/
│   ├── css/
│   │   └── networked-content.css   # Enhanced styles
│   └── js/
│       ├── knowledge-graph.js      # D3.js graph visualization
│       └── search.js                # Search functionality
├── scripts/
│   └── knowledge-builder.py        # Build automation script
├── config.toml                     # Hugo configuration
├── Makefile                        # Build automation
└── README.md                       # This file
```

## 🎯 Use Cases

### Personal Knowledge Base
- Capture ideas and insights from reading
- Build connections between concepts
- Create comprehensive topic maps
- Share knowledge publicly

### Professional Blog
- Demonstrate expertise through networked content
- Show thought evolution over time
- Create comprehensive resource collections
- Engage readers with interactive features

### Research Documentation
- Organize research findings
- Track idea development
- Create literature reviews
- Share research publicly

## 🔧 Customization

### Adding New Content Types

1. **Define in config.toml**:
   ```toml
   [contentTypes."your-type"]
     path = "your-type"
   ```

2. **Create layouts**:
   ```bash
   mkdir layouts/your-type
   ```

3. **Add to knowledge builder**:
   Update `scripts/knowledge-builder.py`

### Customizing Search

Modify `static/js/search.js` to:
- Add new search filters
- Change ranking algorithms
- Integrate external search APIs
- Add search analytics

### Extending Knowledge Graph

Enhance `static/js/knowledge-graph.js` to:
- Add new node types
- Implement different layouts
- Add filtering capabilities
- Create custom visualizations

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- **Hugo**: Static site generator
- **org-roam**: Networked note-taking
- **ox-hugo**: Org-mode to Hugo export
- **D3.js**: Data visualization
- **Traefik**: Reverse proxy

## 📞 Support

For questions and support:
- Create an issue on GitHub
- Check the documentation
- Review the configuration files

---

**Transform your static blog into a dynamic knowledge base!** 🚀