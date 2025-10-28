# 🎨 Figma → Hugo Design Integration Guide

## Overview
This guide walks you through integrating custom Figma designs into your Hugo "Mind & Digital Era" site while preserving all Hugo functionality and ensuring future-proof development.

## 📋 Table of Contents
1. [Prerequisites](#prerequisites)
2. [Phase 1: Design System Setup](#phase-1-design-system-setup)
3. [Phase 2: Component Library](#phase-2-component-library)
4. [Phase 3: Layout Integration](#phase-3-layout-integration)
5. [Phase 4: Advanced Features](#phase-4-advanced-features)
6. [Testing & Deployment](#testing--deployment)
7. [Maintenance & Updates](#maintenance--updates)
8. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### ✅ What You Need
- [Figma account](https://figma.com) (free tier works)
- Your Hugo site running locally
- Basic CSS knowledge
- Git for version control

### ✅ Current Hugo Setup
Your site already has:
- ✅ Base theme: `themes/etch/` (minimal, clean foundation)
- ✅ Custom CSS: `static/css/networked-content.css`
- ✅ Custom templates: `layouts/partials/`
- ✅ Local development server

---

## Phase 1: Design System Setup

### 🎯 Step 1.1: Create Figma Design System

#### 1.1.1: Set Up Figma File
```
📁 Figma File Structure:
├── 🎨 Design System
│   ├── Colors
│   ├── Typography
│   ├── Spacing
│   └── Components
├── 📱 Pages
│   ├── Desktop Homepage
│   ├── Mobile Homepage
│   ├── Article Page
│   └── About Page
└── 🧩 Components
    ├── Header
    ├── Navigation
    ├── Cards
    └── Buttons
```

#### 1.1.2: Define Design Tokens
Create these in Figma:

**Colors:**
```css
/* Primary Brand Colors */
--figma-primary: #6366f1;      /* Indigo - Digital Innovation */
--figma-secondary: #8b5cf6;   /* Purple - Creativity & Mind */
--figma-accent: #06b6d4;       /* Cyan - Digital Connectivity */

/* Semantic Colors */
--figma-success: #10b981;     /* Green */
--figma-warning: #f59e0b;     /* Amber */
--figma-error: #ef4444;       /* Red */

/* Neutral Colors */
--figma-gray-50: #f9fafb;
--figma-gray-100: #f3f4f6;
--figma-gray-900: #111827;
```

**Typography:**
```css
/* Font Families */
--figma-font-primary: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
--figma-font-mono: 'JetBrains Mono', 'Fira Code', monospace;

/* Font Sizes */
--figma-text-xs: 0.75rem;     /* 12px */
--figma-text-sm: 0.875rem;    /* 14px */
--figma-text-base: 1rem;      /* 16px */
--figma-text-lg: 1.125rem;    /* 18px */
--figma-text-xl: 1.25rem;     /* 20px */
--figma-text-2xl: 1.5rem;     /* 24px */
--figma-text-3xl: 1.875rem;   /* 30px */
--figma-text-4xl: 2.25rem;    /* 36px */

/* Font Weights */
--figma-font-light: 300;
--figma-font-normal: 400;
--figma-font-medium: 500;
--figma-font-semibold: 600;
--figma-font-bold: 700;
```

**Spacing:**
```css
/* Spacing Scale */
--figma-space-1: 0.25rem;     /* 4px */
--figma-space-2: 0.5rem;       /* 8px */
--figma-space-3: 0.75rem;      /* 12px */
--figma-space-4: 1rem;         /* 16px */
--figma-space-5: 1.25rem;      /* 20px */
--figma-space-6: 1.5rem;       /* 24px */
--figma-space-8: 2rem;         /* 32px */
--figma-space-10: 2.5rem;      /* 40px */
--figma-space-12: 3rem;        /* 48px */
--figma-space-16: 4rem;        /* 64px */
```

### 🎯 Step 1.2: Export Design Tokens

#### 1.2.1: Create CSS Variables File
Create `static/css/figma-tokens.css`:

```css
/* Figma Design Tokens - Generated from Figma */
:root {
    /* Colors */
    --figma-primary: #6366f1;
    --figma-secondary: #8b5cf6;
    --figma-accent: #06b6d4;
    --figma-success: #10b981;
    --figma-warning: #f59e0b;
    --figma-error: #ef4444;
    
    /* Neutral Colors */
    --figma-gray-50: #f9fafb;
    --figma-gray-100: #f3f4f6;
    --figma-gray-200: #e5e7eb;
    --figma-gray-300: #d1d5db;
    --figma-gray-400: #9ca3af;
    --figma-gray-500: #6b7280;
    --figma-gray-600: #4b5563;
    --figma-gray-700: #374151;
    --figma-gray-800: #1f2937;
    --figma-gray-900: #111827;
    
    /* Typography */
    --figma-font-primary: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    --figma-font-mono: 'JetBrains Mono', 'Fira Code', monospace;
    
    /* Font Sizes */
    --figma-text-xs: 0.75rem;
    --figma-text-sm: 0.875rem;
    --figma-text-base: 1rem;
    --figma-text-lg: 1.125rem;
    --figma-text-xl: 1.25rem;
    --figma-text-2xl: 1.5rem;
    --figma-text-3xl: 1.875rem;
    --figma-text-4xl: 2.25rem;
    
    /* Font Weights */
    --figma-font-light: 300;
    --figma-font-normal: 400;
    --figma-font-medium: 500;
    --figma-font-semibold: 600;
    --figma-font-bold: 700;
    
    /* Spacing */
    --figma-space-1: 0.25rem;
    --figma-space-2: 0.5rem;
    --figma-space-3: 0.75rem;
    --figma-space-4: 1rem;
    --figma-space-5: 1.25rem;
    --figma-space-6: 1.5rem;
    --figma-space-8: 2rem;
    --figma-space-10: 2.5rem;
    --figma-space-12: 3rem;
    --figma-space-16: 4rem;
    
    /* Border Radius */
    --figma-radius-sm: 0.25rem;
    --figma-radius-md: 0.375rem;
    --figma-radius-lg: 0.5rem;
    --figma-radius-xl: 0.75rem;
    --figma-radius-2xl: 1rem;
    
    /* Shadows */
    --figma-shadow-sm: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
    --figma-shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
    --figma-shadow-lg: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
    --figma-shadow-xl: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
    
    /* Transitions */
    --figma-transition-fast: 150ms ease-in-out;
    --figma-transition-normal: 250ms ease-in-out;
    --figma-transition-slow: 350ms ease-in-out;
}
```

#### 1.2.2: Include Tokens in Head Template
Update `layouts/partials/head.html`:

```html
<!-- Add this line after your existing CSS includes -->
<link rel="stylesheet" href="{{ "css/figma-tokens.css" | relURL }}">
```

---

## Phase 2: Component Library

### 🎯 Step 2.1: Create Base Components

#### 2.1.1: Create Components CSS File
Create `static/css/figma-components.css`:

```css
/* Figma Components Library */

/* ===== BUTTONS ===== */
.figma-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    padding: var(--figma-space-2) var(--figma-space-4);
    font-family: var(--figma-font-primary);
    font-size: var(--figma-text-sm);
    font-weight: var(--figma-font-medium);
    line-height: 1.5;
    border: none;
    border-radius: var(--figma-radius-md);
    cursor: pointer;
    transition: all var(--figma-transition-fast);
    text-decoration: none;
}

.figma-btn-primary {
    background: var(--figma-primary);
    color: white;
}

.figma-btn-primary:hover {
    background: #5856eb;
    transform: translateY(-1px);
    box-shadow: var(--figma-shadow-md);
}

.figma-btn-secondary {
    background: var(--figma-gray-100);
    color: var(--figma-gray-700);
    border: 1px solid var(--figma-gray-300);
}

.figma-btn-secondary:hover {
    background: var(--figma-gray-200);
    border-color: var(--figma-gray-400);
}

/* ===== CARDS ===== */
.figma-card {
    background: white;
    border-radius: var(--figma-radius-lg);
    box-shadow: var(--figma-shadow-sm);
    border: 1px solid var(--figma-gray-200);
    overflow: hidden;
    transition: all var(--figma-transition-normal);
}

.figma-card:hover {
    box-shadow: var(--figma-shadow-md);
    transform: translateY(-2px);
}

.figma-card-header {
    padding: var(--figma-space-6);
    border-bottom: 1px solid var(--figma-gray-200);
}

.figma-card-body {
    padding: var(--figma-space-6);
}

.figma-card-footer {
    padding: var(--figma-space-6);
    background: var(--figma-gray-50);
    border-top: 1px solid var(--figma-gray-200);
}

/* ===== NAVIGATION ===== */
.figma-nav {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: var(--figma-space-4) 0;
}

.figma-nav-brand {
    font-size: var(--figma-text-xl);
    font-weight: var(--figma-font-bold);
    color: var(--figma-primary);
    text-decoration: none;
}

.figma-nav-links {
    display: flex;
    list-style: none;
    margin: 0;
    padding: 0;
    gap: var(--figma-space-6);
}

.figma-nav-link {
    color: var(--figma-gray-600);
    text-decoration: none;
    font-weight: var(--figma-font-medium);
    transition: color var(--figma-transition-fast);
}

.figma-nav-link:hover {
    color: var(--figma-primary);
}

/* ===== TYPOGRAPHY ===== */
.figma-heading-1 {
    font-size: var(--figma-text-4xl);
    font-weight: var(--figma-font-bold);
    line-height: 1.2;
    color: var(--figma-gray-900);
    margin-bottom: var(--figma-space-6);
}

.figma-heading-2 {
    font-size: var(--figma-text-3xl);
    font-weight: var(--figma-font-semibold);
    line-height: 1.3;
    color: var(--figma-gray-900);
    margin-bottom: var(--figma-space-5);
}

.figma-heading-3 {
    font-size: var(--figma-text-2xl);
    font-weight: var(--figma-font-semibold);
    line-height: 1.4;
    color: var(--figma-gray-800);
    margin-bottom: var(--figma-space-4);
}

.figma-text {
    font-size: var(--figma-text-base);
    line-height: 1.6;
    color: var(--figma-gray-700);
    margin-bottom: var(--figma-space-4);
}

.figma-text-muted {
    color: var(--figma-gray-500);
}

/* ===== LAYOUTS ===== */
.figma-container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 0 var(--figma-space-4);
}

.figma-grid {
    display: grid;
    gap: var(--figma-space-6);
}

.figma-grid-2 {
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
}

.figma-grid-3 {
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
}

.figma-flex {
    display: flex;
}

.figma-flex-col {
    flex-direction: column;
}

.figma-items-center {
    align-items: center;
}

.figma-justify-between {
    justify-content: space-between;
}

.figma-gap-4 {
    gap: var(--figma-space-4);
}

.figma-gap-6 {
    gap: var(--figma-space-6);
}

/* ===== RESPONSIVE DESIGN ===== */
@media (max-width: 768px) {
    .figma-nav {
        flex-direction: column;
        gap: var(--figma-space-4);
    }
    
    .figma-nav-links {
        flex-wrap: wrap;
        justify-content: center;
    }
    
    .figma-grid-2,
    .figma-grid-3 {
        grid-template-columns: 1fr;
    }
    
    .figma-heading-1 {
        font-size: var(--figma-text-3xl);
    }
    
    .figma-heading-2 {
        font-size: var(--figma-text-2xl);
    }
}
```

#### 2.1.2: Include Components CSS
Update `layouts/partials/head.html`:

```html
<!-- Add this line after figma-tokens.css -->
<link rel="stylesheet" href="{{ "css/figma-components.css" | relURL }}">
```

---

## Phase 3: Layout Integration

### 🎯 Step 3.1: Update Header Template

#### 3.1.1: Create New Header Design
Update `layouts/partials/header.html`:

```html
<header class="figma-header">
    <div class="figma-container">
        <nav class="figma-nav">
            <a href="{{ .Site.BaseURL }}" class="figma-nav-brand">
                {{ .Site.Title }}
            </a>
            
            <ul class="figma-nav-links">
                {{ range .Site.Menus.main }}
                <li>
                    <a href="{{ .URL }}" class="figma-nav-link">
                        {{ .Name }}
                    </a>
                </li>
                {{ end }}
            </ul>
        </nav>
        
        <!-- Keep your existing search functionality -->
        <div class="search-container">
            <!-- Your existing search HTML -->
        </div>
    </div>
</header>
```

#### 3.1.2: Add Header Styles
Add to `static/css/figma-components.css`:

```css
/* ===== HEADER ===== */
.figma-header {
    background: white;
    border-bottom: 1px solid var(--figma-gray-200);
    position: sticky;
    top: 0;
    z-index: 100;
    backdrop-filter: blur(10px);
    background: rgba(255, 255, 255, 0.95);
}

.figma-header .search-container {
    margin-top: var(--figma-space-4);
    padding-top: var(--figma-space-4);
    border-top: 1px solid var(--figma-gray-200);
}
```

### 🎯 Step 3.2: Update Homepage Layout

#### 3.2.1: Create Custom Index Template
Create `layouts/index.html`:

```html
{{ define "main" }}
<div class="figma-hero">
    <div class="figma-container">
        <h1 class="figma-heading-1">Mind & Digital Era</h1>
        <p class="figma-text figma-text-muted">
            A networked knowledge base exploring the human mind, epistemology, and the new digital era.
        </p>
    </div>
</div>

<div class="figma-container">
    <section class="figma-posts-section">
        <h2 class="figma-heading-2">Latest Articles</h2>
        
        <div class="figma-grid figma-grid-2">
            {{ range first 6 .Site.RegularPages }}
            <article class="figma-card">
                <div class="figma-card-body">
                    <h3 class="figma-heading-3">
                        <a href="{{ .Permalink }}" class="figma-card-link">
                            {{ .Title }}
                        </a>
                    </h3>
                    
                    <p class="figma-text-muted">
                        {{ .Summary | truncate 150 }}
                    </p>
                    
                    <div class="figma-card-meta">
                        <time class="figma-text-muted">
                            {{ .Date.Format "January 2, 2006" }}
                        </time>
                        
                        {{ if .Params.topics }}
                        <div class="figma-tags">
                            {{ range first 3 .Params.topics }}
                            <span class="figma-tag">{{ . }}</span>
                            {{ end }}
                        </div>
                        {{ end }}
                    </div>
                </div>
            </article>
            {{ end }}
        </div>
        
        <div class="figma-text-center">
            <a href="/posts/" class="figma-btn figma-btn-primary">
                View All Articles
            </a>
        </div>
    </section>
</div>
{{ end }}
```

#### 3.2.2: Add Supporting Styles
Add to `static/css/figma-components.css`:

```css
/* ===== HOMEPAGE ===== */
.figma-hero {
    background: linear-gradient(135deg, var(--figma-primary), var(--figma-secondary));
    color: white;
    padding: var(--figma-space-16) 0;
    text-align: center;
}

.figma-hero h1 {
    color: white;
}

.figma-hero p {
    color: rgba(255, 255, 255, 0.9);
    font-size: var(--figma-text-lg);
}

.figma-posts-section {
    padding: var(--figma-space-16) 0;
}

.figma-card-link {
    color: var(--figma-gray-900);
    text-decoration: none;
}

.figma-card-link:hover {
    color: var(--figma-primary);
}

.figma-card-meta {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-top: var(--figma-space-4);
    padding-top: var(--figma-space-4);
    border-top: 1px solid var(--figma-gray-200);
}

.figma-tags {
    display: flex;
    gap: var(--figma-space-2);
}

.figma-tag {
    background: var(--figma-gray-100);
    color: var(--figma-gray-600);
    padding: var(--figma-space-1) var(--figma-space-2);
    border-radius: var(--figma-radius-sm);
    font-size: var(--figma-text-xs);
    font-weight: var(--figma-font-medium);
}

.figma-text-center {
    text-align: center;
    margin-top: var(--figma-space-8);
}
```

---

## Phase 4: Advanced Features

### 🎯 Step 4.1: Article Page Enhancement

#### 4.1.1: Create Article Template
Create `layouts/_default/single.html`:

```html
{{ define "main" }}
<article class="figma-article">
    <div class="figma-container">
        <header class="figma-article-header">
            <h1 class="figma-heading-1">{{ .Title }}</h1>
            
            <div class="figma-article-meta">
                <time class="figma-text-muted">
                    {{ .Date.Format "January 2, 2006" }}
                </time>
                
                {{ if .Params.topics }}
                <div class="figma-tags">
                    {{ range .Params.topics }}
                    <span class="figma-tag">{{ . }}</span>
                    {{ end }}
                </div>
                {{ end }}
            </div>
        </header>
        
        <div class="figma-article-content">
            {{ .Content }}
        </div>
        
        {{ if .Params.related }}
        <aside class="figma-related-posts">
            <h3 class="figma-heading-3">Related Articles</h3>
            <div class="figma-grid figma-grid-2">
                {{ range .Params.related }}
                <div class="figma-card">
                    <div class="figma-card-body">
                        <h4><a href="{{ .url }}" class="figma-card-link">{{ .title }}</a></h4>
                        <p class="figma-text-muted">{{ .description }}</p>
                    </div>
                </div>
                {{ end }}
            </div>
        </aside>
        {{ end }}
    </div>
</article>
{{ end }}
```

#### 4.1.2: Add Article Styles
Add to `static/css/figma-components.css`:

```css
/* ===== ARTICLE PAGE ===== */
.figma-article {
    padding: var(--figma-space-16) 0;
}

.figma-article-header {
    text-align: center;
    margin-bottom: var(--figma-space-12);
    padding-bottom: var(--figma-space-8);
    border-bottom: 1px solid var(--figma-gray-200);
}

.figma-article-meta {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: var(--figma-space-4);
    margin-top: var(--figma-space-4);
}

.figma-article-content {
    max-width: 800px;
    margin: 0 auto;
    font-size: var(--figma-text-lg);
    line-height: 1.7;
}

.figma-article-content h2 {
    margin-top: var(--figma-space-12);
    margin-bottom: var(--figma-space-6);
}

.figma-article-content h3 {
    margin-top: var(--figma-space-8);
    margin-bottom: var(--figma-space-4);
}

.figma-related-posts {
    margin-top: var(--figma-space-16);
    padding-top: var(--figma-space-12);
    border-top: 1px solid var(--figma-gray-200);
}
```

### 🎯 Step 4.2: Export Assets from Figma

#### 4.2.1: Export Icons and Images
1. **In Figma**: Select icons/images
2. **Export**: Choose SVG format for icons, PNG/WebP for images
3. **Save to**: `static/images/figma/`

#### 4.2.2: Optimize Assets
Create `scripts/optimize-figma-assets.sh`:

```bash
#!/bin/bash
# Optimize Figma exported assets

echo "Optimizing Figma assets..."

# Optimize SVGs
find static/images/figma -name "*.svg" -exec svgo {} \;

# Optimize PNGs
find static/images/figma -name "*.png" -exec pngquant --force --ext .png {} \;

# Convert to WebP
find static/images/figma -name "*.png" -exec cwebp {} -o {}.webp \;

echo "Assets optimized!"
```

---

## Testing & Deployment

### 🎯 Step 5.1: Local Testing

#### 5.1.1: Test Checklist
```bash
# Start local server
hugo server --bind 0.0.0.0 --port 1313

# Test these pages:
# ✅ Homepage: http://localhost:1313/
# ✅ Article page: http://localhost:1313/posts/your-article/
# ✅ About page: http://localhost:1313/about/
# ✅ Search functionality
# ✅ Mobile responsiveness
# ✅ Dark mode (if implemented)
```

#### 5.1.2: Browser Testing
- **Chrome**: Latest version
- **Firefox**: Latest version
- **Safari**: Latest version (if on Mac)
- **Mobile**: Chrome mobile, Safari mobile

### 🎯 Step 5.2: Performance Testing

#### 5.2.1: Lighthouse Audit
```bash
# Install Lighthouse CLI
npm install -g lighthouse

# Run audit
lighthouse http://localhost:1313 --output html --output-path ./lighthouse-report.html
```

#### 5.2.2: Performance Checklist
- ✅ **Page Speed**: < 3 seconds load time
- ✅ **Core Web Vitals**: Good scores
- ✅ **Accessibility**: WCAG compliance
- ✅ **SEO**: Meta tags, structured data
- ✅ **Mobile**: Responsive design

### 🎯 Step 5.3: Deploy to VPS

#### 5.3.1: Build and Deploy
```bash
# Build site
hugo --minify

# Deploy to VPS (your existing workflow)
git add .
git commit -m "Add Figma design integration"
git push origin main
```

---

## Maintenance & Updates

### 🎯 Step 6.1: Design System Updates

#### 6.1.1: Update Process
1. **Figma**: Make design changes
2. **Export**: New tokens/components
3. **Update**: CSS files
4. **Test**: Local development
5. **Deploy**: Push to production

#### 6.1.2: Version Control
```bash
# Track design changes
git add static/css/figma-*
git commit -m "Update Figma design system v1.2"
git tag -a v1.2 -m "Figma design system v1.2"
```

### 🎯 Step 6.2: Theme Updates

#### 6.2.1: Safe Update Process
```bash
# Backup current theme
cp -r themes/etch themes/etch-backup

# Update theme (if needed)
cd themes/etch
git pull origin master

# Test compatibility
hugo server --bind 0.0.0.0 --port 1313
```

---

## Troubleshooting

### 🚨 Common Issues

#### Issue 1: CSS Not Loading
**Problem**: Figma styles not appearing
**Solution**:
```bash
# Check file paths
ls -la static/css/figma-*

# Verify head.html includes
grep -n "figma" layouts/partials/head.html

# Clear Hugo cache
hugo --gc
```

#### Issue 2: Layout Conflicts
**Problem**: Custom layouts not working
**Solution**:
```bash
# Check template hierarchy
ls -la layouts/

# Verify template names
# index.html for homepage
# _default/single.html for articles
# _default/list.html for listing pages
```

#### Issue 3: Mobile Responsiveness
**Problem**: Design breaks on mobile
**Solution**:
```css
/* Add mobile-first CSS */
@media (max-width: 768px) {
    .figma-container {
        padding: 0 var(--figma-space-2);
    }
    
    .figma-grid-2 {
        grid-template-columns: 1fr;
    }
}
```

#### Issue 4: Performance Issues
**Problem**: Slow loading
**Solution**:
```bash
# Optimize images
hugo --minify

# Check bundle size
hugo --gc --minify --verbose
```

---

## 🎉 Success Metrics

### ✅ Design Integration Complete When:
- [ ] Figma design tokens implemented
- [ ] Component library created
- [ ] Layouts updated
- [ ] Mobile responsive
- [ ] Performance optimized
- [ ] Deployed to production
- [ ] All Hugo features working

### 📊 Performance Targets:
- **Page Speed**: < 3 seconds
- **Lighthouse Score**: > 90
- **Mobile Score**: > 90
- **Accessibility**: > 95

---

## 🚀 Next Steps

### Phase 5: Advanced Features
1. **Animations**: CSS animations from Figma
2. **Interactions**: JavaScript enhancements
3. **Dark Mode**: Theme switching
4. **PWA**: Progressive Web App features

### Phase 6: Content Integration
1. **Rich Media**: Images, videos, embeds
2. **Interactive Elements**: Charts, diagrams
3. **Advanced Search**: Enhanced search features
4. **Social Features**: Comments, sharing

---

## 📚 Resources

### Design Tools:
- [Figma](https://figma.com) - Design tool
- [Figma Tokens](https://www.figma.com/community/plugin/843461159747178946/Figma-Tokens) - Design tokens plugin
- [Figma to Code](https://www.figma.com/community/plugin/747985167520967365/Figma-to-HTML-CSS) - Export plugin

### Hugo Resources:
- [Hugo Documentation](https://gohugo.io/documentation/)
- [Hugo Themes](https://themes.gohugo.io/)
- [Hugo Community](https://discourse.gohugo.io/)

### CSS Resources:
- [CSS Custom Properties](https://developer.mozilla.org/en-US/docs/Web/CSS/Using_CSS_custom_properties)
- [CSS Grid](https://css-tricks.com/snippets/css/complete-guide-grid/)
- [Flexbox](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)

---

**🎨 Happy Designing! Your Hugo site is now ready for beautiful Figma integration!**
