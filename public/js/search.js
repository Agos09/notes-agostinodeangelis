// Client-side search functionality
class SearchEngine {
    constructor() {
        this.index = null;
        this.results = [];
        this.init();
    }

    async init() {
        await this.loadSearchIndex();
        this.setupSearchUI();
    }

    async loadSearchIndex() {
        try {
            const response = await fetch('/search-index.json');
            this.index = await response.json();
        } catch (error) {
            console.error('Error loading search index:', error);
            // Fallback: build index from page content
            this.buildIndexFromDOM();
        }
    }

    buildIndexFromDOM() {
        // Fallback method to build search index from current page content
        const pages = document.querySelectorAll('article, .post');
        this.index = {
            pages: Array.from(pages).map(page => ({
                title: page.querySelector('h1, h2')?.textContent || '',
                content: page.textContent || '',
                url: window.location.pathname,
                tags: this.extractTags(page),
                date: this.extractDate(page)
            }))
        };
    }

    extractTags(element) {
        const tagElements = element.querySelectorAll('.tag, [data-tag]');
        return Array.from(tagElements).map(el => el.textContent.trim());
    }

    extractDate(element) {
        const dateElement = element.querySelector('time');
        return dateElement ? dateElement.getAttribute('datetime') : '';
    }

    setupSearchUI() {
        // Create search input if it doesn't exist
        let searchInput = document.getElementById('search-input');
        if (!searchInput) {
            searchInput = document.createElement('input');
            searchInput.id = 'search-input';
            searchInput.type = 'text';
            searchInput.placeholder = 'Search articles...';
            searchInput.className = 'search-input';
            
            // Add to header or create search container
            const header = document.querySelector('header') || document.body;
            header.appendChild(searchInput);
        }

        // Create results container
        let resultsContainer = document.getElementById('search-results');
        if (!resultsContainer) {
            resultsContainer = document.createElement('div');
            resultsContainer.id = 'search-results';
            resultsContainer.className = 'search-results';
            searchInput.parentNode.appendChild(resultsContainer);
        }

        // Add event listeners
        searchInput.addEventListener('input', (e) => {
            this.search(e.target.value);
        });

        searchInput.addEventListener('focus', () => {
            resultsContainer.style.display = 'block';
        });

        // Hide results when clicking outside
        document.addEventListener('click', (e) => {
            if (!searchInput.contains(e.target) && !resultsContainer.contains(e.target)) {
                resultsContainer.style.display = 'none';
            }
        });
    }

    search(query) {
        if (!query.trim()) {
            this.hideResults();
            return;
        }

        const results = this.performSearch(query);
        this.displayResults(results);
    }

    performSearch(query) {
        if (!this.index) return [];

        const searchTerms = query.toLowerCase().split(' ').filter(term => term.length > 0);
        const results = [];

        this.index.pages.forEach(page => {
            let score = 0;
            const title = page.title.toLowerCase();
            const content = page.content.toLowerCase();
            const tags = page.tags.map(tag => tag.toLowerCase());

            // Title matches (highest weight)
            searchTerms.forEach(term => {
                if (title.includes(term)) score += 10;
                if (title.startsWith(term)) score += 5;
            });

            // Tag matches (medium weight)
            searchTerms.forEach(term => {
                if (tags.some(tag => tag.includes(term))) score += 5;
            });

            // Content matches (lower weight)
            searchTerms.forEach(term => {
                const matches = (content.match(new RegExp(term, 'g')) || []).length;
                score += matches;
            });

            if (score > 0) {
                results.push({
                    ...page,
                    score,
                    snippet: this.generateSnippet(page.content, searchTerms)
                });
            }
        });

        return results.sort((a, b) => b.score - a.score).slice(0, 10);
    }

    generateSnippet(content, searchTerms) {
        const maxLength = 200;
        const lowerContent = content.toLowerCase();
        
        // Find the first occurrence of any search term
        let bestIndex = -1;
        searchTerms.forEach(term => {
            const index = lowerContent.indexOf(term);
            if (index !== -1 && (bestIndex === -1 || index < bestIndex)) {
                bestIndex = index;
            }
        });

        if (bestIndex === -1) {
            return content.substring(0, maxLength) + '...';
        }

        const start = Math.max(0, bestIndex - 50);
        const end = Math.min(content.length, start + maxLength);
        let snippet = content.substring(start, end);

        if (start > 0) snippet = '...' + snippet;
        if (end < content.length) snippet = snippet + '...';

        // Highlight search terms
        searchTerms.forEach(term => {
            const regex = new RegExp(`(${term})`, 'gi');
            snippet = snippet.replace(regex, '<mark>$1</mark>');
        });

        return snippet;
    }

    displayResults(results) {
        const container = document.getElementById('search-results');
        if (!container) return;

        if (results.length === 0) {
            container.innerHTML = '<div class="no-results">No results found</div>';
            return;
        }

        const html = results.map(result => `
            <div class="search-result" onclick="window.location.href='${result.url}'">
                <h4>${this.highlightTerms(result.title, this.getSearchTerms())}</h4>
                <div class="snippet">${result.snippet}</div>
                <div class="meta">
                    ${result.tags.map(tag => `<span class="tag">${tag}</span>`).join('')}
                    ${result.date ? `<span class="date">${new Date(result.date).toLocaleDateString()}</span>` : ''}
                </div>
            </div>
        `).join('');

        container.innerHTML = html;
        container.style.display = 'block';
    }

    highlightTerms(text, terms) {
        let highlighted = text;
        terms.forEach(term => {
            const regex = new RegExp(`(${term})`, 'gi');
            highlighted = highlighted.replace(regex, '<mark>$1</mark>');
        });
        return highlighted;
    }

    getSearchTerms() {
        const input = document.getElementById('search-input');
        return input ? input.value.toLowerCase().split(' ').filter(term => term.length > 0) : [];
    }

    hideResults() {
        const container = document.getElementById('search-results');
        if (container) {
            container.style.display = 'none';
        }
    }
}

// Initialize search when page loads
document.addEventListener('DOMContentLoaded', function() {
    new SearchEngine();
});

// Keyboard shortcuts
document.addEventListener('keydown', function(e) {
    // Ctrl/Cmd + K to focus search
    if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
        e.preventDefault();
        const searchInput = document.getElementById('search-input');
        if (searchInput) {
            searchInput.focus();
        }
    }
    
    // Escape to close search results
    if (e.key === 'Escape') {
        const resultsContainer = document.getElementById('search-results');
        if (resultsContainer) {
            resultsContainer.style.display = 'none';
        }
    }
});
