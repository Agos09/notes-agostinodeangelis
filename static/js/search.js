/**
 * Advanced Search Functionality for Hugo Site
 * Provides grep-like search capabilities with content indexing
 */

class SiteSearch {
    constructor() {
        this.searchIndex = null;
        this.searchResults = [];
        this.isLoaded = false;
        this.searchContainer = null;
        this.resultsContainer = null;
        this.searchInput = null;
        
        this.init();
    }
    
    async init() {
        console.log('🔍 Initializing SiteSearch...');
        await this.loadSearchIndex();
        this.createSearchInterface();
        this.bindEvents();
        console.log('✅ SiteSearch initialized successfully');
    }
    
    async loadSearchIndex() {
        try {
            const response = await fetch('/search-index.json');
            this.searchIndex = await response.json();
            this.isLoaded = true;
            console.log(`🔍 Search index loaded: ${this.searchIndex.total_pages} pages`);
        } catch (error) {
            console.error('Failed to load search index:', error);
            this.showError('Search index not available');
        }
    }
    
    createSearchInterface() {
        // Find existing search container in header
        this.searchContainer = document.querySelector('.search-container');
        
        if (!this.searchContainer) {
            console.error('Search container not found in header');
            return;
        }
        
        // Get references to existing elements
        this.searchInput = document.getElementById('search-input');
        this.resultsContainer = document.getElementById('search-results');
        this.resultsList = this.resultsContainer.querySelector('.search-results-list');
        this.resultsCount = this.resultsContainer.querySelector('.results-count');
        this.suggestionsContainer = document.getElementById('search-suggestions');
        
        if (!this.searchInput || !this.resultsContainer) {
            console.error('Required search elements not found');
            return;
        }
    }
    
    bindEvents() {
        if (!this.searchInput) return;
        
        // Search input events
        this.searchInput.addEventListener('input', (e) => {
            this.handleSearch(e.target.value);
        });
        
        this.searchInput.addEventListener('keydown', (e) => {
            if (e.key === 'Escape') {
                this.clearSearch();
            }
        });
        
        // Filter events
        const filters = document.querySelectorAll('.search-filters input');
        filters.forEach(filter => {
            filter.addEventListener('change', () => {
                this.handleSearch(this.searchInput.value);
            });
        });
        
        // Clear button
        const clearBtn = this.searchContainer.querySelector('.search-clear');
        clearBtn.addEventListener('click', () => {
            this.clearSearch();
        });
    }
    
    handleSearch(query) {
        console.log('🔍 Searching for:', query);
        if (!this.isLoaded || !query.trim()) {
            this.hideResults();
            return;
        }
        
        const results = this.searchContent(query);
        console.log('📊 Found', results.length, 'results');
        this.displayResults(results, query);
        this.updateSuggestions(query);
    }
    
    searchContent(query) {
        if (!this.searchIndex) return [];
        
        const searchTerms = query.toLowerCase().split(/\s+/).filter(term => term.length > 0);
        const results = [];
        
        // Get active filters
        const showPosts = document.getElementById('filter-posts')?.checked ?? true;
        const showMocs = document.getElementById('filter-mocs')?.checked ?? true;
        const showPages = document.getElementById('filter-pages')?.checked ?? true;
        
        for (const page of this.searchIndex.pages) {
            // Apply filters
            const isPost = page.file_path.startsWith('posts/');
            const isMoc = page.file_path.startsWith('mocs/');
            const isPage = !isPost && !isMoc;
            
            if ((isPost && !showPosts) || (isMoc && !showMocs) || (isPage && !showPages)) {
                continue;
            }
            
            const score = this.calculateRelevanceScore(page, searchTerms);
            if (score > 0) {
                results.push({ ...page, score });
            }
        }
        
        // Sort by relevance score
        return results.sort((a, b) => b.score - a.score);
    }
    
    calculateRelevanceScore(page, searchTerms) {
        let score = 0;
        const title = page.title.toLowerCase();
        const content = page.content.toLowerCase();
        const tags = page.tags.join(' ').toLowerCase();
        const topics = page.topics.join(' ').toLowerCase();
        const domains = page.domains.join(' ').toLowerCase();
        const concepts = page.concepts.join(' ').toLowerCase();
        
        for (const term of searchTerms) {
            // Title matches (highest weight)
            if (title.includes(term)) {
                score += 10;
                if (title.startsWith(term)) score += 5;
            }
            
            // Tag matches (high weight)
            if (tags.includes(term)) {
                score += 8;
            }
            
            // Topic/domain matches (high weight)
            if (topics.includes(term) || domains.includes(term)) {
                score += 7;
            }
            
            // Concept matches (medium weight)
            if (concepts.includes(term)) {
                score += 6;
            }
            
            // Content matches (lower weight)
            const contentMatches = (content.match(new RegExp(term, 'g')) || []).length;
            score += contentMatches * 2;
            
            // Keyword matches (medium weight)
            if (page.keywords.includes(term)) {
                score += 5;
            }
        }
        
        return score;
    }
    
    displayResults(results, query) {
        if (results.length === 0) {
            this.showNoResults(query);
            return;
        }
        
        this.resultsCount.textContent = `${results.length} result${results.length !== 1 ? 's' : ''}`;
        
        const resultsHTML = results.map(result => this.createResultHTML(result, query)).join('');
        this.resultsList.innerHTML = resultsHTML;
        
        this.showResults();
    }
    
    createResultHTML(result, query) {
        const highlightedTitle = this.highlightText(result.title, query);
        const highlightedExcerpt = this.highlightText(result.excerpt, query);
        
        return `
            <div class="search-result-item">
                <h3 class="result-title">
                    <a href="${result.url}">${highlightedTitle}</a>
                </h3>
                <div class="result-meta">
                    <span class="result-date">${new Date(result.date).toLocaleDateString()}</span>
                    ${result.tags.length > 0 ? `<span class="result-tags">${result.tags.map(tag => `<span class="tag">${tag}</span>`).join('')}</span>` : ''}
                    ${result.topics.length > 0 ? `<span class="result-topics">${result.topics.map(topic => `<span class="topic">${topic}</span>`).join('')}</span>` : ''}
                </div>
                <div class="result-excerpt">${highlightedExcerpt}</div>
                <div class="result-score">Relevance: ${Math.round(result.score)}</div>
            </div>
        `;
    }
    
    highlightText(text, query) {
        if (!query.trim()) return text;
        
        const terms = query.toLowerCase().split(/\s+/).filter(term => term.length > 0);
        let highlightedText = text;
        
        terms.forEach(term => {
            const regex = new RegExp(`(${term})`, 'gi');
            highlightedText = highlightedText.replace(regex, '<mark>$1</mark>');
        });
        
        return highlightedText;
    }
    
    updateSuggestions(query) {
        if (!query.trim() || query.length < 2) {
            this.suggestionsContainer.style.display = 'none';
            return;
        }
        
        const suggestions = this.getSuggestions(query);
        if (suggestions.length === 0) {
            this.suggestionsContainer.style.display = 'none';
            return;
        }
        
        const suggestionsHTML = suggestions.map(suggestion => 
            `<div class="suggestion-item" data-query="${suggestion}">${this.highlightText(suggestion, query)}</div>`
        ).join('');
        
        this.suggestionsContainer.innerHTML = suggestionsHTML;
        this.suggestionsContainer.style.display = 'block';
        
        // Bind suggestion clicks
        this.suggestionsContainer.querySelectorAll('.suggestion-item').forEach(item => {
            item.addEventListener('click', () => {
                this.searchInput.value = item.dataset.query;
                this.handleSearch(item.dataset.query);
                this.suggestionsContainer.style.display = 'none';
            });
        });
    }
    
    getSuggestions(query) {
        const suggestions = new Set();
        const queryLower = query.toLowerCase();
        
        for (const page of this.searchIndex.pages) {
            // Add matching titles
            if (page.title.toLowerCase().includes(queryLower)) {
                suggestions.add(page.title);
            }
            
            // Add matching tags
            page.tags.forEach(tag => {
                if (tag.toLowerCase().includes(queryLower)) {
                    suggestions.add(tag);
                }
            });
            
            // Add matching topics
            page.topics.forEach(topic => {
                if (topic.toLowerCase().includes(queryLower)) {
                    suggestions.add(topic);
                }
            });
            
            // Add matching concepts
            page.concepts.forEach(concept => {
                if (concept.toLowerCase().includes(queryLower)) {
                    suggestions.add(concept);
                }
            });
        }
        
        return Array.from(suggestions).slice(0, 5);
    }
    
    showResults() {
        this.resultsContainer.style.display = 'block';
        this.searchContainer.querySelector('.search-clear').style.display = 'block';
    }
    
    hideResults() {
        this.resultsContainer.style.display = 'none';
        this.suggestionsContainer.style.display = 'none';
        this.searchContainer.querySelector('.search-clear').style.display = 'none';
    }
    
    showNoResults(query) {
        this.resultsCount.textContent = 'No results found';
        this.resultsList.innerHTML = `
            <div class="no-results">
                <p>No content found for "<strong>${query}</strong>"</p>
                <div class="search-tips">
                    <h4>Search Tips:</h4>
                    <ul>
                        <li>Try different keywords</li>
                        <li>Check spelling</li>
                        <li>Use broader terms</li>
                        <li>Try related concepts</li>
                    </ul>
                </div>
            </div>
        `;
        this.showResults();
    }
    
    clearSearch() {
        this.searchInput.value = '';
        this.hideResults();
        this.searchInput.focus();
    }
    
    showError(message) {
        if (this.resultsContainer) {
            this.resultsCount.textContent = 'Error';
            this.resultsList.innerHTML = `<div class="search-error">${message}</div>`;
            this.showResults();
        }
    }
}

// Initialize search when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new SiteSearch();
});