# Makefile for Hugo Knowledge Base
.PHONY: help build serve clean deploy install-deps knowledge-build

# Default target
help:
	@echo "Available targets:"
	@echo "  build          - Build the Hugo site"
	@echo "  serve          - Start Hugo development server"
	@echo "  clean          - Clean build artifacts"
	@echo "  knowledge-build - Build knowledge base features (search, backlinks, graph)"
	@echo "  deploy         - Deploy to Hostinger VPS"
	@echo "  install-deps   - Install Python dependencies"
	@echo "  full-build     - Complete build with knowledge base features"

# Install Python dependencies
install-deps:
	python3 -m venv ./venv
	./venv/bin/pip install python-frontmatter

# Build knowledge base features
knowledge-build: install-deps
	./venv/bin/python scripts/knowledge-builder.py

# Build Hugo site
build: knowledge-build
	hugo --minify

# Full build with all features
full-build: clean knowledge-build build

# Start development server
serve:
	hugo server --bind 0.0.0.0 --port 1313 --disableFastRender

# Clean build artifacts
clean:
	rm -rf public/
	rm -rf resources/_gen/

# Deploy to Hostinger VPS (requires SSH key setup)
deploy: full-build
	@echo "Deploying to Hostinger VPS..."
	rsync -avz --delete public/ root@72.60.32.111:/var/www/notes-agostino/public/
	@echo "Deployment complete!"

# Development workflow
dev: knowledge-build serve

# Production build
prod: full-build
	@echo "Production build complete!"
	@echo "Files ready in public/ directory"

# Test build
test: build
	@echo "Testing build..."
	python3 -m http.server 8000 --directory public
	@echo "Test server running at http://localhost:8000"
