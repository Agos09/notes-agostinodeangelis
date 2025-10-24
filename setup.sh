#!/bin/bash
# IP Spark Setup Script
# This script sets up your networked knowledge base and blog

set -e

echo "🚀 Setting up IP Spark - Networked Knowledge Base & Blog"
echo "=================================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_header() {
    echo -e "${BLUE}[SETUP]${NC} $1"
}

# Check if we're in the right directory
if [ ! -f "config.toml" ]; then
    print_error "Please run this script from the Hugo site root directory"
    exit 1
fi

print_header "Checking prerequisites..."

# Check for Hugo
if ! command -v hugo &> /dev/null; then
    print_error "Hugo is not installed. Please install Hugo first:"
    echo "  - Visit: https://gohugo.io/installation/"
    echo "  - Or use: brew install hugo (macOS) / apt install hugo (Ubuntu)"
    exit 1
fi

# Check Hugo version
HUGO_VERSION=$(hugo version | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1)
print_status "Found Hugo $HUGO_VERSION"

# Check for Python
if ! command -v python3 &> /dev/null; then
    print_error "Python 3 is not installed. Please install Python 3.11+ first."
    exit 1
fi

PYTHON_VERSION=$(python3 --version | cut -d' ' -f2)
print_status "Found Python $PYTHON_VERSION"

# Check for Git
if ! command -v git &> /dev/null; then
    print_error "Git is not installed. Please install Git first."
    exit 1
fi

print_status "Found Git $(git --version | cut -d' ' -f3)"

print_header "Installing Python dependencies..."

# Install Python dependencies
if [ -f "requirements.txt" ]; then
    pip3 install -r requirements.txt
else
    pip3 install python-frontmatter
fi

print_status "Python dependencies installed"

print_header "Setting up directory structure..."

# Create necessary directories
mkdir -p content/mocs
mkdir -p layouts/shortcodes
mkdir -p layouts/_default
mkdir -p static/css
mkdir -p static/js
mkdir -p static/api
mkdir -p scripts
mkdir -p .github/workflows
mkdir -p .emacs.d

print_status "Directory structure created"

print_header "Building knowledge base features..."

# Run the knowledge builder
if [ -f "scripts/knowledge-builder.py" ]; then
    python3 scripts/knowledge-builder.py
    print_status "Knowledge base features built"
else
    print_warning "Knowledge builder script not found, skipping..."
fi

print_header "Building Hugo site..."

# Build the site
hugo --minify
print_status "Hugo site built successfully"

print_header "Setting up Git hooks..."

# Create pre-commit hook
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Pre-commit hook to build knowledge base features

echo "Building knowledge base features..."
python3 scripts/knowledge-builder.py

echo "Building Hugo site..."
hugo --minify

echo "Knowledge base build complete!"
EOF

chmod +x .git/hooks/pre-commit
print_status "Git hooks configured"

print_header "Creating development environment..."

# Create .env file for development
cat > .env << 'EOF'
# Development environment variables
HUGO_BASE_URL=http://localhost:1313
HUGO_ENV=development
ENABLE_SEARCH=true
ENABLE_GRAPH=true
ENABLE_BACKLINKS=true
EOF

print_status "Development environment configured"

print_header "Setting up Emacs integration..."

# Create Emacs configuration if it doesn't exist
if [ ! -f ".emacs.d/ox-hugo-config.el" ]; then
    print_warning "Emacs configuration already exists, skipping..."
else
    print_status "Emacs configuration created"
fi

print_header "Testing the setup..."

# Test the build
if [ -d "public" ]; then
    print_status "Build test successful - public directory created"
    
    # Check for key files
    if [ -f "public/search-index.json" ]; then
        print_status "Search index generated"
    else
        print_warning "Search index not found - run 'make knowledge-build'"
    fi
    
    if [ -f "public/api/graph-data.json" ]; then
        print_status "Knowledge graph data generated"
    else
        print_warning "Knowledge graph data not found - run 'make knowledge-build'"
    fi
else
    print_error "Build test failed - public directory not created"
    exit 1
fi

print_header "Setup complete! 🎉"

echo ""
echo "Next steps:"
echo "==========="
echo ""
echo "1. Configure your Emacs:"
echo "   Add this to your init.el:"
echo "   (load-file \"$(pwd)/.emacs.d/ox-hugo-config.el\")"
echo ""
echo "2. Start development server:"
echo "   make serve"
echo "   or"
echo "   hugo server --bind 0.0.0.0 --port 1313"
echo ""
echo "3. Create your first blog post:"
echo "   In Emacs: M-x org-roam-node-find"
echo "   Choose 'Blog Post' template"
echo ""
echo "4. Export to Hugo:"
echo "   C-c C-e H h"
echo ""
echo "5. Build and deploy:"
echo "   make deploy"
echo ""
echo "6. Configure GitHub Actions for automated deployment:"
echo "   - Set up repository secrets:"
echo "     - HOSTINGER_HOST"
echo "     - HOSTINGER_USERNAME" 
echo "     - HOSTINGER_SSH_KEY"
echo ""
echo "Available commands:"
echo "==================="
echo "make help          - Show all available commands"
echo "make serve         - Start development server"
echo "make build         - Build the site"
echo "make knowledge-build - Build knowledge base features"
echo "make full-build    - Complete build with all features"
echo "make deploy        - Deploy to Hostinger VPS"
echo ""
echo "Documentation:"
echo "============="
echo "README.md          - Complete documentation"
echo "config.toml        - Hugo configuration"
echo ".emacs.d/ox-hugo-config.el - Emacs integration"
echo ""
echo "Happy writing! 📝✨"
