# Deployment Guide for Hostinger VPS

This guide will help you deploy your networked knowledge base to your Hostinger VPS using Traefik as the reverse proxy.

**Target Domain**: `notes.agostinodeangelis.com`

## Prerequisites

- Hostinger VPS with root access
- Domain `agostinodeangelis.com` with DNS control
- SSH key pair for secure access
- Docker and Docker Compose installed on VPS

## Step 1: VPS Setup

### 1.1 Install Docker and Docker Compose

```bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker $USER

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Logout and login again to apply group changes
```

### 1.2 Create Project Directory Structure

Since your Hostinger VPS contains multiple projects, we'll organize them properly:

```bash
# Create main project directories
mkdir -p /var/www/notes-agostino
mkdir -p /var/www/agostino-main

# Navigate to the knowledge base project
cd /var/www/notes-agostino
```

**Directory Structure:**
```
/var/www/
├── agostino-main/              # Your main website
│   ├── public/
│   └── docker-compose.yml
├── notes-agostino/             # Your knowledge base (this project)
│   ├── public/
│   ├── traefik/
│   └── docker-compose.yml
└── [other-projects]/           # Your other existing projects
```

## Step 2: Traefik Configuration

### 2.1 Create Traefik Configuration

```bash
mkdir -p /var/www/notes-agostino/traefik
```

Create `/var/www/notes-agostino/traefik/traefik.yml`:

```yaml
api:
  dashboard: true
  insecure: true

entryPoints:
  web:
    address: ":80"
    http:
      redirections:
        entrypoint:
          to: websecure
          scheme: https
  websecure:
    address: ":443"

providers:
  docker:
    endpoint: "unix:///var/run/docker.sock"
    exposedByDefault: false

certificatesResolvers:
  letsencrypt:
    acme:
      email: agostino@agostinodeangelis.com
      storage: /letsencrypt/acme.json
      httpChallenge:
        entryPoint: web
```

### 2.2 Create Docker Compose Configuration

Create `/var/www/notes-agostino/docker-compose.yml`:

```yaml
version: '3.8'

services:
  traefik:
    image: traefik:v2.10
    container_name: traefik
    restart: unless-stopped
    ports:
      - "80:80"
      - "443:443"
      - "8080:8080"  # Traefik dashboard
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
      - ./traefik:/etc/traefik
      - letsencrypt:/letsencrypt
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.traefik.rule=Host(`traefik.agostinodeangelis.com`)"
      - "traefik.http.routers.traefik.tls=true"
      - "traefik.http.routers.traefik.tls.certresolver=letsencrypt"
      - "traefik.http.routers.traefik.service=api@internal"

  hugo-site:
    image: nginx:alpine
    container_name: notes-hugo-site
    restart: unless-stopped
    volumes:
      - ./public:/usr/share/nginx/html:ro
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.notes.rule=Host(`notes.agostinodeangelis.com`)"
      - "traefik.http.routers.notes.tls=true"
      - "traefik.http.routers.notes.tls.certresolver=letsencrypt"
      - "traefik.http.services.notes.loadbalancer.server.port=80"

volumes:
  letsencrypt:
```

**Note**: The container name is `notes-hugo-site` to avoid conflicts with other projects on your VPS.

## Step 3: GitHub Actions Setup

### 3.1 Repository Secrets

In your GitHub repository, go to Settings > Secrets and add:

- `HOSTINGER_HOST`: Your VPS IP address
- `HOSTINGER_USERNAME`: Your VPS username (usually `root`)
- `HOSTINGER_SSH_KEY`: Your private SSH key content

### 3.2 SSH Key Setup

```bash
# On your local machine, generate SSH key if you haven't
ssh-keygen -t rsa -b 4096 -C "your-email@example.com"

# Copy public key to VPS
ssh-copy-id root@your-vps-ip

# Test SSH connection
ssh root@your-vps-ip
```

## Step 4: Deployment Process

### 4.1 Manual Deployment

```bash
# Build the site locally
make full-build

# Deploy to VPS (targets /var/www/notes-agostino/public/)
make deploy
```

**Deployment Target**: The deployment will copy your local `public/` directory to `/var/www/notes-agostino/public/` on your VPS.

### 4.2 Automated Deployment

The GitHub Actions workflow will automatically:

1. Build the Hugo site
2. Generate knowledge base features
3. Deploy to your VPS
4. Restart Traefik

## Step 5: Domain Configuration

### 5.1 DNS Settings

Add the following DNS records to your `agostinodeangelis.com` domain:

```
A Record: notes.agostinodeangelis.com -> YOUR_VPS_IP
A Record: traefik.agostinodeangelis.com -> YOUR_VPS_IP
```

**Note**: Replace `YOUR_VPS_IP` with your actual Hostinger VPS IP address.

### 5.2 SSL Certificate

Traefik will automatically obtain SSL certificates from Let's Encrypt for both:
- `notes.agostinodeangelis.com` (your knowledge base)
- `traefik.agostinodeangelis.com` (Traefik dashboard)

### 5.3 Hugo Configuration

Ensure your Hugo configuration uses the correct base URL. In your `config.toml`:

```toml
baseURL = 'https://notes.agostinodeangelis.com/'
```

This ensures all internal links and assets are generated with the correct subdomain.

### 5.4 Integration with Main Website

To integrate your knowledge base with your main website (`agostinodeangelis.com`):

1. **Add navigation link** on your main site:
   ```html
   <a href="https://notes.agostinodeangelis.com">Knowledge Base</a>
   ```

2. **Add "About" link** on your knowledge base pointing to main site:
   ```html
   <a href="https://agostinodeangelis.com">About Agostino</a>
   ```

3. **Cross-domain analytics** (optional):
   - Use Google Analytics with cross-domain tracking
   - Set up Google Search Console for both domains

## Step 6: Monitoring and Maintenance

### 6.1 Check Deployment Status

```bash
# Check if containers are running
docker ps

# Check Traefik logs
docker logs traefik

# Check Hugo site logs (note the updated container name)
docker logs notes-hugo-site

# Check if files are deployed correctly
ls -la /var/www/notes-agostino/public/
```

### 6.2 Update Process

```bash
# Pull latest changes
git pull origin main

# Rebuild and redeploy
make full-build
make deploy

# Or use GitHub Actions (automatic)
git push origin main
```

## Step 7: Backup Strategy

### 7.1 Content Backup

```bash
# Backup content directory
tar -czf content-backup-$(date +%Y%m%d).tar.gz content/

# Backup to external storage
rsync -avz content/ backup-server:/backups/ip-spark/
```

### 7.2 Site Backup

```bash
# Backup entire site
tar -czf site-backup-$(date +%Y%m%d).tar.gz /var/www/notes-agostino/
```

## Troubleshooting

### Common Issues

1. **SSL Certificate Issues**
   ```bash
   # Check Let's Encrypt logs
   docker logs traefik | grep acme
   
   # Restart Traefik
   docker restart traefik
   ```

2. **Site Not Loading**
   ```bash
   # Check if files are deployed
   ls -la /var/www/notes-agostino/public/
   
   # Check Nginx logs (note the updated container name)
   docker logs notes-hugo-site
   ```

3. **Search Not Working**
   ```bash
   # Check if search index exists
   ls -la /var/www/notes-agostino/public/search-index.json
   
   # Rebuild knowledge base
   python3 scripts/knowledge-builder.py
   ```

### Performance Optimization

1. **Enable Gzip Compression**
   ```bash
   # Add to nginx config
   gzip on;
   gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;
   ```

2. **Set Cache Headers**
   ```bash
   # Add to nginx config
   location ~* \.(css|js|png|jpg|jpeg|gif|ico|svg)$ {
       expires 1y;
       add_header Cache-Control "public, immutable";
   }
   ```

## Security Considerations

1. **Firewall Setup**
   ```bash
   # Allow only necessary ports
   ufw allow 22    # SSH
   ufw allow 80    # HTTP
   ufw allow 443   # HTTPS
   ufw enable
   ```

2. **Regular Updates**
   ```bash
   # Update system packages
   apt update && apt upgrade -y
   
   # Update Docker images
   docker-compose pull
   docker-compose up -d
   ```

3. **Monitor Logs**
   ```bash
   # Set up log monitoring
   tail -f /var/log/syslog | grep traefik
   ```

## Success Checklist

- [ ] VPS setup with Docker and Traefik
- [ ] Project directory structure created (`/var/www/notes-agostino/`)
- [ ] Cloudflare DNS records configured (gray cloud for direct VPS access)
- [ ] Domain pointing to VPS
- [ ] SSL certificates working
- [ ] Site accessible via HTTPS at `https://notes.agostinodeangelis.com`
- [ ] Traefik dashboard accessible at `https://traefik.agostinodeangelis.com`
- [ ] Search functionality working
- [ ] Knowledge graph loading
- [ ] GitHub Actions deployment working
- [ ] Container names updated to avoid conflicts (`notes-hugo-site`)
- [ ] Backup strategy in place
- [ ] Monitoring and logging configured

## 🏗️ Multi-Project Architecture

Your VPS now supports multiple projects with this structure:

```
/var/www/
├── notes-agostino/             # This knowledge base project
│   ├── public/                 # Hugo static files
│   ├── traefik/                # Traefik configuration
│   └── docker-compose.yml      # Container orchestration
├── agostino-main/              # Your main website (future)
└── [other-projects]/           # Your existing projects
```

**Benefits:**
- ✅ **Project isolation** - Each project runs independently
- ✅ **Easy management** - Clear separation of concerns
- ✅ **Scalability** - Easy to add new projects
- ✅ **No conflicts** - Unique container names and configurations

Your networked knowledge base is now live! 🚀
