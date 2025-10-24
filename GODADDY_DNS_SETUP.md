# Cloudflare DNS Configuration Guide

This guide will help you set up the subdomain `notes.agostinodeangelis.com` using Cloudflare DNS and point it to your Hostinger VPS.

## 🌐 **Current DNS Status**

Your domain `agostinodeangelis.com` is currently using **Cloudflare DNS**:
- **Nameservers**: `pedro.ns.cloudflare.com` and `dns.cloudflare.com`
- **Current IP**: `34.111.179.208` (Cloudflare proxy)
- **Subdomains**: `notes` and `traefik` need to be created

## 🌐 **Cloudflare DNS Configuration**

### **Step 1: Access Cloudflare DNS Management**

1. **Log into Cloudflare**
   - Go to [dash.cloudflare.com](https://dash.cloudflare.com)
   - Sign in to your account

2. **Navigate to DNS Management**
   - Find `agostinodeangelis.com` in your domain list
   - Click on the domain to access DNS settings

### **Step 2: Add Subdomain Records**

You need to add **two A records**:

#### **Record 1: Knowledge Base Subdomain**
```
Type: A
Name: notes
Content: YOUR_HOSTINGER_VPS_IP
Proxy status: DNS only (gray cloud)
TTL: Auto
```

#### **Record 2: Traefik Dashboard Subdomain**
```
Type: A
Name: traefik
Content: YOUR_HOSTINGER_VPS_IP
Proxy status: DNS only (gray cloud)
TTL: Auto
```

### **Step 3: Detailed Cloudflare Interface Steps**

1. **Click "Add record" button** in the DNS records section

2. **For the first record:**
   - **Type**: Select "A"
   - **Name**: Enter `notes`
   - **IPv4 address**: Enter your Hostinger VPS IP address
   - **Proxy status**: Click the gray cloud to make it "DNS only" (important!)
   - **TTL**: Leave as "Auto"
   - **Save**

3. **For the second record:**
   - **Type**: Select "A"
   - **Name**: Enter `traefik`
   - **IPv4 address**: Enter your Hostinger VPS IP address (same as above)
   - **Proxy status**: Click the gray cloud to make it "DNS only" (important!)
   - **TTL**: Leave as "Auto"
   - **Save**

### **⚠️ Important: Proxy Status**

**CRITICAL**: Make sure both records have **gray cloud** (DNS only) status, NOT orange cloud (proxied). This is essential because:
- Orange cloud = Traffic goes through Cloudflare proxy
- Gray cloud = Direct connection to your VPS
- Your VPS needs direct connections for SSL certificates and Traefik to work properly

## 🔍 **Finding Your Hostinger VPS IP**

If you don't know your VPS IP address:

1. **Log into Hostinger**
2. **Go to VPS Management**
3. **Find your VPS details** - the IP address will be listed there
4. **Or SSH into your VPS** and run:
   ```bash
   curl ifconfig.me
   ```

## ⏱️ **DNS Propagation**

After adding the records:

- **Propagation time**: 15 minutes to 48 hours
- **Typical time**: 1-2 hours
- **Check status**: Use [whatsmydns.net](https://www.whatsmydns.net) to monitor propagation

## 🧪 **Testing DNS Configuration**

### **Test Commands:**
```bash
# Test the subdomain resolution
nslookup notes.agostinodeangelis.com
nslookup traefik.agostinodeangelis.com

# Test from your VPS
dig notes.agostinodeangelis.com
dig traefik.agostinodeangelis.com
```

### **Expected Results:**
Both commands should return your Hostinger VPS IP address.

## 📋 **Complete DNS Records Summary**

Your Cloudflare DNS should have these records:

| Type | Name | Content | Proxy Status | TTL |
|------|------|---------|--------------|-----|
| A | @ | YOUR_VPS_IP | Gray cloud (DNS only) | Auto |
| A | www | YOUR_VPS_IP | Gray cloud (DNS only) | Auto |
| A | notes | YOUR_VPS_IP | Gray cloud (DNS only) | Auto |
| A | traefik | YOUR_VPS_IP | Gray cloud (DNS only) | Auto |

**Note**: The `@` and `www` records might already exist and point to Cloudflare's proxy (orange cloud). You can leave them as-is or change them to gray cloud if you want direct VPS access.

## 🚨 **Common Issues & Solutions**

### **Issue 1: Subdomain not resolving**
- **Solution**: Wait for DNS propagation (up to 48 hours)
- **Check**: Use `nslookup` or online DNS checker

### **Issue 2: Wrong IP address**
- **Solution**: Double-check your Hostinger VPS IP
- **Verify**: SSH into VPS and confirm IP with `ip addr show`

### **Issue 3: Cloudflare interface different**
- **Solution**: Look for "DNS" tab in your domain dashboard
- **Alternative**: Contact Cloudflare support for guidance

### **Issue 4: SSL certificate not working**
- **Solution**: Ensure DNS is fully propagated before deploying
- **Check**: Verify subdomain resolves to correct IP

## 🔧 **After DNS is Configured**

Once DNS propagation is complete:

1. **Deploy your site**:
   ```bash
   make deploy
   ```

2. **Test access**:
   - Visit `https://notes.agostinodeangelis.com`
   - Check Traefik dashboard at `https://traefik.agostinodeangelis.com`

3. **Verify SSL certificates**:
   - Traefik will automatically obtain Let's Encrypt certificates
   - Check browser shows green lock icon

## 📱 **Mobile Testing**

Test from different devices/networks to ensure DNS propagation is complete everywhere.

## 🎯 **Quick Checklist**

- [ ] Logged into Cloudflare
- [ ] Found DNS management for `agostinodeangelis.com`
- [ ] Added A record for `notes` → YOUR_VPS_IP (gray cloud)
- [ ] Added A record for `traefik` → YOUR_VPS_IP (gray cloud)
- [ ] Waited for DNS propagation
- [ ] Tested with `nslookup`
- [ ] Deployed site to VPS
- [ ] Verified HTTPS access

## 🔗 **Useful Links**

- [Cloudflare DNS Management](https://dash.cloudflare.com)
- [DNS Propagation Checker](https://www.whatsmydns.net)
- [Let's Encrypt Certificate Status](https://crt.sh/)

## 📞 **Support**

If you encounter issues:

1. **Cloudflare Support**: Contact Cloudflare for DNS-related issues
2. **Hostinger Support**: Contact Hostinger for VPS-related issues
3. **Technical Issues**: Check the main DEPLOYMENT.md guide

---

**Your subdomain will be live at `https://notes.agostinodeangelis.com` once DNS propagation is complete!** 🚀
