// Knowledge Graph Visualization using D3.js
class KnowledgeGraph {
    constructor(containerId, data) {
        this.container = d3.select(containerId);
        this.data = data;
        this.width = 800;
        this.height = 600;
        this.margin = { top: 20, right: 20, bottom: 20, left: 20 };
        
        this.init();
    }

    init() {
        // Create SVG
        this.svg = this.container
            .append("svg")
            .attr("width", this.width)
            .attr("height", this.height)
            .style("background-color", "#f8f9fa");

        // Create zoom behavior
        this.zoom = d3.zoom()
            .scaleExtent([0.1, 4])
            .on("zoom", (event) => {
                this.g.attr("transform", event.transform);
            });

        this.svg.call(this.zoom);

        // Create main group
        this.g = this.svg.append("g");

        // Add arrow markers
        this.g.append("defs").selectAll("marker")
            .data(["end"])
            .enter().append("marker")
            .attr("id", String)
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 15)
            .attr("refY", -1.5)
            .attr("markerWidth", 6)
            .attr("markerHeight", 6)
            .attr("orient", "auto")
            .append("path")
            .attr("d", "M0,-5L10,0L0,5");

        this.render();
    }

    render() {
        // Create force simulation
        this.simulation = d3.forceSimulation(this.data.nodes)
            .force("link", d3.forceLink(this.data.links).id(d => d.id).distance(100))
            .force("charge", d3.forceManyBody().strength(-300))
            .force("center", d3.forceCenter(this.width / 2, this.height / 2));

        // Create links
        this.links = this.g.append("g")
            .selectAll("line")
            .data(this.data.links)
            .enter().append("line")
            .attr("stroke", "#999")
            .attr("stroke-opacity", 0.6)
            .attr("stroke-width", d => Math.sqrt(d.value))
            .attr("marker-end", "url(#end)");

        // Create nodes
        this.nodes = this.g.append("g")
            .selectAll("circle")
            .data(this.data.nodes)
            .enter().append("circle")
            .attr("r", d => Math.sqrt(d.connections) * 3 + 5)
            .attr("fill", d => this.getNodeColor(d))
            .attr("stroke", "#fff")
            .attr("stroke-width", 2)
            .call(this.drag());

        // Add labels
        this.labels = this.g.append("g")
            .selectAll("text")
            .data(this.data.nodes)
            .enter().append("text")
            .text(d => d.title)
            .attr("font-size", "12px")
            .attr("font-family", "Arial, sans-serif")
            .attr("text-anchor", "middle")
            .attr("dy", "0.35em");

        // Add click handlers
        this.nodes.on("click", (event, d) => {
            window.location.href = d.url;
        });

        // Update positions on simulation tick
        this.simulation.on("tick", () => {
            this.links
                .attr("x1", d => d.source.x)
                .attr("y1", d => d.source.y)
                .attr("x2", d => d.target.x)
                .attr("y2", d => d.target.y);

            this.nodes
                .attr("cx", d => d.x)
                .attr("cy", d => d.y);

            this.labels
                .attr("x", d => d.x)
                .attr("y", d => d.y);
        });
    }

    drag() {
        return d3.drag()
            .on("start", (event, d) => {
                if (!event.active) this.simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            })
            .on("drag", (event, d) => {
                d.fx = event.x;
                d.fy = event.y;
            })
            .on("end", (event, d) => {
                if (!event.active) this.simulation.alphaTarget(0);
                d.fx = null;
                d.fy = null;
            });
    }

    getNodeColor(node) {
        const colors = [
            "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
            "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"
        ];
        return colors[node.id.length % colors.length];
    }

    updateData(newData) {
        this.data = newData;
        this.render();
    }
}

// Initialize knowledge graph when page loads
document.addEventListener('DOMContentLoaded', function() {
    const graphContainer = document.getElementById('graph-container');
    if (graphContainer) {
        const pageId = graphContainer.dataset.pageId;
        
        // Fetch graph data (this would typically come from a Hugo data file)
        fetch('/api/graph-data.json')
            .then(response => response.json())
            .then(data => {
                // Filter data to show local graph around current page
                const localData = filterLocalGraph(data, pageId, 2);
                new KnowledgeGraph('#graph-container', localData);
            })
            .catch(error => {
                console.error('Error loading graph data:', error);
                // Fallback: create a simple graph with current page
                const fallbackData = {
                    nodes: [{ id: pageId, title: document.title, connections: 1 }],
                    links: []
                };
                new KnowledgeGraph('#graph-container', fallbackData);
            });
    }
});

// Filter function to show local graph around a specific page
function filterLocalGraph(data, centerId, depth) {
    const visited = new Set();
    const queue = [{ id: centerId, depth: 0 }];
    const nodes = new Map();
    const links = [];

    while (queue.length > 0) {
        const { id, depth } = queue.shift();
        if (visited.has(id) || depth > depth) continue;
        
        visited.add(id);
        const node = data.nodes.find(n => n.id === id);
        if (node) {
            nodes.set(id, node);
            
            if (depth < depth) {
                const nodeLinks = data.links.filter(l => l.source.id === id || l.target.id === id);
                nodeLinks.forEach(link => {
                    const otherId = link.source.id === id ? link.target.id : link.source.id;
                    if (!visited.has(otherId)) {
                        queue.push({ id: otherId, depth: depth + 1 });
                        links.push(link);
                    }
                });
            }
        }
    }

    return {
        nodes: Array.from(nodes.values()),
        links: links
    };
}
