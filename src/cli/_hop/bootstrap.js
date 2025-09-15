// Hop Development Mode Bootstrap Script
// This script is loaded when a component is rendered in development mode
// It fetches the actual component HTML from the dev server and renders it
// It also handles hot module reloading by listening to SSE events

import Idiomorph from '/_hop/idiomorph.js';

// Parse query parameters from script src
const scriptTag = document.currentScript || document.querySelector('script[src*="bootstrap.js"]');
const url = new URL(scriptTag.src);
const entrypoint = url.searchParams.get('entrypoint');
const paramsEncoded = url.searchParams.get('params');

// Decode the parameters
let params = {};
if (paramsEncoded) {
    try {
        params = JSON.parse(decodeURIComponent(paramsEncoded));
    } catch (e) {
        console.error('Failed to parse parameters:', e);
    }
}

// Function to fetch the rendered component
async function fetchComponent() {
    const renderUrl = new URL('http://localhost:33861/render');
    renderUrl.searchParams.set('entrypoint', entrypoint);
    renderUrl.searchParams.set('params', JSON.stringify(params));
    
    const response = await fetch(renderUrl);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    return await response.text();
}

// Setup hot module reloading
function setupHMR() {
    const eventSource = new EventSource('http://localhost:33861/_hop/event_source');
    
    eventSource.onmessage = function(event) {
        if (event.data === 'reload') {
            fetchComponent()
                .then(html => {
                    const parser = new DOMParser();
                    const doc = parser.parseFromString(html, 'text/html');
                    // Morph the entire document to enable head merging
                    Idiomorph.morph(document.documentElement, doc.documentElement, {
                        head: {
                            shouldPreserve: function(elt) {
                                // Preserve elements with im-preserve attribute (default behavior)
                                if (elt.getAttribute('im-preserve') === 'true') {
                                    return true;
                                }
                                // Preserve Tailwind CSS style tags
                                if (elt.tagName === 'STYLE' && elt.textContent && 
                                    elt.textContent.includes('/*! tailwindcss')) {
                                    return true;
                                }
                                return false;
                            }
                        }
                    });
                })
                .catch(error => {
                    console.error('Hot reload fetch error:', error);
                });
        }
    };
    
    eventSource.onerror = function(event) {
        console.log('Hot reload connection error:', event);
        setTimeout(() => {
            eventSource.close();
            location.reload();
        }, 1000);
    };
    
    window.addEventListener("beforeunload", function() {
        // This is important on chrome, not closing the event source will leave it open even when the
        // user navigates away.
        eventSource.close();
    });
}

// Main bootstrap function
async function bootstrap() {
    try {
        // Fetch and render the initial component
        const html = await fetchComponent();
        
        // Write the HTML to the document
        document.write(html);
        document.close();
        
        // Setup hot module reloading after initial render
        setupHMR();
    } catch (error) {
        console.error('Failed to load from development server:', error);
        
        // Show error message
        document.write(`
            <div style="font-family: monospace; color: #dc2626; padding: 2rem; max-width: 800px; margin: 2rem auto; border: 2px solid #dc2626; border-radius: 8px; background-color: #fef2f2;">
                <h2 style="margin-top: 0; font-size: 1.5rem;">🚨 Development Server Error</h2>
                <p style="font-size: 1rem; line-height: 1.5;">Failed to connect to <strong>localhost:33861</strong></p>
                <p style="font-size: 0.875rem; color: #7f1d1d;">Make sure the Hop development server is running:</p>
                <pre style="background-color: #1e293b; color: #f8fafc; padding: 1rem; border-radius: 4px; overflow-x: auto;">hop dev</pre>
                <details style="margin-top: 1rem;">
                    <summary style="cursor: pointer; font-size: 0.875rem; color: #7f1d1d;">Error Details</summary>
                    <pre style="margin-top: 0.5rem; padding: 0.5rem; background-color: white; border: 1px solid #e5e7eb; border-radius: 4px; font-size: 0.75rem; overflow-x: auto;">${error.message}</pre>
                </details>
            </div>
        `);
        document.close();
    }
}

// Execute the bootstrap function
bootstrap();