// Hop Development Mode Bootstrap Script
// This script is loaded when an entrypoint is rendered in development mode
// It fetches the actual entrypoint HTML from the dev server and renders it
// It also handles hot module reloading by listening to SSE events

import Idiomorph from './idiomorph.js';

/**
 * @typedef {object} Config
 *
 * @property {string} entrypoint
 * @property {any} params
 */

/** @returns {Config} */
function loadConfig() {
	let el = document.getElementById('hop-config');
	if (!el) {
		throw new Error('Element hop-config not found');
	}
	if (!el.textContent) {
		throw new Error('Text content of hop-config is empty');
	}
	return JSON.parse(el.textContent);
}

/**
 * Function to fetch the rendered entrypoint from the dev server
 * @param {Config} cfg
 */
async function renderEntryPoint(cfg) {
    const renderUrl = new URL('http://localhost:33861/render');
    renderUrl.searchParams.set('entrypoint', cfg.entrypoint);
    renderUrl.searchParams.set('params', JSON.stringify(cfg.params));
    
    const response = await fetch(renderUrl);
    if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    return await response.text();
}

/**
 * Function to update the DOM with new HTML using morphing
 * @param {string} html - The new HTML content to morph into the DOM
 */
function morphDOM(html) {
    const parser = new DOMParser();
    const doc = parser.parseFromString(html, 'text/html');
    // Morph the entire document to enable head merging
    Idiomorph.morph(document.documentElement, doc.documentElement);
}

/**
 * Setup hot module reloading by connecting to the dev server's SSE endpoint
 * @param {Config} cfg
 */
function setupHMR(cfg) {
    const eventSource = new EventSource('http://localhost:33861/_hop/event_source');
    
    /**
     * @param {MessageEvent} event
     */
    eventSource.onmessage = function(event) {
        if (event.data === 'reload') {
            renderEntryPoint(cfg)
                .then(html => morphDOM(html))
                .catch(error => {
                    console.error('Hot reload fetch error:', error);
                });
        }
    };
    
    eventSource.onerror = (event) => {
        console.log('Hot reload connection error:', event);
        setTimeout(() => {
            eventSource.close();
            location.reload();
        }, 1000);
    };
    
    window.addEventListener("beforeunload", () => {
		// This is important on chrome, not closing the event source
		// will leave it open even when the user navigates away.
        eventSource.close();
    });
}

/**
 * Main bootstrap function that initializes the development environment
 * Fetches the initial component, updates the DOM, and sets up HMR
 */
async function bootstrap() {
    try {
		// Parse configuration from JSON script tag
		const cfg = loadConfig();

        // Fetch and render the initial component
        const html = await renderEntryPoint(cfg);
        
        // Update the DOM with the fetched HTML
        morphDOM(html);
        
        // Setup hot module reloading after initial render
        setupHMR(cfg);
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
