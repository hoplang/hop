// Hop Development Mode Bootstrap Script
// This script is loaded when an entrypoint is rendered in development mode
// It fetches the actual entrypoint HTML from the dev server and renders it
// It also handles hot module reloading by listening to SSE events

// Mark when script execution starts
performance.mark('hop-script-start');

// Lazy load Idiomorph only when needed for HMR
let Idiomorph = null;

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
async function morphDOM(html) {
    // Lazy load Idiomorph on first hot reload
    if (!Idiomorph) {
        const module = await import('./idiomorph.js');
        Idiomorph = module.default;
    }

    const parser = new DOMParser();
    const doc = parser.parseFromString(html, 'text/html');
    // Morph the entire document to enable head merging
    Idiomorph.morph(document.documentElement, doc.documentElement);
}

/**
 * Function to replace the entire document with new HTML
 * @param {string} html - The new HTML content to replace the document with
 */
function replaceDocument(html) {
    performance.mark('hop-parse-start');

    const parser = new DOMParser();
    const newDoc = parser.parseFromString(html, 'text/html');

    performance.mark('hop-parse-end');
    performance.measure('hop-parse', 'hop-parse-start', 'hop-parse-end');

    // Replace the entire document element
    document.documentElement.replaceWith(newDoc.documentElement);
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
    eventSource.onmessage = async function(event) {
        if (event.data === 'reload') {
            try {
                const html = await renderEntryPoint(cfg);
                await morphDOM(html);
            } catch (error) {
                console.error('Hot reload fetch error:', error);
            }
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

        // Start timing the initial render
        performance.mark('hop-fetch-start');

        // Fetch and render the initial component
        const html = await renderEntryPoint(cfg);

        performance.mark('hop-fetch-end');
        performance.measure('hop-fetch', 'hop-fetch-start', 'hop-fetch-end');

        // Time the document replacement
        performance.mark('hop-replace-start');

        // Replace the entire document on initial load (no need to morph)
        replaceDocument(html);

        performance.mark('hop-replace-end');
        performance.measure('hop-replace', 'hop-replace-start', 'hop-replace-end');

        // Log the performance metrics
        const fetchTime = performance.getEntriesByName('hop-fetch')[0].duration;
        const parseTime = performance.getEntriesByName('hop-parse')[0].duration;
        const replaceTime = performance.getEntriesByName('hop-replace')[0].duration;

        // Calculate time from script start to fetch start
        performance.measure('hop-script-to-fetch', 'hop-script-start', 'hop-fetch-start');
        const scriptToFetchTime = performance.getEntriesByName('hop-script-to-fetch')[0].duration;

        // Get absolute times from navigation start using performance.getEntriesByName
        const scriptStartTime = performance.getEntriesByName('hop-script-start')[0].startTime;
        const fetchEndTime = performance.getEntriesByName('hop-fetch-end')[0].startTime;
        const parseEndTime = performance.getEntriesByName('hop-parse-end')[0].startTime;
        const replaceEndTime = performance.getEntriesByName('hop-replace-end')[0].startTime;

        const replaceDomTime = replaceTime - parseTime;

        console.log(`[Hop Dev] Initial render performance:
  Script startup: +${scriptToFetchTime.toFixed(2)}ms (${scriptStartTime.toFixed(2)}ms)
  Fetch: +${fetchTime.toFixed(2)}ms (${fetchEndTime.toFixed(2)}ms)
  Parse HTML: +${parseTime.toFixed(2)}ms (${parseEndTime.toFixed(2)}ms)
  Replace DOM: +${replaceDomTime.toFixed(2)}ms (${replaceEndTime.toFixed(2)}ms)`);

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
