// Hop Development Mode Bootstrap Script
// This script is loaded when a component is rendered in development mode
// It fetches the actual component HTML from the dev server and renders it

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

// Main bootstrap function
async function bootstrap() {
    try {
        // Fetch the rendered component from the dev server
        const renderUrl = new URL('http://localhost:33861/render');
        renderUrl.searchParams.set('entrypoint', entrypoint);
        renderUrl.searchParams.set('params', JSON.stringify(params));
        
        const response = await fetch(renderUrl);
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        
        const html = await response.text();
        
        // Write the HTML to the document
        document.write(html);
        document.close();
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