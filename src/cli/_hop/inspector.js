import Idiomorph from '/_hop/idiomorph.js';

const eventSource = new EventSource('/_hop/event_source');
eventSource.onmessage = async function(event) {
    if (event.data === 'reload') {
        // Reload all iframes
        const iframes = document.querySelectorAll('iframe');
        for (const iframe of iframes) {
            try {
                const previewUrl = iframe.src;
                const response = await fetch(previewUrl);
                const html = await response.text();
                if (iframe.contentDocument) {
                    // Parse the HTML and morph the entire iframe document
                    const parser = new DOMParser();
                    const doc = parser.parseFromString(html, 'text/html');
                    if (iframe.contentDocument && iframe.contentDocument.documentElement) {
                        Idiomorph.morph(iframe.contentDocument.documentElement, doc.documentElement, {
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
                    }
                }
            } catch (error) {
                console.error('Error reloading iframe:', error);
                // Fallback to full iframe reload
                iframe.src = iframe.src;
            }
        }
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
    eventSource.close();
});