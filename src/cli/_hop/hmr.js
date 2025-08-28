import Idiomorph from '/_hop/idiomorph.js';

const eventSource = new EventSource('/_hop/event_source');
eventSource.onmessage = function(event) {
    if (event.data === 'reload') {
        fetch(window.location.href)
            .then(response => response.text())
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