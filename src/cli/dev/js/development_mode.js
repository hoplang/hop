/* vim: set noexpandtab */
/**
 * Development mode
 *
 * This script is loaded when an entrypoint is rendered in development mode.
 * It fetches the actual entrypoint HTML from the dev server and renders it.
 * It also handles hot module reloading by listening to SSE events.
 */

const EVENT_SOURCE_URL = "/api/events";
const RENDER_URL_BASE = "/api/render";
const DEBOUNCE_DELAY_MS = 15;
const ERROR_OVERLAY_ID = "hop-error-overlay";
const ENTRYPOINT = window.location.pathname.split("/").pop();
const CACHE_KEY = `hop-html-cache-${ENTRYPOINT}`;

/**
 * The ID of the timer that debounces hot reloading.
 * @type {number | null}
 */
let reloadTimeoutId = null;

/**
 * Check if we have cached HTML in localStorage and render it immediately.
 */
function restoreHTMLFromCache() {
	const cachedHTML = localStorage.getItem(CACHE_KEY);

	if (cachedHTML) {
		performance.mark("parse-start");
		const parser = new DOMParser();
		const newDoc = parser.parseFromString(cachedHTML, "text/html");
		performance.mark("parse-end");
		document.documentElement.replaceWith(newDoc.documentElement);
		performance.mark("replace-end");
		const parseMeasure = performance.measure(
			"parse",
			"parse-start",
			"parse-end",
		);
		const replaceMeasure = performance.measure(
			"replace",
			"parse-end",
			"replace-end",
		);
		const total = parseMeasure.duration + replaceMeasure.duration;
		console.log(
			`[HOP] parse: ${parseMeasure.duration.toFixed(1)}ms, replace: ${replaceMeasure.duration.toFixed(1)}ms, total: ${total.toFixed(1)}ms (${performance.now().toFixed(1)}ms since page load)`,
		);
	}
}

/**
 * Shows the error overlay with pre-styled HTML from the server.
 * Uses Shadow DOM to isolate styles from the host page.
 *
 * @param {string} html - The pre-styled HTML to display
 */
function showErrorOverlay(html) {
	let overlay = document.getElementById(ERROR_OVERLAY_ID);
	if (!overlay) {
		overlay = document.createElement("div");
		overlay.id = ERROR_OVERLAY_ID;
		overlay.style.cssText = "position: fixed; inset: 0; z-index: 2147483647;";
		document.documentElement.appendChild(overlay);
	}
	const shadowRoot =
		overlay.shadowRoot ?? overlay.attachShadow({ mode: "open" });
	shadowRoot.innerHTML = html;
}

/**
 * Hides the error overlay if it exists.
 */
function hideErrorOverlay() {
	const overlay = document.getElementById(ERROR_OVERLAY_ID);
	if (overlay) {
		overlay.remove();
	}
}

/**
 * Fetches the entrypoint HTML from the dev server
 */
async function fetchEntryPoint() {
	const url = new URL(
		`${RENDER_URL_BASE}/${ENTRYPOINT}`,
		window.location.origin,
	);
	const response = await fetch(url);
	if (!response.ok) {
		const body = await response.text();
		throw new Error(body || `HTTP error! status: ${response.status}`);
	}
	return await response.text();
}

/**
 * Updates the DOM with new HTML using morphing.
 *
 * @param {string} html - The new HTML content to morph into the DOM
 */
async function morphDOM(html) {
	const { morph } = await import("./idiomorph.js");
	const parser = new DOMParser();
	const doc = parser.parseFromString(html, "text/html");
	morph(document.documentElement, doc.documentElement);
	localStorage.setItem(CACHE_KEY, html);
}

/**
 * Perform hot-reloading by fetching the latest version of the
 * entrypoint from the server and merging it with the current version
 * in the DOM.
 */
async function reload() {
	try {
		performance.mark("fetch-start");
		const html = await fetchEntryPoint();
		performance.mark("fetch-end");
		hideErrorOverlay();
		await morphDOM(html);
		performance.mark("morph-end");
		const fetchMeasure = performance.measure(
			"fetch",
			"fetch-start",
			"fetch-end",
		);
		const morphMeasure = performance.measure("morph", "fetch-end", "morph-end");
		const total = fetchMeasure.duration + morphMeasure.duration;
		console.log(
			`[HOP] fetch: ${fetchMeasure.duration.toFixed(1)}ms, morph: ${morphMeasure.duration.toFixed(1)}ms, total: ${total.toFixed(1)}ms`,
		);
	} catch (error) {
		if (error instanceof Error) {
			showErrorOverlay(error.message);
		}
	}
	reloadTimeoutId = null;
}

/**
 * Setup hot module reloading by connecting to the dev server's SSE
 * endpoint.
 */
function setupEventListener() {
	const eventSource = new EventSource(EVENT_SOURCE_URL);

	/** @param {MessageEvent} event */
	eventSource.onmessage = (event) => {
		if (event.data === "reload") {
			if (reloadTimeoutId) {
				clearTimeout(reloadTimeoutId);
			}
			reloadTimeoutId = setTimeout(reload, DEBOUNCE_DELAY_MS);
		}
	};

	/** @param {Event} event */
	eventSource.onerror = (event) => {
		console.log("Hot reload connection error:", event);
		setTimeout(() => {
			eventSource.close();
			location.reload();
		}, 1000);
	};

	/**
	 * Add a listener to close the event source.
	 *
	 * NOTE: This is important on chrome as not closing the event source
	 * will leave it open even when the user navigates away which
	 * will lead to resource exhaustion.
	 */
	window.addEventListener("beforeunload", () => {
		eventSource.close();
		if (reloadTimeoutId) {
			clearTimeout(reloadTimeoutId);
		}
	});
}

restoreHTMLFromCache();
reload();
setupEventListener();

