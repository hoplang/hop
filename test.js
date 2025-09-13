function escapeHtml(str) {
    if (typeof str !== 'string') return str;
    return str
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

export function hop_error_pages_generic_error({ error }) {
    let output = "";
    output += "\n\t<!DOCTYPE html>\n\t<html>\n\t<head>\n\t\t<title>Error</title>\n\t\t<script src=\"https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4\"></script>\n\t\t<link href=\"https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap\" rel=\"stylesheet\">\n        \n\t</head>\n\t<body>\n\t\t<div data-hop-id=\"hop/ui/page-container\" class=\"max-w-6xl px-4 my-12 mx-auto\">\n\t\n\t\t\t<div data-hop-id=\"hop/error_pages/error-generic-error\" class=\"flex flex-col gap-4\">\n\t<div data-hop-id=\"hop/ui/heading-box\" class=\"border border-2 shadow-[4px_4px_rgba(0,0,0,0.1)]\">";
    const title = "Error";
    output += "\n\t<div class=\"p-3 py-2 flex justify-between border-b-2\">\n\t\t<div>\n\t\t\t<span class=\"font-medium uppercase\">\n\t\t\t  ";
    output += escapeHtml(title);
    output += "\n\t\t\t</span>\n\t\t</div>\n\t</div>\n\t<div class=\"p-5 gap-5 flex flex-col\" data-id=\"body\">\n\t\t\n\t\t\n\t\n\t</div>\n";
    output += "</div>\n</div>\n\t\t\n</div>\n\t</body>\n\t</html>\n";
    return output;
}

export function hop_error_pages_not_found_error({ path, available_routes }) {
    let output = "";
    output += "\n\t<!DOCTYPE html>\n\t<html>\n\t<head>\n\t\t<title>404 Not Found</title>\n\t\t<script src=\"https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4\"></script>\n\t\t<link href=\"https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap\" rel=\"stylesheet\">\n        \n\t</head>\n\t<body>\n\t\t<div data-hop-id=\"hop/ui/page-container\" class=\"max-w-6xl px-4 my-12 mx-auto\">\n\t\n\t\t\t<div data-hop-id=\"hop/error_pages/error-not-found-error\" class=\"flex flex-col gap-4\">";
    const requested_path = path;
    const available_routes_1 = available_routes;
    output += "\n\t<div data-hop-id=\"hop/ui/heading-box\" class=\"border border-2 shadow-[4px_4px_rgba(0,0,0,0.1)]\">";
    const title_2 = "Error - Route not found";
    output += "\n\t<div class=\"p-3 py-2 flex justify-between border-b-2\">\n\t\t<div>\n\t\t\t<span class=\"font-medium uppercase\">\n\t\t\t  ";
    output += escapeHtml(title_2);
    output += "\n\t\t\t</span>\n\t\t</div>\n\t</div>\n\t<div class=\"p-5 gap-5 flex flex-col\" data-id=\"body\">\n\t\t\n\t\t<div>\n\t\tThe requested route <span data-hop-id=\"hop/error_pages/code-text\">\n\t<code class=\"italic\">";
    output += escapeHtml(requested_path);
    output += "</code>\n</span> was not found in the build file.\n\t\t</div>\n\t\tAvailable routes:<br>\n\t\t<ul class=\"ml-6\" style=\"list-style-type: square;\">\n\t\t\t";
    for (const route of available_routes_1) {
        output += "\n\t\t\t\t<li><a href=\"";
        output += escapeHtml(route);
        output += "\">";
        output += escapeHtml(route);
        output += "</a></li>\n\t\t\t";
    }
    output += "\n\t\t</ul>\n\t\t<p>\n\t\t\tTo add this route, update your <span data-hop-id=\"hop/error_pages/code-text\">\n\t<code class=\"italic\">build.hop</code>\n</span> file with an entry for this path.\n\t\t</p>\n\t\n\t</div>\n";
    output += "</div>\n";
    output += "</div>\n\t\t\n</div>\n\t</body>\n\t</html>\n";
    return output;
}

