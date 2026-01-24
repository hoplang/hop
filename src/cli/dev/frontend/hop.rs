fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

pub struct OverlayParams<'a> {
    pub message: &'a str,
}

pub fn overlay(params: OverlayParams<'_>) -> String {
    let message = params.message;
    let mut output = String::new();
    if (std::env::var("HOP_DEV_MODE".to_string()).unwrap_or_default() == "enabled".to_string()) {
        output.push_str("<!DOCTYPE html>\n");
        output.push_str("<script type=\"application/json\">{\"module\": \"index\", \"component\": \"Overlay\", \"params\": ");
        output.push_str("{\"message\":");
        output.push_str(&serde_json::to_string(&message).unwrap());
        output.push_str("}}</script>\n<script src=\"http://localhost:");
        output.push_str(&std::env::var("HOP_DEV_PORT".to_string()).unwrap_or_default());
        output.push_str("/development_mode.js\"></script>");
    } else {
        output.push_str("<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta");
        output.push_str(" content=\"width=device-width, initial-scale=1\"");
        output.push_str(" name=\"viewport\"><style>");
        output.push_str("/*! tailwindcss v4.1.13 | MIT License | https://tailwindcss.com */\n@layer properties{@supports (((-webkit-hyphens:none)) and (not (margin-trim:inline))) or ((-moz-orient:inline) and (not (color:rgb(from red r g b)))){*,:before,:after,::backdrop{--tw-border-style:solid;--tw-leading:initial}}}@layer theme{:root,:host{--font-sans:ui-sans-serif,system-ui,sans-serif,\"Apple Color Emoji\",\"Segoe UI Emoji\",\"Segoe UI Symbol\",\"Noto Color Emoji\";--font-mono:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,\"Liberation Mono\",\"Courier New\",monospace;--color-white:#fff;--spacing:.25rem;--container-4xl:56rem;--radius-md:.375rem;--default-font-family:var(--font-sans);--default-mono-font-family:var(--font-mono)}}@layer base{*,:after,:before,::backdrop{box-sizing:border-box;border:0 solid;margin:0;padding:0}::file-selector-button{box-sizing:border-box;border:0 solid;margin:0;padding:0}html,:host{-webkit-text-size-adjust:100%;tab-size:4;line-height:1.5;font-family:var(--default-font-family,ui-sans-serif,system-ui,sans-serif,\"Apple Color Emoji\",\"Segoe UI Emoji\",\"Segoe UI Symbol\",\"Noto Color Emoji\");font-feature-settings:var(--default-font-feature-settings,normal);font-variation-settings:var(--default-font-variation-settings,normal);-webkit-tap-highlight-color:transparent}hr{height:0;color:inherit;border-top-width:1px}abbr:where([title]){-webkit-text-decoration:underline dotted;text-decoration:underline dotted}h1,h2,h3,h4,h5,h6{font-size:inherit;font-weight:inherit}a{color:inherit;-webkit-text-decoration:inherit;-webkit-text-decoration:inherit;-webkit-text-decoration:inherit;text-decoration:inherit}b,strong{font-weight:bolder}code,kbd,samp,pre{font-family:var(--default-mono-font-family,ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,\"Liberation Mono\",\"Courier New\",monospace);font-feature-settings:var(--default-mono-font-feature-settings,normal);font-variation-settings:var(--default-mono-font-variation-settings,normal);font-size:1em}small{font-size:80%}sub,sup{vertical-align:baseline;font-size:75%;line-height:0;position:relative}sub{bottom:-.25em}sup{top:-.5em}table{text-indent:0;border-color:inherit;border-collapse:collapse}:-moz-focusring{outline:auto}progress{vertical-align:baseline}summary{display:list-item}ol,ul,menu{list-style:none}img,svg,video,canvas,audio,iframe,embed,object{vertical-align:middle;display:block}img,video{max-width:100%;height:auto}button,input,select,optgroup,textarea{font:inherit;font-feature-settings:inherit;font-variation-settings:inherit;letter-spacing:inherit;color:inherit;opacity:1;background-color:#0000;border-radius:0}::file-selector-button{font:inherit;font-feature-settings:inherit;font-variation-settings:inherit;letter-spacing:inherit;color:inherit;opacity:1;background-color:#0000;border-radius:0}:where(select:is([multiple],[size])) optgroup{font-weight:bolder}:where(select:is([multiple],[size])) optgroup option{padding-inline-start:20px}::file-selector-button{margin-inline-end:4px}::placeholder{opacity:1}@supports (not ((-webkit-appearance:-apple-pay-button))) or (contain-intrinsic-size:1px){::placeholder{color:currentColor}@supports (color:color-mix(in lab, red, red)){::placeholder{color:color-mix(in oklab,currentcolor 50%,transparent)}}}textarea{resize:vertical}::-webkit-search-decoration{-webkit-appearance:none}::-webkit-date-and-time-value{min-height:1lh;text-align:inherit}::-webkit-datetime-edit{display:inline-flex}::-webkit-datetime-edit-fields-wrapper{padding:0}::-webkit-datetime-edit{padding-block:0}::-webkit-datetime-edit-year-field{padding-block:0}::-webkit-datetime-edit-month-field{padding-block:0}::-webkit-datetime-edit-day-field{padding-block:0}::-webkit-datetime-edit-hour-field{padding-block:0}::-webkit-datetime-edit-minute-field{padding-block:0}::-webkit-datetime-edit-second-field{padding-block:0}::-webkit-datetime-edit-millisecond-field{padding-block:0}::-webkit-datetime-edit-meridiem-field{padding-block:0}::-webkit-calendar-picker-indicator{line-height:1}:-moz-ui-invalid{box-shadow:none}button,input:where([type=button],[type=reset],[type=submit]){appearance:button}::file-selector-button{appearance:button}::-webkit-inner-spin-button{height:auto}::-webkit-outer-spin-button{height:auto}[hidden]:where(:not([hidden=until-found])){display:none!important}}@layer components;@layer utilities{.absolute{position:absolute}.inset-0{inset:calc(var(--spacing)*0)}.mx-auto{margin-inline:auto}.flex{display:flex}.grid{display:grid}.hidden{display:none}.max-w-4xl{max-width:var(--container-4xl)}.flex-col{flex-direction:column}.justify-center{justify-content:center}.gap-6{gap:calc(var(--spacing)*6)}.overflow-auto{overflow:auto}.overflow-scroll{overflow:scroll}.border-t-5{border-top-style:var(--tw-border-style);border-top-width:5px}.border-t-\\[\\#ff6a6a\\]{border-top-color:#ff6a6a}.bg-\\[\\#201f1f\\]{background-color:#201f1f}.bg-\\[rgba\\(41\\,_39\\,_39\\,_0\\.98\\)\\]{background-color:#292727fa}.p-12{padding:calc(var(--spacing)*12)}.py-8{padding-block:calc(var(--spacing)*8)}.py-12{padding-block:calc(var(--spacing)*12)}.leading-none{--tw-leading:1;line-height:1}.text-\\[\\#ff6a6a\\]{color:#ff6a6a}.text-white{color:var(--color-white)}@media (min-width:48rem){.md\\:block{display:block}.md\\:grid-cols-\\[1fr_32px\\]{grid-template-columns:1fr 32px}.md\\:rounded-md{border-radius:var(--radius-md)}.md\\:px-14{padding-inline:calc(var(--spacing)*14)}}}@property --tw-border-style{syntax:\"*\";inherits:false;initial-value:solid}@property --tw-leading{syntax:\"*\";inherits:false}");
        output.push_str("</style></head><body><div class=\"");
        output.push_str("bg-[rgba(41,_39,_39,_0.98)] absolute inset-0 overflow-auto\"");
        output.push_str("><div class=\"");
        output.push_str("py-12 md:px-14 max-w-4xl mx-auto flex flex-col gap-6\"><div");
        output.push_str(" class=\"flex justify-center\"><pre class=\"");
        output.push_str("leading-none text-[#ff6a6a]\">");
        output.push_str("   ___  ______________  _____ \n ");
        output.push_str(" / _ \\/ ___/ ___/ __ \\/ ___/ \n ");
        output.push_str("/  __/ /  / /  / /_/ / /     \n ");
        output.push_str("\\___/_/  /_/   \\____/_/      \n</pre></div><div class=\"");
        output.push_str("bg-[#201f1f] p-12 py-8 md:rounded-md border-t-[#ff6a6a] border-t-5 text-white grid md:grid-cols-[1fr_32px]");
        output.push_str("\"><pre class=\"overflow-scroll\">");
        output.push_str(&escape_html(&message));
        output.push_str("</pre><svg class=\"hidden md:block text-[#ff6a6a]\"");
        output.push_str(" xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"32\"");
        output.push_str(" fill=\"currentColor\" viewBox=\"0 0 256 256\"><path");
        output.push_str(" d=\"M128,24A104,104,0,1,0,232,128,104.11,104.11,0,0,0,128,24Zm0,192a88,88,0,1,1,88-88A88.1,88.1,0,0,1,128,216Zm-8-80V80a8,8,0,0,1,16,0v56a8,8,0,0,1-16,0Zm20,36a12,12,0,1,1-12-12A12,12,0,0,1,140,172Z\"");
        output.push_str("></path></svg></div></div></div></body></html>");
    }
    output
}
