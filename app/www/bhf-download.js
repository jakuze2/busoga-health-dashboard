/* Download any visual on the dashboard as a labelled PNG.
   Adds a small download button to every card that holds a chart, map or table, to every
   overview tile and to the headline strips. The image includes the card's own title, the
   area and period selected on the page, the source and the download date. */
(function () {
  "use strict";
  var ICON = '<svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true"><path fill="currentColor" d="M8 1a.75.75 0 0 1 .75.75v6.69l2.22-2.22a.75.75 0 1 1 1.06 1.06l-3.5 3.5a.75.75 0 0 1-1.06 0l-3.5-3.5a.75.75 0 1 1 1.06-1.06l2.22 2.22V1.75A.75.75 0 0 1 8 1Zm-6 10a.75.75 0 0 1 .75.75v1.5c0 .14.11.25.25.25h10a.25.25 0 0 0 .25-.25v-1.5a.75.75 0 0 1 1.5 0v1.5A1.75 1.75 0 0 1 13 15H3a1.75 1.75 0 0 1-1.75-1.75v-1.5A.75.75 0 0 1 2 11Z"/></svg>';
  // charts, maps and tables (decorative icon SVGs are aria-hidden and do not count)
  var VISUAL = ".html-widget, .shiny-plot-output, .reactable, table, svg:not([aria-hidden='true'])";

  function text(el) { return el ? (el.innerText || el.textContent || "").replace(/\s+/g, " ").trim() : ""; }

  function titleOf(node) {
    var h = node.querySelector(".card-header, .kpi-label, .h-lab, h4, h5");
    if (h) { h = h.cloneNode(true); h.querySelectorAll(".sub, .bhf-dl").forEach(function (x) { x.remove(); }); }
    var t = h ? (h.textContent || "").replace(/\s+/g, " ").trim() : "";
    if (!t) { var pane = node.closest(".tab-pane"); t = text(pane && pane.querySelector(".page-head h2")); }
    return t || "Busoga Health Forum chart";
  }

  // "District / City: Jinja · Period: Last 12 months (...)" from the page's own filter bar
  function contextOf(node) {
    var pane = node.closest(".tab-pane") || document;
    var bar = pane.querySelector(".filter-bar"); if (!bar) return "";
    var parts = [];
    bar.querySelectorAll(".form-group, .shiny-input-container").forEach(function (g) {
      if (g.offsetParent === null) return;                       // hidden (e.g. custom dates not in use)
      var l = g.querySelector("label");
      var lab = l ? (l.textContent || "").replace(/\s+/g, " ").trim() : ""; var sel = g.querySelector("select");
      if (!lab || !sel || sel.selectedIndex < 0) return;
      var v = text(sel.options[sel.selectedIndex]);
      if (!v || v === "All" || v === "Custom range...") return;
      parts.push(lab + ": " + v);
    });
    return parts.join(" · ");
  }

  function slug(s) { return s.toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^_|_$/g, "").slice(0, 60) || "chart"; }

  function footer(node) {
    var f = document.createElement("div");
    f.className = "bhf-dl-foot";
    var ctx = contextOf(node);
    var d = new Date().toLocaleDateString("en-GB", { day: "numeric", month: "short", year: "numeric" });
    f.innerHTML = (ctx ? "<div>" + ctx.replace(/</g, "&lt;") + "</div>" : "") +
      "<div>Source: Uganda Ministry of Health DHIS2 (HMIS) and open datasets · Busoga Health Forum Dashboard · downloaded " + d + "</div>";
    return f;
  }

  function save(url, name) {
    var a = document.createElement("a"); a.href = url; a.download = name + ".png";
    document.body.appendChild(a); a.click(); a.remove();
  }

  function capture(node, btn) {
    if (!window.htmlToImage) { alert("Download is still loading, please try again."); return; }
    var name = slug(titleOf(node)) + "_" + new Date().toISOString().slice(0, 10);
    var foot = footer(node); node.appendChild(foot);
    node.classList.add("bhf-capturing");
    if (btn) btn.classList.add("busy");
    var skip = function (el) {
      return !(el.classList && (el.classList.contains("bhf-dl") || el.classList.contains("modebar-container") ||
               el.classList.contains("bslib-full-screen-enter") || el.classList.contains("leaflet-control-container")));
    };
    var opts = { backgroundColor: "#ffffff", pixelRatio: 2, cacheBust: true, filter: skip };
    var done = function () { foot.remove(); node.classList.remove("bhf-capturing"); if (btn) btn.classList.remove("busy"); };
    htmlToImage.toPng(node, opts).then(function (url) { save(url, name); done(); })
      .catch(function () {
        // map tiles from other servers can block capture: retry without the basemap
        var o2 = Object.assign({}, opts, { filter: function (el) { return skip(el) && !(el.classList && el.classList.contains("leaflet-tile-pane")); } });
        htmlToImage.toPng(node, o2).then(function (url) { save(url, name); done(); })
          .catch(function (e) { done(); console.error("download failed", e); alert("Sorry, this visual could not be saved as an image."); });
      });
  }
  window.bhfCapture = capture;

  function addButton(node) {
    if (node.dataset.bhfDl) return;
    if (!node.matches(".kpi, .hero") && !node.querySelector(VISUAL)) return;
    node.dataset.bhfDl = "1";
    var b = document.createElement("button");
    b.type = "button"; b.className = "bhf-dl"; b.title = "Download as image (PNG)"; b.setAttribute("aria-label", "Download as image");
    b.innerHTML = ICON;
    b.addEventListener("click", function (e) { e.preventDefault(); e.stopPropagation(); capture(node, b); });
    node.appendChild(b);
  }

  function scan() {
    document.querySelectorAll(".bslib-card, .card.themed, .kpi, .hero").forEach(function (n) {
      if (n.classList.contains("no-dl") || n.closest(".no-dl")) return;
      addButton(n);
    });
  }
  var pending = false;
  new MutationObserver(function () {
    if (pending) return; pending = true;
    setTimeout(function () { pending = false; scan(); }, 300);
  }).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", scan);
})();
