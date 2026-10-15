// Privacy-friendly usage statistics (GoatCounter): page views per tab, country/region/city, device,
// and downloads. No cookies, no personal identifiers, no raw IP addresses stored. Does nothing until
// the GoatCounter address is set below, and never runs on localhost or when the browser sends
// "Do Not Track".
(function () {
  var ENDPOINT = "";   // e.g. "https://busogahealth.goatcounter.com/count"
  if (!ENDPOINT) return;
  var h = location.hostname;
  if (h === "localhost" || h === "127.0.0.1" || h.slice(-5) === ".test") return;
  if (navigator.doNotTrack === "1" || window.doNotTrack === "1") return;
  window.goatcounter = { no_onload: true, endpoint: ENDPOINT };
  var s = document.createElement("script");
  s.async = true; s.src = "https://gc.zgo.at/count.js";
  s.onload = function () { view(currentTab() || "overview"); };
  document.head.appendChild(s);

  function count(o) { try { window.goatcounter && window.goatcounter.count && window.goatcounter.count(o); } catch (e) {} }
  function view(tab) { count({ path: "/tab/" + tab, title: tab }); }
  function currentTab() { var a = document.querySelector(".navbar .nav-link.active[data-value], .navbar [data-value].active"); return a ? a.getAttribute("data-value") : null; }

  // every tab or menu page the visitor opens
  document.addEventListener("shown.bs.tab", function (e) {
    var v = e.target && e.target.getAttribute && e.target.getAttribute("data-value");
    if (v) view(v);
  });
  // downloads (CSV, Brief, PNG)
  document.addEventListener("click", function (e) {
    var a = e.target.closest && e.target.closest("a.shiny-download-link, .bhf-dl, [data-bhf-download]");
    if (a) count({ path: "/download/" + (a.id || a.getAttribute("data-bhf-download") || "file"), title: "Download", event: true });
  });
})();
