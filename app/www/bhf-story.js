// Count-up animation for the big numbers on the "Busoga in focus" page. Runs once per number,
// when it first scrolls into view; honours "reduce motion".
(function () {
  function fmt(v, dec) { return v.toLocaleString('en-GB', { minimumFractionDigits: dec, maximumFractionDigits: dec }); }
  function run(el) {
    var end = parseFloat(el.getAttribute('data-count')); if (!isFinite(end)) return;
    var dec = parseInt(el.getAttribute('data-dec') || '0', 10), suf = el.getAttribute('data-suffix') || '';
    if (window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches) { el.textContent = fmt(end, dec) + suf; return; }
    var t0 = null, dur = 1400;
    function step(t) { if (!t0) t0 = t; var k = Math.min(1, (t - t0) / dur); k = 1 - Math.pow(1 - k, 3);
      el.textContent = fmt(end * k, dec) + suf; if (k < 1) requestAnimationFrame(step); }
    requestAnimationFrame(step);
  }
  window.bhfCountUp = function () {
    setTimeout(function () {
      var els = document.querySelectorAll('.st-num-v[data-count]:not([data-done])');
      var io = 'IntersectionObserver' in window ? new IntersectionObserver(function (es) {
        es.forEach(function (e) { if (e.isIntersecting) { e.target.setAttribute('data-done', '1'); run(e.target); io.unobserve(e.target); } });
      }, { threshold: 0.4 }) : null;
      els.forEach(function (el) { if (io) io.observe(el); else { el.setAttribute('data-done', '1'); run(el); } });
    }, 50);
  };
})();
