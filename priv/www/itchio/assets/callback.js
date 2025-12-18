const qs = window.location.hash.slice(1);
window.location = new URL("callback2?" + qs, window.location);
