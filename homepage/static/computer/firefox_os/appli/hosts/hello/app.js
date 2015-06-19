var installBtn = document.getElementById('install-btn');

if (installBtn) {
	installBtn.style.display = 'none';
	alert(navigator.mozApps);

	if (navigator.mozApps) {
		alert("hige");
		installBtn.addEventListener('click', function() {
			alert("ooooo");
			navigator.mozApps.install(location.href + 'manifest.webapp');
			alert("poooo");
		}, false);

		var req = navigator.mozApps.getSelf();
		req.onerror = function () { alert(req.error.name); };
		req.onsuccess = function() {
			if (!req.result) {
				installBtn.style.display = 'block';
			}
		};
		alert("huge");
	}
}
