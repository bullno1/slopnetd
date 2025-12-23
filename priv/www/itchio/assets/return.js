if (document.readyState !== 'loading') {
	main();
} else {
	window.addEventListener("DOMContentLoaded", main);
}

function main () {
	const data = JSON.parse(document.querySelector("#data").innerText);
	window.opener.postMessage(data.result, data.origin);
	window.close();
}
