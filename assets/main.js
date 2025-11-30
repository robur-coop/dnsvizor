let queryStreamEvent = null;

document.addEventListener('DOMContentLoaded', () => {
    if (window.location.pathname.startsWith("/querylog")) {
	startEventSource();
    }

    const fileInput = document.getElementById('dnsmasq_config_file');
    const selectedFileNameSpan = document.getElementById('selected_file_name');
    const fileContentDisplayArea = document.getElementById('dnsmasq_config');

    // Listen for changes on the hidden file input
    fileInput.addEventListener('change', (event) => {
        const file = event.target.files[0]; // Get the first selected file

        if (file) {
            selectedFileNameSpan.textContent = file.name;
            const reader = new FileReader();
            reader.onload = (e) => {
                fileContentDisplayArea.value = e.target.result;
            };
            reader.onerror = () => {
                selectedFileNameSpan.textContent = "Error reading file.";
                fileContentDisplayArea.value = "Could not read file content.";
                console.error("Error reading file:", reader.error);
            };
            reader.readAsText(file);
        } else {
            selectedFileNameSpan.textContent = "No file chosen";
            fileContentDisplayArea.value = "";
        }
    });
});

function startEventSource() {
	if (queryStreamEvent) return;

	if (!window.location.pathname.startsWith("/querylog")) return;

	const query_logs = document.getElementById("query-logs");

    queryStreamEvent = new EventSource(`/api/queries`);

    const typ_class = "p-2"
    const ok_class = "p-2 text-green-800"
    const err_class = "p-2 text-red-800"
    function button_html (domain) {
        return "<form action=\"/blocklist/add\" method=\"POST\" enctype=\"multipart/form-data\" class=\"flex mb-4 gap-2\"><input type=\"hidden\" name=\"domain\" value=\"" + domain + "\"><button class=\"text-white border font-semibold hover:bg-red-100 hover:text-red-800 bg-red-800 rounded p-2 cursor-pointer\">Block</button></form>";
    }

	queryStreamEvent.onmessage = ({ data }) => {
		try {
		    const payload = JSON.parse(data);
                    let css;
                    if (`${payload.rcode}` == "no error") { css = ok_class } else { css = err_class };
                    let row = query_logs.insertRow(1);
                    let c1 = row.insertCell(0);
                    c1.innerText = `${payload.fin}`;
                    c1.classList = css;
                    let c2 = row.insertCell(1);
                    c2.innerText = `${payload.typ}`;
                    c2.classList = typ_class;
                    let c3 = row.insertCell(2);
                    c3.innerText = `${payload.domain}`;
                    c3.classList = css;
                    let c4 = row.insertCell(3);
                    c4.innerText = `${payload.client}`;
                    c4.classList = css;
                    let c5 = row.insertCell(4);
                    c5.innerText = `${payload.rcode}`;
                    c5.classList = css;
                    let c6 = row.insertCell(5);
                    c6.innerText = `${payload.time_taken} ms`;
                    c6.classList = css;
                    let c7 = row.insertCell(6);
                    c7.innerText = `${payload.status}`;
                    c7.classList = css;
                    let c8 = row.insertCell(7);
                    c8.innerHTML = button_html(`${payload.domain}`);
                    if (query_logs.rows.length > 101) {
                        query_logs.deleteRow(-1);
                    }
		} catch (err) {
			console.error("Failed to parse SSE payload:", err, data);
		}
	};

}

function stopEventSource() {
	if (queryStreamEvent) {
		queryStreamEvent.close();
		queryStreamEvent = null;
	}
}

document.addEventListener("visibilitychange", () => {
	if (document.visibilityState === "visible") {
		console.log("Resuming SSE stream...");
		startEventSource();
	} else {
		console.log("Pausing SSE stream...");
		stopEventSource();
	}
});
