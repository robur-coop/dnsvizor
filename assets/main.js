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

	queryStreamEvent.onmessage = ({ data }) => {
		try {
		    const payload = JSON.parse(data);
                    let row = query_logs.insertRow(1);
                    let c1 = row.insertCell(0);
                    c1.innerText = `${payload.fin}`;
                    let c2 = row.insertCell(1);
                    c2.innerText = `${payload.typ}`;
                    let c3 = row.insertCell(2);
                    c3.innerText = `${payload.domain}`;
                    let c4 = row.insertCell(3);
                    c4.innerText = `${payload.client}`;
                    let c5 = row.insertCell(4);
                    c5.innerText = `${payload.rcode}`;
                    let c6 = row.insertCell(5);
                    c6.innerText = `${payload.time_taken} ms`;
                    let c7 = row.insertCell(6);
                    c7.innerText = `${payload.status}`;
                    let c8 = row.insertCell(7);
                    c8.innerText = "BLOCK (fake)";
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
