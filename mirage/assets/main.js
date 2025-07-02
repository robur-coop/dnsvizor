
document.addEventListener('DOMContentLoaded', () => {
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
