function uploadFile(input, uploadProgress, _delete, name) {
    var fileInput = document.getElementById(input);
    var progressElement = document.getElementById(uploadProgress);
    var deleteBtn = document.getElementById(_delete);
    var name = document.getElementById(name);
    // progressElement.style.display = 'None';
    fileInput.click();

    fileInput.addEventListener("change", function(){
        var file = fileInput.files[0];
        var fileSize = file.size;
        var chunkSize = 10 * 1024 * 1024; // 1MB chunk size (adjust as needed)
        var offset = 0;
        progressElement.style.display = 'block'; // Display the progress bar
        deleteBtn.style.display = 'block';
        name.innerText = "";

        var reader = new FileReader();

        // reader.onabort = function(event){
        //     progressElement.value = 0;
        // }

        var flag = true;

        function deleteFile(fileInput, progressElement, deleteBtn){
            fileInput.value = null;
            progressElement.style.display = 'none';
            deleteBtn.style.display = 'none';
            name.innerText = "";
        }

        deleteBtn.addEventListener("click", function(){
            flag = false;
            console.log(flag);
            deleteFile(fileInput, progressElement, deleteBtn);
        });

        function uploadChunk() {
            if(!flag){
                progressElement.value = 0;
                return ;
            }
            var chunk = file.slice(offset, offset + chunkSize);
            reader.readAsArrayBuffer(chunk);
            
            reader.onload = function(event) {
                if (event.target.readyState === FileReader.DONE) {
                    // Simulating an asynchronous upload process
                    setTimeout(function() {
                    offset += chunkSize;
                    progressElement.value = Math.min((offset / fileSize) * 100, 100);
                    
                    if (offset < fileSize) {
                        uploadChunk(); // Upload the next chunk
                    } else {
                        name.innerText = file.name;
                        reader.onloadend = null; // Remove the event listener
                        progressElement.style.display = 'none';
                        // deleteBtn.style.display = "block";
                    }
                    }, 1000); // 1 second delay (adjust as needed)
                }
            };
        }
        uploadChunk(); // Start uploading the chunks
    })
}

