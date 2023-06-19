from fastapi import FastAPI, File, UploadFile, Response, Cookie, HTTPException
import fastapi.responses
from typing import Union
import rpy2.robjects as robjects
from starlette.concurrency import run_in_threadpool
from fastapi.staticfiles import StaticFiles
from datetime import datetime
import logging
import os

logging.basicConfig(level=logging.WARNING)

app = FastAPI()

app.mount("/static", StaticFiles(directory="public"), name="static")

@app.get("/")
async def root():
    with open("index.html") as fh:
        data = fh.read()
    
    return Response(content=data, media_type="text/html")

async def process_file(matrix_path: str, metadata_path: str, result_prefix: str):
    r_script_path = 'utils.R'
    robjects.r.source(r_script_path)

    flag = None

    if metadata_path is None:
        try:
            flag = await run_in_threadpool(robjects.r.timeInferenceFunc, f"{matrix_path}", robjects.NULL, result_prefix)
        except Exception as e:
            pass
    else:
        try:
            flag = await run_in_threadpool(robjects.r.timeInferenceFunc, f"{matrix_path}", f"{metadata_path}", result_prefix)
        except Exception as e:
            pass

    if flag is not None:
        if flag[0] == True:
            response_data = {"image_url": f"{result_prefix}_result.png", "latent_filename": f"{result_prefix}_latent.tsv", "result_filename": f"{result_prefix}_pt.tsv"}
        else:
            response_data = {"error": flag[0]}

        return fastapi.responses.JSONResponse(content=response_data)
    else:
        return fastapi.responses.JSONResponse(content={"error": "Unknown error"})

    

@app.post("/upload1")
async def upload_file1(fileMatrix: UploadFile = File(...), scDHA_session_id: Union[str, None] = Cookie(None)):
    # You can perform any necessary processing on the uploaded file here
    # For example, you can save it to a specific directory or process its contents
    logging.info("line 40 - function upload_file1 is run")

    now = datetime.now()

    t = now.strftime("%Y%m%d%H%M%S")
    
    # Save the file to disk
    with open(f"./uploaded_files/{scDHA_session_id}{t}_matrix.tsv", "wb") as f:
        f.write(await fileMatrix.read())
    
    result = await process_file(f"./uploaded_files/{scDHA_session_id}{t}_matrix.tsv", None, f"{scDHA_session_id}{t}")

    # Return a response
    return result

@app.post("/upload2")
async def upload_file2(fileMatrix: UploadFile = File(...), fileMetadata: UploadFile = File(...), scDHA_session_id: Union[str, None] = Cookie(None)):
    # You can perform any necessary processing on the uploaded file here
    # For example, you can save it to a specific directory or process its contents
    
    now = datetime.now()

    t = now.strftime("%Y%m%d%H%M%S")

    # Save the file to disk
    with open(f"./uploaded_files/{scDHA_session_id}{t}_matrix.tsv", "wb") as f:
        f.write(await fileMatrix.read())
    
    with open(f"./uploaded_files/{scDHA_session_id}{t}_metadata.tsv", "wb") as f:
        f.write(await fileMetadata.read())

    result = await process_file(f"./uploaded_files/{scDHA_session_id}{t}_matrix.tsv", f"./uploaded_files/{scDHA_session_id}{t}_metadata.tsv", f"{scDHA_session_id}{t}")

    # Return a response
    return result

@app.post("/upload3")
async def upload_file3(data: dict, scDHA_session_id: Union[str, None] = Cookie(None)):
    dataset_name = data['dataset_name']
    response_data = {"image_url": f"{dataset_name}_result.png", "latent_filename": f"{dataset_name}_latent.tsv", "result_filename": f"{dataset_name}_pt.tsv"}

    return fastapi.responses.JSONResponse(content=response_data)

@app.get("/download/{filename}")
async def download_file(filename: str):
    processed_file_path = f"./processed_files/{filename}"
    if not os.path.exists(processed_file_path):
        raise HTTPException(status_code=404, detail="Item not found")
    return fastapi.responses.FileResponse(processed_file_path, filename=filename)

@app.get("/showResultImg/{filename}")
async def show_result_img(filename: str):
    result_img_path = f"./images/{filename}"
    if not os.path.exists(result_img_path):
        raise HTTPException(status_code=404, detail="Item not found")
    
    return fastapi.responses.FileResponse(result_img_path, media_type="image/jpeg")

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)