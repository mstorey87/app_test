# ============================================================
# --- Load Libraries ---
# ============================================================
library(reticulate)
library(devtools)

# ============================================================
# --- R Package Dependencies ---
# ============================================================
# Install your custom package (done locally before deploy)
# devtools::install_github("mstorey87/wfprogression")

# ============================================================
# --- Python Dependencies (auto-managed by shinyapps.io) ---
# ============================================================
# shinyapps.io provides an isolated virtualenv for Python.
# py_require() ensures packages are installed automatically.

reticulate::py_require("torch>=2.5.1")
reticulate::py_require("torchvision>=0.20.1")
reticulate::py_require("requests")
reticulate::py_require("opencv-python")
reticulate::py_require("git+https://github.com/facebookresearch/sam2.git")

# ============================================================
# --- Check Installation ---
# ============================================================
py_run_string("
import sam2, torch, cv2, requests
print('✅ Torch:', torch.__version__)
print('✅ OpenCV:', cv2.__version__)
print('✅ SAM2:', getattr(sam2, '__version__', 'installed'))
")

# # ============================================================
# # --- Download Model Checkpoints + YAMLs ---
# # ============================================================
# py_run_string("
# import os, sys, requests
# from urllib.parse import urlparse
# 
# checkpoints = [
#   'https://dl.fbaipublicfiles.com/segment_anything_2/092824/sam2.1_hiera_tiny.pt',
#   'https://dl.fbaipublicfiles.com/segment_anything_2/092824/sam2.1_hiera_small.pt',
#   'https://dl.fbaipublicfiles.com/segment_anything_2/092824/sam2.1_hiera_base_plus.pt',
#   'https://dl.fbaipublicfiles.com/segment_anything_2/092824/sam2.1_hiera_large.pt'
# ]
# 
# yamls = [
#   'https://raw.githubusercontent.com/facebookresearch/sam2/refs/heads/main/sam2/configs/sam2.1/sam2.1_hiera_t.yaml',
#   'https://raw.githubusercontent.com/facebookresearch/sam2/refs/heads/main/sam2/configs/sam2.1/sam2.1_hiera_s.yaml',
#   'https://raw.githubusercontent.com/facebookresearch/sam2/refs/heads/main/sam2/configs/sam2.1/sam2.1_hiera_b+.yaml',
#   'https://raw.githubusercontent.com/facebookresearch/sam2/refs/heads/main/sam2/configs/sam2.1/sam2.1_hiera_l.yaml'
# ]
# 
# ckpt_dir = os.path.join(sys.prefix, 'checkpoints')
# os.makedirs(ckpt_dir, exist_ok=True)
# headers = {'User-Agent': 'Mozilla/5.0'}
# 
# def download(url):
#     filename = os.path.basename(urlparse(url).path)
#     path = os.path.join(ckpt_dir, filename)
#     if not os.path.exists(path):
#         print(f'Downloading {filename}...')
#         r = requests.get(url, headers=headers, stream=True)
#         r.raise_for_status()
#         with open(path, 'wb') as f:
#             for chunk in r.iter_content(8192):
#                 f.write(chunk)
#         print(f'✅ {filename} downloaded to {ckpt_dir}')
#     else:
#         print(f'{filename} already exists in {ckpt_dir}')
# 
# for u in checkpoints + yamls:
#     download(u)
# 
# print('\\n📁 All model files stored at:', ckpt_dir)
# ")

# ============================================================
# --- Optional: Check where Python lives ---
# ============================================================
#py_config()
