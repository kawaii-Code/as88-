from pathlib import Path
import os
import subprocess

directory = Path("examples")
for file in os.listdir(directory):
    path_to_example = f'examples/{file}'
    if os.path.isdir(path_to_example):
        continue
        
    completed_process = subprocess.run(["./zig-out/bin/s88", path_to_example])
    assert(completed_process.returncode == 0)