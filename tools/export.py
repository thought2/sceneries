import bpy
import os
import sys
import argparse
import shutil


# PARSE ARGS

parser = argparse.ArgumentParser()

parser.add_argument('--input-dir')
parser.add_argument('--output-dir')

argsAll = sys.argv
args = argsAll[argsAll.index('--')+1:]
config = parser.parse_args(args)

# CLEAN OUTPUT

if os.path.exists(config.output_dir):
    shutil.rmtree(config.output_dir)

os.makedirs(config.output_dir)

# EXPORT DATA

for file in os.listdir(config.input_dir):
    input_path = os.path.join(config.input_dir, file)
    output_path = os.path.join(config.output_dir, file + ".obj")

    bpy.ops.wm.open_mainfile(filepath = input_path)
    bpy.ops.export_scene.obj(filepath = output_path)

# WRITE INDEX

index_path = os.path.join(config.output_dir, "index.txt")
index_content = "\n".join(os.listdir(config.output_dir))

with open(index_path, "w") as file:
    print(index_content, file=file)
