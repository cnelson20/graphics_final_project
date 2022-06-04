from script import run
import sys

if len(sys.argv) == 3:
    run(sys.argv[1], sys.argv[2])
elif len(sys.argv) < 3:
    print("usage: python src/main.py script_file.mdl output_filename\n")
else:
    print("Too many arguments.")
