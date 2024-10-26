import os
import subprocess
import sys

def run_files_with_interpreter(interpreter, directory):
    # Ensure the directory exists
    if not os.path.isdir(directory):
        print(f"Directory '{directory}' does not exist.")
        return
    
    # List all files in the directory
    files = os.listdir(directory)
    
    # Run each file with the specified interpreter
    for file in files:
        file_path = os.path.join(directory, file)
        if os.path.isfile(file_path):
            command = f"./{interpreter} {file_path}"
            print(f"Running command: {command}")
            try:
                result = subprocess.run(command, shell=True, capture_output=True, text=True)
                print(f"Output of {file}:{result.stdout}")
                if result.stderr:
                    print(f"Error in {file}:\n{result.stderr}", file=sys.stderr)
            except Exception as e:
                print(f"Failed to run {file} with error: {e}", file=sys.stderr)

if __name__ == "__main__":
    interpreter = 'interpreter'
    directory = 'bad_examples'
    run_files_with_interpreter(interpreter, directory)
    print("-------------------------------------------------")
    print("NEW EXAMPLES")
    print("-------------------------------------------------")
    interpreter = 'interpreter'
    directory1 = 'new_bad_examples'
    run_files_with_interpreter(interpreter, directory1)