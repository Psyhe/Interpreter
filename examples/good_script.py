import subprocess
import os

def run_interpreter(command):
    try:
        result = subprocess.run(command, shell=True, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        print(f"Error running command: {e.stderr}")
        return None

def read_file(filepath):
    try:
        with open(filepath, 'r') as file:
            return file.read()
    except IOError as e:
        print("ERROR")
        print(filepath)
        print(f"Error reading file {filepath}: {e}")
        return None

def run(interpreter, input_dir, output_dir):
    input_files = os.listdir(input_dir)

    for input_file in input_files:
        input_path = os.path.join(input_dir, input_file)
        output_path = os.path.join(output_dir, input_file)  # Zakładamy, że plik wyjściowy ma taką samą nazwę jak plik wejściowy

        if not os.path.exists(output_path):
            print(f"Output file {output_path} does not exist, skipping.")
            continue

        print(input_file)
        command = f"{interpreter} {input_path}"
        interpreter_output = run_interpreter(command)
        if interpreter_output is None:
            continue
        
        file_content = read_file(output_path)
        if file_content is None:
            continue

        if interpreter_output == file_content:
            print(f"The outputs are identical for {input_file}.")
        else:
            print(f"\033[91m!!! PROBLEM: !!!\033[0m")
            print(f"The outputs are different for {input_file}.")
            print(f"Expected output: {file_content}")
            print(f"Interpreter output: {interpreter_output}")

if __name__ == "__main__":
    interpreter = './interpreter'
    input_dir1 = 'good_examples'
    output_dir1 = 'good_out'
    run(interpreter, input_dir1, output_dir1)
    print("-------------------------------------------------")
    print("NEW EXAMPLES")
    print("-------------------------------------------------")
    input_dir2 = 'new_good_examples'
    output_dir2 = 'new_good_out'
    run(interpreter, input_dir2, output_dir2)