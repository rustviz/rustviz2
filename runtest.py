import os
import subprocess
import difflib
# Define the folder path containing the files
folder_path = 'tests'

# Iterate through files in the folder
for filename in os.listdir(folder_path):
    # Check if the file name matches the pattern "test+number"
    if filename.startswith('test'):
        test_file_path = os.path.join(folder_path, filename)
        
        # Extract the number from the file name
        test_name = filename.split('.')[0].split('test')[1]
        
        # Define the corresponding "out+number" file path
        out_file_path = os.path.join(folder_path, f'out{test_name}.out')
        
        # Read the content from the test file
        with open(test_file_path, 'r') as test_file:
            test_content = test_file.read()
        
        # Append the test content to lib.rs
        lib_rs_path = 'test-crate/src/lib.rs'
        with open(lib_rs_path, 'a') as lib_rs_file:
            # clear the file first
            lib_rs_file.seek(0)
            lib_rs_file.truncate()
            lib_rs_file.write(test_content)
        
        # Run the command (e.g., "cargo print-all-items")
        command = ['cargo', 'print-all-items']
        # subprocess.run(command, cwd='test-crate', check=True)
        
        # Read the content from the corresponding "out+number" file
        with open(out_file_path, 'r') as out_file:
            expected_output = out_file.read()
        
        actual_output = subprocess.run(command, cwd='test-crate', check=True, capture_output=True, text=True).stdout
        # Compare the output with the expected content
        print(actual_output)
        #print("expect: " + expected_output)
        
        if actual_output.replace('\r\n', '\n') == expected_output.replace('\r\n', '\n'):
            print(f"Test {test_name}: PASSED")
        else:
            print(f"Test {test_name}: FAILED (Output doesn't match expected)")
            #d = difflib.Differ()
            #diff = list(d.compare(expected_output.splitlines(), actual_output.splitlines()))
            #print('\ndiff\n'.join(diff))
       
# Optionally, you can reset lib.rs to its original state here if needed.
