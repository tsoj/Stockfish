def read_binary_file(file_path):
    with open(file_path, 'rb') as file:
        return file.read()

def generate_cpp_header(data, output_path):
    with open(output_path, 'w') as file:
        file.write('#pragma once\n\n')
        file.write('namespace Eval { constexpr char rawBaeContent[] = R"(')
        file.write(''.join(f'\\x{byte:02x}' for byte in data))
        file.write(')"; }\n')

if __name__ == "__main__":

    binary_file_path = "./bae_params.bin"
    cpp_header_path = "./src/bae_params.h"

    binary_data = read_binary_file(binary_file_path)
    generate_cpp_header(binary_data, cpp_header_path)
