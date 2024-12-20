# CodeMigrate Converter

The **CodeMigrate Converter** is a utility tool that converts COBOL source code into Python. This tool aims to simplify the process of migrating legacy COBOL applications to modern Python-based systems, while also generating Python code that adheres to the functional structure of the original COBOL program.

---

## Features

- **Automatic Translation**: Converts COBOL code, including variable declarations, records, and procedures, into Python classes and methods.
- **File Parsing**: Supports parsing file assignments and generates Python code with proper file handling.
- **Customizable Parsing Rules**: Leverages regex patterns to interpret COBOL structures like `WORKING-STORAGE` and `PROCEDURE DIVISION`.
- **Detailed Output**: Generates Python scripts with the corresponding logic based on COBOL field lengths and offsets.

---

## Getting Started

### Prerequisites

- Python 3.7 or higher installed on your system. You can download it [here](https://www.python.org/downloads/).
- Basic knowledge of running Python scripts via the command line.

### Installation

1. Clone the repository:

    ```bash
    git clone https://github.com/Shristirajpoot/CodeMigrate.git
    cd cobol-to-python
    ```

2. No additional installations are required for this project.

### Usage

To convert a COBOL file to Python:

```bash
python cobol_converter.py <input_cobol_file> <output_python_file>
```

#### Example:
```bash
python cobol_converter.py payroll_system.cob payroll_system.py
```

This command will generate `payroll_system.py` with Python code translated from `payroll_system.cob`.

---

## Input Data Format

The converter assumes specific field layouts for COBOL records. Below is the expected input data format for the `employees.dat` file:

| **Field**       | **Length (Chars)** | **Position** |
|------------------|--------------------|--------------|
| Employee ID      | 5                 | 0-4          |
| Space            | 1                 | 5            |
| Employee Name    | 20                | 6-25         |
| Space            | 1                 | 26           |
| Department       | 19                | 27-45        |
| Space            | 1                 | 46           |
| Salary           | 8                 | 47-54        |
| Space            | 1                 | 55           |
| Tax Rate         | 5                 | 56-60        |

---

### Output Example

For a COBOL input program like this:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL-SYSTEM.
...
```

The converter generates Python code like:

```python
import decimal

class PAYROLL_SYSTEM:
    class EMPLOYEERecord:
        def __init__(self):
            self.emp_id = 0
            self.emp_name = ""
            self.emp_department = ""
            self.emp_salary = 0.0
            self.emp_tax_rate = 0.0

    def __init__(self):
        self.employee_file_path = 'employees.dat'
        ...
```

---

### Running the Generated Script

After generating the Python script, run it with:

```bash
python <output_python_file>
```

Example:
```bash
python payroll_system.py
```

Ensure the input data file (`employees.dat`) exists in the same directory as the script.

---

## Contributing

Contributions are welcome! If you find any issues or have suggestions for improvement, please create an issue or submit a pull request.

---

## License

This project is licensed under the [MIT License](LICENSE).

---

## Author

**CodēCodes**  
GitHub: [Shristirajpoot](https://github.com/Shristirajpoot)

---

### Future Enhancements

- Add support for more COBOL constructs.
- Include unit tests for the converted Python code.
- Enhance error handling during conversion.
