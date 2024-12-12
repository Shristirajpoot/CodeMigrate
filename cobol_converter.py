import re
import sys
import decimal
from typing import Dict, Any

class CobolToPythonConverter:
    def __init__(self):
        """
        Initialize the COBOL to Python converter with parsing and conversion rules.
        """
        # Basic mapping of COBOL PIC types to Python types
        self.type_mappings = {
            'PIC 9': 'int',
            'PIC X': 'str',
            'PIC 9(5)': 'int',
            'PIC 9(10)V99': 'float',
            'PIC 9(3)V99': 'float',
            'COMP': 'int',
            'COMP-3': 'decimal.Decimal'
        }

        # Regex patterns for parsing COBOL structures
        self.patterns = {
            'variable_declaration': r'(\d+)\s+(\w[\w-]*)\s+PIC\s+([X9]+)(\(\d+\))?(\s*V\d+)?',
            'procedure_division': r'PROCEDURE\s+DIVISION',
            'program_id': r'PROGRAM-ID\.\s+(\w[\w-]*)',
            'working_storage': r'WORKING-STORAGE\s+SECTION\.',
            'file_section': r'FILE\s+SECTION\.',
            'record_declaration': r'01\s+(\w+)-RECORD',
            'file_declaration': r'FD\s+(\w+)-FILE',
            'select_file': r'SELECT\s+(\w+)-FILE\s+ASSIGN\s+TO\s+"([^"]+)"'
        }

    def cobol_name_to_python_class(self, name: str) -> str:
        # Convert COBOL program-id with hyphens to a Pythonic class name (uppercase)
        return name.replace('-', '_').upper()

    def cobol_name_to_python_var(self, name: str) -> str:
        # Convert COBOL variable/field names with hyphens to Pythonic underscores (lowercase)
        return name.replace('-', '_').lower()

    def determine_python_type(self, full_pic_type: str) -> str:
        # Check if we have a direct mapping
        if full_pic_type in self.type_mappings:
            return self.type_mappings[full_pic_type]

        # Heuristic for numeric vs. string
        if full_pic_type.startswith('PIC 9'):
            if 'V' in full_pic_type:
                return 'float'
            else:
                return 'int'
        elif full_pic_type.startswith('PIC X'):
            return 'str'
        # Default to string if unknown
        return 'str'

    def convert_file(self, cobol_file_path: str, output_file_path: str) -> None:
        """
        Convert a full COBOL file to Python.
        """
        with open(cobol_file_path, 'r') as cobol_file:
            cobol_code = cobol_file.read()

        python_code = self.convert_code(cobol_code)

        with open(output_file_path, 'w') as python_file:
            python_file.write(python_code)

        print(f"Conversion complete. Output saved to {output_file_path}")

    def convert_code(self, cobol_code: str) -> str:
        """
        Convert COBOL code to Python code.
        """
        # Extract program ID
        program_id_match = re.search(self.patterns['program_id'], cobol_code, re.IGNORECASE)
        program_name = program_id_match.group(1) if program_id_match else 'CobolProgram'
        class_name = self.cobol_name_to_python_class(program_name)

        # Parse file assignments from SELECT statements
        file_assignments = self.parse_file_assignments(cobol_code)

        python_code_lines = [
            "import decimal",
            "import csv",
            "",
            f"class {class_name}:",
        ]

        # Parse record structures
        record_matches = list(re.finditer(self.patterns['record_declaration'], cobol_code, re.IGNORECASE))
        record_structures = {}

        for record_match in record_matches:
            record_name = record_match.group(1)
            record_structure = self.parse_record_structure(cobol_code, record_name)
            record_structures[record_name] = record_structure

            python_code_lines.extend([
                "",
                f"    class {record_name}Record:",
                "        def __init__(self):"
            ])

            for var_name, var_details in record_structure.items():
                python_type = var_details['type']
                if python_type == 'int':
                    init_value = '0'
                elif python_type == 'str':
                    init_value = '""'
                elif python_type == 'float':
                    init_value = '0.0'
                else:
                    init_value = 'None'

                python_var_name = self.cobol_name_to_python_var(var_name)
                python_code_lines.append(f"            self.{python_var_name} = {init_value}")

        # Parse working storage variables
        python_code_lines.append("\n    def __init__(self):")
        working_storage_vars = self.parse_working_storage(cobol_code)

        for var_name, python_type in working_storage_vars.items():
            if python_type == 'int':
                init_value = '0'
            elif python_type == 'str':
                init_value = '""'
            elif python_type == 'float':
                init_value = '0.0'
            else:
                init_value = 'None'
            python_var_name = self.cobol_name_to_python_var(var_name)
            python_code_lines.append(f"        self.{python_var_name} = {init_value}")

        # Parse file declarations and use the actual assigned filenames if available
        file_matches = list(re.finditer(self.patterns['file_declaration'], cobol_code, re.IGNORECASE))
        for file_match in file_matches:
            file_name = file_match.group(1)
            python_var_name = self.cobol_name_to_python_var(file_name)
            assigned_filename = file_assignments.get(f"{file_name}-FILE", f"{python_var_name}.dat")
            # Use the actual assigned filename if found, else default
            python_code_lines.append(f"        self.{python_var_name}_file_path = '{assigned_filename}'")

        # Adjust parsing logic based on given field lengths and spaces:
        # ID: chars [0:5]
        # space after ID: [5]
        # NAME: chars [6:26] (20 chars), then space at [26]
        # DEPT: chars [27:46] (19 chars), then space at [46]
        # SALARY: chars [47:55] (8 chars), then space at [55]
        # TAX RATE: chars [56:61] (5 chars)
        
        python_code_lines.extend([
            "",
            "    def process_files(self):",
            "        # Example file processing logic",
            "        try:",
            "            with open(self.employee_file_path, 'r') as emp_file, open('payroll_report.txt', 'w') as report_file:",
            "                total_payroll = 0.0",
            "                total_tax = 0.0",
            "                total_net = 0.0",
            "",
            "                for line in emp_file:",
            "                    employee = self.EMPLOYEERecord()",
            "                    # Updated slicing according to your specifications",
            "                    try:",
            "                        employee.emp_id = int(line[0:5].strip())",
            "                        employee.emp_name = line[6:26].strip()",
            "                        employee.emp_department = line[27:46].strip()",
            "                        employee.emp_salary = float(line[47:55].strip())",
            "                        employee.emp_tax_rate = float(line[56:61].strip())",
            "                    except ValueError:",
            "                        # If parsing fails, skip this line",
            "                        continue",
            "",
            "                    tax_amount = employee.emp_salary * (employee.emp_tax_rate / 100.0)",
            "                    net_pay = employee.emp_salary - tax_amount",
            "                    total_payroll += employee.emp_salary",
            "                    total_tax += tax_amount",
            "                    total_net += net_pay",
            "",
            "                    report_line = (",
            "                        f'{employee.emp_id} | {employee.emp_name} | {employee.emp_department} | Gross: ${employee.emp_salary:.2f} | Tax: ${tax_amount:.2f} | Net: ${net_pay:.2f}'",
            "                    )",
            "                    report_file.write(report_line + '\\n')",
            "",
            "                # Write summary",
            "                report_file.write('\\n===== PAYROLL SUMMARY =====\\n')",
            "                report_file.write(f'Total Gross Payroll: ${total_payroll:.2f}\\n')",
            "                report_file.write(f'Total Tax Collected: ${total_tax:.2f}\\n')",
            "                report_file.write(f'Total Net Payroll: ${total_net:.2f}\\n')",
            "",
            "        except FileNotFoundError:",
            "            print('Employee file not found.')",
            "",
            "    def run(self):",
            "        self.process_files()",
            "",
            "def main():",
            f"    program = {class_name}()",
            "    program.run()",
            "",
            "if __name__ == '__main__':",
            "    main()"
        ])

        return "\n".join(python_code_lines)

    def parse_record_structure(self, cobol_code: str, record_name: str) -> Dict[str, Dict]:
        """
        Parse the structure of a specific record in the COBOL code.
        """
        record_structure = {}
        record_pattern = rf'01\s+{record_name}-RECORD\.(.*?)(?=01|FD|WORKING-STORAGE|PROCEDURE|IDENTIFICATION|$)'
        record_match = re.search(record_pattern, cobol_code, re.IGNORECASE | re.DOTALL)

        if record_match:
            record_code = record_match.group(1)
            var_matches = re.finditer(self.patterns['variable_declaration'], record_code, re.IGNORECASE)
            for var_match in var_matches:
                _, var_name, pic_type, length, decimal_part = var_match.groups()
                length = length or ''
                decimal_part = decimal_part or ''
                full_pic_type = f'PIC {pic_type}{length}{decimal_part}'.strip()
                python_type = self.determine_python_type(full_pic_type)
                record_structure[var_name] = {
                    'type': python_type,
                    'pic_type': full_pic_type
                }

        return record_structure

    def parse_working_storage(self, cobol_code: str) -> Dict[str, str]:
        """
        Parse variables declared in the WORKING-STORAGE SECTION.
        """
        vars_dict = {}
        working_storage_match = re.search(
            r'WORKING-STORAGE\s+SECTION\.(.*?)(?=PROCEDURE|FILE|LINKAGE|REPORT|LOCAL-STORAGE|END PROGRAM|\Z)',
            cobol_code, re.IGNORECASE | re.DOTALL
        )
        if working_storage_match:
            working_storage_code = working_storage_match.group(1)
            variable_matches = re.finditer(self.patterns['variable_declaration'], working_storage_code, re.IGNORECASE)
            for match in variable_matches:
                _, var_name, pic_type, length, decimal_part = match.groups()
                length = length or ''
                decimal_part = decimal_part or ''
                full_pic_type = f'PIC {pic_type}{length}{decimal_part}'.strip()
                python_type = self.determine_python_type(full_pic_type)
                vars_dict[var_name] = python_type

        return vars_dict

    def parse_file_assignments(self, cobol_code: str) -> Dict[str, str]:
        """
        Parse file assignments (SELECT ... ASSIGN TO "filename") from the FILE-CONTROL.
        """
        file_assignments = {}
        select_matches = re.finditer(self.patterns['select_file'], cobol_code, re.IGNORECASE)
        for select_match in select_matches:
            file_name = select_match.group(1) + '-FILE'
            assigned_filename = select_match.group(2)
            file_assignments[file_name] = assigned_filename
        return file_assignments

    def analyze_cobol_complexity(self, cobol_code: str) -> Dict[str, Any]:
        """
        Analyze the complexity of the COBOL program.
        """
        working_storage_match = re.search(r'WORKING-STORAGE\s+SECTION\.(.*?)(?=PROCEDURE|\Z)', cobol_code,
                                          re.IGNORECASE | re.DOTALL)

        analysis = {
            'variable_count': 0,
            'record_count': len(re.findall(self.patterns['record_declaration'], cobol_code, re.IGNORECASE)),
            'has_procedure_division': bool(re.search(self.patterns['procedure_division'], cobol_code, re.IGNORECASE)),
            'has_working_storage': bool(re.search(self.patterns['working_storage'], cobol_code, re.IGNORECASE)),
            'has_file_section': bool(re.search(self.patterns['file_section'], cobol_code, re.IGNORECASE))
        }

        if working_storage_match:
            working_storage_code = working_storage_match.group(1)
            analysis['variable_count'] = len(re.findall(self.patterns['variable_declaration'], working_storage_code,
                                                        re.IGNORECASE))

        return analysis


def main():
    if len(sys.argv) != 3:
        print("Usage: python cobol_converter.py <input_cobol_file> <output_python_file>")
        sys.exit(1)

    converter = CobolToPythonConverter()
    input_file = sys.argv[1]
    output_file = sys.argv[2]

    try:
        converter.convert_file(input_file, output_file)

        # Optional: Print complexity analysis
        with open(input_file, 'r') as f:
            cobol_code = f.read()
        complexity = converter.analyze_cobol_complexity(cobol_code)
        print("\nCOBOL Program Complexity Analysis:")
        for key, value in complexity.items():
            print(f"{key}: {value}")

    except Exception as e:
        print(f"Conversion error: {e}")
        sys.exit(1)


if __name__ == '__main__':
    main()
