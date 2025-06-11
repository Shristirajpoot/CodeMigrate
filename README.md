# 🛠️ CodeMigrate – Modernize COBOL in a Click! 🐍✨  
> A lightning-fast COBOL ➜ Python converter built with ❤️ in pure Python.

![GitHub Repo stars](https://img.shields.io/github/stars/Shristirajpoot/CodeMigrate?style=social)
![GitHub last commit](https://img.shields.io/github/last-commit/Shristirajpoot/CodeMigrate?color=brightgreen)
![Built with](https://img.shields.io/badge/Built%20with-Python%203.7%2B-blue)

---

## 🌟 Overview

**CodeMigrate** is a utility that **translates legacy COBOL programs into clean, ready-to-run Python**.  
Ideal for teams moving main-frame logic to modern stacks without losing decades of business rules.

---

## 🎨 Features

- ⚡ **Automatic Translation** &nbsp;—&nbsp; WORKING-STORAGE ➜ Python classes, PROCEDURE DIVISION ➜ functions  
- 📂 **File Parsing** &nbsp;—&nbsp; Converts COBOL file I/O to Python file handling  
- 🔍 **Regex-Driven Rules** &nbsp;—&nbsp; Easily extend patterns for custom COBOL syntax  
- 📝 **Human-Readable Output** &nbsp;—&nbsp; Well-structured, commented Python code  

---

## 📂 Project Structure
```plaintext
CodeMigrate/
├── cobol_converter.py      # Main converter script
├── payroll_system.cob      # Sample COBOL program
├── employees.dat           # Sample data file
├── payroll_system.py       # Generated sample output
├── README.md               # This file
└── LICENSE                 # MIT
```

## 🚀 Getting Started

### 📦 Prerequisites
Python 3.7 +

## 🔧 Installation
```bash
git clone https://github.com/Shristirajpoot/CodeMigrate.git
cd CodeMigrate
```
(No extra packages required.)

### ▶️ Convert a File
```bash

python cobol_converter.py payroll_system.cob payroll_system.py
```
### ▶️ Run the Generated Script
```bash
python payroll_system.py
```
Ensure employees.dat is in the same directory.

### 🔢 Input Data Layout (employees.dat)
| Field         | Length | Offset |
| ------------- | ------ | ------ |
| Employee ID   | 5      | 0–4    |
| Employee Name | 20     | 6–25   |
| Department    | 19     | 27–45  |
| Salary        | 8      | 47–54  |
| Tax Rate      | 5      | 56–60  |

---

## Output Example

COBOL Snippet

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PAYROLL-SYSTEM.
...
```

Auto-generated Python

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

Ensure the input data file (`employees.dat`) exists in the same directory as the script.

---

## 📈 Roadmap / Future Enhancements
- 🚀 Support additional COBOL constructs (PERFORM, EVALUATE, etc.)

- 🧪 Add unit-test scaffolding for generated code

- 🛡️ Improve error handling & logging

## 🤝 Contributing
1. Fork the repo 🡆

2. git checkout -b feature/awesome

3. Commit & push, then open a PR!

## 👩‍💻 Author
### Shristi Rajpoot
-  📧 shristirajpoot369@gmail.com
-  🔗 @Shristirajpoot

## 📄 License
CodeMigrate is released under the MIT License.

### ⭐ If this project saved you hours of manual migration, please star it and share!
