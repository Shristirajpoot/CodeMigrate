import decimal
import csv

class PAYROLL_SYSTEM:

    class EMPLOYEERecord:
        def __init__(self):
            self.emp_id = 0
            self.emp_name = ""
            self.emp_department = ""
            self.emp_salary = 0.0
            self.emp_tax_rate = 0.0

    def __init__(self):
        self.ws_total_payroll = 0.0
        self.ws_total_tax = 0.0
        self.ws_net_payroll = 0.0
        self.ws_eof = ""
        self.employee_file_path = 'employees.dat'

    def process_files(self):
        # Example file processing logic
        try:
            with open(self.employee_file_path, 'r') as emp_file, open('payroll_report.txt', 'w') as report_file:
                total_payroll = 0.0
                total_tax = 0.0
                total_net = 0.0

                for line in emp_file:
                    employee = self.EMPLOYEERecord()
                    # Updated slicing according to your specifications
                    try:
                        employee.emp_id = int(line[0:5].strip())
                        employee.emp_name = line[6:26].strip()
                        employee.emp_department = line[27:46].strip()
                        employee.emp_salary = float(line[47:55].strip())
                        employee.emp_tax_rate = float(line[56:61].strip())
                    except ValueError:
                        # If parsing fails, skip this line
                        continue

                    tax_amount = employee.emp_salary * (employee.emp_tax_rate / 100.0)
                    net_pay = employee.emp_salary - tax_amount
                    total_payroll += employee.emp_salary
                    total_tax += tax_amount
                    total_net += net_pay

                    report_line = (
                        f'{employee.emp_id} | {employee.emp_name} | {employee.emp_department} | Gross: ${employee.emp_salary:.2f} | Tax: ${tax_amount:.2f} | Net: ${net_pay:.2f}'
                    )
                    report_file.write(report_line + '\n')

                # Write summary
                report_file.write('\n===== PAYROLL SUMMARY =====\n')
                report_file.write(f'Total Gross Payroll: ${total_payroll:.2f}\n')
                report_file.write(f'Total Tax Collected: ${total_tax:.2f}\n')
                report_file.write(f'Total Net Payroll: ${total_net:.2f}\n')

        except FileNotFoundError:
            print('Employee file not found.')

    def run(self):
        self.process_files()

def main():
    program = PAYROLL_SYSTEM()
    program.run()

if __name__ == '__main__':
    main()