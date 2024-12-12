IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employees.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-REPORT ASSIGN TO "payroll_report.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID                 PIC 9(5).
           05 EMP-NAME               PIC X(30).
           05 EMP-DEPARTMENT         PIC X(20).
           05 EMP-SALARY             PIC 9(10)V99.
           05 EMP-TAX-RATE           PIC 9(3)V99.
       
       FD PAYROLL-REPORT.
       01 REPORT-LINE                PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 WS-TOTAL-PAYROLL       PIC 9(15)V99 VALUE ZERO.
           05 WS-TOTAL-TAX           PIC 9(15)V99 VALUE ZERO.
           05 WS-NET-PAYROLL         PIC 9(15)V99 VALUE ZERO.
           05 WS-EOF                 PIC X VALUE 'N'.
               88 END-OF-FILE        VALUE 'Y'.
       
       01 WS-CALCULATIONS.
           05 WS-TAX-AMOUNT          PIC 9(10)V99.
           05 WS-NET-PAY             PIC 9(10)V99.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-REPORT
           
           PERFORM PROCESS-EMPLOYEE-FILE 
           UNTIL END-OF-FILE
           
           PERFORM PRINT-SUMMARY
           
           CLOSE EMPLOYEE-FILE
           CLOSE PAYROLL-REPORT
           
           STOP RUN.
       
       PROCESS-EMPLOYEE-FILE.
           READ EMPLOYEE-FILE
               AT END 
                   MOVE 'Y' TO WS-EOF
               NOT AT END 
                   PERFORM CALCULATE-PAYROLL
                   PERFORM WRITE-REPORT-LINE
           END-READ.
       
       CALCULATE-PAYROLL.
           COMPUTE WS-TAX-AMOUNT = EMP-SALARY * (EMP-TAX-RATE / 100)
           COMPUTE WS-NET-PAY = EMP-SALARY - WS-TAX-AMOUNT
           
           ADD EMP-SALARY TO WS-TOTAL-PAYROLL
           ADD WS-TAX-AMOUNT TO WS-TOTAL-TAX
           ADD WS-NET-PAY TO WS-NET-PAYROLL.
       
       WRITE-REPORT-LINE.
           MOVE SPACES TO REPORT-LINE
           STRING 
               EMP-ID DELIMITED BY SIZE, 
               ' | ' DELIMITED BY SIZE,
               EMP-NAME DELIMITED BY SIZE, 
               ' | ' DELIMITED BY SIZE,
               EMP-DEPARTMENT DELIMITED BY SIZE, 
               ' | Gross: $' DELIMITED BY SIZE,
               EMP-SALARY DELIMITED BY SIZE,
               ' | Tax: $' DELIMITED BY SIZE,
               WS-TAX-AMOUNT DELIMITED BY SIZE,
               ' | Net: $' DELIMITED BY SIZE,
               WS-NET-PAY DELIMITED BY SIZE
               INTO REPORT-LINE
           
           WRITE REPORT-LINE.
       
       PRINT-SUMMARY.
           MOVE SPACES TO REPORT-LINE
           STRING 
               '===== PAYROLL SUMMARY =====' DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING 
               'Total Gross Payroll: $' DELIMITED BY SIZE,
               WS-TOTAL-PAYROLL DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING 
               'Total Tax Collected: $' DELIMITED BY SIZE,
               WS-TOTAL-TAX DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE
           
           MOVE SPACES TO REPORT-LINE
           STRING 
               'Total Net Payroll: $' DELIMITED BY SIZE,
               WS-NET-PAYROLL DELIMITED BY SIZE
               INTO REPORT-LINE
           WRITE REPORT-LINE.