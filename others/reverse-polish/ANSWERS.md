# Answers to experiments written in README.md of 2022-01-27 Lab

3. I noticed that while compiling the code after commenting out %left lines, the CFG becomes ambiguous and ml-yacc itself tried to do 16 `shift/reduce conflicts`

4. By interchanging the order of both of these lines, the precedence order changes and the results of the calculation on the sample test cases also changes due to that.
