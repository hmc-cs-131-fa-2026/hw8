# CS 131 HW 8: Lambda Calculus

The assignment itself (what to write, and why) is on the course site:
**https://www.cs.hmc.edu/~bang/cs131/assignments/hw08/**

This README only covers the mechanics.

## Files

- `hw8.lc`: the **only file you edit**. The definitions at the top (Booleans, pairs, numerals, `Y`) are provided.
  The ones at the bottom (`Leq = 0;` and so on) are placeholders for you to replace. Add any helper definitions you
  like.
- `run-tests`: runs the tests.
- `test/`: the test runner and the test cases. Don't edit them.

## Working in lc

From the top of the repo, on the course server:

```
lc
:load hw8.lc
```

After each change, use `:reload`. `:set trace off` shows only the result and step count, which you'll want for the
bigger problems.

## Running the tests

```
./run-tests
```

Each test prints the expression it tried, what it reduced to, and the points it earned. A failing test also shows
what was expected. Until you replace a placeholder, its tests fail. That's expected.

## Submitting

```
./run-tests > testing.log
```

Then, on Gradescope (**HW 08: Lambda Calculus**), paste the contents of `testing.log`, upload `hw8.lc`, and answer
the short questions. Commit and push your work too. See the course site's "The Homework Workflow" page for the full
process.
