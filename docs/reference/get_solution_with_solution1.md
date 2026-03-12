# Ensure a solution has a \`solution_1\` column

Helper that returns \`x\$solution\`, adding a \`solution_1\` column if
missing by copying the first column in \`x\$solution\` that is not
already in \`x\$planning_units\`.

## Usage

``` r
get_solution_with_solution1(x)
```

## Arguments

- x:

  A list-like object containing \`solution\` and \`planning_units\`.

## Value

A data.frame (or tibble) like \`x\$solution\`, guaranteed to include
\`solution_1\` if a suitable column exists.
