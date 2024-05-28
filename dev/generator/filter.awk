BEGIN {
  FS="&"
}

// {
  for (i = 1; i <= NF; i++) {
    # Remove duplicate expressions
    for (j = 1; j < i; j++)
      if ($i == $j) { next }
    # Remove symmetric binary operations
    split($i, a, ">")
    if (a[1] == a[2]) { next }
  }
  # Add spaces around conjunctions
  gsub(/\&/, " & ")
  print
}
