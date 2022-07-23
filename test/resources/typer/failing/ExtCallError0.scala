object CallError {
  // Can't call non-functions!
  (if (true) { 1 } else { 2 })()
}
