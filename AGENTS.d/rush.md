# Rush

- Tests that use rush must not be run in parallel since they share the same Redis instance. Always run them sequentially, e.g. `devtools::test(filter = '^LearnerClassifAutoRanger$')` one at a time, never multiple rush test files concurrently.
