Utility to measure the average latency to query a variable number of keys in the UserTime data load for RiakTS

```
Query Data
```

With 6 months of data loaded for 100 users, perform 50 queries for 30 days of data at a time

./user_time_latency -h <HOST> -u 100 -d 180 -q 30 -c 50