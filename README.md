# openligadbR
Simple functions to query the https://www.openligadb.de/ API

## TODOs

- error handling when querying database
- input checks
- tests?
- proper description
- fetch_match("bl1", 2018123, group_id = 3) should return a data.frame with appropriate columns and maybe a warning
- fetch_match("bl1", 2018) returns one row with all NA except match_id for future games