CREATE EXTERNAL TABLE IF NOT EXISTS shortgps (db_key INT, tim INT, long DOUBLE, lat DOUBLE)
LOCATION 's3://cdczdong/input/';
CREATE EXTERNAL TABLE IF NOT EXISTS records_gps (db_key INT, userid INT, sigtype STRING)
LOCATION 's3://cdczdong/input/';
SELECT * FROM records_gps
  NATURAL LEFT JOIN shortgps
  ORDER BY userid;