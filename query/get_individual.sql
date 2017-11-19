USE MDC_Exp;

SELECT
  C1 as 'db_key',
  C2 as 'time',
  C3 as 'longitude',
  C4 as 'latitude'
  FROM gps
  ORDER BY C1;

SELECT
  db_key as 'db_key',
  userid as 'userid',
  type as 'type'
  FROM records
  WHERE type = 'gps'
  ORDER BY userid;


SELECT * FROM records_gps
  NATURAL LEFT JOIN shortgps
  WHERE userid= 5448;


SELECT
  db_key as 'db_key',
  userid as 'userid',
  r_time as 'r_time',
  type   as 'type',
  tz     as 'tz'
  FROM records
  WHERE type = 'bt'
  ORDER BY db_key;
