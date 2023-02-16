/*
 Navicat Premium Data Transfer

 Source Server         : mimic4-2.0
 Source Server Type    : PostgreSQL
 Source Server Version : 100017
 Source Host           : localhost:5436
 Source Catalog        : mimic4
 Source Schema         : public

 Target Server Type    : PostgreSQL
 Target Server Version : 100017
 File Encoding         : 65001

 Date: 16/02/2023 16:59:21
*/


-- ----------------------------
-- Function structure for bigquery_format_to_psql
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."bigquery_format_to_psql"("format_str" varchar);
CREATE OR REPLACE FUNCTION "public"."bigquery_format_to_psql"("format_str" varchar)
  RETURNS "pg_catalog"."text" AS $BODY$
BEGIN
RETURN 
    -- use replace to convert BigQuery string format to postgres string format
    -- only handles a few cases since we don't extensively use this function
    REPLACE(
    REPLACE(
    REPLACE(
    REPLACE(
    REPLACE(
    REPLACE(
        format_str
        , '%S', 'SS'
    )
        , '%M', 'MI'
    )
        , '%H', 'HH24'
    )
        , '%d', 'dd'
    )
        , '%m', 'mm'
    )
        , '%Y', 'yyyy'
    )
;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for date_add
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."date_add"("dt" date, "intvl" interval);
CREATE OR REPLACE FUNCTION "public"."date_add"("dt" date, "intvl" interval)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN CAST(dt AS TIMESTAMP(3)) + intvl;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for date_sub
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."date_sub"("dt" date, "intvl" interval);
CREATE OR REPLACE FUNCTION "public"."date_sub"("dt" date, "intvl" interval)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN CAST(dt AS TIMESTAMP(3)) - intvl;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for datetime
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."datetime"("year" int4, "month" int4, "day" int4, "hour" int4, "minute" int4, "second" int4);
CREATE OR REPLACE FUNCTION "public"."datetime"("year" int4, "month" int4, "day" int4, "hour" int4, "minute" int4, "second" int4)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN TO_TIMESTAMP(
    TO_CHAR(year, '0000') || TO_CHAR(month, '00') || TO_CHAR(day, '00') || TO_CHAR(hour, '00') || TO_CHAR(minute, '00') || TO_CHAR(second, '00'),
    'yyyymmddHH24MISS'
);
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for datetime
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."datetime"("dt" date);
CREATE OR REPLACE FUNCTION "public"."datetime"("dt" date)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN CAST(dt AS TIMESTAMP(3));
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for datetime_add
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."datetime_add"("datetime_val" timestamp, "intvl" interval);
CREATE OR REPLACE FUNCTION "public"."datetime_add"("datetime_val" timestamp, "intvl" interval)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN datetime_val + intvl;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for datetime_diff
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."datetime_diff"("endtime" timestamp, "starttime" timestamp, "datepart" text);
CREATE OR REPLACE FUNCTION "public"."datetime_diff"("endtime" timestamp, "starttime" timestamp, "datepart" text)
  RETURNS "pg_catalog"."numeric" AS $BODY$
BEGIN
RETURN 
    EXTRACT(EPOCH FROM endtime - starttime) /
    CASE
        WHEN datepart = 'SECOND' THEN 1.0
        WHEN datepart = 'MINUTE' THEN 60.0
        WHEN datepart = 'HOUR' THEN 3600.0
        WHEN datepart = 'DAY' THEN 24*3600.0
        WHEN datepart = 'YEAR' THEN 365.242*24*3600.0
    ELSE NULL END;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for datetime_sub
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."datetime_sub"("datetime_val" timestamp, "intvl" interval);
CREATE OR REPLACE FUNCTION "public"."datetime_sub"("datetime_val" timestamp, "intvl" interval)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN datetime_val - intvl;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for format_date
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."format_date"("format_str" varchar, "datetime_val" timestamp);
CREATE OR REPLACE FUNCTION "public"."format_date"("format_str" varchar, "datetime_val" timestamp)
  RETURNS "pg_catalog"."text" AS $BODY$
BEGIN
RETURN TO_CHAR(
    datetime_val,
    -- use replace to convert BigQuery string format to postgres string format
    -- only handles a few cases since we don't extensively use this function
    BIGQUERY_FORMAT_TO_PSQL(format_str)
);
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for format_datetime
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."format_datetime"("format_str" varchar, "datetime_val" timestamp);
CREATE OR REPLACE FUNCTION "public"."format_datetime"("format_str" varchar, "datetime_val" timestamp)
  RETURNS "pg_catalog"."text" AS $BODY$
BEGIN
RETURN TO_CHAR(
    datetime_val,
    -- use replace to convert BigQuery string format to postgres string format
    -- only handles a few cases since we don't extensively use this function
    BIGQUERY_FORMAT_TO_PSQL(format_str)
);
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for generate_array
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."generate_array"("i" int4, "j" int4);
CREATE OR REPLACE FUNCTION "public"."generate_array"("i" int4, "j" int4)
  RETURNS SETOF "pg_catalog"."int4" AS $BODY$
    SELECT GENERATE_SERIES(i, j)
$BODY$
  LANGUAGE sql VOLATILE
  COST 100
  ROWS 1000;

-- ----------------------------
-- Function structure for parse_date
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."parse_date"("format_str" varchar, "string_val" varchar);
CREATE OR REPLACE FUNCTION "public"."parse_date"("format_str" varchar, "string_val" varchar)
  RETURNS "pg_catalog"."date" AS $BODY$
BEGIN
RETURN TO_DATE(
    string_val,
    -- use replace to convert BigQuery string format to postgres string format
    -- only handles a few cases since we don't extensively use this function
    BIGQUERY_FORMAT_TO_PSQL(format_str)
);
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for parse_datetime
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."parse_datetime"("format_str" varchar, "string_val" varchar);
CREATE OR REPLACE FUNCTION "public"."parse_datetime"("format_str" varchar, "string_val" varchar)
  RETURNS "pg_catalog"."timestamp" AS $BODY$
BEGIN
RETURN TO_TIMESTAMP(
    string_val,
    -- use replace to convert BigQuery string format to postgres string format
    -- only handles a few cases since we don't extensively use this function
    BIGQUERY_FORMAT_TO_PSQL(format_str)
);
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for regexp_contains
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."regexp_contains"("str" text, "pattern" text);
CREATE OR REPLACE FUNCTION "public"."regexp_contains"("str" text, "pattern" text)
  RETURNS "pg_catalog"."bool" AS $BODY$
BEGIN
RETURN str ~ pattern;
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;

-- ----------------------------
-- Function structure for regexp_extract
-- ----------------------------
DROP FUNCTION IF EXISTS "public"."regexp_extract"("str" text, "pattern" text);
CREATE OR REPLACE FUNCTION "public"."regexp_extract"("str" text, "pattern" text)
  RETURNS "pg_catalog"."text" AS $BODY$
BEGIN
RETURN substring(str from pattern);
END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
