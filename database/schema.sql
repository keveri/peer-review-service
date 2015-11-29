-- create types
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'status') THEN
    CREATE TYPE status AS enum (
        'WAITING',
        'REVIEWED',
        'ACCEPTED'
    );
  END IF;
END$$;

-- create tables
CREATE TABLE IF NOT EXISTS peer_reviews (
  task_id text NOT NULL,
  comment text,
  score int,
  reviewer_id text NOT NULL,
  status STATUS NOT NULL
);
