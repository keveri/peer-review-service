-- This file is always called at start up.
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'status') THEN
    CREATE TYPE status AS enum (
        'waiting',
        'reviewed',
        'accepted'
    );
  END IF;
END$$;

CREATE TABLE IF NOT EXISTS peer_reviews (
  id            bigserial   PRIMARY KEY,
  submission_id text        NOT NULL,
  task_id       text        NOT NULL,
  comment       text,
  score         int,
  reviewer_id   text        NOT NULL,
  status        status      NOT NULL DEFAULT 'waiting',
  created_at    timestamptz NOT NULL DEFAULT now()
);
