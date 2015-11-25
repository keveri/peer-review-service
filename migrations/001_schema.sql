create type STATUS as enum ('WAITING', 'REVIEWED', 'ACCEPTED');
create table peer_reviews (
  task_id text not null,
  comment text,
  score int,
  reviewer_id text not null,
  status STATUS not null
);
