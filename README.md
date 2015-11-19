[![Build Status](https://travis-ci.org/keveri/peer-review-service.svg?branch=master)](https://travis-ci.org/keveri/peer-review-service)

# Peer review service

Web service for handling course peer reviews.

## Development
This project requires `stack` tool.

### Setup
First enable scripts:
```
chmod +x script/*
```

Then run the setup:
```
script/setup
```

### Running the app

```
script/run
```

### Running tests

```
script/test
```

## API end points
More detailed documentation for the API will be available separately.

GET `/` - Show API documentation.

GET `/peer-reviews` - returns peer reviews done by given email.

GET `/peer-reviews/new` - returns reviewable task for email if one can be found.

PUT `/peer-reviews/:id/accept` - mark a review as accepted (used by the main application).

GET `/peer-reviews/completed` - returns a list of completed peer reviews that still need to be checked by an 'admin'. These are grouped by the submission they are reviewing. Definition of complete can be for example 3 PASS reviews for one submission.
