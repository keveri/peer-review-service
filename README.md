[![Build Status](https://img.shields.io/travis/keveri/peer-review-service/master.svg?style=flat-square)](https://travis-ci.org/keveri/peer-review-service)
[![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg?style=flat-square)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
[![Coverage Status](https://img.shields.io/coveralls/keveri/peer-review-service/master.svg?style=flat-square)](https://coveralls.io/github/keveri/peer-review-service)

# Peer review service

Web service for handling course peer reviews.

## Development
This project requires following tools:
 - `stack`
 - `docker` (this can be avoided by installing dependencies locally)

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

## API endpoints
More detailed documentation for the API will be available separately.

GET `/` - Show API documentation.

GET `/peer-reviews` - returns peer reviews done by given email.

GET `/peer-reviews/new` - returns reviewable task for email if one can be found.

PUT `/peer-reviews/:id` - update review.

GET `/peer-reviews/completed` - returns a list of completed peer reviews that still need to be checked by an 'admin'. These are grouped by the submission they are reviewing. Definition of complete can be for example 3 PASS reviews for one submission.
