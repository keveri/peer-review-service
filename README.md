[![Build Status](https://img.shields.io/travis/keveri/peer-review-service/master.svg?style=flat-square)](https://travis-ci.org/keveri/peer-review-service)
[![Coverage Status](https://img.shields.io/coveralls/keveri/peer-review-service/master.svg?style=flat-square)](https://coveralls.io/github/keveri/peer-review-service)
[![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg?style=flat-square)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)


# Peer review service

Web service for handling course peer reviews. The first version is done with only one course in mind. Allthough it will have abstractions for data sources it will require some work to be more general solution.

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

For test coverage run:
```
stack clean
stack test --docker --coverage
```

## API endpoints
More detailed documentation for the API will be available separately.

GET `/` - Show API documentation OR return web client for the API.

GET `/api/peer-reviews` - gets all peer reviews

Accepts the following filters:
* `filter[task]=:taskId`     - filters reviews by task
* `filter[reviewer]=:email`  - filters reviews in which user is the reviewer.
* `filter[reviewee]=:email`  - filters peer reviews in which user is the reviewee.

GET `/api/peer-reviews/:id`  - finds review by id

POST `/api/peer-reviews/create` - creates a new empty peer review for user

PUT `/api/peer-reviews/:id`     - updates a peer review
