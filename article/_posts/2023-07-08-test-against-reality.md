---
title: Test Against Reality
summary: Against mocking.
---

When I started working as a software engineer web apps looked like this:

<img src="/assets/content/test-against-reality/first.svg" style="margin-left: auto; margin-right: auto;"/>

Then they looked like this:

<img src="/assets/content/test-against-reality/second.svg" style="margin-left: auto; margin-right: auto;"/>

Now they look more like this:

<img src="/assets/content/test-against-reality/third.svg" style="margin-left: auto; margin-right: auto;"/>

It is common to have API endpoints that look like this:

<img src="/assets/content/test-against-reality/workflow.svg" style="margin-left: auto; margin-right: auto;"/>

That is, workflows where your own code is interleaved with (or often intricately
intertwined with) external services. Increasingly, web application servers are
message brokers that do some authentication and string together I/O to databases
and external cloud services.

Testing has gotten harder. Tests that give you real confidence your code is
correct are harder to write because your code is dependent on external services
that have very complex internal rules. To make the tests resemble reality at
all, you either have to have access to these external services---cloud
dependencies---at test time, or you need to fake their behaviour perfectly.

If you had to spend money every time you ran unit tests that would be a very bad
developer experience. So the usual solution to this problem is:

1. Don't test at all, except by clicking around `staging` before deploying to
   production.
2. Mocking.

- the usual approach is mocking
  - use monkeypatching to intercept http requests, returning the values you expect
  - the problem with mocking is the tests become completely tautological
  - you're sending an http request and then faking the output
  - then that's not an integration test
  - it's just a bunch of unit tests jammed together and with very poor integration
- at previous job we had a cloud dependency: an external API we had to call from the depths of the backend
  - the way we tested it is we used patch to incerept http requests
  - and replace the response with hardcoded json
  - eventually this became very repetitive, so we have a `ServiceFake` class to simplify and factor out the mocks
  - for each api endpoint we had a method that returned a fake json blob
  - and we had a public method that exposed a ready to use monkeypatching interface
  - this really sucked
- the solution is this
  - don't mock
  - test against reality
  - if the service is part of your application, you should stand up a copy of it during testing
  - if the service is external, you should build a replica of it
    - it doesn't have to be sophisticated
    - it doesn't have to be complete, just the api endpoints you need
    - it doesn't have to have a database, in-memory hash tables are fine
  - it just has to be at least realistic
- the problem is: who tests the test server?
  - the actual question here is: where do the tests diverge from reality?
  - when you have a fake server, you only have one place for divergence: the test server.
  - when you do ad-hoc mocks, everywhere, everywhere you have mocks you can diverge from reality
  - it's easier to ensure that a tiny, sub-1000 lines Python FastAPI server that provides a fake version of S3 or Twilio or whatever is correct
  - than that a thousand ad-hoc mocks are correct
