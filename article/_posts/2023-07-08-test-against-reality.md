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

That is, workflows where your own code is interleaved with (and often
intricately intertwined with) external services. Increasingly, web application
servers are message brokers that do some authentication and string together I/O
to databases and external cloud services.

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

# Mocking

Mocking is when you write a module or class that provides the same interface as
the real thing, with different internals. Mocks can be classified by how close
to reality they are.

A common type of mock is to replace specific operations with hardcoded
outputs. For example, where you might upload an object to S3, you intercept the
HTTP request and return a hardcoded JSON output with a fake ID and a successful
status code. Further down, where you might download the data from S3, you again
intercept the request and request hardcoded data. You can test the whole
workflow this way, every time you need access to an external service, you
hardcode the outputs.

The problem with this approach is it's completely tautological. What are you
asserting? The tests themselves. Your tests reduce to:

```python
response = "derp"
assert response == "derp"
```

But this isn't obvious, because hundreds of lines of boilerplate obscure this.

Another problem with fine-grained mocking is the mocked operations are smeared
across all the tests, rather than being centralized. If you make a mistake in
the hardcoded output, the mistake is not in one central place where fixing it
fixes it for all tests, it's smeared everywhere.

Your tests look like integration tests, but in fact what you have are a bunch of
unit tests jammed into the same file, with a bunch of ad-hoc glue in between.

Bad mocks are:

1. Too fine-grained.
2. Have hardcoded values that have to be coordinated with the unit tests.
3. Reduce to tautologies.

# Test Against Reality

Prefer, in descending order:

1. Test against a local instance of the real service.
1. Either:
   1. Write a **service stub:** test against a stub object that internally
      implements the semantics of the service.
   1. Write a **fake server:** test against a fake server that internally
      implements the semantics of the service.

So, if you don't have the source code of the service, you have to reimplement
it, either in the source code

# Approach: Service Stubs

Define an interface for the service. Write an implementation that hits the real
thing. Write another implementation that provides the same services. Store data
in memory. When setting up the tests, pass in the fake implementation.

This has two problems:

1. What if the real implementation is wrong? (e.g.: parses JSON responses
   incorrectly)
2. What if the fake incorrectly implements the real thing?

To solve both problems, you can write a script that performs operations against
both implementations, and checks that their values are either correct or
compatible. This doesn't have to run along the main unit tests (because it costs
money), but it can be run occassionally or to debug issues in the service stub.

The term is from [_Patterns of Enterprise Application Architecture_][peaa].

[peaa]: https://martinfowler.com/eaaCatalog/serviceStub.html

# Approach: Fake Servers

Sometimes, access to the external service is not easily centralized, maybe
because you have multiple services (in multiple languages) that all access
it. In that case, rather than faking the interface for each service, you fake
the whole thing: you write a quick and dirty server that exposes the endpoints
you need, and implements the semantics of the real service, storing data in
memory or whatever.

It doesn't have to be a complete, or particularly sophisticated implementation:
a sub-1000 file Python script often works. It's then easier to ensure the fake
server is correct, than to check the correctness of a thousand ad-hoc mocks
smeared across the codebase.

# Case Study

At a previous job we had a cloud dependency: the backend called an external API
with complex internals. We had an API client class, so access to the API was
centralized in one place. But the class was instantiated whereever it was
needed, rather than passed in from the top.

The tests of operations that needed the API would mock individual requests, by
using [`patch`][patch] to override the HTTP client, intercepting the requests by
testing against the URL, and returning hardcoded JSON blobs. And the JSON had to
be finely-tuned to that specific unit tests, so the assertions would work out.

[patch]: https://docs.python.org/3/library/unittest.mock.html#patch

At some point I, or a coworker, factored some of that out into an `AcmeFake`
class that returned a standard set of fake hardcoded values. I think that at one
point we had two slightly-different fake classes in different places because of
import visibility rules.

A better approach would have been to write an API client class that replicated
the external service, albeit at a lower fidelity, and monkeypatching the
function that instantiates the client. Or, if we wanted to test we were sending
correctly-structured JSON requests, and correctly parsing the JSON responses, we
could have build a fake server in Python and stood it up during the tests, and
changed the service endpoint.
