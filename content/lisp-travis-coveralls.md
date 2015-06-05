[Travis][travis] is a service for running unit tests in the cloud. Every commit
you make and every pull request your project receives is seen by Travis, which
spins up a [Docker][docker] container and runs your tests. With a little work,
it supports Common Lisp.

Travis is controlled by a file in your repo's root, `.travis.yml`. It defines
some options, like the `language` the project is written in, the list of shell
commands that have to be executed prior to running the tests, and whom to send
notifications to.

YAML is an easy to read format for structured data. Tools like
[Ansible][ansible], [Chef][chef] and [Salt][salt] have made it popular for
configuration management and deployment. The result is basically a shell script,
but structured into different sections.

To test a Common Lisp project, we use [cl-travis][cl-travis]. This provides a
script that you can tell Travis to download, which sets up the Common Lisp
implementation of your choice (you can test on many -- more on that below), and
installs Quicklisp.

Without further ado, this is what the simplest `.travis.yml` looks like:

```
language: common-lisp
sudo: required

env:
  matrix:
    - LISP=sbcl
    - LISP=ccl

install:
  # Install cl-travis
  - curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | bash

script:
  - cl -l fiveam -l cl-coveralls
       -e '(setf fiveam:*debug-on-error* t
                 fiveam:*debug-on-failure* t)'
       -e '(setf *debugger-hook*
                 (lambda (c h)
                   (declare (ignore c h))
                   (uiop:quit -1)))'
       -e '(ql:quickload :my-project-test)'

notifications:
  email:
    - my-email@gmail.com
```

For projects that you'd like to test using the latest version of dependencies,
you can clone them to the `~/lisp` folder. For instance, this is the `install`
list of the `.travis.yml` file for [Scriba][scriba]:

```
install:
  # Install cl-travis
  - curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | bash
  # Clone the latest common-doc
  - git clone https://github.com/CommonDoc/common-doc.git ~/lisp/common-doc
  # Clone the latest common-doc-plump
  - git clone https://github.com/CommonDoc/common-doc-plump.git ~/lisp/common-doc-plump
```

Projects which require system libraries to run the tests, like [Crane][crane],
can install and configure those in the `install` list:

```
install:
  # Install cl-travis
  - curl https://raw.githubusercontent.com/luismbo/cl-travis/master/install.sh | bash
  # Install the latest versions of the major dependencies
  - git clone https://github.com/fukamachi/sxql.git quicklisp/local-projects/sxql
  - git clone https://github.com/fukamachi/cl-dbi.git quicklisp/local-projects/cl-dbi
  # Update package repos
  - sudo apt-get update
  # Install SQLite
  - sudo apt-get install -y sqlite3
  # Set up Postgres
  - sudo -u postgres createdb crane_test_db
  - sudo -u postgres psql -c "CREATE USER crane_test_user WITH PASSWORD 'crane_test_user'"
  - sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE crane_test_db TO crane_test_user"
```

Each project has a status badge you can put in the README to let users know
upfront that the project's in working order.

Coveralls also has a coverage badge, which reports the percentage of source
lines of code that are touched by unit tests.


[travis]: travis-ci.org
[docker]: https://www.docker.com/
[ansible]: http://www.ansible.com/home
[chef]: https://www.chef.io/chef/
[salt]: http://saltstack.com/
[scriba]: https://github.com/CommonDoc/scriba
[crane]: https://github.com/eudoxia0/crane
[coveralls]: https://coveralls.io/
[cl-travis]: https://github.com/luismbo/cl-travis
[cl-coveralls]: https://github.com/fukamachi/cl-coveralls
