pack-it-forms msgfmt
====================

This Haskell package contains message handling code for generating and
processing form messages created with
[pack-it-forms](https://github.com/pack-it-forms/pack-it-forms).

Usage Basics
============
TDB

Contributing
============

Toolchain
---------

For repeatable builds we are using the "stack" build tool and the
associated curated repositories.  For information on stack, see the
[stack github page](https://github.com/commercialhaskell/stack#readme).

The easiest way to get setup with stack is to use one of the binary
packages.  See the documentation for instructions.  If you have an
existing GHC/cabal installation and prefer to install using that you
can do:

    $ cabal install stack

You will want to make sure that the generated executable is on your
path, either by copying it to a location already on your path or by
adding the install directory to your path.

For the best reproducability of builds we default to using stack's
support for a building using a container with the build tools.  This
works well on Linux, but may not work very well on other possible
build hosts.  In those cases you can disable the use of docker and
switch to building using a natively installed toolchain by setting
"docker -> enable" to "false".  Stack will install the toolchain in
user-isolated fashion if required.

If you use the build container you may run into a few things that need
to be dealt with to get a working setup.

First, you will need to have docker installed and running
successfully.  If you follow the
[install & getting started guide](https://docs.docker.com/mac/started/)
to the point where the hello-world container has successfully run then
it should be good enough for stack.

Secondly, when you run stack for the first time it will ask you to run
"stack docker pull" to get the required container images.  Just go
ahead and do this.  It's best to do this inside the project directory
as otherwise the version pulled may not match the version required by
the project and you will end up with multiple versions.

Finally, a more frustrating possible problem occurs when the stack
commands fail with an error about loading shared libraries.  This is
caused by the fact that the stack executable from outside the
container is mapped into the container and run inside the container to
complete the work.  However, the dynamic libraries in the container
may not match those of the host.  You might be able to work around
this by adding extra "-v" arguments to map the needed libraries into
the container in the right place.  For example adding the following to
~/stack/stack.yaml resolved the problem on one system we use:

    docker:
      enable: false
      run-args: ["-v", "/lib64/libpcre.so.1:/lib/libpcre.so.1",
                 "-v", "/lib64/libpcre.so.1.2.5:/lib/libprce.so.1.2.5"]


Explicitly setting enable to false here means that docker won't be
used by default.  Projects like this one that enable docker in their
stack configuration will override this setting, but it is convenient
to avoid spinning up a container when you are using stack outside of a
project, for example to create a new project template.


Building and Testing
--------------------

Once you have stack installed and working, building the code should be
as simple as:

    $ stack build

Testing it:

    $ stack test
