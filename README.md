# concrete: enhance your rebar build experience #

Concrete enhances your rebar based Erlang project by providing a
common Makefile wrapper, a dialyzer make target that caches PLT
analysis of your project's dependencies, and a mechanism to specify
development only dependencies.

## Features ##

### Standard make targets ###

* all
* allclean
* clean
* compile
* dialyzer
* distclean
* doc
* eunit
* tags
* test

### Dialyzer config ###

The makefile rules included in concrete will check for a
`~/.dialyzer.plt` file and will create a reusable PLT for the OTP
modules.

By default, concrete will build a local PLT file containing analysis
of the dependencies of your project found in the `deps`
directory. When you run the dialyzer target via `make dialyzer`, the
global `~/.dialyzer.plt` is combined with the project-specific
`deps.plt` to analze your code. Including dependencies in the analysis
is important to get the most out of dialyzer and precomputing a PLT
for your deps saves time.

You may encounter some dependenices which do not play well with
dialyzer. You can tell concrete to omit these problem dependencies by
adding them to a `DIALYZER_SKIP_DEPS` make variable in your
`Makefile`.

In general, concrete should be able to detect when `deps.plt` needs to
be rebuilt. If you are encountering confusing dialyzer warnings and
have recently updated your dependencies, you can remove
`deps.plt` and rebuild.

### Dev only dependencies ###

You can specify dependencies that are only needed during development
using the `dev_only_deps` key in your `rebar.config` file. Example:
```
{dev_only_deps,
 [
  {proper, ".*", {git, "git://github.com/manopapad/proper.git", "master"}}
 ]}.
```

When you run `make`, concrete will create `.concrete/DEV_MODE` and use
this file as an indicator to include dev_only_deps in the build. These
dependencies will not be incurred by other projects that add your
project as a dependency.

When in dev mode, concrete will define a macro `DEV_ONLY` which can be
used to conditionally include test code that makes use of a dev only
dependency.

### Generate markdown docs via edown ###

By default, concrete will include [edown][] as a dev only dependency
and use it to generate markdown from the edoc in your code when you
run `make doc`. You can disable this behavior by adding the following
to your `rebar.config` file:

```
 {use_edown, false}.
```

## Installation ##

1. Put a copy of concrete in rebar's template directory.

   ```
     mkdir -p ~/.rebar/templates
     cd ~/.rebar/templates
     git clone git://github.com/opscode/concrete.git
     cd concrete
     rebar compile escriptize
     # you could also just run make since concrete self-hosts
   ```

2. Add the concrete executable to your path. The concrete escript
   needs to access files in the priv directory of the checkout. You
   cannot relocate the escript.

    ```
     echo "export PATH=~/.rebar/templates/concrete:$PATH" >> ~/.zshrc
    ```

## Concrete Examples ##

### Initialize a new project with `concrete init` ###

0. Make sure the `concrete` escript is on your `PATH`.
1. Create a directory for your new project. It is important that it is
   empty.
2. Run `concrete init` and provide name and description when
   prompted (newline terminates input).

The whole sequence should look like this:

```
 $ mkdir apples
 $ cd apples
 $ concrete init
 Initialize a new project with concrete

 Project name: apples
 Short Description:
 What do you want, this is just an example
 Creating apples via 'rebar create template=concrete_project name=apples description="What do you want, this is just an example"'
 Now try: make

 $ ls -a
   .concrete/  .gitignore  Makefile  README.md  include/  priv/  rebar.config  rebar.config.script  src/  test/
```

**important** Add the files that concrete created for you to git.
Be sure that you `git add1 the following:

```
git add .concrete/rebar.mk
git add rebar.config
git add rebar.config.script
git add Makefile
```

### Updating an existing project ###

When you run `concrete update`, concrete will create backup copies of
your `Makefile`, `rebar.config.script`, and `.concrete/rebar.mk`. Then
it will copy the latest version of those files over.

The purpose of the update command is to make it easier to receive
fixes and features when they are added to the make rules or rebar
config script that are part of concrete's project templates.

You can also use the update command to help convert an existing
project to use concrete.

## How It Works ##

A project that uses concrete, will have a minimal `Makefile` that
includes a set of standard rules in `.concrete/rebar.mk`. The project
will also contain both a `rebar.config` file and a
`rebar.config.script` file. The `.script` config is evaluated by rebar
after the standard config is read. The script looks for
`.concrete/DEV_MODE` and decides whether or not to add dev_only_deps.

Initially, I was planning on making concrete a dependency that you
would include in a project with the possibility of picking up the
latest build rules by refetching the concrete dep. Keeping working
builds working seems more important and in the common case it should
still be easy to pickup updates when desired.

## License ##

|                      |                                          |
|:---------------------|:-----------------------------------------|
| **Copyright:**       | Copyright (c) 2013 Opscode, Inc.
| **License:**         | Apache License, Version 2.0

See [LICENSE](./LICENSE).
