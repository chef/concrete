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
`~/.concrete_dialyzer_plt_<apps hash>_<erlang version>.plt` file and
will create a reusable PLT for the OTP modules.

Here are some examples:

```
~/.concrete_dialyzer_plt_4667e0f8e4ec738d28efbc1212495b1d_17.plt
~/.concrete_dialyzer_plt_4667e0f8e4ec738d28efbc1212495b1d_R16B02.plt
~/.concrete_dialyzer_plt_4667e0f8e4ec738d28efbc1212495b1d_R16B03.plt
~/.concrete_dialyzer_plt_ce4f2cc7cbd1bdd337c6ba5d475c1290_R16B03.plt
```

By default, concrete will build a local PLT file containing analysis
of the dependencies of your project found in the `deps`
directory. When you run the dialyzer target via `make dialyzer`, the
global `~/.concrete_dialyzer_plt_<apps hash>_<erlang version>.plt` is
combined with the project-specific `deps.plt` to analze your
code. Including dependencies in the analysis is important to get the
most out of dialyzer and precomputing a PLT for your deps saves time.

You may encounter some dependenices which do not play well with
dialyzer. You can tell concrete to omit these problem dependencies by
adding them to a `DIALYZER_SKIP_DEPS` make variable in your
`Makefile`.

In general, concrete should be able to detect when `deps.plt` needs to
be rebuilt. If you are encountering confusing dialyzer warnings and
have recently updated your dependencies, you can remove
`deps.plt` and rebuild.

#### Travis CI ####

Concrete will attempt to pull in cached PLTs from S3 when running on
Travis CI. They're built for Travis, so they won't be downloaded
locally. We got them here:
[ESL/erlang-plts](https://github.com/esl/erlang-plts). Thanks ESL!

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

Another way to think of `DEV_MODE` is "top level project mode". When
your project is included as someone else's dependency, `rebar` will
not pull in the `dev_only_deps` into their project.

### Generate markdown docs via edown ###

By default, concrete will include [edown][] as a dev only dependency
and use it to generate markdown from the edoc in your code when you
run `make doc`. You can disable this behavior by adding the following
to your `rebar.config` file:

```
 {use_edown, false}.
```

### Custom Makefile Targets ###

Concrete supports a `custom.mk` file which will not be overwritten
when concrete is upgraded. You can put any custom makefile targets or
environment variables in this file. Any modifications you make to
`Makefile` and `concrete.mk` will be overwritten when you run
`concrete update`, so put anything you don't want to lose in
`custom.mk`

## Installation ##

1. Clone the concrete repo
2. Build the project
3. Add the `concrete` escript to your PATH. NOTE: the concrete escript
   cannot be relocated because it locates the template files in
   `priv/templates` based on the location of the executable.

   ```
     git clone git://github.com/opscode/concrete.git
     cd concrete
     make
     # now add `pwd` to your PATH
   ```

Is this thing on? Let's find out!

 ```
 concrete init infodata
 cd infodata
 make
 ```

## Concrete Examples ##

### Initialize a new project with `concrete init` ###

1. Make sure the `concrete` escript is on your `PATH`.
2. Run `concrete init NAME`, where `NAME` is your desired project
   name. A directory named `NAME` will be created in your current
   working directory with project skeleton. You will be asked if you
   want an active application. If you answer "yes", then the generated
   project will include a supervisor and the application will be
   startable. In this case, concrete will also generate a
   `relx.config` and you can build an OTP release via `make rel`.

The whole sequence should look like this:

```
$ concrete init apples
Creating the apples project with concrete

Would you like an active application? (y/n): y
Now try: cd apples; make

$ ls -a apples
   .concrete/  .gitignore  Makefile  README.md  concrete.mk include/  priv/  rebar.config  rebar.config.script  src/  test/
```

**important** Add the files that concrete created for you to git.
Be sure that you `git add` the following:

```
git add concrete.mk
git add rebar.config
git add rebar.config.script
git add Makefile
```

### Updating an existing project ###

When you run `concrete update`, concrete will create backup copies of
your `Makefile`, `rebar.config.script`, and `concrete.mk`. Then
it will copy the latest version of those files over.

The purpose of the update command is to make it easier to receive
fixes and features when they are added to the make rules or rebar
config script that are part of concrete's project templates.

You can also use the update command to help convert an existing
project to use concrete.

## How It Works ##

A project that uses concrete, will have a minimal `Makefile` that
includes a set of standard rules in `concrete.mk`. The project
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
| **Copyright:**       | Copyright (c) 2013-2014 Opscode, Inc.
| **License:**         | Apache License, Version 2.0

See [LICENSE](./LICENSE).
