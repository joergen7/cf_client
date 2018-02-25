# cf_client
###### A Cuneiform client implementation.

[![hex.pm](https://img.shields.io/hexpm/v/cf_client.svg?style=flat-square)](https://hex.pm/packages/cf_client) [![Build Status](https://travis-ci.org/joergen7/cf_client.svg?branch=master)](https://travis-ci.org/joergen7/cf_client)

cf_client is a client implementation for the common runtime environment (CRE) instantiating an interpreter for the Cuneiform functional language for large-scale data analysis.

This application packages a Cuneiform scanner and parser, as well as an interpreter that distributes tasks by sending it to a CRE instance. The Cuneiform client can be used to consume a Cuneiform source file from the file system or to start an interactive shell.

## Usage

### Adding the CRE to a Project

Although the Cuneiform client application can be imported also directly from GitHub, we recommend adding a dependency via [hex.pm](https://hex.pm). Here, we show how this can be done using the build tools [rebar3](https://www.rebar3.org) or mix.


#### rebar3

To integrate the Cuneiform client application into a rebar3-managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{cf_client, "0.1.0"}`.

```erlang
{deps, [{cf_client, "0.1.0"}]}.
```


#### mix

```elixir
{:cf_client, "~> 0.1.0"}
```

### Compiling

Having rebar3 available on your system, compile the project as an Erlang project by entering

    rebar3 compile

If you want to drive the project from the command line please compile the project by entering

    rebar3 escriptize

### Starting the Cuneiform Client

The Cuneiform client can be started in several different ways. It can be started from the command line, as an Erlang application, as a supervision tree hosting a single client process, or directly as a process. In all cases there has to be a way for the client to find the CRE instance it connects to.

#### Starting from the Command Line

Compiling the Cuneiform client using `escriptize` creates an Erlang script file `cf_client` which allows starting the Cuneiform client via the command line. Start the script and connect with a running CRE instance by entering

    ./cf_client -c cre@my_node

Here, we assume that the CRE runs on an Erlang node identified as `cre@my_node`.

#### Starting as an Erlang Application

If a CRE instance is already running on the same Erlang node you can start the Cuneiform client application by calling

```erlang
cf_client:start()
```

Which is exactly the same as calling

```erlang
application:start( cf_client ).
```

#### Starting Under the Default Supervisor

#### Starting Directly

### Interacting with a Cuneiform Client


## System Requirements

- [Erlang](https://www.erlang.org) OTP 18.0 or higher
- [Rebar3](https://www.rebar3.org) 3.0.0 or higher

## Resources

- [joergen7/cre](https://github.com/joergen7/cre). A common runtime environment (CRE) for distributed workflow languages.
- [joergen7/cuneiform](https://github.com/joergen7/cuneiform). A functional language for large-scale data analysis whose distributed execution environment is implemented on top of the CRE.


## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)