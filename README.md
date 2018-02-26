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
cf_client:start().
```

Which is exactly the same as calling

```erlang
application:start( cf_client ).
```

#### Starting Under the Default Supervisor

To start the Cuneiform client default supervisor under a custom supervision tree enter

```erlang
CreNode = node().
cf_client_sup:start_link( CreNode ).
```

This will register the Cuneiform client locally under the name `cf_client`.

#### Starting Directly

The Cuneiform client process can be started directly. There are several ways to do this. The first is to start the process with a function that allows it to locate the CRE:

```erlang
CreNode = node().
F = fun() -> cre:pid( CreNode ) end.
{ok, ClientPid} = cf_client_process:start_link( F ).
```

Giving a function instead of a plain Cre process identifier has the advantage, that if the CRE crashes, taking the Cuneiform client with it, the restarted client instance connects uses the output of the function, which offers the possibility of locating the CRE under its new process identifier.

If this is too tedious, one can start it giving the CRE process identifier directly:

```erlang
CrePid = cre:pid( node() ).
{ok, ClientPid} = cf_client_process:start_link( CrePid ).
```

Both previous direct starting methods do not register the Cuneiform client with any registry service. However, one can register the client process by starting it either with a function:

```erlang
ClientName = cf_client.
CreNode = node().
F = fun() -> cre:pid( CreNode ) end.
{ok, ClientPid} = cf_client_process:start_link( {local, ClientName}, F ).
```

or with the CRE process identifier:

```erlang
ClientName = cf_client.
CrePid = cre:pid( node() ).
{ok, ClientPid} = cf_client_process:start_link( {local, ClientName}, CrePid ).
```

### Interacting with a Cuneiform Client

After starting the Cuneiform client instance one can interact with it by sending it expressions conforming the Cuneiform intermediate representation. The client will reply with the value that corresponds to the expression or with an error term. Communication with the client is synchronous.

#### Sending Cuneiform Expressions

E.g., since strings are values which evaluate to themselves we can give the client a string expression, which it will parrot:

```erlang
E = cuneiform_lang:str( <<"bla">> ).
cre_client:eval( cf_client, E ).
```

Here we assume, that a Cuneiform client instance is running registered locally under `cf_client`.

#### Connecting a Shell

The Cuneiform client comes with an interactive shell which can be connected to a client instance registered under `cf_client` by entering

```erlang
cuneiform_shell:shell( cf_client ).
```

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