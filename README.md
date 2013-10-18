bootstrap
=========

The `bootstrap` application is a simple, yet poweful application to bootstrap an
Erlang cluster without having to know the exact nodenames or hostnames in
advance. It automates the process of dynamically populating the cluster with new
nodes.

* [Code](http://github.com/schlagert/bootstrap)
* [EDoc](http://schlagert.github.com/bootstrap)
* [![Build Status](https://travis-ci.org/schlagert/bootstrap.png?branch=master)](https://travis-ci.org/schlagert/bootstrap)

Features
--------

* Automatically discover and connect to nodes in an Erlang cluster.
* Utilize UDP broadcast or multicast for node discovery.
* Suppport multiple nodes per host.
* Support creation of complex topologies using `hidden` connections.
* Provide node connection/disconnection notification system.

How it works
------------

The node discovery process is based on a simple, distributed P2P protocol
defining two PDUs, the `bootstrap` _PING_ and _PONG_ messages. As mentioned
earlier, these messages are distributed via either UDP broadcast or multicast.
All nodes in the cluster __must__ run the same protocol. See below for the
representation of the messages.

```erlang
%% PING:
term_to_binary({bootstrap, {ping, PingNode :: node(), PingAddr :: inet:ip4_address()}}).

%% PONG:
term_to_binary({bootstrap, {pong, PongNode :: node(), PingNode :: node()}}).
```

It's well known that broadcast as well as multicast can load a network pretty
heavy. Therefore, the `bootstrap` application takes measures to reduce the
network traffic to a minimum using strategies known from P2P overlay protocols
designed for the use in mobile Ad-hoc networks (which are very sensitive to
extensive medium usage).

Most of the ideas behind the `bootstrap` protocol are inspired by the _Local
Broadcast Cluster_ concept described in [1]. _PING_ __and__ _PONG_ messages are
distributed as broadcast/multicast. This makes it possible to have only one
active instance of the protocol at a time. To ensure this, the `bootstrap`
protocol will detect and solve _PING_ collisions. Furthermore, it will use
information from _PONG_ messages to make distributed decisions whether to ping
or not to ping. All traffic will be stopped as soon as all nodes in the cluster
meet their connectivity needs.

TODO make more detailed description (e.g. with figures)

Configuration
-------------

The `bootstrap` application already comes with sensible defaults (except for the
regex used to decide whether to connect to a certain node or not). However,
many things can be customized if desired. For this purpose the following
configuration options are available and can be configured in the application
environment:

* `{connect_regex, Regex :: string()}`

  Specifies a regular expression to be compiled using `re:compile/1`. A
  bootstrap instance will only connect to a node if its name matches this
  expression. Furthermore, transitive connections implicitly made by `global`
  (in visible mode) will only be reported if they match this expression. Default
  is `".*"`.

* `{connect_mode, visible | hidden}`

  Specifies the type of connections that will be established by this protocol
  instance. Visible connections will most probably pull in transitive
  connections to other nodes. If a specialized, custom topology is desired this
  value should be set to `hidden`. However, __global name registration is
  disfunctional__ over `hidden` connections. Default is `visible`.

* `{min_connections, non_neg_integer() | infinity}`

  Specifies the minimum number of __matching__ connections. A node will actively
  try to get connections to matching nodes up to this number. Further
  connections may be established, but the active part of the discovery will be
  stopped when reaching this number. Active discovery will start again when the
  number of connections drops beneath this value. Default is `infinity`.

* `{primary_port, inet:port_number()}`

  Specifies the main network port broadcast or multicast packets will be sent
  over. This value must be equal for all `bootstrap` instances on all nodes.
  Default is `50337`.

* `{secondary_ports, [inet:port_number()]}`

  Specifies additional listen ports. This feature is necessary if the system
  involves more than one node per host, since there's no common way to listen
  on the same network port from multiple processes. For each additional node per
  host another network port is required. E.g. if a system has a host with three
  nodes this list must contain two entries (different to the `primary_port`
  value). Default is `[50338, 50339]`.

* `{protocol, broadcast | multicast}`

  Specifies the network protocol used for node discovery. Default is
  `broadcast`.

* `{ping_timeout, non_neg_integer()}`

  Specifies the rough time in milliseconds between two consecutive, active node
  discovery messages. Default is `10000`.

* `{broadcast_ip, BroadcastAddr :: inet:ip4_address()}`

  Specifies the address to use for broadcast discovery. This parameter is
  __optional__. If not specified `bootstrap` scans all available network
  interfaces and uses those capable of broadcast. This parameter is useful if
  you want to limit `bootstrap` traffic to a specific subnet.

* `{multicast_ip, MulticastAddr :: inet:ip4_address()}`

  Specifies the multicast address to be used for node discovery when using
  the `multicast` protocol. Default is `{239, 192, 0, 1}`.

* `{multicast_ttl, non_neg_integer()}`

  Specifies the time-to-live (TTL) of outgoing multicast packets. When setting
  the TTL to `1` all multicast packets are limited to the local network. Default
  is `1`.

The default configuration can be used to form an unbounded, mesh-connected
Erlang cluster utilizing the UDP broadcast protocol. Please note that to be able
to connect to other Erlang nodes, these nodes __must have the same Erlang
cookie__ configured.

Notifications
-------------

If the you use `bootstrap` to automatically establish connections between nodes,
configuring and starting the application on all nodes is basically all you need.
However, some use cases may make it necessary to get notified whenever a
__matching__ connection is established or lost.

For this purpose the `bootstrap` application provides the `bootstrap` behaviour.
To get notifications about node actions the two functions `on_connected/2` and
`on_disconnected/3` must be implemented. The implementing handler can then be
managed using the functions provided in the `bootstrap` module. If you already
know the `gen_event` behaviour, this will be nothing new for you. All functions
except for `add_sup_handler/2` basically do the same as the `gen_event`
equivalents. The only difference between `add_handler/2` and `add_sup_handler/2`
is that the added handler will automatically be removed when the calling process
exits. No messages will be sent to the calling process.

`bootstrap` handlers will get initial notifications for all __matching__ nodes
that are currently connected.

The `net_kernel` module also allows registration of process for node up/down
messages. However, there are some major advantages when using the `bootstrap`
notification system:
* Only notification for __matching__ nodes will be delivered.
* If using visible connections, the `on_connected/2` callback will be delayed
  until all global name servers are in sync. This means, in contrary to the
  `net_kernel` messages, a globally registered process on a newly connected node
  can be used immediatelly after receiving the corresponding `on_connected/2`
  callback.

The `example` directory contains a simple example of a `gen_server` subscribing
for `bootstrap` notifications. For more information, please refer to the `edoc`
of the `bootstrap` module.

Security
--------

The `bootstrap` application __is__ insecure. This means everyone with access to
the used broadcast/multicast domain can send, receive and read `bootstrap`
packets. __However__, to be able to connect to any of the gathered nodenames an
attacker must know the used Erlang cookie. The `bootstrap` application will
never transmit cookies over the wire in any form.

Examples
--------

This section gives some configuration examples along with respective resulting
topologies. A small note to the used figures:

* The grey `bootstrap` box outlines the view of node connections a `bootstrap`
  handler would see (the handler would get notifications for these nodes).

* The grey `net_kernel` box outlines the actual connections of a node. This is
  equivalent to the list of nodes you get from `erlang:nodes(connected)`.

### Mesh Topology

This is the basic setup you get when connecting to other nodes. Every node is
connected with every other node in the cluster. The view for `bootstrap`
handlers does not differ from the `net_kernel` view:

<img src="http://schlagert.github.com/bootstrap/mesh.svg" alt="Mesh Topology with visible connections." />

The `sys.config` configuration to build a topology like this would look like the
following:

```erlang
%% on all nodes:
[{bootstrap, [{connect_regex, ".*"}, {min_connections, 1}]}].
```

### Star Topology

This example shows the difference between `visible` and `hidden` connections.
While `visible` connections (default Erlang) always build a mesh connected
cluster (from the `net_kernel` view), `hidden` connections allow building custom
cluster topologies. However, global name registration cannot be used in this
case!

The left figure shows the `visible` connection example. In this case the
`bootstrap` filters connection notifications according to the configured
`connect_regex`. This means when connecting the cluster `bootstrap` handlers
will only get notifications for the nodes part of the `bootstrap` view of the
cluster (top of the figure), although other connections may be established
automatically by the `kernel` application.

The right figure shows the star topology _forced_ to the `net_kernel` using
`hidden` node connections that may not be used by the `kernel` to connect to
other nodes automatically.

<img src="http://schlagert.github.com/bootstrap/star.svg" alt="Star Topology with visible and hidden connections." />

The `sys.config` configuration to build a topology as shown in the __left__
example would look like the following:

```erlang
%% master node:
[{bootstrap, [{connect_regex, "slave@.*"},  {min_connections, 1}]}].

%% slave node:
[{bootstrap, [{connect_regex, "master@.*"}, {min_connections, 1}]}].
```

The `sys.config` configuration to build a topology as shown in the __right__
example would look like the following:

```erlang
%% master node:
[{bootstrap, [{connect_regex, "slave@.*"},  {connect_mode, hidden}, {min_connections, 1}]}].

%% slave node:
[{bootstrap, [{connect_regex, "master@.*"}, {connect_mode, hidden}, {min_connections, 1}]}].
```

### Tree Topology

This is a more complex example using `hidden` connections. The configurations
not only differ from level to level, beginning at level two, a node must
additionally decide to which branch of the tree it wants to connect. As can be
seen in the configurations below, level one nodes use a combined regular
expression to match `root` and `level2` nodes. A regular expression matching
only `root` nodes would be sufficient to connect the tree, however, in this case
level one nodes would not get notifications about connected level two nodes.

<img src="http://schlagert.github.com/bootstrap/tree.svg" alt="Tree Topology with hidden connections." />

The `sys.config` configuration to build a topology like this would look like the
following:

```erlang
%% root node:
[{bootstrap, [{connect_regex, "level1.*"}, {connect_mode, hidden}, {min_connections, 1}]}].

%% level1 nodes:
[{bootstrap, [{connect_regex, "(root|level2)@.*"}, {connect_mode, hidden}, {min_connections, 2}]}].

%% level2 nodes (depending on which part of the tree a node should connect to):
[{bootstrap, [{connect_regex, "level1a@.*"}, {connect_mode, hidden}, {min_connections, 1}]}].
%% or
[{bootstrap, [{connect_regex, "level1b@.*"}, {connect_mode, hidden}, {min_connections, 1}]}].
```

History
-------

### Version 0.0.1

* Regular expression based node matching
* UDP broadcast support
* UDP multicast support
* Multi-node-per-host support
* Avoid duplicate broadcasts to minimize network usage
* Behaviour-based notification system

References
----------

[1] Basissoftware für drahtlose Ad-hoc- und Sensornetze,
    P. Baumung and M. Zitterbart, 2009 Universitätsverlag Karlsruhe
