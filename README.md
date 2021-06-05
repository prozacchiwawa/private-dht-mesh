# private-dht-mesh

A mesh network created using a simple DHT implementation.  Endpoints may communicate over the connected mesh or directly, depending on what's possible.

An example use of this dht network as a pubsub medium over NATS is in nats/wsquery.js

As presented, it was built with fable-compiler@0.7.14, in which it ran as a standalone compiler and not as webpack plugin.
