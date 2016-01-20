HackSense REST API
==================

REST API for the HackSense system: https://hsbp.org/HackSense

Dependencies
------------

 - Erlang R16 (works with R15, but you need to work around the usage of `binary_to_integer/1` in `src/hacksense_status.erl`)
 - everything else is automatically installed if you use rebar either directly or by running `make`

Building
--------

	$ git clone https://github.com/hsbp/hacksense-webmachine.git
	$ cd hacksense-webmachine
	$ make

Starting up
-----------

Executing `./start.sh` boots the Erlang VM and starts the REST API on http://localhost:8000 while giving you an Erlang shell. If you don't see the prompt (`1>` for the first command), press `Enter`.

To clone the backend storage from another instance, use the following command in the Erlang shell. (Don't forget the dot at the end, it's like `;` in C-like languages.)

	hacksense:import_remote_csv("https://vsza.hu/hacksense/history.csv").

Configuration
-------------

The API looks for the secret signing key in `priv/hacksense.key`, you should put some random bytes there.

License
-------

The whole project is available under MIT license, see `LICENSE.txt`.
