Try changing the motd and saving the file. It'll reload. The log
messages suggest that the server is dropping extant connection - as
expected - but it immediately comes back momentarily before going away
properly. The session is able to reboot due to the glitching in
assertion of the listen port *more quickly* than the latency of
teardown of the previous connection; so the new session-listener
responds to the assertions from the old connection before the old
connection has a chance to die. Of course, it *does* die (since commit
11de40c), but having that zombie reborn new session is annoying.

 - This is thorny. You'd think that having a session wait for its
   line-reader to go would be enough, but the multiple nested
   during/spawns creating the sessions mean that no matter how long
   the old session instance sticks around, a new session will appear
   before we're ready! ... maybe there's no way *at all* to
   disambiguate old/new instances without, say, a unique
   listener-socket identifier??
