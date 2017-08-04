Try changing the motd and saving the file. It'll reload. The log
messages suggest that the server is dropping extant connection - as
expected - but they don't actually drop. They just internally reboot
as if each extant connection was fresh. The problem seems to be
something like latency in shutdown-propagation:

 - the new line-reader instance seems to be spawning before the old one exits (!!)

 - but then also we see that the reconfiguration of the session
   listener is "too quick" and doesn't wait for the old connections to
   drop, so even if the line-reader instance correctly glitched out
   the old connection, we'd still have a zombie reborn session to deal
   with.
