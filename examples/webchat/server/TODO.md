✓ Remove delete-account, use delete-resource of an account instead

✓ Reimplement spawn-session-monitor and end-session to work in terms
of create-resource and delete-resource, but leave login-link
idiosyncratic

Factor out resource management into its own module. Introduce a macro
for describing resources, their cascading deletion conditions, and
their potential automatic expiries.

Build a persistent resource management module. Adjust
`immediate-query` to be able to use an alternative `flush!` routine.

  NOTE that automatic expiry in the direct implementation is as simple
  as `stop-when-timeout`, but cannot be this simple in a persistent
  implementation: instead, I plan on producing a special "expiries"
  table, each entry of which specifies a message to send upon expiry.

  NOTE that the trick of producing a base `p:follow` grant record on
  account creation has to be done differently when there's no
  always-on account process.

  NOTE that the trick of deleting an `invitation` when a matching
  `in-conversation` appears also has to be done differently, similarly
  to the `p:follow` grant mentioned above. However, this might be able
  to be automated: if there's some kind of `(stop-when E)` where `E`
  mentions some field or fields of a resource, matching resources can
  be deleted from the persistent store by an auxiliary process. This
  would require fairly hairy syntactic analysis though, so it might be
  better to have some kind of `cascading-delete-when` form to spell
  out what should be removed on a given event. (Then the `p:follow`
  case above can be implemented with some kind of
  `cascading-insert-when`?)

  NOTE that these differences are OK: this is the first time Syndicate
  has tackled persistence at all in any interesting way.
