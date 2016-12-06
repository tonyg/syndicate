## Sorting out contact states

### Design

Contacts are symmetric: If A follows B, then B follows A.

Let's look at how the state of the A/B relationship changes:

 - Initial state: neither A nor B follows the other.
    - ACTION: A adds B to their contacts
       - A proposes an A/B link.
          - ACTION: A may cancel the proposition
             - Return to initial state.
          - ACTION: B may approve the proposition
             - A/B link established.
          - ACTION: B may reject the proposition
             - Return to initial state.
          - ACTION: B may ignore the proposition
             - B's user interface no longer displays the request,
               but if B subsequently proposes an A/B link, it is
               as if B approved the previously-proposed link.

 - From "A/B link established":
    - ACTION: A may cancel the link
       - Return to initial state.
    - ACTION: B may cancel the link
       - Return to initial state.

B should appear in A's contact list in any of these cases:

 1. A has proposed an A/B link.
 2. An A/B link exists.

In the first case, B should appear as a "pending link request": as
offline, with a "cancel link request" action available.

In the second case, B should appear as fully linked, either offline or
online, with a "delete contact" action available.
