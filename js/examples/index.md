---
---

# Syndicate/js Examples

This page describes some of the example programs that are part of the
JavaScript implementation of Syndicate.

## Clickable Button

This is a simple clickable button; each time the button is clicked,
the number on the face of the button is incremented.

The actor maintaining the counter also maintains the button's label
and listens to click events. It uses the Syndicate/js DOM driver to
publish the button's label text based on its internal state, and the
Syndicate/js jQuery driver to subscribe to button click events.

 - [DEMO](button/)
 - [Source code](button/index.js) using the Syndicate/js DSL

## DOM example

This example demonstrates two actors, each using the Syndicate/js DOM
driver to display user interface, and the jQuery driver to receive
events from it. The first actor presents a button to the user, which
when clicked sends a message to the other actor. The second actor
receives messages from the first, updates its internal state, and
reflects its new internal state in its visible UI.

 - [DEMO](dom/)
 - [Source code](dom/index.js) in plain JavaScript

## jQuery Example

This example is similar to the button example, but uses plain
JavaScript instead of the Syndicate/js DSL, calling out to Syndicate
as a library. It uses the Syndicate/js jQuery driver to receive click
events from the button, but does not use the Syndicate/js DOM driver;
instead, it updates the DOM directly.

 - [DEMO](jquery/)
 - [Source code](jquery/index.js) in plain JavaScript

## Text Entry Widget

This is a simple text entry GUI control, following a design of
[Hesam Samimi](http://www.hesam.us/cs/cooplangs/textfield.pdf).

Samimi's design proceeds in two stages. In the first, it calls for two
components: one representing the *model* of a text field, including
its contents and cursor position, and one acting as the *view*,
responsible for drawing the widget and interpreting keystrokes. In the
second stage, a *search* component is added, responsible for searching
the current content of the model for a pattern and collaborating with
the view to highlight the results.

This Syndicate solution naturally has an actor for each of the three
components. The model actor maintains the current contents and cursor
position as assertions in the shared dataspace. The view actor
observes these assertions and, when they change, updates the display.
It also subscribes to keystroke events and translates them into
messages understandable to the model actor. The addition of the search
actor necessitates no changes to the model actor. The search actor
observes the assertion of the current content of the field in the same
way the view actor does. If it finds a matching substring, it asserts
this fact. The view actor must observe these assertions and highlight
any corresponding portion of text.

There are two implementations, one using Syndicate events and actions
directly, and one using the high-level Syndicate DSL.

 - High-level DSL implementation
    - [DEMO](textfield-dsl/)
    - [Source code](textfield-dsl/index.js) using the Syndicate/js DSL
 - Low-level implementation
    - [DEMO](textfield/)
    - [Source code](textfield/index.js) in plain JavaScript
