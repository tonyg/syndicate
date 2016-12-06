(function () {
  // N.B.: "window.status" is an HTML-defined property, and always a
  // string, so naming things at "global"-level `status` will not have
  // the desired effect!
  assertion type online();
  assertion type present(email);

  assertion type uiTemplate(name, data) = "ui-template";

  assertion type permitted(issuer, email, permission, isDelegable);
  assertion type grant(issuer, grantor, grantee, permission, isDelegable);
  assertion type permissionRequest(issuer, grantee, permission) = "permission-request";

  assertion type conversation(id, title, creator, blurb);
  assertion type invitation(conversationId, inviter, invitee);
  assertion type inConversation(conversationId, member) = "in-conversation";
  assertion type post(id, timestamp, conversationId, contentType, content);

  message type createResource(description) = "create-resource";
  message type updateResource(description) = "update-resource";
  message type deleteResource(description) = "delete-resource";

  assertion type pFollow(email) = "p:follow";
  // assertion type pInvite(email) = "p:invite";
  // assertion type pSeePresence(email) = "p:see-presence";

  assertion type contactListEntry(owner, member) = "contact-list-entry";

  assertion type question(id, timestamp, klass, target, title, blurb, type);
  assertion type answer(id, value);
  assertion type yesNoQuestion(falseValue, trueValue) = "yes/no-question";
  assertion type optionQuestion(options) = "option-question";
  // ^ options = [[Any, Markdown]]
  assertion type textQuestion(isMultiline) = "text-question";
  assertion type acknowledgeQuestion() = "acknowledge-question";

  //---------------------------------------------------------------------------

  var brokerConnected = Syndicate.Broker.brokerConnected;
  var brokerConnection = Syndicate.Broker.brokerConnection;
  var toBroker = Syndicate.Broker.toBroker;
  var fromBroker = Syndicate.Broker.fromBroker;
  var forceBrokerDisconnect = Syndicate.Broker.forceBrokerDisconnect;

  ///////////////////////////////////////////////////////////////////////////

  function compute_broker_url() {
    var u = new URL(document.location);
    u.protocol = (u.protocol === 'http:') ? 'ws:' : 'wss:';
    u.pathname = '/broker';
    u.hash = '';
    return u.toString();
  }

  var sessionInfo = {}; // filled in by 'load' event handler
  var brokerUrl = compute_broker_url();

  function outbound(x) {
    return toBroker(brokerUrl, x);
  }

  function inbound(x) {
    return fromBroker(brokerUrl, x);
  }

  function avatar(email) {
    return 'https://www.gravatar.com/avatar/' + md5(email.trim().toLowerCase()) + '?s=48&d=retro';
  }

  ///////////////////////////////////////////////////////////////////////////

  window.addEventListener('load', function () {
    if (document.body.id === 'webchat-main') {
      $('head meta').each(function (_i, tag) {
        var itemprop = tag.attributes.itemprop;
        var prefix = 'webchat-session-';
        if (itemprop && itemprop.value.startsWith(prefix)) {
          var key = itemprop.value.substring(prefix.length);
          var value = tag.attributes.content.value;
          sessionInfo[key] = value;
        }
      });
      webchat_main();
    }
  });

  function webchat_main() {
    ground dataspace G {
      Syndicate.UI.spawnUIDriver({
        defaultLocationHash: '/conversations'
      });
      Syndicate.WakeDetector.spawnWakeDetector();
      Syndicate.Broker.spawnBrokerClientDriver();
      spawnInputChangeMonitor();

      actor {
        this.ui = new Syndicate.UI.Anchor();
        var mainpage_c = this.ui.context('mainpage');

        field this.connectedTo = null;
        field this.myRequestCount = 0; // requests *I* have made of others
        field this.otherRequestCount = 0; // requests *others* have made of me
        field this.questionCount = 0; // questions from the system
        field this.globallyVisible = false; // mirrors *other people's experience of us*
        field this.locallyVisible = true;
        field this.showRequestsFromOthers = false;

        assert brokerConnection(brokerUrl);

        on asserted brokerConnected($url) { this.connectedTo = url; }
        on retracted brokerConnected(_) { this.connectedTo = null; }

        during inbound(online()) {
          on start { this.globallyVisible = true; }
          on stop { this.globallyVisible = false; }
        }

        during inbound(question($qid, _, _, sessionInfo.email, _, _, _)) {
          on start { this.questionCount++; }
          on stop { this.questionCount--; }
        }

        during inbound(permissionRequest($issuer, sessionInfo.email, $permission)) {
          on start { this.myRequestCount++; }
          on stop { this.myRequestCount--; }
        }

        during inbound(uiTemplate("nav-account.html", $entry)) {
          var c = this.ui.context('nav', 0, 'account');
          assert outbound(online()) when (this.locallyVisible);
          assert c.html('#nav-ul', Mustache.render(entry, {
            email: sessionInfo.email,
            avatar: avatar(sessionInfo.email),
            questionCount: this.questionCount,
            myRequestCount: this.myRequestCount,
            otherRequestCount: this.otherRequestCount,
            globallyVisible: this.globallyVisible,
            locallyVisible: this.locallyVisible
          }));
          on message c.event('.toggleInvisible', 'click', _) {
            this.locallyVisible = !this.locallyVisible;
          }
        }

        during Syndicate.UI.locationHash('/contacts') {
          during inbound(uiTemplate("page-contacts.html", $mainEntry)) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {}));
          }

          during inbound(uiTemplate("contact-entry.html", $entry)) {
            during mainpage_c.fragmentVersion($mainpageVersion) {
              during inbound(contactListEntry(sessionInfo.email, $contact)) {
                field this.pendingContactRequest = false;
                field this.isPresent = false;
                during inbound(present(contact)) {
                  on start { this.isPresent = true; }
                  on stop { this.isPresent = false; }
                }
                during inbound(permissionRequest(contact, sessionInfo.email, pFollow(contact))) {
                  on start { this.pendingContactRequest = true; }
                  on stop { this.pendingContactRequest = false; }
                }
                var c = this.ui.context(mainpageVersion, 'all-contacts', contact);
                assert c.html('.contact-list', Mustache.render(entry, {
                  email: contact,
                  avatar: avatar(contact),
                  pendingContactRequest: this.pendingContactRequest,
                  isPresent: this.isPresent
                }));
                on message c.event('.delete-contact', 'click', _) {
                  if (confirm((this.pendingContactRequest
                               ? "Cancel contact request to "
                               : "Delete contact ")
                              + contact + "?")) {
                    :: outbound(deleteResource(permitted(sessionInfo.email,
                                                         contact,
                                                         pFollow(sessionInfo.email),
                                                         false))); // TODO: true too?!
                  }
                }
              }
            }
          }

          during mainpage_c.fragmentVersion($mainpageVersion) {
            during inputValue('#add-contact-email', $rawContact) {
              var contact = rawContact && rawContact.trim();
              if (contact) {
                on message mainpage_c.event('#add-contact', 'click', _) {
                  :: outbound(createResource(grant(sessionInfo.email,
                                                   sessionInfo.email,
                                                   contact,
                                                   pFollow(sessionInfo.email),
                                                   false)));
                  $('#add-contact-email').val('');
                }
              }
            }
          }
        }

        during Syndicate.UI.locationHash('/permissions') {
          during inbound(uiTemplate("page-permissions.html", $mainEntry)) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {}));
          }

          during inbound(uiTemplate("permission-entry.html", $entry)) {
            during mainpage_c.fragmentVersion($mainpageVersion) {
              during inbound(permitted($i, $e, $p, $d)) {
                if (i !== sessionInfo.email) {
                  var c = this.ui.context(mainpageVersion, 'permitted', i, e, p, d);
                  assert c.html('#permissions', Mustache.render(entry, {
                    issuer: i,
                    email: e,
                    permission: JSON.stringify(p),
                    isDelegable: d,
                    isRelinquishable: i !== e
                  }));
                  on message c.event('.relinquish', 'click', _) {
                    :: outbound(deleteResource(permitted(i, e, p, d)));
                  }
                }
              }
            }
          }

          during inbound(uiTemplate("grant-entry.html", $entry)) {
            during mainpage_c.fragmentVersion($mainpageVersion) {
              during inbound(grant($i, sessionInfo.email, $ge, $p, $d)) {
                var c = this.ui.context(mainpageVersion, 'granted', i, ge, p, d);
                assert c.html('#grants', Mustache.render(entry, {
                  issuer: i,
                  grantee: ge,
                  permission: JSON.stringify(p),
                  isDelegable: d
                }));
                on message c.event('.revoke', 'click', _) {
                  :: outbound(deleteResource(grant(i, sessionInfo.email, ge, p, d)));
                }
              }
            }
          }
        }

        during Syndicate.UI.locationHash('/my-requests') {
          during inbound(uiTemplate("page-my-requests.html", $mainEntry)) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
              myRequestCount: this.myRequestCount
            }));
          }

          during inbound(permissionRequest($issuer, sessionInfo.email, $permission)) {
            during inbound(uiTemplate("permission-request-out-GENERIC.html", $genericEntry)) {
              during mainpage_c.fragmentVersion($mainpageVersion) {
                var c = this.ui.context(mainpageVersion, 'my-permission-request', issuer, permission);
                field this.entry = genericEntry;
                assert c.html('#my-permission-requests', Mustache.render(this.entry, {
                  issuer: issuer,
                  permission: permission,
                  permissionJSON: JSON.stringify(permission)
                })) when (this.entry);
                var specificTemplate = "permission-request-out-" +
                    encodeURIComponent(permission.meta.label) + ".html";
                on asserted inbound(uiTemplate(specificTemplate, $specificEntry)) {
                  this.entry = specificEntry || genericEntry;
                }
                on message c.event('.cancel', 'click', _) {
                  :: outbound(deleteResource(permissionRequest(issuer, sessionInfo.email, permission)));
                }
              }
            }
          }
        }

        during Syndicate.UI.locationHash('/questions') {
          during inbound(uiTemplate("page-questions.html", $mainEntry)) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
              questionCount: this.questionCount,
              otherRequestCount: this.otherRequestCount,
              showRequestsFromOthers: this.showRequestsFromOthers
            }));
          }

          during mainpage_c.fragmentVersion($mainpageVersion) {
            during inputValue('#show-all-requests-from-others', $showRequestsFromOthers) {
              on start { this.showRequestsFromOthers = showRequestsFromOthers; }
            }
          }

          during inbound(uiTemplate("permission-request-in-GENERIC.html", $genericEntry)) {
            during mainpage_c.fragmentVersion($mainpageVersion) {
              during inbound(permissionRequest($issuer, $grantee, $permission)) {
                if (grantee !== sessionInfo.email) {
                  on start { this.otherRequestCount++; }
                  on stop { this.otherRequestCount--; }

                  var c = this.ui.context(mainpageVersion, 'others-permission-request', issuer, grantee, permission);
                  field this.entry = genericEntry;
                  assert c.html('#others-permission-requests', Mustache.render(this.entry, {
                    issuer: issuer,
                    grantee: grantee,
                    permission: permission,
                    permissionJSON: JSON.stringify(permission)
                  })) when (this.entry);
                  var specificTemplate = "permission-request-in-" +
                      encodeURIComponent(permission.meta.label) + ".html";
                  on asserted inbound(uiTemplate(specificTemplate, $specificEntry)) {
                    this.entry = specificEntry || genericEntry;
                  }
                  on message c.event('.grant', 'click', _) {
                    :: outbound(createResource(grant(issuer,
                                                     sessionInfo.email,
                                                     grantee,
                                                     permission,
                                                     false)));
                  }
                  on message c.event('.deny', 'click', _) {
                    :: outbound(deleteResource(permissionRequest(issuer, grantee, permission)));
                  }
                }
              }
            }
          }

          during inbound(question($qid, $timestamp, $klass, sessionInfo.email, $title, $blurb, $qt))
          {
            during mainpage_c.fragmentVersion($mainpageVersion) {
              var c = this.ui.context(mainpageVersion, 'question', timestamp, qid);

              switch (qt.meta.label) {
                case "option-question": {
                  var options = qt.fields[0];
                  during inbound(uiTemplate("option-question.html", $entry)) {
                    assert c.html('#question-container', Mustache.render(entry, {
                      questionClass: klass,
                      title: title,
                      blurb: blurb,
                      options: options
                    }));
                    on message c.event('.response', 'click', $e) {
                      react { assert outbound(answer(qid, e.target.dataset.value)); }
                    }
                  }
                  break;
                }
                default: {
                  break;
                }
              }
            }
          }
        }

        var conversations_re = /^\/conversations(\/(.*))?/;
        during Syndicate.UI.locationHash($locationHash) {
          var m = locationHash.match(conversations_re);
          if (m) {
            var selectedCid = m[2] || null;

            during inbound(uiTemplate("page-conversations.html", $mainEntry)) {
              assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
                selectedCid: selectedCid
              }));
            }

            during inbound(uiTemplate("conversation-index-entry.html", $indexEntry)) {
              during mainpage_c.fragmentVersion($mainpageVersion) {
                during inbound(inConversation($cid, sessionInfo.email)) {
                  field this.members = Immutable.Set();
                  on asserted inbound(inConversation(cid, $who)) {
                    this.members = this.members.add(who);
                  }
                  on retracted inbound(inConversation(cid, $who)) {
                    this.members = this.members.remove(who);
                  }
                  during inbound(conversation(cid, $title, $creator, $blurb)) {
                    var c = this.ui.context(mainpageVersion, 'conversationIndex', cid);
                    assert c.html('#conversation-list', Mustache.render(indexEntry, {
                      isSelected: selectedCid === cid,
                      selectedCid: selectedCid,
                      cid: cid,
                      title: title,
                      creator: creator,
                      members: this.members.toArray()
                    }));
                    on message c.event('.card-block', 'click', _) {
                      if (selectedCid === cid) {
                        :: Syndicate.UI.setLocationHash('/conversations');
                      } else {
                        :: Syndicate.UI.setLocationHash('/conversations/' + cid);
                      }
                    }
                  }
                }
              }
            }
          }
        }

        during Syndicate.UI.locationHash('/new-chat') {
          field this.invitees = Immutable.Set();
          field this.searchString = '';
          field this.displayedSearchString = ''; // avoid resetting HTML every keystroke. YUCK
          field this.suggestedTitle = '';

          dataflow {
            this.suggestedTitle = this.invitees.toArray().join(', ');
          }

          during inbound(uiTemplate("page-new-chat.html", $mainEntry)) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
              noInvitees: this.invitees.isEmpty(),
              searchString: this.displayedSearchString,
              suggestedTitle: this.suggestedTitle
            }));
          }

          during mainpage_c.fragmentVersion($mainpageVersion) {
            on message Syndicate.UI.globalEvent('#search-contacts', 'keyup', $e) {
              this.searchString = e.target.value.trim();
            }

            on message Syndicate.UI.globalEvent('.create-conversation', 'click', _) {
              // TODO: ^ Would like to use
              // mainpage_c.event('.create-conversation', 'click', _)
              // here, but the DOM nodes aren't created in time, it seems
              if (!this.invitees.isEmpty()) {
                var title = $('#conversation-title').val() || this.suggestedTitle;
                var blurb = $('#conversation-blurb').val();
                var cid = random_hex_string(32);
                :: outbound(createResource(conversation(cid, title, sessionInfo.email, blurb)));
                :: outbound(createResource(inConversation(cid, sessionInfo.email)));
                this.invitees.forEach(function (invitee) {
                  :: outbound(createResource(invitation(cid, sessionInfo.email, invitee)));
                });
                :: Syndicate.UI.setLocationHash('/conversations/' + cid);
              }
            }
          }

          during inbound(uiTemplate("invitee-entry.html", $entry)) {
            during mainpage_c.fragmentVersion($mainpageVersion) {
              during inbound(contactListEntry(sessionInfo.email, $contact)) {
                field this.isPresent = false;
                field this.isInvited = false;
                dataflow {
                  this.isInvited = this.invitees.contains(contact);
                }
                during inbound(present(contact)) {
                  on start { this.isPresent = true; }
                  on stop { this.isPresent = false; }
                }
                var c = this.ui.context(mainpageVersion, 'all-contacts', contact);
                assert c.html('.contact-list', Mustache.render(entry, {
                  email: contact,
                  avatar: avatar(contact),
                  isPresent: this.isPresent,
                  isInvited: this.isInvited
                })) when (this.isInvited ||
                          !this.searchString ||
                          contact.indexOf(this.searchString) !== -1);
                on message c.event('.toggle-invitee-status', 'click', _) {
                  if (this.invitees.contains(contact)) {
                    this.invitees = this.invitees.remove(contact);
                  } else {
                    this.invitees = this.invitees.add(contact);
                  }
                  this.displayedSearchString = this.searchString;
                }
              }
            }
          }
        }
      }
    }

    // G.dataspace.setOnStateChange(function (mux, patch) {
    //   $("#debug-space").text(Syndicate.prettyTrie(mux.routingTable));
    // });
  }
})();

///////////////////////////////////////////////////////////////////////////
// Input control value monitoring

assertion type inputValue(selector, value);

function spawnInputChangeMonitor() {
  function valOf(e) {
    return e ? (e.type === 'checkbox' ? e.checked : e.value) : null;
  }

  actor {
    during Syndicate.observe(inputValue($selector, _)) actor {
      field this.value = valOf($(selector)[0]);
      assert inputValue(selector, this.value);
      on message Syndicate.UI.globalEvent(selector, 'change', $e) {
        this.value = valOf(e.target);
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////

function random_hex_string(halfLength) {
  var bs = new Uint8Array(halfLength);
  var encoded = [];
  crypto.getRandomValues(bs);
  for (var i = 0; i < bs.length; i++) {
    encoded.push("0123456789abcdef"[(bs[i] >> 4) & 15]);
    encoded.push("0123456789abcdef"[bs[i] & 15]);
  }
  return encoded.join('');
}
