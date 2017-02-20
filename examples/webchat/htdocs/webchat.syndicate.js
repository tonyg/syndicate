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
  assertion type post(id, timestamp, conversationId, author, items);

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
  // Local assertions and messages

  assertion type selectedCid(cid); // currently-selected conversation ID, or null
  message type windowWidthChanged(newWidth);

  assertion type draftItem(timestamp, dataURL);
  message type draftSent();

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

  document.addEventListener('dragover', function (e) {
    e.preventDefault(); // make it so drag-and-drop doesn't load the dropped object into the browser
  });

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

      spawn {
        this.ui = new Syndicate.UI.Anchor();
        var mainpage_c = this.ui.context('mainpage');

        field this.connectedTo = null;
        field this.myRequestCount = 0; // requests *I* have made of others
        field this.otherRequestCount = 0; // requests *others* have made of me
        field this.questionCount = 0; // questions from the system
        field this.globallyVisible = false; // mirrors *other people's experience of us*
        field this.locallyVisible = true;
        field this.showRequestsFromOthers = false;
        field this.miniMode = $(window).width() < 768;

        window.addEventListener('resize', Syndicate.Dataspace.wrap(function () {
          :: windowWidthChanged($(window).width());
        }));

        on message windowWidthChanged($newWidth) {
          this.miniMode = newWidth < 768;
        }

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
            assert selectedCid(m[2] || false);
          }
        }

        during inbound(uiTemplate("page-conversations.html", $mainEntry)) {
          during selectedCid(false) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
              miniMode: this.miniMode,
              showConversationList: true,
              showConversationMain: !this.miniMode,
              showConversationInfo: false,
              showConversationPosts: false,
              selected: false
            }));
          }
        }

        // Move to the conversation index page when we leave a
        // conversation (which also happens automatically when it is
        // deleted)
        during selectedCid($selected) {
          on retracted inbound(inConversation(selected, sessionInfo.email)) {
            :: Syndicate.UI.setLocationHash('/conversations');
          }
        }

        during inbound(inConversation($cid, sessionInfo.email)) {
          field this.members = Immutable.Set();
          field this.title = '';
          field this.creator = '';
          field this.blurb = '';
          field this.editingTitle = false;
          field this.editingBlurb = false;

          field this.membersJSON = [];
          dataflow {
            this.membersJSON = this.members.map(function (m) { return {
              email: m,
              avatar: avatar(m)
            }; }).toArray();
          }

          on asserted inbound(inConversation(cid, $who)) {
            this.members = this.members.add(who);
          }
          on retracted inbound(inConversation(cid, $who)) {
            this.members = this.members.remove(who);
          }

          on asserted inbound(conversation(cid, $title, $creator, $blurb)) {
            this.title = title;
            this.creator = creator;
            this.blurb = blurb;
          }

          during inbound(uiTemplate("page-conversations.html", $mainEntry)) {
            during selectedCid($selected) {
              if (selected === cid) {
                field this.showInfoMode = false;
                field this.latestPostTimestamp = 0;
                field this.latestPostId = null;

                field this.draftItems = Immutable.Map();
                on asserted draftItem($ts, $d) { this.draftItems = this.draftItems.set(ts, d); }
                on retracted draftItem($ts, _) { this.draftItems = this.draftItems.remove(ts); }

                assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
                  miniMode: this.miniMode,
                  showConversationList: !this.miniMode,
                  showConversationMain: true,
                  showConversationInfo: !this.miniMode || this.showInfoMode,
                  showConversationPosts: !this.miniMode || !this.showInfoMode,
                  selected: selected,
                  title: this.title,
                  blurb: this.blurb,
                  members: this.membersJSON,
                  editingTitle: this.editingTitle,
                  editingBlurb: this.editingBlurb,
                  overflowMenuItems: [
                    {label: "Invite user...", action: "invite-to-conversation"},
                    {label: "Leave conversation", action: "leave-conversation"},
                    {separator: true,
                     hidden: sessionInfo.email !== this.creator},
                    {label: "Delete conversation", action: "delete-conversation",
                     hidden: sessionInfo.email !== this.creator}
                  ]
                }));

                on message mainpage_c.event('#message-input', 'focus', $e) {
                  setTimeout(function () { e.target.scrollIntoView(false); }, 500);
                }

                var spawnItemFromDataURL = (function (ui) {
                  return function (dataURL) {
                    var timestamp = +(new Date());
                    spawn {
                      field this.ui = ui.context('draft-post', timestamp);
                      assert draftItem(timestamp, dataURL);
                      manifestPostItem(this.ui,
                                       '#pending-draft-items',
                                       {
                                         isDraft: true,
                                         postId: 'draft',
                                         timestamp: timestamp,
                                         fromMe: true,
                                         author: sessionInfo.email
                                       },
                                       dataURL);
                      stop on message draftSent();
                      stop on message this.ui.event('.close-draft', 'click', _);
                    }
                  };
                })(this.ui);

                var handleDataTransfer = function (dataTransfer) {
                  return dataTransferFiles(dataTransfer, Syndicate.Dataspace.wrap(
                    function (dataURLs) {
                      dataURLs.forEach(spawnItemFromDataURL);
                    }));
                };

                on message mainpage_c.event('#conversation-main', 'drop', $e) {
                  handleDataTransfer.call(this, e.dataTransfer);
                }

                on message mainpage_c.event('#message-input', '+paste', $e) {
                  if (handleDataTransfer.call(this, e.clipboardData)) {
                    e.preventDefault();
                  }
                }

                on message mainpage_c.event('#attach-item-button', 'click', _) {
                  console.log('clickenating');
                  $('#attach-item-file').click();
                }
                on message mainpage_c.event('#attach-item-file', 'change', $e) {
                  if (e.target.files) {
                    for (var i = 0; i < e.target.files.length; i++) {
                      var file = e.target.files[i];
                      var reader = new FileReader();
                      reader.addEventListener('load', Syndicate.Dataspace.wrap(function (e) {
                        spawnItemFromDataURL(e.target.result);
                      }));
                      reader.readAsDataURL(file);
                    }
                  }
                }

                on message mainpage_c.event('#send-message-button', 'click', _) {
                  var timestamp = +(new Date());
                  var items = this.draftItems.entrySeq().toArray();
                  items.sort(function (a, b) { return a[0] - b[0]; });
                  var message = ($("#message-input").val() || '').trim();
                  if (message) {
                    var b64 = btoa(unescape(encodeURIComponent(message))); // utf-8, then base64
                    items.push([timestamp,
                                "data:text/plain;charset=utf-8;base64," + encodeURIComponent(b64)]);
                  }
                  if (items.length) {
                    :: outbound(createResource(post(random_hex_string(16),
                                                    timestamp,
                                                    cid,
                                                    sessionInfo.email,
                                                    items.map(function (di) { return di[1]; }))));
                  }
                  $("#message-input").val('').focus();
                  :: draftSent();
                }

                on message mainpage_c.event('.invite-to-conversation', 'click', _) {
                  $('#invitation-modal').modal({});
                }

                on message mainpage_c.event('.send-invitation', 'click', _) {
                  var invitee = $('#invited-username').val().trim();
                  if (invitee) {
                    :: outbound(createResource(invitation(cid, sessionInfo.email, invitee)));
                    $('#invited-username').val('');
                    $('#invitation-modal').modal('hide');
                  }
                }

                on message mainpage_c.event('.leave-conversation', 'click', _) {
                  :: outbound(deleteResource(inConversation(cid, sessionInfo.email)));
                }

                on message mainpage_c.event('.delete-conversation', 'click', _) {
                  if (confirm("Delete this conversation?")) {
                    :: outbound(deleteResource(conversation(cid,
                                                            this.title,
                                                            this.creator,
                                                            this.blurb)));
                  }
                }

                on message mainpage_c.event('.toggle-info-mode', 'click', _) {
                  this.showInfoMode = !this.showInfoMode;
                }
                on message mainpage_c.event('.end-info-mode', 'click', _) {
                  this.showInfoMode = false;
                }

                on message mainpage_c.event('#edit-conversation-title', 'click', _) {
                  this.editingTitle = true;
                }
                on message mainpage_c.event('#title-heading', 'dblclick', _) {
                  this.editingTitle = true;
                }
                on message mainpage_c.event('#accept-conversation-title', 'click', _) {
                  this.title = $('#conversation-title').val();
                  :: outbound(updateResource(conversation(cid,
                                                          this.title,
                                                          this.creator,
                                                          this.blurb)));
                  this.editingTitle = false;
                }
                on message mainpage_c.event('#cancel-edit-conversation-title', 'click', _) {
                  this.editingTitle = false;
                }

                on message mainpage_c.event('#edit-conversation-blurb', 'click', _) {
                  this.editingBlurb = true;
                }
                on message mainpage_c.event('#blurb', 'dblclick', _) {
                  this.editingBlurb = true;
                }
                on message mainpage_c.event('#accept-conversation-blurb', 'click', _) {
                  this.blurb = $('#conversation-blurb').val();
                  :: outbound(updateResource(conversation(cid,
                                                          this.title,
                                                          this.creator,
                                                          this.blurb)));
                  this.editingBlurb = false;
                }
                on message mainpage_c.event('#cancel-edit-conversation-blurb', 'click', _) {
                  this.editingBlurb = false;
                }

                during mainpage_c.fragmentVersion($mainpageVersion) {
                  during inbound(post($pid, $timestamp, cid, $author, $items)) {
                    var fromMe = (author === sessionInfo.email);
                    var postInfo = {
                      isDraft: false,
                      postId: pid,
                      timestamp: timestamp,
                      date: new Date(timestamp).toString(),
                      time: new Date(timestamp).toTimeString().substr(0, 8),
                      fromMe: fromMe,
                      author: author
                    };
                    if (timestamp > this.latestPostTimestamp) {
                      this.latestPostTimestamp = timestamp;
                      this.latestPostId = pid;
                    }
                    var c = this.ui.context(mainpageVersion, 'post', timestamp, pid);
                    during inbound(uiTemplate("post-entry.html", $postEntryTemplate)) {
                      assert c.html('.posts', Mustache.render(postEntryTemplate, postInfo));
                      during c.fragmentVersion(_) {
                        var itemCounter = 0;
                        items.forEach((function (itemURL) {
                          manifestPostItem(c.context('item', itemCounter++),
                                           '#post-' + pid + ' .post-item-container',
                                           postInfo,
                                           itemURL);
                        }).bind(this));
                      }
                    }
                  }
                }
              }

              during inbound(uiTemplate("conversation-index-entry.html", $indexEntry)) {
                during mainpage_c.fragmentVersion($mainpageVersion) {
                  var c = this.ui.context(mainpageVersion, 'conversationIndex', cid);
                  assert c.html('#conversation-list', Mustache.render(indexEntry, {
                    isSelected: selected === cid,
                    selected: selected,
                    cid: cid,
                    title: this.title,
                    creator: this.creator,
                    members: this.membersJSON
                  }));
                  on message c.event('.card-block', 'click', _) {
                    if (selected === cid) {
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

        during Syndicate.UI.locationHash('/new-chat') {
          field this.invitees = Immutable.Set();
          field this.searchString = '';
          field this.displayedSearchString = ''; // avoid resetting HTML every keystroke. YUCK

          during inbound(uiTemplate("page-new-chat.html", $mainEntry)) {
            assert mainpage_c.html('div#main-div', Mustache.render(mainEntry, {
              noInvitees: this.invitees.isEmpty(),
              searchString: this.displayedSearchString
            }));
          }

          during mainpage_c.fragmentVersion($mainpageVersion) {
            on message Syndicate.UI.globalEvent('#search-contacts', 'keyup', $e) {
              this.searchString = e.target.value.trim();
            }

            on message mainpage_c.event('.create-conversation', 'click', _) {
              if (!this.invitees.isEmpty()) {
                var title = $('#conversation-title').val();
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

  var nextItemid = 0;
  function manifestPostItem(uiContext, containerSelector, postInfo, itemURL) {
    function cleanContentType(t) {
      t = t.toLowerCase();
      if (t.startsWith('image/')) {
        t = 'image';
      } else {
        t = t.replace('/', '-');
      }
      return t;
    }

    var item = parseDataURL(itemURL);
    var itemId = 'post-' + postInfo.postId + '-item-' + nextItemid++;
    var contentClass = cleanContentType(item.type);
    var itemInfo = {
      itemId: itemId,
      postInfo: postInfo,
      contentClass: contentClass,
      item: item,
      itemURL: itemURL
    };

    during inbound(uiTemplate("post-item.html", $postItemTemplate)) {
      field this.entry = false;
      on asserted inbound(uiTemplate("post-item-" + contentClass + ".html", $entry)) {
        if (entry) this.entry = entry;
      }
      on asserted inbound(uiTemplate("post-item-application-octet-stream.html", $entry)) {
        if (entry && !this.entry) this.entry = entry;
      }
      assert uiContext.html(containerSelector, Mustache.render(postItemTemplate, itemInfo));
      on asserted uiContext.fragmentVersion(_) {
        var innerContext = uiContext.context('item-body');
        assert innerContext.html('#' + itemId + ' .post-item-body-container',
                                 Mustache.render(this.entry, itemInfo)) when (this.entry);
        if (!postInfo.isDraft) {
          on asserted innerContext.fragmentVersion(_) {
            if ((this.latestPostTimestamp === postInfo.timestamp) &&
                (this.latestPostId === postInfo.postId)) {
              setTimeout(function () { $("#post-" + postInfo.postId)[0].scrollIntoView(false); }, 1);
            }
          }
        }
      }
    }
  }
})();

///////////////////////////////////////////////////////////////////////////
// Input control value monitoring

assertion type inputValue(selector, value);

function spawnInputChangeMonitor() {
  function valOf(e) {
    return e ? (e.type === 'checkbox' ? e.checked : e.value) : null;
  }

  spawn {
    during Syndicate.observe(inputValue($selector, _)) spawn {
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

///////////////////////////////////////////////////////////////////////////

function parseDataURL(u) {
  var pieces;

  if (!u.startsWith('data:')) return null;
  u = u.substr(5);

  pieces = u.split(',');
  if (pieces.length !== 2) return null;

  var mimeType = pieces[0];
  var data = decodeURIComponent(pieces[1]);
  var isBase64 = false;

  if (mimeType.endsWith(';base64')) {
    mimeType = mimeType.substr(0, mimeType.length - 7);
    isBase64 = true;
  }

  if (isBase64) {
    data = atob(data);
  }

  pieces = mimeType.split(';');
  var type = pieces[0];

  var parameters = {};
  for (var i = 1; i < pieces.length; i++) {
    var m = pieces[i].match(/^([^=]+)=(.*)$/);
    if (m) {
      parameters[m[1].toLowerCase()] = m[2];
    }
  }

  if (type.startsWith('text/')) {
    var charset = (parameters.charset || 'US-ASCII').toLowerCase();
    switch (charset) {
      case 'utf-8':
        data = decodeURIComponent(escape(data));
        break;
      case 'us-ascii':
      case 'ascii':
      case 'latin1':
      case 'iso-8859-1':
        break;
      default:
        console.warn('Unknown charset while decoding data URL:', charset);
        break;
    }
  }

  return {
    type: type,
    parameters: parameters,
    data: data
  };
}

///////////////////////////////////////////////////////////////////////////

// Extract file contents from a DataTransfer object
function dataTransferFiles(d, k) {
  var items = d.items;
  var types = d.types;
  var files = d.files;

  var results = [];
  var expectedCount = files.length;
  var completedCount = 0;

  function completeOne() {
    completedCount++;
    if (completedCount === expectedCount) {
      k(results);
    }
  }

  for (var i = 0; i < items.length; i++) {
    (function (i) {
      var item = items[i];
      var type = types[i];
      if (type === 'text/uri-list') {
        expectedCount++;
        item.getAsString(function (itemstr) {
          var firstChunk = itemstr.substr(0, 6).toLowerCase();
          if (firstChunk.startsWith('http:') || firstChunk.startsWith('https:')) {
            $.ajax({
              type: "GET",
              url: itemstr,
              beforeSend: function (xhr) {
                xhr.overrideMimeType('text/plain; charset=x-user-defined');
              },
              success: function (_data, _status, xhr) {
                var contentType = xhr.getResponseHeader('content-type');
                var rawdata = xhr.responseText;
                var data = [];
                for (var j = 0; j < rawdata.length; j++) {
                  data = data + String.fromCharCode(rawdata.charCodeAt(j) & 0xff);
                }
                results.push('data:' + contentType + ';base64,' + encodeURIComponent(btoa(data)));
                completeOne();
              },
              error: function () {
                completeOne();
              }
            });
          } else {
            completeOne();
          }
        });
      }
    })(i);
  }

  for (var i = 0; i < files.length; i++) {
    (function (i) {
      var file = files[i];
      var reader = new FileReader();
      reader.addEventListener('load', function (e) {
        results.push(e.target.result);
        completeOne();
      });
      reader.readAsDataURL(file);
    })(i);
  }

  return (expectedCount > 0);
}
