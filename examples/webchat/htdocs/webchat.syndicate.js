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
        field this.connectedTo = null;
        field this.myRequestCount = 0; // requests *I* have made of others
        field this.otherRequestCount = 0; // requests *others* have made of me
        field this.questionCount = 0; // questions from the system
        field this.globallyVisible = false; // mirrors *other people's experience of us*
        field this.locallyVisible = true;

        assert brokerConnection(brokerUrl);

        on asserted brokerConnected($url) { this.connectedTo = url; }
        on retracted brokerConnected(_) { this.connectedTo = null; }

        var mainpage_c = this.ui.context('mainpage');
        during inbound(uiTemplate("mainpage.html", $mainpage)) {
          assert mainpage_c.html('div#main-div', Mustache.render(
            mainpage,
            {
              questionCount: this.questionCount,
              myRequestCount: this.myRequestCount,
              otherRequestCount: this.otherRequestCount,
              globallyVisible: this.globallyVisible
            }));
        }

        during inbound(online()) {
          on start { this.globallyVisible = true; }
          on stop { this.globallyVisible = false; }
        }

        during mainpage_c.fragmentVersion($mainpageVersion) {
          // We track mainpageVersion so that changes to mainpage.html force re-creation
          // of nested widgetry. If we didn't include mainpageVersion in each subwidget's
          // context, then so long as the subwidget's content itself remained unchanged,
          // the user would see the subwidget disappear when mainpage.html changed.

          on asserted Syndicate.UI.locationHash($hash) {
            var tab = hash.substr(1);
            console.log("Switching tab to", tab);
            $('#main-tabs-bodies > div').hide();
            $('#main-tabs-tabs a.nav-link').removeClass('active');
            $('#main-tab-body-' + tab).show();
            $('#main-tab-tab-' + tab).addClass('active');
          }

          during inbound(uiTemplate("nav-account.html", $entry)) {
            var c = this.ui.context(mainpageVersion, 'nav', 0, 'account');
            assert outbound(online()) when (this.locallyVisible);
            assert c.html('#nav-ul', Mustache.render(
              entry,
              {
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

          during inbound(uiTemplate("contact-entry.html", $entry)) {
            during Syndicate.UI.locationHash('/contacts') {
              during inbound(contactListEntry(sessionInfo.email, $contact)) {
                field this.isPresent = false;
                on asserted inbound(present(contact)) { this.isPresent = true; }
                on retracted inbound(present(contact)) { this.isPresent = false; }
                var c = this.ui.context(mainpageVersion, 'all-contacts', contact);
                assert c.html('#main-tab-body-contacts .contact-list',
                              Mustache.render(entry, {
                                email: contact,
                                avatar: avatar(contact),
                                isPresent: this.isPresent
                              }));
                on message c.event('.do-hi', 'click', $e) {
                  alert(contact);
                }
              }
            }
          }

          during inputValue('#add-contact-email', $contact) {
            during inputValue('#reciprocate', $reciprocate) {
              on message mainpage_c.event('#add-contact', 'click', _) {
                if (reciprocate) {
                  :: outbound(createResource(grant(sessionInfo.email,
                                                   sessionInfo.email,
                                                   contact,
                                                   pFollow(sessionInfo.email),
                                                   false)));
                }

                :: outbound(createResource(contactListEntry(sessionInfo.email, contact)));
                :: outbound(createResource(permissionRequest(contact,
                                                             sessionInfo.email,
                                                             pFollow(contact))));

                // :: outbound(createResource(permissionRequest(contact,
                //                                              sessionInfo.email,
                //                                              pInvite(contact))));
                // :: outbound(createResource(permissionRequest(contact,
                //                                              sessionInfo.email,
                //                                              pSeePresence(contact))));
                $('#add-contact-email').val('');
              }
            }
          }

          during inbound(uiTemplate("permission-entry.html", $entry)) {
            during inbound(permitted($i, $e, $p, $d)) {
              if (i !== sessionInfo.email) {
                var c = this.ui.context(mainpageVersion, 'permitted', i, e, p, d);
                assert c.html('#permissions', Mustache.render(entry,
                                                              {issuer: i,
                                                               email: e,
                                                               permission: JSON.stringify(p),
                                                               isDelegable: d,
                                                               isRelinquishable: i !== e}));
                on message c.event('.relinquish', 'click', _) {
                  :: outbound(deleteResource(permitted(i, e, p, d)));
                }
              }
            }
          }

          during inbound(uiTemplate("grant-entry.html", $entry)) {
            during inbound(grant($i, sessionInfo.email, $ge, $p, $d)) {
              var c = this.ui.context(mainpageVersion, 'granted', i, ge, p, d);
              assert c.html('#grants', Mustache.render(entry, {issuer: i,
                                                               grantee: ge,
                                                               permission: JSON.stringify(p),
                                                               isDelegable: d}));
              on message c.event('.revoke', 'click', _) {
                :: outbound(deleteResource(grant(i, sessionInfo.email, ge, p, d)));
              }
            }
          }

          during inbound(uiTemplate("permission-request-out-GENERIC.html", $genericEntry)) {
            during inbound(permissionRequest($issuer, sessionInfo.email, $permission)) {
              on start { this.myRequestCount++; }
              on stop { this.myRequestCount--; }

              var c = this.ui.context(mainpageVersion, 'my-permission-request', issuer, permission);
              field this.entry = genericEntry;
              assert c.html('#my-permission-requests',
                            Mustache.render(this.entry,
                                            {issuer: issuer,
                                             permission: permission,
                                             permissionJSON: JSON.stringify(permission)}))
                when (this.entry);
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

          during inputValue('#show-all-requests-from-others', $showRequestsFromOthers) {
            on start {
              var d = $('#all-requests-from-others-div');
              if (showRequestsFromOthers) { d.show(); } else { d.hide(); }
            }
          }

          during inbound(uiTemplate("permission-request-in-GENERIC.html", $genericEntry)) {
            during inbound(permissionRequest($issuer, $grantee, $permission)) {
              if (grantee !== sessionInfo.email) {
                on start { this.otherRequestCount++; }
                on stop { this.otherRequestCount--; }

                var c = this.ui.context(mainpageVersion, 'others-permission-request', issuer, grantee, permission);
                field this.entry = genericEntry;
                assert c.html('#others-permission-requests',
                              Mustache.render(this.entry,
                                              {issuer: issuer,
                                               grantee: grantee,
                                               permission: permission,
                                               permissionJSON: JSON.stringify(permission)}))
                  when (this.entry);
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

          during inbound(question($qid, $timestamp, $klass, sessionInfo.email, $title, $blurb, $qt))
          {
            on start { this.questionCount++; }
            on stop { this.questionCount--; }

            var c = this.ui.context(mainpageVersion, 'question', timestamp, qid);

            switch (qt.meta.label) {
              case "option-question": {
                var options = qt.fields[0];
                during inbound(uiTemplate("option-question.html", $entry)) {
                  assert c.html('#question-container',
                                Mustache.render(entry, {questionClass: klass,
                                                        title: title,
                                                        blurb: blurb,
                                                        options: options}));
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
    }

    G.dataspace.setOnStateChange(function (mux, patch) {
      $("#debug-space").text(Syndicate.prettyTrie(mux.routingTable));
    });
  }
})();

///////////////////////////////////////////////////////////////////////////
// Input control value monitoring

assertion type inputValue(selector, value);

function spawnInputChangeMonitor() {
  function valOf(e) {
    return e.type === 'checkbox' ? e.checked : e.value;
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
