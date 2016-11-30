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

        assert brokerConnection(brokerUrl);

        on asserted brokerConnected($url) { this.connectedTo = url; }
        on retracted brokerConnected(_) { this.connectedTo = null; }

        var mainpage_c = this.ui.context('mainpage');
        during inbound(uiTemplate("mainpage.html", $mainpage)) {
          assert mainpage_c.html('div#main-div', mainpage);
        }

        during mainpage_c.fragmentVersion($mainpageVersion) {
          // We track mainpageVersion so that changes to mainpage.html force re-creation
          // of nested widgetry. If we didn't include mainpageVersion in each subwidget's
          // context, then so long as the subwidget's content itself remained unchanged,
          // the user would see the subwidget disappear when mainpage.html changed.
          on start { console.log('mainpage up', mainpageVersion); }
          on stop { console.log('mainpage down', mainpageVersion); }

          during inputValue('#invisible', false) {
            assert outbound(online());
          }

          on asserted Syndicate.UI.locationHash($hash) {
            var tab = hash.substr(1);
            console.log("Switching tab to", tab);
            $('#main-tabs-bodies > div').hide();
            $('#main-tabs-tabs a.nav-link').removeClass('active');
            $('#main-tab-body-' + tab).show();
            $('#main-tab-tab-' + tab).addClass('active');
          }

          during inbound(uiTemplate("present-entry.html", $presentEntry)) {
            during inbound(present($who)) {
              var c = this.ui.context(mainpageVersion, 'present', who);
              assert c.html('#present-entries', Mustache.render(presentEntry,
                                                                {email: who}));
            }
          }

          during inputValue('#add-contact-email', $contact) {
            on message mainpage_c.event('#add-contact', 'click', _) {
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

          during inbound(uiTemplate("my-permission-request.html", $entry)) {
            during inbound(permissionRequest($issuer, sessionInfo.email, $permission)) {
              var c = this.ui.context(mainpageVersion, 'my-permission-request', issuer, permission);
              assert c.html('#my-permission-requests',
                            Mustache.render(entry, {issuer: issuer,
                                                    permission: JSON.stringify(permission)}));
              on message c.event('.cancel', 'click', _) {
                :: outbound(deleteResource(permissionRequest(issuer, sessionInfo.email, permission)));
              }
            }
          }

          during inbound(uiTemplate("others-permission-request.html", $entry)) {
            field this.requestCount = 0;

            assert mainpage_c.context(mainpageVersion, 'requestCount')
              .html('#request-count', this.requestCount);
            assert Syndicate.UI.uiAttribute('.request-count-sensitive',
                                            'class',
                                            'count' + this.requestCount);

            during inbound(permissionRequest($issuer, $grantee, $permission)) {
              if (grantee !== sessionInfo.email) {
                on start { this.requestCount++; }
                on stop { this.requestCount--; }

                var c = this.ui.context(mainpageVersion, 'others-permission-request', issuer, grantee, permission);
                assert c.html('#others-permission-requests',
                              Mustache.render(entry, {issuer: issuer,
                                                      grantee: grantee,
                                                      permission: JSON.stringify(permission)}));
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
      }
    }
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
