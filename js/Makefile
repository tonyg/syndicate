all:
	npm install .

keys: private-key.pem server-cert.pem

private-key.pem:
	openssl genrsa -des3 -passout pass:a -out $@ 1024
	openssl rsa -passin pass:a -in $@ -out $@

server-cert.pem: private-key.pem
	openssl req -new -x509 -nodes -sha1 -days 365 \
		-subj /CN=server.minimart.leastfixedpoint.com \
		-passin pass:a \
		-key private-key.pem > $@

clean-keys:
	rm -f private-key.pem server-cert.pem

clean:
	rm -f dist/*.js

veryclean: clean
	rm -rf node_modules/

SYNDICATE_WEB=$(CURDIR)/../../syndicate-web
deploy: all deploy-copy
deploy-copy:
	(git diff --quiet && git diff --cached --quiet) || (echo "Commit changes first."; false)

	[ -d $(SYNDICATE_WEB) ]
	[ -f $(SYNDICATE_WEB)/CNAME ]
	[ -f $(SYNDICATE_WEB)/_config.yml ]
	[ -f $(SYNDICATE_WEB)/index.md ]

	git clone .. TEMP_CHECKOUT
	for d in examples dist third-party; do \
		[ ! -d $(SYNDICATE_WEB)/$$d ] || (cd $(SYNDICATE_WEB); git rm -rf $$d); \
		cp -a TEMP_CHECKOUT/js/$$d $(SYNDICATE_WEB)/$$d; \
		(cd $(SYNDICATE_WEB); git add $$d); \
	done
	rm -rf TEMP_CHECKOUT
