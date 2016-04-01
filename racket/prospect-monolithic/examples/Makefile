keys: private-key.pem server-cert.pem

private-key.pem:
	openssl genrsa -des3 -passout pass:a -out $@ 1024
	openssl rsa -passin pass:a -in $@ -out $@

server-cert.pem: private-key.pem
	openssl req -new -x509 -nodes -sha1 -days 365 \
		-subj /CN=example.racket-rfc6455.leastfixedpoint.com \
		-passin pass:a \
		-key private-key.pem > $@

clean-keys:
	rm -f private-key.pem server-cert.pem
