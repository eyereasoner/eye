FROM swipl:latest as builder

LABEL maintainer="https://github.com/bdevloed"

RUN apt-get -qq update
RUN apt-get -qqy install build-essential git flex

# Compile Carl: Another Rule Language
# cf https://github.com/melgi/carl/
RUN git clone https://github.com/melgi/carl.git && \
	cd carl && \
	make maintainer-clean && make CXXFLAGS='-O2 -Wall'

# Compile CTurtle:
#    a tool for parsing RDF 1.1 Turtle files
#    and outputting the resulting triples in "N3P" format.
# cf https://github.com/melgi/cturtle/
RUN git clone https://github.com/melgi/cturtle.git && \
	cd cturtle && \
	make maintainer-clean && make CXXFLAGS='-O2 -Wall'

FROM swipl:latest
LABEL maintainer="https://github.com/bdevloed"

# Install EYE:
# - Download EYE
# - Verify integrity
# - Install EYE (including turtle parser)
# - clean up temporary files

COPY --from=builder /carl/carl /cturtle/cturtle /usr/local/bin/

RUN DEBIAN_FRONTEND=noninteractive apt-get -qq update && \
	`# Install dependencies:` \
	apt-get -qqy --no-install-recommends install jq curl unzip libarchive13 nyancat libgmp10 ca-certificates && \
	`# remove unnescesary files` \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/* && \
	rm -rf /var/cache/debconf/*

RUN chmod +x /usr/local/bin/cturtle && \
	chmod +x /usr/local/bin/carl && \
  echo "IyEvYmluL2Jhc2gKaWYgW1sgJCogPT0gKiItLWZlZXN0IiogXV0KdGhlbgogIGV4ZWMgbnlhbmNhdAplbHNlCiAgZXhlYyBleWUgIiRAIjsKZmkK" | base64 -d > /ep && \
	chmod +x /ep && \
  mkdir eye && \
	cd eye && \
	curl -fsS -L -O "https://raw.githubusercontent.com/josd/eye/master/eye.zip" && \
	unzip eye && \
	curl -o ./eye/eye.sh.in https://raw.githubusercontent.com/josd/eye/master/eye.sh.in && \
	./eye/install.sh

ENTRYPOINT ["/ep"]
