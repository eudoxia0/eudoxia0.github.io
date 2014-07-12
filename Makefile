GEM_REQS = sass bourbon neat pandoc-ruby nanoc adsf

default: all

reqs:
	$(foreach GEM, $(GEM_REQS), gem install $(GEM);)
	bower install
	cd static; bourbon install; neat install

all:
	nanoc

serve: all
	cd output; python2 -m SimpleHTTPServer 5000
