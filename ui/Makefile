PLAIN=../static/js/gopro.js
MIN=../static/js/gopro.min.js

install: $(MIN)

$(PLAIN): src/Main.elm src/Geo.elm src/Formats.elm src/Media.elm src/ScreenOverlay.elm
	elm make --optimize --output $(PLAIN) src/Main.elm

$(MIN): $(PLAIN)
	uglifyjs $(PLAIN) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$(MIN)
