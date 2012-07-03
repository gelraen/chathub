#!/bin/sh

erl -pa ebin -make
modlist=$(ls ebin/*.beam | sed -e 's/.beam$//' -e 's,^.*/,,' | tr -s '\n' ',' | sed -e 's/,$//')
sed -e "s/@MODULES@/${modlist}/" src/chathub.app.in > chathub.app
