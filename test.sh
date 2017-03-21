#!/bin/sh

exec node -e 'require("./out/test.js").test.main()'
