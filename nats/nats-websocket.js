// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// Query String Utilities

var QueryString = (function( ){
  var QueryString = {};
  var urlDecode   = decodeURI;


  // If obj.hasOwnProperty has been overridden, then calling
  // obj.hasOwnProperty(prop) will break.
  // See: https://github.com/joyent/node/issues/1707
  function hasOwnProperty(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop);
  }


  function charCode(c) {
    return c.charCodeAt(0);
  }


  // a safe fast alternative to decodeURIComponent
  QueryString.unescapeBuffer = function(s, decodeSpaces) {
    var out = new Buffer(s.length);
    var state = 'CHAR'; // states: CHAR, HEX0, HEX1
    var n, m, hexchar;

    for (var inIndex = 0, outIndex = 0; inIndex <= s.length; inIndex++) {
      var c = s.charCodeAt(inIndex);
      switch (state) {
        case 'CHAR':
          switch (c) {
            case charCode('%'):
              n = 0;
              m = 0;
              state = 'HEX0';
              break;
            case charCode('+'):
              if (decodeSpaces) c = charCode(' ');
              // pass thru
            default:
              out[outIndex++] = c;
              break;
          }
          break;

        case 'HEX0':
          state = 'HEX1';
          hexchar = c;
          if (charCode('0') <= c && c <= charCode('9')) {
            n = c - charCode('0');
          } else if (charCode('a') <= c && c <= charCode('f')) {
            n = c - charCode('a') + 10;
          } else if (charCode('A') <= c && c <= charCode('F')) {
            n = c - charCode('A') + 10;
          } else {
            out[outIndex++] = charCode('%');
            out[outIndex++] = c;
            state = 'CHAR';
            break;
          }
          break;

        case 'HEX1':
          state = 'CHAR';
          if (charCode('0') <= c && c <= charCode('9')) {
            m = c - charCode('0');
          } else if (charCode('a') <= c && c <= charCode('f')) {
            m = c - charCode('a') + 10;
          } else if (charCode('A') <= c && c <= charCode('F')) {
            m = c - charCode('A') + 10;
          } else {
            out[outIndex++] = charCode('%');
            out[outIndex++] = hexchar;
            out[outIndex++] = c;
            break;
          }
          out[outIndex++] = 16 * n + m;
          break;
      }
    }

    // TODO support returning arbitrary buffers.

    return out.slice(0, outIndex - 1);
  };


  QueryString.unescape = function(s, decodeSpaces) {
    return QueryString.unescapeBuffer(s, decodeSpaces).toString();
  };


  QueryString.escape = function(str) {
    return encodeURIComponent(str);
  };

  var stringifyPrimitive = function(v) {
    switch (typeof v) {
      case 'string':
        return v;

      case 'boolean':
        return v ? 'true' : 'false';

      case 'number':
        return isFinite(v) ? v : '';

      default:
        return '';
    }
  };


  QueryString.stringify = QueryString.encode = function(obj, sep, eq, name) {
    sep = sep || '&';
    eq = eq || '=';
    obj = (obj === null) ? undefined : obj;

    switch (typeof obj) {
      case 'object':
        return Object.keys(obj).map(function(k) {
          if (Array.isArray(obj[k])) {
            return obj[k].map(function(v) {
              return QueryString.escape(stringifyPrimitive(k)) +
                     eq +
                     QueryString.escape(stringifyPrimitive(v));
            }).join(sep);
          } else {
            return QueryString.escape(stringifyPrimitive(k)) +
                   eq +
                   QueryString.escape(stringifyPrimitive(obj[k]));
          }
        }).join(sep);

      default:
        if (!name) return '';
        return QueryString.escape(stringifyPrimitive(name)) + eq +
               QueryString.escape(stringifyPrimitive(obj));
    }
  };

  // Parse a key=val string.
  QueryString.parse = QueryString.decode = function(qs, sep, eq, options) {
    sep = sep || '&';
    eq = eq || '=';
    var obj = {},
        maxKeys = 1000;

    // Handle maxKeys = 0 case
    if (options && typeof options.maxKeys === 'number') {
      maxKeys = options.maxKeys;
    }

    if (typeof qs !== 'string' || qs.length === 0) {
      return obj;
    }

    var regexp = /\+/g;
    qs = qs.split(sep);

    // maxKeys <= 0 means that we should not limit keys count
    if (maxKeys > 0) {
      qs = qs.slice(0, maxKeys);
    }

    for (var i = 0, len = qs.length; i < len; ++i) {
      var x = qs[i].replace(regexp, '%20'),
          idx = x.indexOf(eq),
          kstr = x.substring(0, idx),
          vstr = x.substring(idx + 1), k, v;

      try {
        k = decodeURIComponent(kstr);
        v = decodeURIComponent(vstr);
      } catch (e) {
        k = QueryString.unescape(kstr, true);
        v = QueryString.unescape(vstr, true);
      }

      if (!hasOwnProperty(obj, k)) {
        obj[k] = v;
      } else if (!Array.isArray(obj[k])) {
        obj[k] = [obj[k], v];
      } else {
        obj[k].push(v);
      }
    }

    return obj;
  };

  return QueryString;
})();
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.


if(typeof url == "undefined") {
  var url = (function(){
    if(typeof QueryString == "undefined"){
      console.error("QueryString is required");
      return;
    }

    /**
     * A copy of PunyCode (https://github.com/bestiejs/punycode.js) trimmed down
     * to only the methods required for url.js.
     *
     * Direct concerns to Caleb Crane <punycode@simulacre.org>
     */
    var punycode = (function(){
      var
        self = this,
        /** Highest positive signed 32-bit float value */
        maxInt = 2147483647, // aka. 0x7FFFFFFF or 2^31-1

        /** Bootstring parameters */
        base = 36,
        tMin = 1,
        tMax = 26,
        skew = 38,
        damp = 700,
        initialBias = 72,
        initialN = 128, // 0x80
        delimiter = '-', // '\x2D'

        /** Regular expressions */
        regexNonASCII = /[^ -~]/, // unprintable ASCII chars + non-ASCII chars
        regexPunycode = /^xn--/,

        /** Convenience shortcuts */
        baseMinusTMin = base - tMin,
        floor = Math.floor,
        stringFromCharCode = String.fromCharCode,

        /** Temporary variable */
        key;

      /**
       * Creates an array containing the decimal code points of each Unicode
       * character in the string. While JavaScript uses UCS-2 internally,
       * this function will convert a pair of surrogate halves (each of which
       * UCS-2 exposes as separate characters) into a single code point,
       * matching UTF-16.
       * @see `punycode.ucs2.encode`
       * @see <http://mathiasbynens.be/notes/javascript-encoding>
       * @memberOf punycode.ucs2
       * @name decode
       * @param {String} string The Unicode input string (UCS-2).
       * @returns {Array} The new array of code points.
       */
      function ucs2decode(string) {
        var output = [],
            counter = 0,
            length = string.length,
            value,
            extra;
        while (counter < length) {
          value = string.charCodeAt(counter++);
          if ((value & 0xF800) == 0xD800) {
            extra = string.charCodeAt(counter++);
            if ((value & 0xFC00) != 0xD800 || (extra & 0xFC00) != 0xDC00)
              throw RangeError('UCS-2(decode): illegal sequence')
            value = ((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000;
          }
          output.push(value);
        }
        return output;
      }

      /**
       * Converts a digit/integer into a basic code point.
       * @see `basicToDigit()`
       * @private
       * @param {Number} digit The numeric value of a basic code point.
       * @returns {Number} The basic code point whose value (when used for
       * representing integers) is `digit`, which needs to be in the range
       * `0` to `base - 1`. If `flag` is non-zero, the uppercase form is
       * used; else, the lowercase form is used. The behavior is undefined
       * if flag is non-zero and `digit` has no uppercase form.
       */
      function digitToBasic(digit, flag) {
        //  0..25 map to ASCII a..z or A..Z
        // 26..35 map to ASCII 0..9
        return digit + 22 + 75 * (digit < 26) - ((flag != 0) << 5);
      }

      /**
       * Bias adaptation function as per section 3.4 of RFC 3492.
       * http://tools.ietf.org/html/rfc3492#section-3.4
       * @private
       */
      function adapt(delta, numPoints, firstTime) {
        var k = 0;
        delta = firstTime ? floor(delta / damp) : delta >> 1;
        delta += floor(delta / numPoints);
        for (/* no initialization */; delta > baseMinusTMin * tMax >> 1; k += base) {
          delta = floor(delta / baseMinusTMin);
        }
        return floor(k + (baseMinusTMin + 1) * delta / (delta + skew));
      }

      /**
       * Converts a string of Unicode code points to a Punycode string of ASCII
       * code points.
       * @memberOf punycode
       * @param {String} input The string of Unicode code points.
       * @returns {String} The resulting Punycode string of ASCII code points.
       */
      this.encode = function(input) {
        var n,
            delta,
            handledCPCount,
            basicLength,
            bias,
            j,
            m,
            q,
            k,
            t,
            currentValue,
            output = [],
            /** `inputLength` will hold the number of code points in `input`. */
            inputLength,
            /** Cached calculation results */
            handledCPCountPlusOne,
            baseMinusT,
            qMinusT;

        // Convert the input in UCS-2 to Unicode
        input = ucs2decode(input);

        // Cache the length
        inputLength = input.length;

        // Initialize the state
        n     = initialN;
        delta = 0;
        bias  = initialBias;

        // Handle the basic code points
        for (j = 0; j < inputLength; ++j) {
          currentValue = input[j];
          if (currentValue < 0x80)
            output.push(stringFromCharCode(currentValue));
        }

        handledCPCount = basicLength = output.length;

        // `handledCPCount` is the number of code points that have been handled;
        // `basicLength` is the number of basic code points.

        // Finish the basic string - if it is not empty - with a delimiter
        if (basicLength)
          output.push(delimiter);

        // Main encoding loop:
        while (handledCPCount < inputLength) {
          // All non-basic code points < n have been handled already. Find the next
          // larger one:
          for (m = maxInt, j = 0; j < inputLength; ++j) {
            currentValue = input[j];
            if (currentValue >= n && currentValue < m)
              m = currentValue;
          }

          // Increase `delta` enough to advance the decoder's <n,i> state to <m,0>,
          // but guard against overflow
          handledCPCountPlusOne = handledCPCount + 1;
          if (m - n > floor((maxInt - delta) / handledCPCountPlusOne))
            throw RangeError('Overflow: input needs wider integers to process.');
          delta += (m - n) * handledCPCountPlusOne;
          n = m;
          for (j = 0; j < inputLength; ++j) {
            currentValue = input[j];
            if (currentValue < n && ++delta > maxInt)
              throw RangeError('Overflow: input needs wider integers to process.');
            if (currentValue == n) {
              // Represent delta as a generalized variable-length integer
              for (q = delta, k = base; /* no condition */; k += base) {
                t = k <= bias ? tMin : (k >= bias + tMax ? tMax : k - bias);
                if (q < t) {
                  break;
                }
                qMinusT = q - t;
                baseMinusT = base - t;
                output.push(
                  stringFromCharCode(digitToBasic(t + qMinusT % baseMinusT, 0))
                );
                q = floor(qMinusT / baseMinusT);
              }
              output.push(stringFromCharCode(digitToBasic(q, 0)));
              bias = adapt(delta, handledCPCountPlusOne, handledCPCount == basicLength);
              delta = 0;
              ++handledCPCount;
            }
          }

          ++delta;
          ++n;
        }
        return output.join('');
      }
      return this;
    }).apply({});


    // Reference: RFC 3986, RFC 1808, RFC 2396

    // define these here so at least they only have to be
    // compiled once on the first module load.
    var protocolPattern = /^([a-z0-9.+-]+:)/i,
        portPattern = /:[0-9]*$/,
        // RFC 2396: characters reserved for delimiting URLs.
        delims = ['<', '>', '"', '`', ' ', '\r', '\n', '\t'],
        // RFC 2396: characters not allowed for various reasons.
        unwise = ['{', '}', '|', '\\', '^', '~', '`'].concat(delims),
        // Allowed by RFCs, but cause of XSS attacks.  Always escape these.
        autoEscape = ['\''],
        // Characters that are never ever allowed in a hostname.
        // Note that any invalid chars are also handled, but these
        // are the ones that are *expected* to be seen, so we fast-path
        // them.
        nonHostChars = ['%', '/', '?', ';', '#']
          .concat(unwise).concat(autoEscape),
        nonAuthChars = ['/', '@', '?', '#'].concat(delims),
        hostnameMaxLen = 255,
        hostnamePartPattern = /^[a-zA-Z0-9][a-z0-9A-Z_-]{0,62}$/,
        hostnamePartStart = /^([a-zA-Z0-9][a-z0-9A-Z_-]{0,62})(.*)$/,
        // protocols that can allow "unsafe" and "unwise" chars.
        unsafeProtocol = {
          'javascript': true,
          'javascript:': true
        },
        // protocols that never have a hostname.
        hostlessProtocol = {
          'javascript': true,
          'javascript:': true
        },
        // protocols that always have a path component.
        pathedProtocol = {
          'http': true,
          'https': true,
          'ftp': true,
          'gopher': true,
          'file': true,
          'http:': true,
          'ftp:': true,
          'gopher:': true,
          'file:': true
        },
        // protocols that always contain a // bit.
        slashedProtocol = {
          'http': true,
          'https': true,
          'ftp': true,
          'gopher': true,
          'file': true,
          'http:': true,
          'https:': true,
          'ftp:': true,
          'gopher:': true,
          'file:': true
        },
        querystring = QueryString;

    function urlParse(url, parseQueryString, slashesDenoteHost) {
      if (url && typeof(url) === 'object' && url.href) return url;

      if (typeof url !== 'string')
        throw new TypeError("Parameter 'url' must be a string, not " + typeof url);

      var out = {},
          rest = url;

      // cut off any delimiters.
      // This is to support parse stuff like "<http://foo.com>"
      for (var i = 0, l = rest.length; i < l; i++) {
        if (delims.indexOf(rest.charAt(i)) === -1) break;
      }
      if (i !== 0) rest = rest.substr(i);


      var proto = protocolPattern.exec(rest);
      if (proto) {
        proto = proto[0];
        var lowerProto = proto.toLowerCase();
        out.protocol = lowerProto;
        rest = rest.substr(proto.length);
      }

      // figure out if it's got a host
      // user@server is *always* interpreted as a hostname, and url
      // resolution will treat //foo/bar as host=foo,path=bar because that's
      // how the browser resolves relative URLs.
      if (slashesDenoteHost || proto || rest.match(/^\/\/[^@\/]+@[^@\/]+/)) {
        var slashes = rest.substr(0, 2) === '//';
        if (slashes && !(proto && hostlessProtocol[proto])) {
          rest = rest.substr(2);
          out.slashes = true;
        }
      }

      if (!hostlessProtocol[proto] &&
          (slashes || (proto && !slashedProtocol[proto]))) {
        // there's a hostname.
        // the first instance of /, ?, ;, or # ends the host.
        // don't enforce full RFC correctness, just be unstupid about it.

        // If there is an @ in the hostname, then non-host chars *are* allowed
        // to the left of the first @ sign, unless some non-auth character
        // comes *before* the @-sign.
        // URLs are obnoxious.
        var atSign = rest.indexOf('@');
        if (atSign !== -1) {
          var auth = rest.slice(0, atSign);

          // there *may be* an auth
          var hasAuth = true;
          for (var i = 0, l = nonAuthChars.length; i < l; i++) {
            if (auth.indexOf(nonAuthChars[i]) !== -1) {
              // not a valid auth.  Something like http://foo.com/bar@baz/
              hasAuth = false;
              break;
            }
          }

          if (hasAuth) {
            // pluck off the auth portion.
            out.auth = decodeURIComponent(auth);
            rest = rest.substr(atSign + 1);
          }
        }

        var firstNonHost = -1;
        for (var i = 0, l = nonHostChars.length; i < l; i++) {
          var index = rest.indexOf(nonHostChars[i]);
          if (index !== -1 &&
              (firstNonHost < 0 || index < firstNonHost)) firstNonHost = index;
        }

        if (firstNonHost !== -1) {
          out.host = rest.substr(0, firstNonHost);
          rest = rest.substr(firstNonHost);
        } else {
          out.host = rest;
          rest = '';
        }

        // pull out port.
        var p = parseHost(out.host);
        var keys = Object.keys(p);
        for (var i = 0, l = keys.length; i < l; i++) {
          var key = keys[i];
          out[key] = p[key];
        }

        // we've indicated that there is a hostname,
        // so even if it's empty, it has to be present.
        out.hostname = out.hostname || '';

        // if hostname begins with [ and ends with ]
        // assume that it's an IPv6 address.
        var ipv6Hostname = out.hostname[0] === '[' &&
            out.hostname[out.hostname.length - 1] === ']';

        // validate a little.
        if (out.hostname.length > hostnameMaxLen) {
          out.hostname = '';
        } else if (!ipv6Hostname) {
          var hostparts = out.hostname.split(/\./);
          for (var i = 0, l = hostparts.length; i < l; i++) {
            var part = hostparts[i];
            if (!part) continue;
            if (!part.match(hostnamePartPattern)) {
              var newpart = '';
              for (var j = 0, k = part.length; j < k; j++) {
                if (part.charCodeAt(j) > 127) {
                  // we replace non-ASCII char with a temporary placeholder
                  // we need this to make sure size of hostname is not
                  // broken by replacing non-ASCII by nothing
                  newpart += 'x';
                } else {
                  newpart += part[j];
                }
              }
              // we test again with ASCII char only
              if (!newpart.match(hostnamePartPattern)) {
                var validParts = hostparts.slice(0, i);
                var notHost = hostparts.slice(i + 1);
                var bit = part.match(hostnamePartStart);
                if (bit) {
                  validParts.push(bit[1]);
                  notHost.unshift(bit[2]);
                }
                if (notHost.length) {
                  rest = '/' + notHost.join('.') + rest;
                }
                out.hostname = validParts.join('.');
                break;
              }
            }
          }
        }

        // hostnames are always lower case.
        out.hostname = out.hostname.toLowerCase();

        if (!ipv6Hostname) {
          // IDNA Support: Returns a puny coded representation of "domain".
          // It only converts the part of the domain name that
          // has non ASCII characters. I.e. it dosent matter if
          // you call it with a domain that already is in ASCII.
          var domainArray = out.hostname.split('.');
          var newOut = [];
          for (var i = 0; i < domainArray.length; ++i) {
            var s = domainArray[i];
            newOut.push(s.match(/[^A-Za-z0-9_-]/) ?
                'xn--' + punycode.encode(s) : s);
          }
          out.hostname = newOut.join('.');
        }

        out.host = (out.hostname || '') +
            ((out.port) ? ':' + out.port : '');
        out.href += out.host;

        // strip [ and ] from the hostname
        if (ipv6Hostname) {
          out.hostname = out.hostname.substr(1, out.hostname.length - 2);
          if (rest[0] !== '/') {
            rest = '/' + rest;
          }
        }
      }

      // now rest is set to the post-host stuff.
      // chop off any delim chars.
      if (!unsafeProtocol[lowerProto]) {

        // First, make 100% sure that any "autoEscape" chars get
        // escaped, even if encodeURIComponent doesn't think they
        // need to be.
        for (var i = 0, l = autoEscape.length; i < l; i++) {
          var ae = autoEscape[i];
          var esc = encodeURIComponent(ae);
          if (esc === ae) {
            esc = escape(ae);
          }
          rest = rest.split(ae).join(esc);
        }

        // Now make sure that delims never appear in a url.
        var chop = rest.length;
        for (var i = 0, l = delims.length; i < l; i++) {
          var c = rest.indexOf(delims[i]);
          if (c !== -1) {
            chop = Math.min(c, chop);
          }
        }
        rest = rest.substr(0, chop);
      }


      // chop off from the tail first.
      var hash = rest.indexOf('#');
      if (hash !== -1) {
        // got a fragment string.
        out.hash = rest.substr(hash);
        rest = rest.slice(0, hash);
      }
      var qm = rest.indexOf('?');
      if (qm !== -1) {
        out.search = rest.substr(qm);
        out.query = rest.substr(qm + 1);
        if (parseQueryString) {
          out.query = querystring.parse(out.query);
        }
        rest = rest.slice(0, qm);
      } else if (parseQueryString) {
        // no query string, but parseQueryString still requested
        out.search = '';
        out.query = {};
      }
      if (rest) out.pathname = rest;
      if (slashedProtocol[proto] &&
          out.hostname && !out.pathname) {
        out.pathname = '/';
      }

      //to support http.request
      if (out.pathname || out.search) {
        out.path = (out.pathname ? out.pathname : '') +
                   (out.search ? out.search : '');
      }

      // finally, reconstruct the href based on what has been validated.
      out.href = urlFormat(out);
      return out;
    }

    // format a parsed object into a url string
    function urlFormat(obj) {
      // ensure it's an object, and not a string url.
      // If it's an obj, this is a no-op.
      // this way, you can call url_format() on strings
      // to clean up potentially wonky urls.
      if (typeof(obj) === 'string') obj = urlParse(obj);

      var auth = obj.auth || '';
      if (auth) {
        auth = encodeURIComponent(auth);
        auth = auth.replace(/%3A/i, ':');
        auth += '@';
      }

      var protocol = obj.protocol || '',
          pathname = obj.pathname || '',
          hash = obj.hash || '',
          host = false,
          query = '';

      if (obj.host !== undefined) {
        host = auth + obj.host;
      } else if (obj.hostname !== undefined) {
        host = auth + (obj.hostname.indexOf(':') === -1 ?
            obj.hostname :
            '[' + obj.hostname + ']');
        if (obj.port) {
          host += ':' + obj.port;
        }
      }

      if (obj.query && typeof obj.query === 'object' &&
          Object.keys(obj.query).length) {
        query = querystring.stringify(obj.query);
      }

      var search = obj.search || (query && ('?' + query)) || '';

      if (protocol && protocol.substr(-1) !== ':') protocol += ':';

      // only the slashedProtocols get the //.  Not mailto:, xmpp:, etc.
      // unless they had them to begin with.
      if (obj.slashes ||
          (!protocol || slashedProtocol[protocol]) && host !== false) {
        host = '//' + (host || '');
        if (pathname && pathname.charAt(0) !== '/') pathname = '/' + pathname;
      } else if (!host) {
        host = '';
      }

      if (hash && hash.charAt(0) !== '#') hash = '#' + hash;
      if (search && search.charAt(0) !== '?') search = '?' + search;

      return protocol + host + pathname + search + hash;
    }

    function urlResolve(source, relative) {
      return urlFormat(urlResolveObject(source, relative));
    }

    function urlResolveObject(source, relative) {
      if (!source) return relative;

      source = urlParse(urlFormat(source), false, true);
      relative = urlParse(urlFormat(relative), false, true);

      // hash is always overridden, no matter what.
      source.hash = relative.hash;

      if (relative.href === '') {
        source.href = urlFormat(source);
        return source;
      }

      // hrefs like //foo/bar always cut to the protocol.
      if (relative.slashes && !relative.protocol) {
        relative.protocol = source.protocol;
        //urlParse appends trailing / to urls like http://www.example.com
        if (slashedProtocol[relative.protocol] &&
            relative.hostname && !relative.pathname) {
          relative.path = relative.pathname = '/';
        }
        relative.href = urlFormat(relative);
        return relative;
      }

      if (relative.protocol && relative.protocol !== source.protocol) {
        // if it's a known url protocol, then changing
        // the protocol does weird things
        // first, if it's not file:, then we MUST have a host,
        // and if there was a path
        // to begin with, then we MUST have a path.
        // if it is file:, then the host is dropped,
        // because that's known to be hostless.
        // anything else is assumed to be absolute.
        if (!slashedProtocol[relative.protocol]) {
          relative.href = urlFormat(relative);
          return relative;
        }
        source.protocol = relative.protocol;
        if (!relative.host && !hostlessProtocol[relative.protocol]) {
          var relPath = (relative.pathname || '').split('/');
          while (relPath.length && !(relative.host = relPath.shift()));
          if (!relative.host) relative.host = '';
          if (!relative.hostname) relative.hostname = '';
          if (relPath[0] !== '') relPath.unshift('');
          if (relPath.length < 2) relPath.unshift('');
          relative.pathname = relPath.join('/');
        }
        source.pathname = relative.pathname;
        source.search = relative.search;
        source.query = relative.query;
        source.host = relative.host || '';
        source.auth = relative.auth;
        source.hostname = relative.hostname || relative.host;
        source.port = relative.port;
        //to support http.request
        if (source.pathname !== undefined || source.search !== undefined) {
          source.path = (source.pathname ? source.pathname : '') +
                        (source.search ? source.search : '');
        }
        source.slashes = source.slashes || relative.slashes;
        source.href = urlFormat(source);
        return source;
      }

      var isSourceAbs = (source.pathname && source.pathname.charAt(0) === '/'),
          isRelAbs = (
              relative.host !== undefined ||
              relative.pathname && relative.pathname.charAt(0) === '/'
          ),
          mustEndAbs = (isRelAbs || isSourceAbs ||
                        (source.host && relative.pathname)),
          removeAllDots = mustEndAbs,
          srcPath = source.pathname && source.pathname.split('/') || [],
          relPath = relative.pathname && relative.pathname.split('/') || [],
          psychotic = source.protocol &&
              !slashedProtocol[source.protocol];

      // if the url is a non-slashed url, then relative
      // links like ../.. should be able
      // to crawl up to the hostname, as well.  This is strange.
      // source.protocol has already been set by now.
      // Later on, put the first path part into the host field.
      if (psychotic) {

        delete source.hostname;
        delete source.port;
        if (source.host) {
          if (srcPath[0] === '') srcPath[0] = source.host;
          else srcPath.unshift(source.host);
        }
        delete source.host;
        if (relative.protocol) {
          delete relative.hostname;
          delete relative.port;
          if (relative.host) {
            if (relPath[0] === '') relPath[0] = relative.host;
            else relPath.unshift(relative.host);
          }
          delete relative.host;
        }
        mustEndAbs = mustEndAbs && (relPath[0] === '' || srcPath[0] === '');
      }

      if (isRelAbs) {
        // it's absolute.
        source.host = (relative.host || relative.host === '') ?
                          relative.host : source.host;
        source.hostname = (relative.hostname || relative.hostname === '') ?
                          relative.hostname : source.hostname;
        source.search = relative.search;
        source.query = relative.query;
        srcPath = relPath;
        // fall through to the dot-handling below.
      } else if (relPath.length) {
        // it's relative
        // throw away the existing file, and take the new path instead.
        if (!srcPath) srcPath = [];
        srcPath.pop();
        srcPath = srcPath.concat(relPath);
        source.search = relative.search;
        source.query = relative.query;
      } else if ('search' in relative) {
        // just pull out the search.
        // like href='?foo'.
        // Put this after the other two cases because it simplifies the booleans
        if (psychotic) {
          source.hostname = source.host = srcPath.shift();
          //occationaly the auth can get stuck only in host
          //this especialy happens in cases like
          //url.resolveObject('mailto:local1@domain1', 'local2@domain2')
          var authInHost = source.host && source.host.indexOf('@') > 0 ?
                           source.host.split('@') : false;
          if (authInHost) {
            source.auth = authInHost.shift();
            source.host = source.hostname = authInHost.shift();
          }
        }
        source.search = relative.search;
        source.query = relative.query;
        //to support http.request
        if (source.pathname !== undefined || source.search !== undefined) {
          source.path = (source.pathname ? source.pathname : '') +
                        (source.search ? source.search : '');
        }
        source.href = urlFormat(source);
        return source;
      }
      if (!srcPath.length) {
        // no path at all.  easy.
        // we've already handled the other stuff above.
        delete source.pathname;
        //to support http.request
        if (!source.search) {
          source.path = '/' + source.search;
        } else {
          delete source.path;
        }
        source.href = urlFormat(source);
        return source;
      }
      // if a url ENDs in . or .., then it must get a trailing slash.
      // however, if it ends in anything else non-slashy,
      // then it must NOT get a trailing slash.
      var last = srcPath.slice(-1)[0];
      var hasTrailingSlash = (
          (source.host || relative.host) && (last === '.' || last === '..') ||
          last === '');

      // strip single dots, resolve double dots to parent dir
      // if the path tries to go above the root, `up` ends up > 0
      var up = 0;
      for (var i = srcPath.length; i >= 0; i--) {
        last = srcPath[i];
        if (last == '.') {
          srcPath.splice(i, 1);
        } else if (last === '..') {
          srcPath.splice(i, 1);
          up++;
        } else if (up) {
          srcPath.splice(i, 1);
          up--;
        }
      }

      // if the path is allowed to go above the root, restore leading ..s
      if (!mustEndAbs && !removeAllDots) {
        for (; up--; up) {
          srcPath.unshift('..');
        }
      }

      if (mustEndAbs && srcPath[0] !== '' &&
          (!srcPath[0] || srcPath[0].charAt(0) !== '/')) {
        srcPath.unshift('');
      }

      if (hasTrailingSlash && (srcPath.join('/').substr(-1) !== '/')) {
        srcPath.push('');
      }

      var isAbsolute = srcPath[0] === '' ||
          (srcPath[0] && srcPath[0].charAt(0) === '/');

      // put the host back
      if (psychotic) {
        source.hostname = source.host = isAbsolute ? '' :
                                        srcPath.length ? srcPath.shift() : '';
        //occationaly the auth can get stuck only in host
        //this especialy happens in cases like
        //url.resolveObject('mailto:local1@domain1', 'local2@domain2')
        var authInHost = source.host && source.host.indexOf('@') > 0 ?
                         source.host.split('@') : false;
        if (authInHost) {
          source.auth = authInHost.shift();
          source.host = source.hostname = authInHost.shift();
        }
      }

      mustEndAbs = mustEndAbs || (source.host && srcPath.length);

      if (mustEndAbs && !isAbsolute) {
        srcPath.unshift('');
      }

      source.pathname = srcPath.join('/');
      //to support request.http
      if (source.pathname !== undefined || source.search !== undefined) {
        source.path = (source.pathname ? source.pathname : '') +
                      (source.search ? source.search : '');
      }
      source.auth = relative.auth || source.auth;
      source.slashes = source.slashes || relative.slashes;
      source.href = urlFormat(source);
      return source;
    }

    function parseHost(host) {
      var out = {};
      var port = portPattern.exec(host);
      if (port) {
        port = port[0];
        if (port !== ':') {
          out.port = port.substr(1);
        }
        host = host.substr(0, host.length - port.length);
      }
      if (host) out.hostname = host;
      return out;
    }


    return {
      parse         : urlParse,
      resolve       : urlResolve,
      resolveObject : urlResolveObject,
      format        : urlFormat
    }
  })();
}

/*!
 * Nats
 * Copyright(c) 2011,2012 Derek Collison (derek.collison@gmail.com)
 * MIT Licensed
 */

var NATS = (function(){
  if (typeof EventEmitter != 'function') {
    console.error('The EventEmitter library (https://github.com/Wolfy87/EventEmitter) is required');
    return;
  }
  if (typeof url != 'object') {
    console.error('The url library is required');
    return;
  }

  /**
   * Constants
   */

  var VERSION = '0.2.7',

      DEFAULT_PORT = 4222,
      DEFAULT_PRE  = 'nats://localhost:',
      DEFAULT_URI  =  DEFAULT_PRE + DEFAULT_PORT,

      MAX_CONTROL_LINE_SIZE = 512,

      // Parser state
      AWAITING_CONTROL = 0,
      AWAITING_MSG_PAYLOAD = 1,

      // Reconnect Parameters, 2 sec wait, 10 tries
      DEFAULT_RECONNECT_TIME_WAIT = 2*1000,
      DEFAULT_MAX_RECONNECT_ATTEMPTS = 10,

      // Protocol
      CONTROL_LINE = /^(.*)\r\n/,

      MSG  = /^MSG\s+([^\s\r\n]+)\s+([^\s\r\n]+)\s+(([^\s\r\n]+)[^\S\r\n]+)?(\d+)\r\n/i,
      OK   = /^\+OK\s*\r\n/i,
      ERR  = /^-ERR\s+('.+')?\r\n/i,
      PING = /^PING\r\n/i,
      PONG = /^PONG\r\n/i,
      INFO = /^INFO\s+([^\r\n]+)\r\n/i,

      CR_LF = '\r\n',
      CR_LF_LEN = CR_LF.length,
      EMPTY = '',
      SPC = ' ',

      // Protocol
      PUB     = 'PUB',
      SUB     = 'SUB',
      UNSUB   = 'UNSUB',
      CONNECT = 'CONNECT',

      // Responses
      PING_REQUEST  = 'PING' + CR_LF,
      PONG_RESPONSE = 'PONG' + CR_LF,

      EMPTY = '',

      // Pedantic Mode support
      Q_SUB = /^([^\.\*>\s]+|>$|\*)(\.([^\.\*>\s]+|>$|\*))*$/,
      Q_SUB_NO_WC = /^([^\.\*>\s]+)(\.([^\.\*>\s]+))*$/,

      FLUSH_THRESHOLD = 65536;

  /**
   * Library Version
   */


  /**
   * Generate random hex strings for createInbox.
   *
   * @api private
  */

  function hexRand(limit) {
    return (parseInt(Math.random()*limit, 16).toString(16));
  }

  /**
   * Replacement for Buffer.byteLength
   * @see http://stackoverflow.com/a/2858850/1020416
   *
   * @api private
  */

  function byteLength ( str ) {
    return unescape(encodeURIComponent(str)).length;
  }

  /**
   * Node.js util.inherit
   *
   * @api private
  */

  function inherits(ctor, superCtor) {
    ctor.super_ = superCtor;
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };

  /**
   * Create a properly formatted inbox subject.
   *
   * @api public
  */

  var createInbox = function() {
    return ('_INBOX.' +
            hexRand(0x0010000) +
            hexRand(0x0010000) +
            hexRand(0x0010000) +
            hexRand(0x0010000) +
            hexRand(0x1000000));
  };

  /**
   * Initialize a client with the appropriate options.
   *
   * @param {Mixed} opts
   * @api public
   */

  function Client(opts, listeners) {
    EventEmitter.call(this);
    this.parseOptions(opts);
    this.addListeners(listeners);
    this.initState();
    this.createConnection();
  }

  /**
   * Connect to a nats-server and return the client.
   * Argument can be a url, or an object with a 'url'
   * property and additional options.
   *
   * @params {Mixed} opts
   *
   * @api public
   */

  var connect = function(opts, listeners) {
    return new Client(opts, listeners);
  };

  /**
   * Connected clients are event emitters.
   */

  inherits(Client, EventEmitter);

  /**
   * Allow createInbox to be called on a client.
   *
   * @api public
   */

  Client.prototype.createInbox = createInbox;

  Client.prototype.assignOption = function(opts, prop, assign) {
    if (assign === undefined) {
      assign = prop;
    }
    if (opts[prop] !== undefined) {
      this.options[assign] = opts[prop];
    }
  };

  /**
   * Parse the conctructor/connect options.
   *
   * @param {Mixed} opts
   * @api private
   */

  Client.prototype.parseOptions = function(opts) {
    var options = this.options = {
      'url'                  : DEFAULT_URI,
      'verbose'              : false,
      'pedantic'             : false,
      'reconnect'            : true,
      'maxReconnectAttempts' : DEFAULT_MAX_RECONNECT_ATTEMPTS,
      'reconnectTimeWait'    : DEFAULT_RECONNECT_TIME_WAIT,
      'path'                 : '/nats'
    };
    if ('number' === typeof opts) {
      options.url = DEFAULT_PRE + opts;
    } else if ('string' === typeof opts) {
      options.url = opts;
    } else if ('object' === typeof opts) {
      if (opts.port !== undefined) {
        options.url = DEFAULT_PRE + opts.port;
      }
      // Pull out various options here
      this.assignOption(opts, 'url');
      this.assignOption(opts, 'uri', 'url');
      this.assignOption(opts, 'user');
      this.assignOption(opts, 'pass');
      this.assignOption(opts, 'password', 'pass');
      this.assignOption(opts, 'verbose');
      this.assignOption(opts, 'pedantic');
      this.assignOption(opts, 'reconnect');
      this.assignOption(opts, 'maxReconnectAttempts');
      this.assignOption(opts, 'reconnectTimeWait');
      this.assignOption(opts, 'path');
    }
    options.uri = options.url;

    if (options.url !== undefined) {
      // Parse the url
      this.url = url.parse(options.url);
      if (this.url.auth !== undefined) {
        var auth = this.url.auth.split(':');
        if (options.user === undefined) {
          options.user = auth[0];
        }
        if (options.pass === undefined) {
          options.pass = auth[1];
        }
      }
    }
  };

  /**
   * Initialize listners passed in during construction
   *
   * @api private
  */
  Client.prototype.addListeners = function(list) {
    if('object' != typeof list){
      console.error("listeners must be an Object");
      return;
    }
    var events = ['connect', 'disconnect', 'error', 'close']
    for(e in events) {
      if('function' == typeof list[events[e]])
        this.addListener(events[e], list[events[e]])
    }
  }

  /**
   * Properly setup a stream connection with proper events.
   *
   * @api private
  */

  Client.prototype.createConnection = function() {
    var client = this;

    client.pongs   = [];
    client.pending = [];
    client.pSize   = 0;
    client.pstate  = AWAITING_CONTROL;

    var scheme    = (window.location.protocol || document.location.protocol) == "https:" ? 'wss://' : 'ws://';
    var stream    = new WebSocket(scheme + window.location.host + client.options.path);
    client.stream = stream

    stream.onopen = function() {
      var wasReconnecting = client.reconnecting;
      var event = wasReconnecting === true ? 'reconnect' : 'connect';
      client.connected = true;
      client.reconnecting = false;
      client.reconnects = 0;
      client.flushPending();
      if (wasReconnecting) {
        client.sendSubscriptions();
      }
      client.flush(function() {
        client.emit(event, client);
      });
    };

    stream.onclose =  function(evt) {
      client.closeStream();
      client.emit('disconnect', evt.code && evt.code >= 4000 ? evt.code - 4000 : evt.code, evt.reason);
      if (client.closed === true ||
          client.options.reconnect === false ||
          client.reconnects >= client.options.maxReconnectAttempts) {
        client.emit('close');
      } else {
        client.scheduleReconnect();
      }
    };

    stream.onerror = function(exception) {
      client.closeStream();

      if (client.reconnecting === false) {
        client.emit('error', exception);
      }
      client.emit('disconnect');

      if (client.reconnecting === true) {
        if (client.closed === true ||
            client.reconnects >= client.options.maxReconnectAttempts) {
          client.emit('close');
        } else {
          client.scheduleReconnect();
        }
      }
    };

    stream.onmessage = function (wsmsg) {
      var data = wsmsg.data
      var m;

      console.log(client.pstate,'psize',client.pSize,'wsdata',data);

      client.inbound = client.inbound ? client.inbound + data : data;

      while (!client.closed && client.inbound && client.inbound.length > 0) {
        switch (client.pstate) {
        case AWAITING_CONTROL:
          if ((m = MSG.exec(client.inbound)) != null) {
            client.payload = {
              subj  : m[1],
              sid   : m[2],
              reply : m[4],
              size  : parseInt(m[5], 10)
            };
            client.pstate = AWAITING_MSG_PAYLOAD;
          } else if ((m = OK.exec(client.inbound)) != null) {
            // Ignore for now..
          } else if ((m = ERR.exec(client.inbound)) != null) {
            client.emit('error', m[1]);
          } else if ((m = PONG.exec(client.inbound)) != null) {
            console.log('got pong',m);
            var cb = client.pongs.shift();
            if (cb) { cb(); } // FIXME: Should we check for exceptions?
          } else if ((m = PING.exec(client.inbound)) != null) {
            client.sendCommand(PONG_RESPONSE);
          } else if ((m = INFO.exec(client.inbound)) != null) {
            // Ignore for now..
          } else {
            // FIXME, check line length for something weird.
            // Nothing here yet, return
            return;
          }
          break;

        case AWAITING_MSG_PAYLOAD:

          if (client.inbound.length < client.payload.size + CR_LF_LEN) {
            return;
          }

          // FIXME, may be inefficient.
          client.payload.msg = client.inbound.slice(0, client.payload.size).toString();

          if (client.inbound.length === client.payload.size + CR_LF_LEN) {
            client.inbound = null;
          } else {
            client.inbound = client.inbound.slice(client.payload.size + CR_LF_LEN);
          }
          // process the message
          client.processMsg();
        }

        if (m) {
          // Chop inbound
          var psize = m[0].length;
          if (psize >= client.inbound.length) {
            client.inbound = null;
          } else {
            client.inbound = client.inbound.slice(psize);
          }
        }
        m = null;
      }
    };

    // Queue the connect command.
    var cs = { 'verbose':this.options.verbose, 'pedantic':this.options.pedantic };
    if (this.options.user !== undefined) {
      cs.user = this.options.user;
      cs.pass = this.options.pass;
    }
    this.sendCommand(CONNECT + SPC + JSON.stringify(cs) + CR_LF);
  };

  /**
   * Initialize client state.
   *
   * @api private
   */

  Client.prototype.initState = function() {
    this.ssid         = 1;
    this.subs         = {};
    this.reconnects   = 0;
    this.connected    = false;
    this.reconnecting = false;
  };

  /**
   * Close the connection to the server.
   *
   * @api public
   */

  Client.prototype.close = function() {
    this.closed = true;
    this.removeAllListeners();
    this.closeStream();
    this.ssid     = -1;
    this.subs     = null;
    this.pstate   = -1;
    this.pongs    = null;
    this.pending  = null;
    this.pSize    = 0;
  };

  /**
   * Close down the stream and clear state.
   *
   * @api private
   */

  Client.prototype.closeStream = function() {
    if (this.stream != null) {
      this.stream.close();
      this.stream  = null;
    }
    if (this.connected === true || this.closed === true) {
      this.pongs     = null;
      this.pending   = null;
      this.pSize     = 0;
      this.connected = false;
    }
  };

  /**
   * Flush all pending data to the server.
   *
   * @api private
   */

  Client.prototype.flushPending = function() {
    if (this.connected === false ||
        this.pending == null ||
        this.pending.length === 0) { return; }

    var output = this.pending.join(EMPTY);
    console.log('flush',output);
    this.stream.send(output);
    this.pending = [];
    this.pSize   = 0;
  };

  /**
   * Send commands to the server or queue them up if connection pending.
   *
   * @api private
   */

  Client.prototype.sendCommand = function(cmd) {
    // Buffer to cut down on system calls, increase throughput.
    // When receive gets faster, should make this Buffer based..

    if (this.closed || this.pending == null) { return; }

    this.pending.push(cmd);
    this.pSize += byteLength(cmd);

    if (this.connected === true) {
      // First one let's setup flush..
      if (this.pending.length === 1) {
        var self = this;
        setTimeout(function(){
          self.flushPending();
        }, 1);
      } else if (this.pSize > FLUSH_THRESHOLD) {
        // Flush in place when threshold reached..
        this.flushPending();
      }
    }
  };

  /**
   * Sends existing subscriptions to new server after reconnect.
   *
   * @api private
   */

  Client.prototype.sendSubscriptions = function() {
    var proto;
    for(var sid in this.subs) {
      if (this.subs.hasOwnProperty(sid)) {
        var sub = this.subs[sid];
        if (sub.qgroup) {
    proto = [SUB, sub.subject, sub.qgroup, sid + CR_LF];
        } else {
    proto = [SUB, sub.subject, sid + CR_LF];
        }
        this.sendCommand(proto.join(SPC));
      }
    }
  };

  /**
   * Process a delivered message and deliver to appropriate subscriber.
   *
   * @api private
   */

  Client.prototype.processMsg = function() {
    var sub = this.subs[this.payload.sid];
    if (sub != null) {
      sub.received += 1;
      // Check for a timeout, and cancel if received >= expected
      if (sub.timeout) {
        if (sub.received >= sub.expected) {
          clearTimeout(sub.timeout);
          sub.timeout = null;
        }
      }
      // Check for auto-unsubscribe
      if (sub.max !== undefined) {
        if (sub.received === sub.max) {
          delete this.subs[this.payload.sid];
        } else if (sub.received > sub.max) {
          this.unsubscribe(this.payload.sid);
          sub.callback = null;
        }
      }

      if (sub.callback) {
        sub.callback(this.payload.msg, this.payload.reply, this.payload.subj);
      }
    }

    this.pstate = AWAITING_CONTROL;
    this.payload = null;
  };

  /**
   * Flush outbound queue to server and call optional callback when server has processed
   * all data.
   *
   * @param {Function} opt_callback
   * @api public
   */

  Client.prototype.flush = function(opt_callback) {
    if (typeof opt_callback === 'function') {
      this.pongs.push(opt_callback);
      this.sendCommand(PING_REQUEST);
    }
  };

  /**
   * Publish a message to the given subject, with optional reply and callback.
   *
   * @param {String} subject
   * @param {String} msg
   * @param {String} opt_reply
   * @param {Function} opt_callback
   * @api public
   */

  Client.prototype.publish = function(subject, msg, opt_reply, opt_callback) {
    if (!msg) { msg = EMPTY; }
    if (typeof msg === 'function') {
      if (opt_callback || opt_reply) {
        throw(new Error("Message can't be a function"));
      }
      opt_callback = msg;
      msg = EMPTY;
      opt_reply = undefined;
    }
    if (typeof opt_reply === 'function') {
      if (opt_callback) {
        throw(new Error("Reply can't be a function"));
      }
      opt_callback = opt_reply;
      opt_reply = undefined;
    }

    var proto = [PUB, subject];
    var pmsg = [byteLength(msg), CR_LF, msg, CR_LF];

    if (opt_reply !== undefined) {
      proto.push(opt_reply);
    }

    this.sendCommand(proto.concat(pmsg.join(EMPTY)).join(SPC));

    if (opt_callback !== undefined) {
      this.flush(opt_callback);
    }
    return this;
  };

  /**
   * Subscribe to a given subject, with optional options and callback. opts can be
   * ommitted, even with a callback. The Subscriber Id is returned.
   *
   * @param {String} subject
   * @param {Object} opts
   * @param {Function} callback
   * @return {Mixed}
   * @api public
   */

  Client.prototype.subscribe = function(subject, opts, callback) {
    var qgroup, max;
    if (typeof opts === 'function') {
      callback = opts;
      opts = null;
    } else if (opts && typeof opts === 'object') {
      // FIXME, check exists, error otherwise..
      qgroup = opts.queue;
      max = opts.max;
    }
    this.ssid += 1;
    this.subs[this.ssid] = { 'subject':subject, 'callback':callback, 'received':0 };

    var proto;
    if (typeof qgroup === 'string') {
      this.subs[this.ssid].qgroup = qgroup;
      proto = [SUB, subject, qgroup, this.ssid + CR_LF];
    } else {
      proto = [SUB, subject, this.ssid + CR_LF];
    }
    this.sendCommand(proto.join(SPC));

    if (max) {
      this.unsubscribe(this.ssid, max);
    }
    return this.ssid;
  };

  /**
   * Unsubscribe to a given Subscriber Id, with optional max parameter.
   *
   * @param {Mixed} sid
   * @param {Number} opt_max
   * @api public
   */

  Client.prototype.unsubscribe = function(sid, opt_max) {
    if (!sid) { return; }

    var proto;
    if (opt_max) {
      proto = [UNSUB, sid, opt_max + CR_LF];
    } else {
      proto = [UNSUB, sid + CR_LF];
    }
    this.sendCommand(proto.join(SPC));

    var sub = this.subs[sid];
    if (sub == null) {
      return;
    }
    sub.max = opt_max;
    if (sub.max === undefined || (sub.received >= sub.max)) {
      delete this.subs[sid];
    }
  };

  /**
   * Set a timeout on a subscription.
   *
   * @param {Mixed} sid
   * @param {Number} timeout
   * @param {Number} expected
   * @api public
   */

  Client.prototype.timeout = function(sid, timeout, expected, callback) {
    if (!sid) { return; }
    var sub = this.subs[sid];
    if (sub == null) { return; }
    sub.expected = expected;
    sub.timeout = setTimeout(function() { callback(sid); }, timeout);
  };

  /**
   * Publish a message with an implicit inbox listener as the reply. Message is optional.
   *
   * @param {String} subject
   * @param {String} opt_msg
   * @param {Object} opt_options
   * @param {Function} callback
   * @api public
   */

  Client.prototype.request = function(subject, opt_msg, opt_options, callback) {
    if (typeof opt_msg === 'function') {
      callback = opt_msg;
      opt_msg = EMPTY;
      opt_options = null;
    }
    if (typeof opt_options === 'function') {
      callback = opt_options;
      opt_options = null;
    }
    var inbox = createInbox();
    var s = this.subscribe(inbox, opt_options, function(msg, reply) {
      callback(msg, reply);
    });
    this.publish(subject, opt_msg, inbox);
    return s;
  };

  /**
   * Reconnect to the server.
   *
   * @api private
   */

  Client.prototype.reconnect = function() {
    if (this.closed) { return; }
    this.reconnects += 1;
    this.createConnection();
    this.emit('reconnecting');
  };

  /**
   * Setup a timer event to attempt reconnect.
   *
   * @api private
   */

  Client.prototype.scheduleReconnect = function() {
    var client = this;
    client.reconnecting = true;
    setTimeout(function() { client.reconnect(); }, this.options.reconnectTimeWait);
  };

  return {
    version: VERSION,
    createInbox: createInbox,
    connection: connect
  }
})();
