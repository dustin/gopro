//////////////////////////////////////////////////////////////////////
//
// WebSocket.js
// JavaScript runtime code for Elm PortFunnel.WebSocket module.
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Portions Copyright (c) 2016 Evan Czaplicki
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

//
// This file cooperates with PortFunnel.js to share a single port
// pair with other compliant modules.
//
// For technical details, see:
//
// https://github.com/billstclair/elm-port-funnel/blob/master/DEVELOPERS-GUIDE.md
//

(function(scope) {
  var moduleName = 'WebSocket';

  var sub;

  function init() {
    var PortFunnel = scope.PortFunnel;
    if (!PortFunnel || !PortFunnel.sub || !PortFunnel.modules) {
      // Loop until PortFunnel.js has initialized itself.
      setTimeout(init, 10);
      return;
    }

    sub = PortFunnel.sub;
    PortFunnel.modules[moduleName] = { cmd: dispatcher }

    // Let the Elm code know we've started
    sub.send({ module: moduleName,
               tag: "startup",
               args : null
             });
  }
  init();

  var tagTable =
      { open: doOpen,
        send: doSend,
        getBytesQueued: doGetBytesQueued,
        close: doClose,
        delay: doDelay,
        willopen: sendBack,
        willsend: sendBack,
        willclose: sendBack
      }
        
  function dispatcher(tag, args) {
    let f = tagTable[tag];
    if (f) {
      return f(args, tag);    // most functions ignore the tag
    } else {
      return unimplemented(tag, args);
    }
  }

  // This is for the willxxx commands, which need to have the State
  // to do their thing, so can't be directly executed here.
  // they'll come back as xxx, after the Elm code validates them.
  function sendBack(args, tag) {
    return objectReturn(tag, args);
  }

  function objectReturn(tag, args) {
    return { module: moduleName, tag: tag, args: args };
  }

  function keyedErrorReturn(key, code, description, name, message) {
    var returnMessage = { key: key, code: code, description: description };
    if (name) {
      returnMessage.name = name;
    }
    if (message) {
      returnMessage.message = message;
    }
    return objectReturn("error", returnMessage);
  }

  function errorReturn(code, description) {
    return objectReturn("error", { code: code, description: description });
  }

  function unimplemented(func, args) {
    return errorReturn ("unimplemented",
                        "Not implemented: "+ func +
                        "(" + JSON.stringify(args) + ")");
  }

  var sockets = {};

  function doOpen(args) {
    var key = args.key;
    var url = args.url;
    if (!key) key = url;
    if (sockets[key]) {
      return errorReturn("keyused", "Key already has a socket open: " + key);
    }
    try {
	  var socket = new WebSocket(url);
      sockets[key] = socket;
    }
    catch(err) {
      // The old code returned BadSecurity if err.name was 'SecurityError'
      // or BadArgs otherwise.
      return errorReturn('openfailed',
                         "Can't create socket for URL: " + url,
                         err.name
                        )
    }
    socket.addEventListener("open", function(event) {
      //console.log("Socket connected for URL: " + url);
      sub.send(objectReturn("connected",
                            { key: key,
                              description: "Socket connected for URL: " + url
                            }));
    });
    socket.addEventListener("message", function(event) {
      var message = event.data;
      //console.log("Received for '" + key + "': " + message);
      sub.send(objectReturn("messageReceived",
                            { key: key, message: message }));
    });
    socket.addEventListener("close", function(event) {
	  //console.log("'" + key + "' closed");
      delete sockets[key];        // for open errors
      sub.send(objectReturn("closed",
                            { key: key,
                              bytesQueued: socket.bufferedAmount,
                              code: event.code,
                              reason: "" + event.reason,
                              wasClean: event.wasClean ? true : false
                            }));
    });
    return null;
  } 

  function socketNotOpenReturn(key, name, message) {
    return keyedErrorReturn(key, 'notopen', 'Socket not open', name, message);
  }

  function doSend(args) {
    var key = args.key;
    var message = args.message;
    var socket = sockets[key];
    if (!socket) {
      return socketNotOpenReturn(key, "send", message);
    }
    try {
	  socket.send(message);
    } catch(err) {
      // The old code ignored err.name
      return keyedErrorReturn(key, 'badsend', 'Send error', err.name)
    }
    return null;
  } 

  function doClose(args) {
    var key = args.key;
    var reason = args.reason;   // not used
    var socket = sockets[key];
    if (!socket) {
      return socketNotOpenReturn(key, "close");
    }
    try {
      // Should this happen in the event listener?
      delete sockets[key];
      socket.close();
    } catch(err) {
      // The old code returned BadReason if err.name was 'SyntaxError'
      // or BadCode otherwise
      return keyedErrorReturn(key, 'badclose', 'Close error', err.name)
    }
  } 

  function doGetBytesQueued(args) {
    var key = args.key;
    var socket = sockets[key];
    if (!socket) {
      return socketNotOpenReturn(key, "getBytesQueued");
    }
    return objectReturn("bytesQueued",
                        { key: key,
                          bytesQueued: "" + socket.bufferedAmount
                        });
  } 

  function doDelay(args) {
    var millis = args.millis;
    console.log("Sleeping for", millis, " milliseconds for id: ", args.id);
    function callback() {
      sub.send(objectReturn("delayed", { id: args.id }));
    }
    setTimeout(callback, millis);
  } 

})(this);
