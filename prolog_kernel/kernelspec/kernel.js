define(["codemirror/lib/codemirror"], function(CodeMirror) {
  "use strict";

  return {
    onload: function() {
      console.info("Loading Prolog kernel.js");
      CodeMirror.defineMode("prolog", function(config, parserConfig) {
        return {
          startState: function() {
            return {
              state: "initial",
            };
          },

          token: function(stream, state) {
            switch (state.state) {
              case "initial":
                if (stream.match(/^\%/)) {
                  // Line comment -> consume the rest of the line
                  stream.match(/^.+/);
                  return "comment";
                } else if (stream.match(/^\/\*/)) {
                  // Block comment start -> switch to comment state
                  state.state = "comment";
                  return "comment";
                 } else if (stream.match(/^"(?:[^"])*"/)) {
                  return "string";
                } else if (stream.match(/^'(?:[^'])*'/)) {
                  return "atom";
                } else if (stream.match(/^(?:[0-9]+)/)) {
                  return "number";
                } else if (stream.match(/^(?:!)/)) {
                  return "builtin";
                } else if (stream.match(/^(?:=:=|:-|@<|@>|@=<|@>=|[-+\\/><=*#$?^])/)) {
                  return "operator";
                } else if (stream.match(/^[\s()\[\]{},\.|;@]+/)) {
                  return null;
                } else if (stream.match(/^:+/)) {
                  // ":" needs to be checked on its own
                  // Otherwise, if ":-" occurs after one of the characters above (e.g. ")"), ":" would not be highlighted correctly
                  return null;
                } else {
                  const atom_or_variable = stream.match(/^[A-Za-z_]+[A-Za-z_0-9]*/);
                  if (atom_or_variable && atom_or_variable.toString() !== "") {
                    var firstCharacter = atom_or_variable.toString().charAt(0);
                    if (firstCharacter !== "_" && firstCharacter === firstCharacter.toLowerCase()) {
                      // If the token starts with a lower case letter, it is an atom
                      if (stream.peek() === "(") {
                        // Atoms which are preceded by "(" are highlighted differently
                        return "builtin";
                      } else {
                        return "atom";
                      }
                    } else {
                      return "variable-2";
                    }
                  } else {
                    // Consume the rest of the line and mark it as an error
                    stream.match(/^.+/);
                    return "error";
                  }
                }

              case "comment":
                while (!stream.eol()) {
                  // Consume everything except for "*"
                  stream.match(/^[^\*]+/);
                  if (stream.match(/^\*\//)) {
                    // "*/" -> switch back to initial state
                    state.state = "initial";
                    return "comment";
                  } else {
                    // "*" without "/" -> consume and stay in comment state
                    stream.match(/^\*/);
                  }
                }
                return "comment";

              default:
                throw new Error("Unhandled state: " + state.state);
            }
          },
        };
      });

      CodeMirror.defineMIME("text/x-prolog", "prolog");
    },
  };
});
