// French typography
// (christophe.paris <at> free.fr)
// thyresias <at> gmail.com (www.calorifix.net)
// Nathbot

var DebugMode = false;

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'French typography',
  Description : 'Check text against french typography rules.',
  Color : 0xAF0000,
  Message : 'Subtitle has typo :',
  
  // We use a table of rules to define the typography
  // Each rule is defined by those field :
  //   - re : a regular expression
  //   - msg : a message to display when the text match
  //   - replaceby (optional) : a replace expression used to fix the error
  //   - exception (optional) : if set to true the processing will stop on
  //     this rule and no replacement will be made (msg can be used for debugging)
  Rules : new Array(
    { re : /[\t\v\f]+/mg, msg : "Caractère d'espacement interdit (Tab, VT, FF)", replaceby: " "},
    { re : /^[ \u00A0\u2028\u2029]+/mg, msg : "Pas d'espace en début ligne", replaceby: ""},
    { re : /[ \u00A0\u2028\u2029]+$/mg, msg : "Pas d'espace en fin de ligne", replaceby: ""},
    { re : /[ \u00A0\u2028\u2029]{2,}/mg, msg : "Pas plus d'un espace consécutif", replaceby: " "},
    { re : /([^-])\s+([.,])/mg, msg : "Pas d'espace avant '.' ou ','", replaceby: "$1$2"},
    { re : /(http:\/\/[^\s\)]+)/mg, msg : "Ignorer les points dans les URL (1)", replaceby: "[url1=$1]", exception: true},
    { re : /(\w)([?!:;]+)/mg, msg : "Un espace avant '?', ':', ';' ou '!' (1)", replaceby: "$1 $2"},
    { re : /([?!:;]+)(\b)/mg, msg : "Un espace après '?', ':', ';' ou '!' (1)", replaceby: "$1 $2"},
    // { re : /[\f\t\u2028\u2029]+([?!:;]+)/mg, msg : "Un espace avant '?', ':', ';' ou '!' (2)", replaceby: " $1"},
    { re : /^-(\S)/mg, msg : "Un espace après un '-' en début de ligne", replaceby: "- $1"},
    { re : /''/mg, msg : "Double apostrophe", replaceby: "\""},
    { re : /(\s'\s)|(\s')|('\s)/mg, msg : "Pas d'espace avant et après une apostrophe", replaceby: "'"},
    { re : /"(\w+)"(\w)/mg, msg : "Un espace après des guillemets", replaceby: "\"$1\" $2"},
    { re : /(\w)"(\w+)"/mg, msg : "Un espace avant des guillemets", replaceby: "$1 \"$2\""},
    { re : /^\.\.([^.])/mg, msg : "Signe de ponctuation invalide '..' (1)", replaceby: "...$1"},
    { re : /([^.])\.\.([^.])/mg, msg : "Signe de ponctuation invalide '..' (2)", replaceby: "$1...$2"},
    { re : /([^.])\.\.$/mg, msg : "Signe de ponctuation invalide '..' (3)", replaceby: "$1..."},
    { re : /\.{4,}/mg, msg : "Signe de ponctuation invalide '....'", replaceby: '...'},
    { re : /\.{3}\b/mg, msg : "Un espace après '...'", replaceby: '... '},
    { re : /(www.[^\s)]+)/mg, msg : "Ignorer les points dans les URL (2)", replaceby: "[url2=$1]", exception: true},
    { re : /\b(([A-Z]\.){2,})\B/mg, msg : "Ignorer les points dans les acronymes", replaceby: "[acro=$1]", exception: true},
    //{ re : /([0-9]+[.,])\s+([0-9]+)/mg, msg : "Pas d'espace dans un nombres", replaceby: "$1$2"}, // fonctionne pas pour : "50, 75 kg à peu près."
    { re : /(…)/mg, msg : "Points de suspension en un seul caractère", replaceby: "..."},
    { re : /(’)/mg, msg : "Curly quote --> simple quote", replaceby: "'"},
    { re : /[A-Z]{2,}[a-z]{2,}/mg, msg : "Erreur de majuscule"},
    { re : /([0-9]+[.,][0-9]+)/mg, msg : "Ignorer points et virgules dans les nombres", replaceby: "[nombre=$1]", exception: true},
    { re : /([.,:])\b/mg, msg : "Un espace après un '.', ':' ou ','", replaceby: "$1 "}
  ),
  
  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    var SubText = CurrentSub.Text;
    for (var i = 0; i < this.Rules.length; i++) {
      if (this.Rules[i].re.test(SubText)) {
        // Reset the regexp
        this.Rules[i].re.lastIndex = 0;
        // Print some debug info
        if (DebugMode && this.Rules[i].replaceby !== null) {
          ScriptLog(SubText.replace(this.Rules[i].re, this.Rules[i].replaceby));
          ScriptLog('');
        }
        return (this.Rules[i].exception) ? '' : this.Rules[i].msg;
      }
    }
    return '';
  },
  
  FixError : function(CurrentSub, PreviousSub, NextSub) {
    var SubText = CurrentSub.Text;
    for (var i = 0; i < this.Rules.length; i++) {
      if ((this.Rules[i].replaceby !== null) && (this.Rules[i].re.test(SubText))) {
        if (!this.Rules[i].exception) {
          CurrentSub.Text = SubText.replace(this.Rules[i].re, this.Rules[i].replaceby);
        }
        // Reset the regexp
        this.Rules[i].re.lastIndex = 0;
        return;
      }
    }
  }
};

// ---------------------------------------------------------------------------
// Test
// ---------------------------------------------------------------------------

function assertEqual(ExpectedStr, Str) {
  if (ExpectedStr !== Str) {
    ScriptLog("assertion failed, expected [" + ExpectedStr + "] but was [" + Str + "]");
  }
}

function TestFixError(SrcText, ExpectedResult) {
  var DummySubtitle = new Object();
  DummySubtitle.Text = SrcText;
  VSSPlugin.FixError(DummySubtitle, null, null);
  assertEqual(ExpectedResult, DummySubtitle.Text);
}

function TestHasError(SrcText, ExpectedResult) {
  var DummySubtitle = new Object();
  DummySubtitle.Text = SrcText;
  assertEqual(VSSPlugin.HasError(DummySubtitle, null, null), ExpectedResult);
}

function TestPlugin() {
  // { re : /[\t\v\f]+/mg, msg : "Caractère d'espacement interdit (Tab, VT, FF)", replaceby: " "},
  TestFixError("Pas de\ttabulation",
               "Pas de tabulation");
  
  // { re : /^[ \u00A0\u2028\u2029]+/mg, msg : "Pas d'espace en début ligne", replaceby: ""},
  TestFixError("1 espace\n en début de ligne.",
               "1 espace\nen début de ligne.");

  TestFixError(" 1 espace\nen début de ligne.",
               "1 espace\nen début de ligne.");

  TestFixError("1 espace\n \nen début de ligne.",
               "1 espace\n\nen début de ligne.");

  // { re : /[ \u00A0\u2028\u2029]+$/mg, msg : "Pas d'espace en fin de ligne", replaceby: ""},
  TestFixError("1 espace \nen fin de ligne.",
               "1 espace\nen fin de ligne.");
               
  TestFixError("1 espace\nen fin de ligne. ",
               "1 espace\nen fin de ligne.");

  // { re : /[ \u00A0\u2028\u2029]{2,}/mg, msg : "Pas plus d'un espace consécutif", replaceby: " "},
  TestFixError("2  espaces.",
               "2 espaces.");

  // { re : /([^-])\s+([.,])/mg, msg : "Pas d'espace avant '.' ou ','", replaceby: "$1$2"},
  TestFixError("1 espace avant . un point.",
               "1 espace avant. un point.");

  TestFixError("1 espace avant un point .",
               "1 espace avant un point.");

  TestFixError("1 espace avant , une virgule.",
               "1 espace avant, une virgule.");

  // { re : /(http:\/\/[^\s\)]+)/mg, msg : "Ignorer les points dans les URL (1)", replaceby: "[url1=$1]", exception: true},
  TestFixError("http://visualsubsync.com",
               "http://visualsubsync.com");

  // { re : /(\w)([?!:;]+)/mg, msg : "Un espace avant '?', ':', ';' ou '!' (1)", replaceby: "$1 $2"},
  TestFixError("pas d'espace avant deux-points:",
               "pas d'espace avant deux-points :");
               
  TestFixError("pas d'espace: avant deux-points",
               "pas d'espace : avant deux-points");

  // { re : /([?!:;]+)(\b)/mg, msg : "Un espace après '?', ':', ';' ou '!' (1)", replaceby: "$1 $2"},
  TestFixError("1 espace :après deux-points",
               "1 espace : après deux-points");
               
  TestFixError("1 espace après deux-points : ",
               "1 espace après deux-points :");
  
  // { re : /^-(\S)/mg, msg : "Un espace après un '-' en début de ligne", replaceby: "- $1"},
  TestFixError("-1 espace après un tiret.",
               "- 1 espace après un tiret.");
               
  // { re : /''/mg, msg : "Double apostrophe", replaceby: "\""}
  TestFixError("Double ''apostrophe",
               "Double \"apostrophe");

  // { re : /(\s'\s)|(\s')|('\s)/mg, msg : "Pas d'espace avant et après une apostrophe", replaceby: "'"},
  TestFixError("Pas d 'espace avant une apostrophe.",
               "Pas d'espace avant une apostrophe.");
  
  TestFixError("Pas d' espace après une apostrophe.",
               "Pas d'espace après une apostrophe.");
               
  // { re : /"(\w+)"(\w)/mg, msg : "Un espace après des guillemets", replaceby: "\"$1\" $2"},
  TestFixError("Pas d'espace \"après\"des guillemets.",
               "Pas d'espace \"après\" des guillemets.");

  // { re : /(\w)"(\w+)"/mg, msg : "Un espace avant des guillemets", replaceby: "$1 \"$2\""},
  TestFixError("Pas d'espace\"avant\" des guillemets.",
               "Pas d'espace \"avant\" des guillemets.");

  // { re : /^\.\.([^.])/mg, msg : "Signe de ponctuation invalide '..' (1)", replaceby: "...$1"},
  TestFixError(".. manque un point.",
               "... manque un point.");

  // { re : /([^.])\.\.([^.])/mg, msg : "Signe de ponctuation invalide '..' (2)", replaceby: "$1...$2"},
  TestFixError("manque..un point.",
               "manque...un point.");
               
  // { re : /([^.])\.\.$/mg, msg : "Signe de ponctuation invalide '..' (3)", replaceby: "$1..."},
  TestFixError("manque un point..",
               "manque un point...");

  // { re : /\.{4,}/mg, msg : "Signe de ponctuation invalide '....'", replaceby: '... '},   
  TestFixError("trop de points....",
               "trop de points...");

  // { re : /\.{3}\b/mg, msg : "Un espace après '...'", replaceby: '... '},
  TestFixError("espace après...points de suspension",
               "espace après... points de suspension");
               
  // { re : /(www.[^\s)]+)/mg, msg : "Ignorer les points dans les URL (2)", replaceby: "[url2=$1]", exception: true},
  TestFixError("www.visualsubsync.com",
               "www.visualsubsync.com");
               
  // { re : /\b(([A-Z]\.){2,})\B/mg, msg : "Ignorer les points dans les acronymes", replaceby: "[acro=$1]", exception: true},
  TestFixError("F.B.I.",
               "F.B.I.");

  // { re : /(…)/mg, msg : "Points de suspension en un seul caractère", replaceby: "..."},               
  TestFixError("Points de suspension…",
               "Points de suspension...");

  // { re : /(’)/mg, msg : "Curly quote --> Regular", replaceby: "'"}               
  TestFixError("Curly ’quote’",
               "Curly 'quote'");
               
  // { re : /[A-Z]{2,}[a-z]{2,}/mg, msg : "Erreur de majuscule"}               
  // no fix available, just test if the type of error is detected
  TestHasError("Erreur de MAjuscule", "Erreur de majuscule");
               
  // { re : /([0-9]+[.,][0-9]+)/mg, msg : "Ignorer points et virgules dans les nombres", replaceby: "[nombre=$1]", exception: true},
  TestFixError("10.3",
               "10.3");
  
  // { re : /([.,:])\b/mg, msg : "Un espace après un '.', ':' ou ','", replaceby: "$1 "},
  TestFixError("1 espace après.un point.",
               "1 espace après. un point.");
               
  TestFixError("1 espace après :deux-points",
               "1 espace après : deux-points");

}

//TestPlugin();