// status bar display
// thyresias <at> gmail.com (www.calorifix.net)
// 20-Jan-2007  first version

var templateBarDS = null;
var templateBarRS = null;

function initBar(min, max) {
    var bar = "·····················································";
    var length = max - min + 1;
    for (; bar.length < length ;)
      bar = bar + bar;
    return bar.substr(0, length);
}

function getBar(value, min, max, templateBar) {
    // ~{|}[\]^_`:<=>-+!#%&‘’¡¤§«¬­¯°±´º»
    var iVal = Math.round(value);
    // below min: *  ·························
    if (iVal < min)
      return "«" + templateBar;
    // above max: ·························  *
    else if (iVal > max)
      return templateBar + "»";
    // in the range
    else {
      iVal = iVal - min;
      return templateBar.substr(0, iVal-1) + "¦" + templateBar.substr(iVal+1);
    }
}

function statusBarText(Sub) {

    // current length, duration
    var len = Sub.StrippedText.length;
    var durMS = Sub.Stop - Sub.Start;     // in milliseconds
    var dur = Math.round(durMS/100) / 10; // in seconds, rounded to 1 decimal digit

    // display speed
    var dsMin = 4;
    var dsMax = 22;
    var dsX = len * 1000 / durMS;       // exact
    var ds = Math.round(dsX*10) / 10;   // rounded to 1 decimal digit

    // reading speed
    var rsMin = 5;
    var rsMax = 35;
    var rsX = len * 1000 / (durMS - 500);
    var rs = Math.round(rsX*10) / 10;

    // rating
    var rating;
    if (rs < rsMin)   rating = "TOO SLOW!";
    else if (rs < 10) rating = "Slow, acceptable.";
    else if (rs < 13) rating = "A bit slow.";
    else if (rs < 15) rating = "Good.";
    else if (rs < 23) rating = "Perfect.";
    else if (rs < 27) rating = "Good.";
    else if (rs < 31) rating = "A bit fast.";
    else if (rs < 35) rating = "Fast, acceptable.";
    else              rating = "TOO FAST!";

    // initialize template bars if needed
    if (templateBarDS == null) templateBarDS = initBar(dsMin, dsMax);
    if (templateBarRS == null) templateBarRS = initBar(rsMin, rsMax);

    // get display bars
    var barDS = getBar(dsX, dsMin, dsMax, templateBarDS);
    var barRS = getBar(rsX, rsMin, rsMax, templateBarRS);

    // compute Lavie duration
    var rsIdeal = 20;
    var durLavie = 0.5 + len / rsIdeal;
    durLavie = Math.round(durLavie*10) / 10;

    return "DS: " + ds + " " + barDS
      + "  |  RS: " + rs + " " + barRS
      + "  |  Duration: " + dur + " (ideal: " + durLavie + ")  |  " + rating;

}

VSSPlugin = {

  OnSubtitleModification : function(CurrentSub, PreviousSub, NextSub) {
    SetStatusBarText(statusBarText(CurrentSub));
  },

  OnSelectedSubtitle : function(CurrentSub, PreviousSub, NextSub) {
    SetStatusBarText(statusBarText(CurrentSub));
  },

  // ----------

  ExtraColumns : {
    Col0 : {Name : 'RS', Size : 40}
  },
  
  GetExtraColumnText : function(Index, CurrentSub) {
    switch(Index) {
      case 0: return 'Col0';
    }
  },

  GetExtraColumnColor : function(Index, CurrentSub) {
    switch(Index) {
      case 0: return 0xff0000;
    }
  }

}