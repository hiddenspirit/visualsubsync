// status bar display
// thyresias <at> gmail.com (www.calorifix.net)
// 20-Jan-2007  first version

LoadScript("../common/tools.js");

// ---------------------------------------------------------------------------
// ASCII bar stuff
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------

function statusBarText(Sub) {

    // current length, duration
    var len = Sub.StrippedText.length;
    var durMs = Sub.Stop - Sub.Start;
    var durS = decimal1Round(durMs / 1000);

    // display speed
    var dsMin = 4;
    var dsMax = 22;
    var dsX = len * 1000 / durMs;       // exact
    var ds = decimal1Round(dsX);

    // reading speed
    var rsMin = 5;
    var rsMax = 35;
    if (durMs < 500) {
      durMs = 500;
    }    
    var rsX = len * 1000 / (durMs - 500);
    var rs = decimal1Round(rsX);

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
    if (templateBarDS === null) templateBarDS = initBar(dsMin, dsMax);
    if (templateBarRS === null) templateBarRS = initBar(rsMin, rsMax);

    // get display bars
    var barDS = getBar(dsX, dsMin, dsMax, templateBarDS);
    var barRS = getBar(rsX, rsMin, rsMax, templateBarRS);

    // compute Lavie duration
    var rsIdeal = 20;
    var durLavie = 0.5 + len / rsIdeal;
    durLavie = decimal1Round(durLavie);

    return "DS: " + ds + " " + barDS +
      "  |  RS: " + rs + " " + barRS +
      "  |  Duration: " + durS + " (ideal: " + durLavie + ")  |  " + rating;

}

// ---------------------------------------------------------------------------

function getReadingSpeed(Sub) {
  var len = Sub.StrippedText.length;
  var durMs = Sub.Stop - Sub.Start;
  if (durMs < 500) {
    durMs = 500;
  }
  var rs = (len * 1000) / (durMs - 500);
  return rs;
}

// ---------------------------------------------------------------------------

function getReadingSpeedColor(Sub) {
  var rs = getReadingSpeed(Sub);
  
  // Pure green hue = 120° (red = 0°, blue = 240°)
  // Base color : 0x99FF99

  if (rs < 5)       return 0x9999FF;  // 240° "TOO SLOW!"
  else if (rs < 10) return 0x99CCFF;  // 210° "Slow, acceptable.";
  else if (rs < 13) return 0x99FFFF;  // 180° "A bit slow.";
  else if (rs < 15) return 0x99FFCC;  // 150° "Good."
  else if (rs < 23) return 0x99FF99;  // 120° "Perfect.";
  else if (rs < 27) return 0xCCFF99;  //  90° "Good.";
  else if (rs < 31) return 0xFFFF99;  //  60° "A bit fast.";
  else if (rs < 35) return 0xFFCC99;  //  30° "Fast, acceptable.";
  else              return 0xFF9999;  //   0° "TOO FAST!";
}

// ---------------------------------------------------------------------------

function getReadingSpeedText(Sub) {
  var rs = getReadingSpeed(Sub);  
  var rsRounded = decimal1Round(rs);
  return '' + rsRounded;
}

// ---------------------------------------------------------------------------

VSSPlugin = {

  // Called on subtitle modifications (time or text)
  OnSubtitleModification : function(CurrentSub, PreviousSub, NextSub) {
    SetStatusBarText(statusBarText(CurrentSub));
  },

  // Called when the selected subtitle change
  OnSelectedSubtitle : function(CurrentSub, PreviousSub, NextSub) {
    SetStatusBarText(statusBarText(CurrentSub));
  },
  
  // COLUMNS -----------------------------------------------------------------

  // VSS core columns index
  // VSSCore.INDEX_COL_IDX : Index of the subtitle index column
  // VSSCore.START_COL_IDX : Index of the start time column
  // VSSCore.STOP_COL_IDX  : Index of the stop time colum
  // VSSCore.STYLE_COL_IDX : Index of the style column (SSA/ASS only)
  // VSSCore.TEXT_COL_IDX  : Index of the text column
  //
  // VSSCore.LAST_CORE_COL_IDX : Index of the last column of VSS core
  //
  
  // Declare extra column index here
  RS_COL_IDX : VSSCore.LAST_CORE_COL_IDX + 1, // Reading speed
  
  // Get the number of extra-columns (called only at VSS startup)
  GetExtraColumnsCount : function() {
    return 1;
  },
  
  // Get the title of each extra-column (called only at VSS startup)
  GetColumnTitle : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX: return 'RS';
      default: return '';
    }
  },
  
  // Get the size of each extra-column (called only at VSS startup)
  GetColumnSize : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX: return 40;
      default: return '';
    }
  },

  // Check if a column background can be colorized (called only at VSS startup)
  IsColumnBGColorized : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX:    return true;
      default: return false;
    }
  },
  
  // Check if a column has custom text (called only at VSS startup)
  HasColumnCustomText : function(Index) {
    switch(Index) {
      case this.RS_COL_IDX:    return true;
      default: return false;
    }
  },  
  
  // Get the column background color (called on each cell repaint)
  GetColumnBGColor : function(Index, CurrentSub, PreviousSub, NextSub) {
    switch(Index) {
      case this.RS_COL_IDX: return getReadingSpeedColor(CurrentSub);
      default: return 0xffffff;
    }
  },
  
  // Get the text of the extra-column (called on each cell repaint)
  GetColumnText : function(Index, CurrentSub, PreviousSub, NextSub) {
    switch(Index) {
      case this.RS_COL_IDX: return getReadingSpeedText(CurrentSub);
      default: return '';
    }
  }
  
};