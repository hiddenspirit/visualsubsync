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
    var rating = getReadingSpeedRating(rs);

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

//var readingSpeed = new Array(5, 10, 13, 15, 23, 27, 31, 35);

function getReadingSpeedAsColor(Sub) {
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

function getReadingSpeedAsText(Sub) {
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
      case this.RS_COL_IDX: return getReadingSpeedAsColor(CurrentSub);
      default: return 0xffffff;
    }
  },
  
  // Get the text of the extra-column (called on each cell repaint)
  GetColumnText : function(Index, CurrentSub, PreviousSub, NextSub) {
    switch(Index) {
      case this.RS_COL_IDX: return getReadingSpeedAsText(CurrentSub);
      default: return '';
    }
  }
  
};

// ---------------------------------------------------------------------------

var ReadingSpeedDef = [
  { value: 5,  text: 'TOO SLOW!'},
  { value: 10, text: 'Slow, acceptable.'},
  { value: 13, text: 'A bit slow.'},
  { value: 15, text: 'Good.'},
  { value: 23, text: 'Perfect.'},
  { value: 27, text: 'Good.'},
  { value: 31, text: 'A bit fast.'},
  { value: 35, text: 'TOO FAST!'}
];

function getReadingSpeedIndex(rs) {
  for (var i = 0; i < ReadingSpeedDef.length; i++) {
    if (rs < ReadingSpeedDef[i].value) {
      return i;
    }
  }
  return (ReadingSpeedDef.length - 1);
}

function JSAction_QuickStats() {
  var subCount = VSSCore.GetSubCount();
  ScriptLog('Subtitle count : ' + subCount);

  // Table to store results
  var rsArray = new Array(ReadingSpeedDef.length);
  for (var i = 0; i < rsArray.length; i++) {
    rsArray[i] = 0;
  }
  
  // Iterate over all subtitles
  /*
  var sub = VSSCore.GetFirst();
  while (sub != null) {
    var rs = getReadingSpeed(sub);
    var rsIdx = getReadingSpeedIndex(rs);
    rsArray[rsIdx]++;
    sub = VSSCore.GetNext(sub);
  }*/
  
  for (var i = 0; i < subCount; i++) {
    var sub = VSSCore.GetSubAt(i);
    var rs = getReadingSpeed(sub);
    var rsIdx = getReadingSpeedIndex(rs);
    rsArray[rsIdx]++;
  }
  
  // Display results
  for (var i = 0; i < rsArray.length; i++) {
    var rsCount = rsArray[i];
    var rsCountPercent = (rsCount * 100) / subCount;
    ScriptLog(ReadingSpeedDef[i].text + ' = ' + decimal1Round(rsCountPercent) + '% (' + rsCount + ')');
  }
  
  /*
  var subtitleAt0 = VSSCore.GetSubAt(0);
  ScriptLog('at 0 = ' + ((subtitleAt0 != null) ? subtitleAt0.Text : 'null'));
  
  var firstSubtitle = VSSCore.GetFirst();
  ScriptLog('first = ' + ((firstSubtitle != null) ? firstSubtitle.Text : 'null'));

  var nextSubtitle = VSSCore.GetNext(firstSubtitle);
  ScriptLog('next = ' + ((nextSubtitle != null) ? nextSubtitle.Text : 'null'));
  
  var selectedCursor = VSSCore.GetFirstSelected();
  while (selectedCursor != null) {
    ScriptLog('selected = (' + selectedCursor.Index + ') ' + selectedCursor.Text);
    selectedCursor = VSSCore.GetNextSelected(selectedCursor);
  }
  */
}

VSSCore.RegisterJavascriptAction('JSAction_QuickStats', 'Quick stats', 'Ctrl+M');

// ---------------------------------------------------------------------------