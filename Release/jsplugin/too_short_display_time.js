// Too short display time
// (christophe.paris <at> free.fr)

LoadScript("common/tools.js");

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Too short display time',
  Description : 'An error is detected when the number of Char/s is strictly '+
  	'superior to the specified value. ',
  Color : 0xFFC437,
  Message : 'Subtitle display time is too short :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamMaxCPS : { Value : 25, Unit : 'Char/s', Description : 'Check for subtitle with Char/s > to this (0 to disable)' },
  ParamMaxRS : { Value : 35, Unit : '' , Description : 'Check for subtitle with reading speed > to this (0 to disable)' },

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    // Check for duration
    var durMS = CurrentSub.Stop - CurrentSub.Start;
    if (durMS < VSSCore.MinimumDuration) {
      return decimal1Round(durMS) + 'ms';
    }
    
    // Check for characters per second
    var len = CurrentSub.StrippedText.length;
    var durSec = durMS / 1000;
    var charPerSec = len / durSec;
    if (charPerSec > 0 && charPerSec > this.ParamMaxCPS.Value) {
      return decimal1Round(charPerSec) + ' ' + this.ParamMaxCPS.Unit;
    }
    
    // Check for reading speed
    var rs = len / (durSec - 0.5);
    if (rs > 0 && rs > this.ParamMaxRS.Value) {
      return 'rs: ' + decimal1Round(rs);
    }
    
    return '';
  }
};
