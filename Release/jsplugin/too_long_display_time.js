// Too long display time
// (christophe.paris <at> free.fr)

LoadScript("common/tools.js");

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Too long display time',
  Description : 'An error is detected when the number of Char/s is strictly '+
  	'inferior to the specified value.',
  Color : 0x3787FF, 
  Message : 'Subtitle display time is too long :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamMinCPS : { Value : 4, Unit : 'Char/s', Description : 'Check for subtitle with Char/s < to this (0 to disable)' },
  ParamMinTime : { Value : 10, Unit : 's', Description : 'Check for subtitle with time > to this (0 to disable)' },
  ParamMinRS : { Value : 5, Unit : '' , Description : 'Check for subtitle with reading speed < to this (0 to disable)' },
  ParamIgnoreLinesOf : { Value : 0, Unit : 'Characters' },

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    // Ignore lines with length < to this.ParamIgnoreLinesOf.Value
    var len = CurrentSub.StrippedText.length;
    if(len <= this.ParamIgnoreLinesOf.Value) {
        return '';
    }
    
    // Check for duration
    var durSec = (CurrentSub.Stop - CurrentSub.Start) / 1000;
    if (this.ParamMinTime.Value > 0 && durSec > this.ParamMinTime.Value) {
      return decimal1Round(durSec) + ' ' + this.ParamMinTime.Unit;
    }
    
    // Check for characters per second
    var charPerSec = len / durSec;
    if (charPerSec < this.ParamMinCPS.Value) {
      return decimal1Round(charPerSec) + ' ' + this.ParamMinCPS.Unit;
    }
    
    // Check for reading speed
    var rs = len / (durSec - 0.5);
    if (rs < this.ParamMinRS.Value) {
      return 'rs: ' + decimal1Round(rs);
    }
    
    return '';
  }
};