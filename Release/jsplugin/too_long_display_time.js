// Too long display time
// (christophe.paris <at> free.fr)

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Too long display time',
  Description : 'An error is detected when the number of Char/s is strictly '+
  	'inferior to the specified value.',
  Color : 0x3787FF, 
  Message : 'Subtitle display time is too long :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamMinCPS : { Value : 4, Unit : 'Char/s' },
  ParamIgnoreLinesOf : { Value : 0, Unit : 'Characters' },

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    // Ignore lines with length < to this.ParamIgnoreLinesOf.Value
    if(CurrentSub.StrippedText.length <= this.ParamIgnoreLinesOf.Value)
    {
        return '';
    }
    
    Duration = CurrentSub.Stop - CurrentSub.Start;
    CharPerSec = (CurrentSub.StrippedText.length * 1000) / Duration;
    if (CharPerSec < this.ParamMinCPS.Value) {
    	return (Math.round(CharPerSec) + ' ' + this.ParamMinCPS.Unit);
    } else {
    	return '';
    }
  }
};