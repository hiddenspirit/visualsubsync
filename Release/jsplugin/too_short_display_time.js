VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Too short display time',
  Description : 'An error is detected when the number of Char/s is strictly superior to the specified value.',
  Color : 0xFFC437,
  Message : 'Subtitle display time is too short :',

  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamMaxCPS : { Value : 25, Unit : 'Char/s' },

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    Duration = CurrentSub.Stop - CurrentSub.Start;
    CharPerSec = (CurrentSub.Text.length * 1000) / Duration;
    if (CharPerSec > this.ParamMaxCPS.Value) {
    	return (Math.round(CharPerSec) + ' ' + this.ParamMaxCPS.Unit);
    } else {
    	return '';
    }
  }
}
