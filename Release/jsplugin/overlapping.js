// Overlapping
// (christophe.paris <at> free.fr)

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Overlapping',
  Description : 'An error is detected when the subtitle overlap on next subtitle.',
  Color : 0xFF3737, 
  Message : 'Subtitle overlap on next subtitle :',

  // ----- HasError method called for each subtitle during the error checking -----
  // If there is an error on CurrentSub return a string containing the error description.
  // Otherwise return an empty string.
  // Don't forget that PreviousSub and NextSub can be null
  HasError : function(CurrentSub, PreviousSub, NextSub) {
    if ((NextSub != null) && (CurrentSub.Stop >= NextSub.Start)) {
    	return ((CurrentSub.Stop - NextSub.Start) + 'ms overlap');
    } else {
    	return '';
    }
  }
}
