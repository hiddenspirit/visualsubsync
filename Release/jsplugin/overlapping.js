// Overlapping
// (christophe.paris <at> free.fr)

VSSPlugin = {
  // ----- Plugin constant -----
  Name : 'Overlapping',
  Description : 'An error is detected when the subtitle overlap on next subtitle.',
  Color : 0xFF3737, 
  Message : 'Subtitle overlap on next subtitle :',
  
  // ----- Plugin parameters available from VSS GUI (name must start with "Param") -----
  ParamFixableOverlap : { Value : 100, Unit : 'ms' },

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
  },
  
  FixError : function(CurrentSub, PreviousSub, NextSub) {
    if ((NextSub != null) && (CurrentSub.Stop >= NextSub.Start)) {
    	var OverlapInMs = (CurrentSub.Stop - NextSub.Start);    	
    	if(OverlapInMs <= this.ParamFixableOverlap.Value) {
    		// Fix the overlap by dividing it by 2
    		CurrentSub.Stop -= (OverlapInMs / 2);
    		NextSub.Start += ((OverlapInMs / 2) + 1);
    	}
    }  	
  }
}
